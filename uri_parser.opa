/**
 * A module with parsing rules for URIs.
 */
UriParser =
{{

  /**
   * {2 High-level API}
   */

   /* TODO: this should do in most cases; however for full conformance with
      specification, confront this with RFC 3986 */

  /**
   * A parser for URIs.
   * Parser follows URI format, as outlined  at:
   * {{:http://en.wikipedia.org/wiki/URI_scheme#Generic_syntax} URI scheme: Generic syntax (Wikipedia)}.
   * Also see {!Uri.uri} for details on the accepted format of URIs.
   */
  uri = parser
    | Rule.ws ~schema res={
        match schema with
        | {http} | {https} | {ftp}-> http_uri(some(schema2string(schema)))
        | {mailto} -> mailto_uri
      } -> res
    | Rule.ws ~path ~query? ~fragment? Rule.ws ->
      {path=path.path ~fragment query=query ? []
       is_from_root=path.is_from_root
       is_directory=path.is_directory
      }

  http_uri(schema) = parser
    credentials=authority? ~domain ~port? path={parser "/" p=path -> p}?
    ~query? ~fragment? Rule.ws ->
       { ~schema
       ; credentials=credentials ? {username=none password=none}
       ; ~domain
       ; ~port
       ; path=Option.map(_.path, path) ? []
       ; is_directory=Option.map(_.is_directory, path) ? false
       ; query=query ? []
       ; ~fragment
       } : Uri.uri

  mailto_uri =
    parser address=chars_mailto query=query? -> { ~address query=query ? [] }

  /**
   * {2 Low-level API}
   */

   /**
    * RFC 2396 http://www.ietf.org/rfc/rfc2396.txt
    * The angle-bracket "<" and ">" and double-quote (") characters are
    * excluded because they are often used as the delimiters around URI in
    * text documents and protocol fields.  The character "#" is excluded
    * because it is used to delimit a URI from a fragment identifier in URI
    * references (Section 4). The percent character "%" is excluded because
    * it is used for the encoding of escaped characters.
    *    delims = "<" | ">" | "#" | "%" | <">
    *
    * Other characters are excluded because gateways and other transport
    * agents are known to sometimes modify such characters, or they are
    * used as delimiters.
    *    unwise = "{" | "}" | "|" | "\" | "^" | "[" | "]" | "`"
    */

  unwise = parser t=([{}|\\^\[\]`"]+) -> Text.to_string(t); //"

  reserved = parser t=([;/?:@&=+$,]+) -> Text.to_string(t);

  mark = parser t=([\-_.!~*'()]+) -> Text.to_string(t);

  escaped = parser "%" h1=Rule.hexadecimal h2=Rule.hexadecimal ->
        String.of_utf8_val(h1 * 16 + h2)

  unreserved = parser t=((Rule.alphanum_char | mark)+) -> Text.to_string(t);

  uric = parser a=reserved -> a | a=unreserved -> a | a=escaped -> a;
  chars = parser v=uric+ -> String.flatten(v)

  uric_query = parser a=unreserved -> a | a=escaped -> a | t=([/:@+$, ]) -> Text.to_string(t) | a=unwise -> a;
  chars_query = parser v=uric_query+ -> String.flatten(v)

  uric_path = parser a=unreserved -> a | a=escaped -> a;
  chars_path = parser v=uric_path+ -> String.flatten(v)

  uric_mailto = parser a=uric -> a | t=("@") -> Text.to_string(t);
  chars_mailto = parser v=uric_mailto+ -> String.flatten(v)

  schema2string =
    | {mailto} -> "mailto"
    | {http} -> "http"
    | {https} -> "https"
    | {ftp} -> "ftp"

  schema =
    // TODO do we want to include other schemas here?
    schema_name = parser
    | "ftp" -> {ftp}
    | "https" -> {https}
    | "http" -> {http}
    | "mailto" -> {mailto}
    parser schema=schema_name ":" "//"? -> schema

  authority =
    pwd_parser = parser ":" pwd=Rule.alphanum_string -> pwd
    parser login=Rule.alphanum_string pwd=pwd_parser? "@" -> {username=some(login) ; password=pwd}

  port = parser ":" portno=Rule.natural -> portno

  path =
     // apart from alphanumerical characters we also allow tilde in path segments
     // TODO This string/text distinction in parsers is just not good...
    path_chars = parser v=chars_path -> v
    path_content = Rule.parse_list_sep(false, path_chars, parser [/])
    have_slash = parser r=([/]+)? -> Option.is_some(r)
    parser
       is_from_root=have_slash v=path_content is_directory=have_slash ->
       f(item, acc) =
          match item with
          | "" | "." -> acc //Eliminate noop
          | ".." -> match acc with
             | []     -> [] //Don't climb higher than root
             | [_|tl] -> tl
           end
          | _ -> [item|acc]
       end
       ~{is_from_root is_directory
         path = List.rev(List.fold(f, v, []))}

   /**
    * Domain names are restricted to the 26 characters in the English language, plus numbers.
    * The only "special" characters outside the basic alpha-numeric sets are hyphens.
    * You may use hyphens in a domain name as a method of separating words.
    * Domain names can optionally comprise entirely of letters or numbers.
    * They must not begin or end with a hyphen.
    * One restriction applied to domain names is a limit of 63 characters before the "TLD extension".
    * TLD extensions are the ".com", ".net", or country code like ".com.au" part of the domain name.
    * The "http://www." part of the domain name is not included in the character count.
    * Domain names are also case-insensitive.
    * Upper-case, lower-case, and mixed-case domain names all point to the one site.
    * While it is conventional to display names in lower-case, mixed-case can highlight word separation.
    * Eg. JohnSimmonsBikes.com
    *
    * (from http://www.servergrade.com.au/faq/answers/domain-letters-numbers.html)
    */
  domain =
     /* domain segment is a sequence of alphanumerical characters,
        possibly separated by hyphens (-) (but no hyphen at the
        beginning nor end of the segment is allowed) */
    domain_segment =
      dsp = Rule.parse_list_non_empty(Rule.alphanum_string, parser [\-])
      parser ls=dsp -> List.to_string_using("", "", "-", ls)
     /* domain is a list of domain segments (1 or more) separated with dots (.) */
    segs = Rule.parse_list_non_empty(domain_segment, parser [.])
    parser ls=segs -> List.to_string_using("", "", ".", ls)

  query_element = parser 
                   | key=chars_query "=" value=chars_query -> (key, value)
                   | key=chars_query -> (key, "")
  query_parser = Rule.parse_list(query_element, parser [&;] -> void)

  query =
    parser 
     | "?" "&"? query=query_parser -> query

  fragment = parser
    | "#" fragment=chars -> fragment
    | "#" -> "" // fragment can be empty

}}
