// @author Matthieu Guffroy
// REST Interface

type Rest.Add.pages = {urls : list(string)}
type Rest.Add.links = {url : string links : list(string)}

Rest = {{

	/**
	Extraire le json du body
	*/
	@private extract_json_from_body() =
	    match HttpRequest.get_body() with
	     | {none} -> {failure="no body"}
	     | {some=raw_body} -> 
	         match Json.deserialize(raw_body) with
		  | {none} -> {failure= "impossible de convertir en json {raw_body}"}
		  | {some=jsast} -> {success= OpaSerialize.Json.unserialize_unsorted(jsast)}
		 end
	    end

	/**
	Ajouter des pages
	*/
	add_pages() =
		match extract_json_from_body() with
		 | {~failure} -> 
			do Log.error("REST add_pages", "add_page : {failure}")
			Resource.raw_status({bad_request})
		 | {success=opt} -> 
			match opt with
			 | {none} -> 
				do Log.error("REST add_pages", "add_page : le json ne correspond pas")
				Resource.raw_status({bad_request})
			 | {some=record : Rest.Add.pages} ->
				do _add_pages(record.urls)
				Resource.raw_status({success})
			end
		end

	/**
	Vérifier si une url a besoin d'être vérifié
	**/
	need_a_visit()=
	    match HttpRequest.get_body() with
	     | {~some} -> r = if Graphe.need_visit(some) then "true" else "false"
	                  Resource.raw_response(r, "text/plain", {success})
	     | {none} -> Resource.raw_response("false", "text/plain", {success})
	    end

	/**
		Ajouter des liens
	**/
	add_liens()=
	match extract_json_from_body() with
	 | {~failure} -> 
		do jlog("add_links : {failure}")
		Resource.raw_status({bad_request})
	 | {success=opt} -> 
		match opt with
		 | {none} -> 
			do jlog("add_links : le json ne correspond pas")
			Resource.raw_status({bad_request})
		 | {some=record : Rest.Add.links} ->
			do _add_liens(record.url, record.links)
			Resource.raw_status({success})
		end
	end

	/*
		Obtenir des urls a visiter
	*/
	get_urls()=
    	do jlog("a crawler ask url")
    	Resource.json(OpaSerialize.Json.serialize(Graphe.to_visit(10)))

	run(path) : resource =
    	match HttpRequest.get_method() with
     	| {some = method} -> 
     		match method with
       	    | {post} -> 
       	    	match path with
			    | "need_a_visit" -> need_a_visit()
			    | "add_pages" -> add_pages()
			    | "add_liens" -> add_liens()
				| _ -> Resource.raw_status({bad_request})
				end
	        | {get} -> 
	        	match path with
		     	| "get_urls" -> get_urls()
				| _ -> Resource.raw_status({bad_request})
				end
	  		| _ -> Resource.raw_status({method_not_allowed})
			end
     	| _ -> Resource.raw_status({bad_request})
}}
