import sigmajs
import stdlib.widgets.colorpicker

@publish room = Network.cloud("room"): Network.network(message)

type node = string
type nodes = list(node)
type edge = (string,string)
type edges = list(edge)
type message = {add_node : node} / {add_edge : edge} / {add_nodes : nodes} / {add_edges : edges}

type Domain.ref = string
type state_client = {all} / {domain : Domain.ref}

type Page.ref = string
type Page.data = {
    url : string
    liens : map(Domain.ref, map(Page.ref, bool))
}



db /graphe : map(Domain.ref, map(Page.ref, Page.data))

db /graphe[_][_]/liens[_][_] = { false }

url_to_domain_ref(u : string) = 
    url = Parser.try_parse(UriParser.uri, u)
    match url with
     | {some = s} -> match s : Uri.uri with
       	       	      | {domain = d ; ...} -> d
		      | _ -> do log_server("Url_to_domain failed (is not absolute) {u}")
		      	    	       	  ""
		     end
     | {none} -> do log_server("Url_to_domain failed {u}")
       	      	 ""

url_to_page_ref(u : string) = 
    url = Uri.of_string(u)
    path_to(p : list(string)) = List.fold((v, a -> "{v}/{a}"), p, "")
    match url with
     | {some = s} -> match s : Uri.uri with
       	       	      | {path = p ; ...} -> path_to(p)
		      | _ -> do log_server("Url_to_page_ref failed (is mailto) {u}")
		      	    	       	  ""
		     end
     | {none} -> do log_server("Url_to_page_ref failed {u}")
       	      	 ""

url_need_visit(url : string) : bool = 
    do jlog("url_need_visit {url}")
    d = url_to_domain_ref(url)
    p = url_to_page_ref(url)
    Db.exists(@/graphe[d][p]) && Map.is_empty(/graphe[d][p]/liens)

add_pages(urls : list(string)) =
    do jlog("add_pages {Debug.dump(urls)}")
    do Network.broadcast({add_nodes = urls}, room)
    List.iter((url ->  
    		      domain = url_to_domain_ref(url)
		      page = url_to_page_ref(url)
    		      path = @/graphe[domain][page]
		      if not(Db.exists(path)) then
		          do jlog("{url} n'existait pas comme noeud, le lien a été ajouté")
			      //do /to_visit <- List.append(url, /to_visit) 
		          Db.write(path, {~url liens=Map.empty})), urls)

add_liens(url:string, liens:list(string)) = 
    do add_pages(List.add(url, liens))
    do jlog("add_liens {url} ==> {Debug.dump(liens)}")
    domain = url_to_domain_ref(url)
    page = url_to_page_ref(url)
    make_link(l) = 
      d = url_to_domain_ref(l)
      p = url_to_page_ref(l)
      /graphe[domain][page]/liens[d][p] <- true
    do List.iter((url -> make_link(url)), liens)
    Network.broadcast({add_edges = List.map((lien -> (url, lien)),liens)}, room) 

urls_to_visit(limit : int) =
    fold_domains(_d, domain, acc) =
    (
        fold_pages(_p, page , acc) =
	    (
	     if Map.is_empty(page.liens) then
	       List.add(page.url, acc)
	     else
	       acc
	    )
	Map.fold(fold_pages, domain, acc)
    )
    List.take(limit, Map.fold(fold_domains, /graphe, []))

@client message_from_room(sigma, mode)(msg : message)=
    an_domain(id) = (
        domain = url_to_domain_ref(id)
        sigma.add_node(domain, domain, "#FFFFFF")
    )
    ae_domain((n1, n2)) = (
        d1 = url_to_domain_ref(n1)
        d2 = url_to_domain_ref(n2)
        sigma.add_edge(d1^"_"^d2, d1, d2)
    )
    an_filter(domain)(id) = (
        d = url_to_domain_ref(id)
        if d == domain then
            sigma.add_node(id, id, "#FFFFFF")
        else 
            void
    )
    ae_filter(domain)((n1,n2)) = (
        d1 = url_to_domain_ref(n1)
        d2 = url_to_domain_ref(n2)
        if (d1 == domain && d2 == domain) then 
            sigma.add_edge(n1^"_"^n2, n1, n2)
        else 
            void
    )
    (an, ae) = match mode with 
                | {all} -> (an_domain, ae_domain)
                | {domain = d} -> (an_filter(d), ae_filter(d))
                end
    do match msg with
        | {add_node = id} -> an(id)
        | {add_edge = (n1, n2)} -> ae((n1, n2))
	    | {add_nodes = nodes} -> List.iter(an, nodes)
	    | {add_edges = edges} -> List.iter(ae, edges)
        | _ -> void
    sigma.draw()

@client update_info(sigma) =
    info() =
      <>
        Nodes : {sigma.nodesCount()} <br />
	    Edges : {sigma.edgesCount()} <br />
      </>
    Scheduler.timer(500, (-> Dom.transform([#info <- info()])))

@client load_control(sigma) =
     color = WColorpicker.html("color_picker", {WColorpicker.default_config with 
		on_select = (color -> Dom.set_style_property_unsafe(#sigma_demo, "background-color", Color.color_to_string(color)))
		size = (250,100)
		initial = Color.black})
     control =
         <>
	     <button onclick={_ -> sigma.startForceAtlas2()}>Démarrer</button>
	     <button onclick={_ -> sigma.stopForceAtlas2()}>Stopper</button><br />
	     <button onclick={_ -> do sigma.position(0,0,1) sigma.draw()}>Recentrer</button><br />
	     <>Couleur du fond : {color}</>
	 </>
     Dom.transform([#control <- control])

myIter(fun, myMap, start) =
    fold_domains(d, domain, acc) =
    (
        fold_pages(p, page , acc) =
	(
	    fun(d, p, page, acc)
	)
	Map.fold(fold_pages, domain, acc)
    )
    Map.fold(fold_domains, myMap, start)   

@publish get_nodes() = myIter((_, _, p, a -> List.add(p.url, a)), /graphe, [])

@publish get_nodes_by_domain(domain) = Map.fold((_page_ref, page, acc -> List.add(page.url, acc)), /graphe[domain], [])

@publish get_nodes_domain() = Map.fold((dom_ref, _, acc -> List.add(dom_ref, acc)), /graphe, [])

@publish get_edges_domain() = List.unique_list_of(myIter(
				(dom_ref, _, page, a -> List.add((dom_ref, 
						   myIter(
				      	              (d, _p, _, ac -> List.unique_list_of(List.add(d, ac))), 
					              page.liens, 
					              [])
						   ),
						   a)), 
			        /graphe, 
				[]))

@publish get_edges_by_domain(domain) = Map.fold((page_ref, page, acc ->
                                                        liens = Map.fold((pref, _, a -> List.add(/graphe[domain][pref]/url,a)), /graphe[domain][page_ref]/liens[domain], [])
                                                        List.add((page.url, liens), acc)), /graphe[domain], [])

@publish get_edges() = myIter(
				(_, _, page, a -> List.add((page.url, 
						   myIter(
				      	              (d, p, _, ac -> List.add(/graphe[d][p]/url, ac)), 
					              page.liens, 
					              [])
						   ),
						   a)), 
			        /graphe, 
				[])

@client load_nodes(sigma, state) =
    nodes = match state with 
             | {all} -> get_nodes_domain()
             | {~domain} -> get_nodes_by_domain(domain)
             | _ -> []
    an(v) = sigma.add_node(v,v,"#FFF")
    List.iter(an, nodes)

@client load_edges(sigma, state) =
    edges = match state with
             | {all} -> get_edges_domain()
             | {~domain} -> get_edges_by_domain(domain)
             | _ -> []
    ae((v1,v2)) = sigma.add_edge(v1^"_"^v2,v1,v2)
    f((n1,list_edge)) = List.iter((v2 -> ae((n1, v2))), list_edge)
    List.iter(f, edges)

@server log_server = jlog

@client load_client(state) =
    sigma = Sigmajs(#sigma_demo)
    do load_control(sigma)
    do sigma.drawingProperties("#fff", 12, "#fff", "#000", 6, "curve")
    do sigma.graphProperties(1,3,1,1)
    do sigma.draw()
    do update_info(sigma)
    do Network.add_callback(message_from_room(sigma, state), room)
    do load_nodes(sigma, state)
    do sigma.draw()
    do load_edges(sigma, state)
    do sigma.draw()
    do log_server("a new client is loaded")
    void

content(state) =
    <div id=#sigma_demo onready={_ -> load_client(state)} />
    <div id=#info>
        Nodes : 0 <br />
	    Edges : 0 <br />
    </div>
    <div id=#control>
    </div>

page() = Resource.styled_page("OPASigmaJS :: DEMO", ["/res/css.css"], content({all}))

content_domain() =
    all_domain = Map.fold((domain, pages, acc -> <><li><a onclick={_ -> do Dom.transform([#sigma_demo <- <></>]) load_client({~domain})}>{domain} ({Map.size(pages)})</a></li>{acc}</>), /graphe, <></>) 
    <div id=#sigma_demo>
        <ul>{all_domain}</ul>
    </>
    <div id=#info>
    </div>
    <div id=#control>
    </div>

page_domain() = Resource.styled_page("OPASigmaJS :: DEMO", ["/res/css.css"], content_domain())

help_content() =
    <>  
        <h1>Interface REST</h1>
        <h2>POST /_REST_/need_a_visit</h2>
        Input : RAW url dans le content body d'un POST<br />
        Output : raw_text (false or true) <br />
        Description : Indique si l'url à besoin d'être visité ou non <br />
        <h2>POST /_REST_/add_pages </h2>
        Input : JSON (urls : list(string)) <br />
        Output : Resource.raw_status(success) OR Resource.raw_status(bad_request) <br />
        Description : Ajoute la liste d'url au graphe <br />
        <h2>POST /_REST_/add_liens </h2>
        Input : JSON (url:string, links : list(string)) <br />
        Output : Resource.raw_status(success) OR Resource.raw_status(bad_request) <br />
        Description : Ajoute des liens entre url => urls (PS : Ajoute les pages si elles n'existent pas => add_pages est optionnel) <br />
        <h2>GET /_REST_/get_urls</h2>
        Input: rien <br />
        Output: JSON (list(string))<br />
        Description: Retourne une liste de 5urls à visiter par le crawler. Normalement liste différente à chaque appels<br />
    </>
help() = Resource.html("OPASigmaJS :: DEMO :: HELP", help_content())

/**
Extraire le json du body
*/
extract_json_from_body() =
    match HttpRequest.get_body() with
     | {none} -> {failure="no body"}
     | {some=raw_body} -> 
         match Json.deserialize(raw_body) with
	  | {none} -> {failure= "impossible de convertir en json {raw_body}"}
	  | {some=jsast} -> {success= OpaSerialize.Json.unserialize_unsorted(jsast)}
	 end
    end

type Rest.Add.pages = {urls : list(string)}

/**
Ajouter des pages
*/
rest_add_pages() =
	match extract_json_from_body() with
	 | {~failure} -> 
		do jlog("add_page : {failure}")
		Resource.raw_status({bad_request})
	 | {success=opt} -> 
		match opt with
		 | {none} -> 
			do jlog("add_page : le json ne correspond pas")
			Resource.raw_status({bad_request})
		 | {some=record : Rest.Add.pages} ->
			do add_pages(record.urls)
			Resource.raw_status({success})
		end
	end

/**
Vérifier si une url a besoin d'être vérifié
**/
rest_need_a_visit()=
    match HttpRequest.get_body() with
     | {~some} -> r = if url_need_visit(some) then "true" else "false"
                  Resource.raw_response(r, "text/plain", {success})
     | {none} -> Resource.raw_response("false", "text/plain", {success})
    end

type Rest.Add.links = {url : string links : list(string)}
/**
Ajouter des liens
**/
rest_add_liens()=
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
			do add_liens(record.url, record.links)
			Resource.raw_status({success})
		end
	end

/**
Obtenir des urls a visiter
**/
rest_get_urls()=
    do jlog("a crawler ask url")
    Resource.json(OpaSerialize.Json.serialize(urls_to_visit(5)))

rest(path) =
    match HttpRequest.get_method() with
     | {some = method} -> match method with
       	       	       	   | {post} -> match path with
			     	                   | "need_a_visit" -> rest_need_a_visit()
				                       | "add_pages" -> rest_add_pages()
					                   | "add_liens" -> rest_add_liens()
					                   | _ -> Resource.raw_status({bad_request})
				                    end
	                       | {get} -> match path with
		     	                        | "get_urls" -> rest_get_urls()
				                        | _ -> Resource.raw_status({bad_request})
				                    end
	  | _ -> Resource.raw_status({method_not_allowed})
			  end
     | _ -> Resource.raw_status({bad_request})


urls : Parser.general_parser(http_request -> resource) =
    parser
      | "/_rest_/" path=(.*) -> _req -> rest(Text.to_string(path))
      | "/help" -> _req -> help()
      | "/domain" -> _req -> page_domain()
      | .* -> _req -> page()

server =  Server.make(
             Resource.add_auto_server(@static_resource_directory("res"),
             urls)
         )
