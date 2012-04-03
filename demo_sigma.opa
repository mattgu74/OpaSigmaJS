import sigmajs
import stdlib.widgets.colorpicker

@publish room = Network.cloud("room"): Network.network(message)

type node = string
type nodes = list(node)
type edge = (string,string)
type edges = list(edge)
type message = {add_node : node} / {add_edge : edge} / {add_nodes : nodes} / {add_edges : edges}

type Domain.ref = string

type Page.ref = string
type Page.data = {
    url : string
    liens : map(Domain.ref, map(Page.ref, bool))
}

db /graphe : map(Domain.ref, map(Page.ref, Page.data))

type url_a_visiter = {
     domain : Domain.ref
     page : Page.ref
     count : int
}

db /to_visit : list(url_a_visiter)

db /graphe[_][_]/liens[_][_] = { false }



url_to_domain_ref(u : string) = 
    url = Uri.of_string(u)
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

url_need_visit(url) = 
    do jlog("url_need_visit {url}")
    d = url_to_domain_ref(url)
    p = url_to_page_ref(url)
    List.exists((page -> d==page.domain && p==page.page), /to_visit)
    //Db.exists(@/graphe[d][p]) && Map.is_empty(/graphe[d][p]/liens)

add_pages(urls : list(string)) =
    do jlog("add_pages {Debug.dump(urls)}")
    do Network.broadcast({add_nodes = urls}, room)
    List.iter((url ->  
    		      domain = url_to_domain_ref(url)
		      page = url_to_page_ref(url)
    		      path = @/graphe[domain][page]
		      if not(Db.exists(path)) then
		          do jlog("{url} n'existait pas comme noeud, le lien a été ajouté")
			  do /to_visit <- List.add({~domain ~page count=0}, /to_visit) 
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
    /*fold_domains(_d, domain, acc) =
    (
        fold_pages(_p, page , acc) =
	(
	   if Map.is_empty(page.liens) then
	       List.add(page.url, acc)
	   else
	       acc
	)
	Map.fold(fold_pages, domain, acc)
	List.take(limit, Map.fold(fold_domains, /graphe, []))
    )*/
    do /to_visit <- List.mapi((i, page -> if i < limit then {page with count=page.count+1} else page), /to_visit)
    result = List.take(limit, /to_visit)
    do /to_visit <- List.sort_by((page -> page.count), /to_visit)
    List.map((page -> /graphe[page.domain][page.page]/url), result)


@client message_from_room(sigma)(msg : message)=
    do match msg with
        | {add_node = id} -> sigma.add_node(id, id, "#FFFFFF")
        | {add_edge = (n1, n2)} -> sigma.add_edge(n1^"_"^n2, n1, n2)
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

@client load_nodes(sigma) =
    nodes = get_nodes()
    an(v) = sigma.add_node(v,v,"#FFF")
    List.iter(an, nodes)

@client load_edges(sigma) =
    edges = get_edges()
    ae((v1,v2)) = sigma.add_edge(v1^"_"^v2,v1,v2)
    f((n1,list_edge)) = List.iter((v2 -> ae((n1, v2))), list_edge)
    List.iter(f, edges)

@server log_server = jlog

@client load_client() =
    sigma = Sigmajs(#sigma_demo)
    do load_control(sigma)
    do sigma.drawingProperties("#fff", 12, "#fff", "#000", 6, "curve")
    do sigma.graphProperties(1,3,1,1)
    do sigma.draw()
    do update_info(sigma)
    do Network.add_callback(message_from_room(sigma), room)
    do load_nodes(sigma)
    do sigma.draw()
    do load_edges(sigma)
    do sigma.draw()
    do log_server("a new client is loaded")
    void

content() =
    <div id=#sigma_demo onready={_ -> load_client()} />
    <div id=#info>
        Nodes : 0 <br />
	Edges : 0 <br />
    </div>
    <div id=#control>
    </div>

page() = Resource.styled_page("OPASigmaJS :: DEMO", ["/res/css.css"], content())

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
     | {~some} -> Resource.raw_response(OpaSerialize.serialize(url_need_visit(some)), "text/plain", {success})
     | {none} -> Resource.raw_response(OpaSerialize.serialize(false), "text/plain", {success})
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
      | .* -> _req -> page()

server =  Server.make(
             Resource.add_auto_server(@static_resource_directory("res"),
             urls)
         )
