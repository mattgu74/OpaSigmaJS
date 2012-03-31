import sigmajs
import stdlib.widgets.colorpicker

@publish room = Network.cloud("room"): Network.network(message)

type node = string
type edge = (string,string)
type message = {add_node : node} / {add_edge : edge}

db /graph : stringmap(list(string))

type Domain.ref = string

type Page.ref = string
type Page.data = {
    url : string
    liens : map(Domain.ref, map(Page.ref, bool))
}

db /graphe : map(Domain.ref, map(Page.ref, Page.data))
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
    Map.is_empty(/graphe[url_to_domain_ref(url)][url_to_page_ref(url)]/liens)

add_pages(urls : list(string)) =
    List.iter((url -> /graphe[url_to_domain_ref(url)][url_to_page_ref(url)] <- {~url liens=Map.empty}), urls)

add_liens(url:string, liens:list(string)) =
    domain = url_to_domain_ref(url)
    page = url_to_page_ref(url)
    make_link(l) = 
       d = url_to_domain_ref(l)
       p = url_to_page_ref(l)
       /graphe[domain][page]/liens[d][p] <- true
    List.iter((url -> make_link(url)), liens)


// To improve
// Need to use the limit argument
// 
urls_to_visit(limit : int) =
    fold_domains(d, domain, acc) =
    (
        fold_pages(p, page , acc) =
	(
	   if Map.is_empty(page.liens) then
	       List.add(page.url, acc)
	   else
	       acc
	)
	Map.fold(fold_pages, domain, acc)
    )
    Map.fold(fold_domains, /graphe, [])


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

@publish get_nodes() = Map.To.key_list(/graph)
@publish get_edges() = Map.To.assoc_list(/graph)

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

add_node(n) =
     node = Text.to_string(n)
     do Network.broadcast({add_node = node}, room)
     do /graph[node] <- /graph[node]
     jlog("add node {node}")

add_edge(n1,n2)=
      node1 = Text.to_string(n1)
      node2 = Text.to_string(n2)
      do Network.broadcast({add_edge = (node1,node2)}, room)
      do /graph[node1] <- List.unique_list_of(List.add(node2, /graph[node1]))
      jlog("add edge {node1} {node2}")

/**
Extraire le json du body
*/
extract_json_from_body() =
    match HttpRequest.get_body() with
     | {none} -> {failure="no body"}
     | {some=raw_body} -> 
         match Json.deserialize(raw_body) with
	  | {none} -> {failure= "impossible de convertir en json"}
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

type Rest.Add.links = {url:string links : list(string)}
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
rest_get_urls()=Resource.raw_response(OpaSerialize.serialize(urls_to_visit(50)), "text/plain", {success})

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
      | "/add_node?id=" id=(.*) -> _req -> do add_node(id) Resource.html("OPASigmaJS :: DEMO", <>OK</>)
      | "/add_edge?n1=" n1=((![&] .)*) "&n2=" n2=((![&] .)*) -> _req -> do add_edge(n1,n2) Resource.html("OPASigmaJS :: DEMO", <>OK</>)
      | .* -> _req -> page()

server =  Server.make(
             Resource.add_auto_server(@static_resource_directory("res"),
             urls)
         )
