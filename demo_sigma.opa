import sigmajs
import stdlib.widgets.colorpicker

@publish room = Network.cloud("room"): Network.network(message)

type node = string
type edge = (string,string)
type message = {add_node : node} / {add_edge : edge}

db /graph : stringmap(list(string))

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


urls : Parser.general_parser(http_request -> resource) =
    parser
      | "/add_node?id=" id=(.*) -> _req -> do add_node(id) Resource.html("OPASigmaJS :: DEMO", <>OK</>)
      | "/add_edge?n1=" n1=((![&] .)*) "&n2=" n2=((![&] .)*) -> _req -> do add_edge(n1,n2) Resource.html("OPASigmaJS :: DEMO", <>OK</>)
      | .* -> _req -> page()

server =  Server.make(
             Resource.add_auto_server(@static_resource_directory("res"),
             urls)
         )
