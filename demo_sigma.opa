import sigmajs

room = Network.cloud("room"): Network.network(message)
type message = {add_node : string} / {add_edge : (string, string)}

@publish make_callback(fun) = Network.add_callback(fun, room)

@client Load() =
    xhtml =
          <div id=#sigma_demo style="height: 500px; width:100%; background: black;" />
    info(sig) =
					i() = <>
								<br />
								<h2>Info sur le graph</h2>
								<ul>
									<li>Nombre de noeuds : {Sigmajs.nodesCount(sig)}</li>
									<li>Nombre de liens : {Sigmajs.edgesCount(sig)}</li>
								</ul>
							</>
					Dom.transform([#info <- i()])
    form(add, sig) =
       <>
      	  <input id=#node_name type="text" /> <br />
					<button onclick={_ -> add()}> Ajouter un noeud </button><br />
					<button onclick={_ -> Sigmajs.startForceAtlas2(sig)}> Démarrer la spatialisation</button> <button onclick={_ -> Sigmajs.stopForceAtlas2(sig)}> Stopper la spatialisation</button><br />
					<button onclick={_ -> do Sigmajs.parseGexf(sig, "/res/test.gexf") Sigmajs.draw(sig) }>Load test.gexf</button> <br />
					<button onclick={_ -> info(sig)}>Info</button><br />
					<div id=#info />
       </>
    do Dom.transform([#content <- xhtml])
    sigInst = Sigmajs.init(#sigma_demo)
    message_from_room(msg : message)=
        do match msg with
						| {add_node = id} -> Sigmajs.add_node(sigInst, id, id, "#FFFFFF")
						| {add_edge = (n1, n2)} -> Sigmajs.add_edge(sigInst, n1^"_"^n2, n1, n2)
						| _ -> jlog("Message not understood")
				Sigmajs.draw(sigInst)
    do make_callback(message_from_room)
    do Sigmajs.add_node(sigInst, "hello", "Hello", "#FF0000")
    //do Sigmajs.draw(sigInst)
    //do Sigmajs.add_node(sigInst, "world", "World", "#00FF00")
    //do Sigmajs.add_edge(sigInst, "hello_world","hello","world")
    do Sigmajs.draw(sigInst)
    a() = 
    	 name = Dom.get_value(#node_name)
         do Sigmajs.add_node(sigInst, name, name, "#0000FF")
	 do Sigmajs.add_edge(sigInst, name^"_hello", name, "hello")
	 do Dom.clear_value(#node_name)
	 do Sigmajs.draw(sigInst)
	 void
    Dom.transform([#content +<- form(a, sigInst)])

content() =
    <div id=#content>
        <div id=#info onready={_ -> Load()}>Loading...</>
    </>

page() = Resource.html("OPASigmaJS :: DEMO", content())

add_node(n) =
      do Network.broadcast({add_node = Text.to_string(n)}, room)
      do jlog("try to add node {n}")
      Resource.html("OPASigmaJS :: DEMO", <>OK</>)

add_edge(n1,n2)=
      do Network.broadcast({add_edge = (Text.to_string(n1),Text.to_string(n2))}, room)
      do jlog("try to add edge {n1} {2}")
      Resource.html("OPASigmaJS :: DEMO", <>OK</>)


urls : Parser.general_parser(http_request -> resource) =
    parser
      | "/add_node?id=" id=(.*) -> _req -> add_node(id)
      | "/add_edge?n1=" n1=((![&] .)*) "&n2=" n2=((![&] .)*) -> _req -> add_edge(n1,n2)
      | .* -> _req -> page()

server =  Server.make(
             Resource.add_auto_server(@static_resource_directory("res"),
             urls)
         )
