import sigmajs

Load() =
    xhtml =
          <div id=#sigma_demo style="height: 500px; width:500px;" />
    form(add, sig) =
       <>
      	  <input id=#node_name type="text" /> <br />
	  <button onclick={_ -> add()}> Ajouter un noeud </button><br />
	  <button onclick={_ -> Sigmajs.startForceAtlas2(sig)}> Démarrer la spatialisation</button> <button onclick={_ -> Sigmajs.stopForceAtlas2(sig)}> Stopper la spatialisation</button>
       </>
    do Dom.transform([#content <- xhtml])
    sigInst = Sigmajs.init(#sigma_demo)
    //do Sigmajs.add_node(sigInst, "hello", "Hello", "#FF0000")
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

urls : Parser.general_parser(http_request -> resource) =
    parser
      | .* -> _req -> page()

server = Server.make(urls)