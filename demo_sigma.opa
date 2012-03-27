import sigmajs

Load() =
    xhtml =
      <div id=#sigma_demo style="height: 500px; width:500px;">
      </>
    do Dom.transform([#content <- xhtml])
    sigInst = Sigmajs.init(#sigma_demo)
    do Sigmajs.add_node(sigInst, "hello", "Hello", "#FF0000")
    do Sigmajs.add_node(sigInst, "world", "World", "#00FF00")
    do Sigmajs.add_edge(sigInst, "hello_world","hello","world")
    Sigmajs.draw(sigInst)

content() =
    <div id=#content>
        <div id=#info onready={_ -> Load()}>Loading...</>
    </>

page() = Resource.html("OPASigmaJS :: DEMO", content())

urls : Parser.general_parser(http_request -> resource) =
    parser
      | .* -> _req -> page()

server = Server.make(urls)