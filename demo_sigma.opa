import sigmajs
import stdlib.widgets.colorpicker

@publish room = Network.cloud("room"): Network.network(message)

type node = {id : Node.id ; domain : Domain.ref ; page : Page.ref}
type nodes = list(node)
type edge = {id : Edge.id ; from : Node.id ; to : Node.id}
type edges = list(edge)

type message = {add_node : node} / {add_edge : edge} / {add_nodes : nodes} / {add_edges : edges}
type state_client = {all} / {domain : Domain.ref} / {super}

_add_pages(urls : list(string)) =
  nodes = List.fold(
            (url, acc ->
              (domain, page, id, ajoute) = Graphe.add_page(url)
              if ajoute then
                List.add({~id ~domain ~page} : node, acc)
              else
                acc
            ),
            urls,
            []
          )
  do Network.broadcast({add_nodes = nodes}, room)
  void

_add_liens(url:string, liens:list(string)) = 
  do _add_pages(List.add(url, liens))
  do Graphe.visited(url)
  edges = List.fold(
            (to, acc -> 
              (id, from, to, ajoute) = Graphe.add_lien(url, to)
              if ajoute then
                List.add({~id ~from ~to} : edge, acc)
              else
                acc
            ),
            liens,
            []
          )
  do Network.broadcast({add_edges = edges}, room)
  void


@client message_from_room(sigma, mode)(msg : message)=
    an_domain(n) = (
        if n.page == "" then 
          sigma.add_node("{n.id}", "{n.domain}", "#FFFFFF")
        else 
          void
    )
    an_all(n) = (
        sigma.add_node("{n.id}", "{n.domain} {n.page}", "#FFFFFF")
    )
    ae_domain(e) = (
        sigma.add_edge("{e.id}", "{e.from}", "{e.to}")
    )
    an_filter(domain)(n) = (
        if n.domain == domain then
            sigma.add_node("{n.id}", "{n.domain} {n.page}", "#FFFFFF")
        else 
            void
    )
    (an, ae) = match mode with 
                | {super} -> (an_all, ae_domain)
                | {all} -> (an_domain, ae_domain)
                | {domain = d} -> (an_filter(d), ae_domain)
                end
    do match msg with
        | {add_node = n} -> an(n)
        | {add_edge = e} -> ae(e)
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


@publish get_nodes() : nodes = Iter.fold((node, acc -> List.add({id=node.id domain=node.domain page=node.page}, acc)), DbSet.iterator(/nodes), [])

@publish get_nodes_by_domain(domain) : nodes = Iter.fold((node, acc -> List.add({id=node.id domain=node.domain page=node.page}, acc)), DbSet.iterator(/nodes[domain == domain]), [])

@publish get_nodes_domain() : nodes = Iter.fold((node, acc -> List.add({id=node.id domain=node.domain page=node.page}, acc)), DbSet.iterator(/nodes[page == ""]), [])

@publish get_edges() = Iter.fold((edge, acc -> List.add({id=edge.id from=edge.from to=edge.to}, acc)), DbSet.iterator(/edges), [])

@publish get_edges_domain() = Iter.fold((edge, acc -> List.add({id=edge.id from=edge.from to=edge.to}, acc)), DbSet.iterator(/edges[page_from == "" and page_to == ""]), []) // TODO : IMPROVE

@publish get_edges_by_domain(domain) = Iter.fold((edge, acc -> List.add({id=edge.id from=edge.from to=edge.to}, acc)), DbSet.iterator(/edges[domain_from == domain and t == {inside_domain}]), []) 


@client load_nodes(sigma, state) =
    match state with 
      | {all} -> List.iter((n -> sigma.add_node("{n.id}", "{n.domain}", "#FFF")),get_nodes_domain())
      | {~domain} -> List.iter((n -> sigma.add_node("{n.id}", "{n.domain} {n.page}", "#FFF")), get_nodes_by_domain(domain))
      | {super} -> List.iter((n -> sigma.add_node("{n.id}", "{n.domain} {n.page}", "#FFF")), get_nodes())
      | _ -> void

@client load_edges(sigma, state) =
    edges = match state with
             | {all} -> get_edges_domain()
             | {~domain} -> get_edges_by_domain(domain)
             | {super} -> get_edges()
             | _ -> []
    ae(e) = sigma.add_edge("{e.id}","{e.from}","{e.to}")
    List.iter(ae, edges)

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
page_all() = Resource.styled_page("OPASigmaJS :: DEMO", ["/res/css.css"], content({super}))

content_domain() =
    all_domain = List.fold(
      (r, acc -> 
        <>
        {acc}
        <li>
          <a onclick={_ -> 
            do Dom.transform([#sigma_demo <- <></>]) 
            load_client({domain=r.domain})}>
          {r.domain} ({r.count})
          </a>
        </li>
        </>), 
      Graphe.get_domains_with_nb_pages(), 
      <></>
    )
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

links_domain_domain() = Iter.count(DbSet.iterator(/edges[t == {outside_domain}]))
nb_node_visited() = Iter.count(DbSet.iterator(/nodes[visited == true]))
stats_content() =
    <>  
        <h1>Statistiques</h1>
        <ul>
          <li>Nombre de noeuds : {/nodes_count}</li>
          <li>Nombre de liens : {/edges_count}</li>
          <li>Nombre de domaines : {List.length(/domains)}</li>
          <li>Nombre de liens "externes" : {links_domain_domain()}</li>
          <li>Nombre de noeuds visités : {nb_node_visited()}</li>
        </ul>
    </>
stats() = Resource.html("OPASigmaJS :: DEMO :: Stats", stats_content())





urls : Parser.general_parser(http_request -> resource) =
    parser
      | "/_rest_/" path=(.*) -> _req -> Rest.run(Text.to_string(path))
      | "/help" -> _req -> help()
      | "/stats" -> _req -> stats()
      | "/all" -> _req -> page_all()
      | "/domain" -> _req -> page_domain()
      | .* -> _req -> page()

server =  Server.make(
             Resource.add_auto_server(@static_resource_directory("res"),
             urls)
         )
