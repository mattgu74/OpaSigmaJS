// @author Matthieu Guffroy

import stdlib.apis.mongo

/*
  Déclaration des types
*/
type Domain.ref = string
type Page.ref = string
type Node.id = int
type Node =
{
  id : Node.id
  domain : Domain.ref
  page : Page.ref
  url : string
  visited : bool
}
type Edge.id = int
type Edge.t = {inside_domain} / {outside_domain}
type Edge =
{
  id : int
  from : Node.id
  domain_from : Domain.ref
  page_from : Page.ref
  to : Node.id
  domain_to : Domain.ref
  page_to : Page.ref
  t : Edge.t
}

/*
  Déclaration de la DB
*/
db /nodes[{id}] : Node
db /edges[{id}] : Edge
db /nodes_count : int
db /edges_count : int
db /domains : list(Domain.ref)

db /nodes[_]/visited = false
db /edges[_] full



/*
  Fonctions permettant d'accéder à la db
*/

Graphe = {{

  /*
    Transformation d'url.
  */
  url_parse(u : string) : (Domain.ref, Page.ref) =
    url = Parser.try_parse(UriParser.uri, u)
    path_to(p : list(string)) = List.fold((v, a -> "{a}/{v}"), p, "")
    match url with
     | {some = s} -> match s : Uri.uri with
                      | {domain = d ; path = p ; ...} -> (d,path_to(p))
                      | _ -> do Log.error("Url_parse","Url_parse failed (is not absolute) {u}")
                            ("", "")
                    end
     | {none} -> do Log.error("Url_parse","Url_parse failed {u}") 
                ("", "")

    /*
      Obtenir des urls à visiter
      @param int l nombre maximum d'url à renvoyer
      @return list(string)
    */
    to_visit(l : int) : list(string) =
      s = /nodes_count - l - 1
      s = if s < 0 then 0 else Random.int(s)
      nodes = /nodes[visited == false; skip s; limit l]
      Iter.fold((node, acc -> List.add(node.url, acc)), DbSet.iterator(nodes), [])

    /*
      Cette url a t elle déjà été visité ?
    */
    need_visit(url : string) : bool = 
      do jlog("url_need_visit {url}")
      (d,p) = url_parse(url)
      page = /nodes[domain == d and page == p]
      Iter.fold((node, acc -> not(node.visited) || acc), DbSet.iterator(page), false)

    /*
      Obtenir un id de node a partir d'un domain et d'une page
    */
    node_id(domain, page) : int =
      node = DbSet.iterator(/nodes[domain == domain and page == page])
      node_id_it(node)

    node_id_it(it) : int =
      id_from = Iter.fold((f, a -> f.id), it, -1)
      if id_from == -1 then 
        do Log.error("get_node_id","get node id ... no ID !")
        -1
      else 
        id_from

     /*
      Obtenir un id d'edge a partir d'un domain et d'une page
    */
    edge_id(from, to) : int =
      node = DbSet.iterator(/edges[from == from and to == to])
      edge_id_it(node)

    edge_id_it(it) : int =
      id_from = Iter.fold((f, a -> f.id), it, -1)
      if id_from == -1 then 
        do Log.error("get_edge_id","get edge id ... no ID !")
        -1
      else 
        id_from

    /*
      Ajouter une page au graphe
    */
    add_page(url : string) : (Domain.ref, Page.ref, Node.id, bool) =
      (domain, page) = url_parse(url)
      p = DbSet.iterator(/nodes[domain == domain and page == page])
      if Iter.count(p) == 0 then
        id = /nodes_count + 1
        do /nodes_count <- /nodes_count + 1
        do /domains <- List.add_uniq(String.ordering, domain, /domains)
        do /nodes[{~id}] <- {~id ~url ~domain ~page visited=false}
        (domain, page, id, true)
      else 
        (domain, page, node_id_it(p), false)

    /*
      Ajouter un lien au graphe
    */
    add_lien(from_url : string, to_url : string) : (Edge.id, Node.id, Node.id, bool) =
      (domain_from, page_from) = url_parse(from_url)
      (domain_to, page_to) = url_parse(to_url)
      from = node_id(domain_from, page_from)
      to = node_id(domain_to, page_to)
      e = DbSet.iterator(/edges[from == from and to == to])
      if Iter.count(e) == 0 then
        id = /edges_count + 1
        do /edges_count <- /edges_count + 1
        t = if domain_from == domain_to then {inside_domain} else {outside_domain}
        do /edges[{~id}] <- {~id ~domain_from ~page_from ~from ~domain_to ~page_to ~to ~t}
        (id, from, to, true)
      else 
        (edge_id_it(e), from, to, false)

    /*
      Indiquer qu'un noeud à été visité
    */
    visited(url : string) : void =
      (d, p) = url_parse(url)
      id = node_id(d, p)
      do /nodes[{~id}] <- {/nodes[{~id}] with visited=true}
      void

    /*
      Donne le nombre de noeud d'un domaine
    */
    nodes_in_domain(domain : Domain.ref) : int =
      it = DbSet.iterator(/nodes[domain == domain])
      Iter.count(it)

    get_domains_with_nb_pages() : list({domain:string; count:int}) =
      collection = MongoCollection.openfatal("default","_no_name","nodes") : Mongo.collection(Node)
      key = Bson.opa2doc({domain= true})
      initial = Bson.opa2doc({count=0})
      result = MongoCollection.group(
        collection, key, 
        "function(obj,prev) \{prev.count++;\}",
        initial, 
        none, none
      )
      r_analyze = MongoCollection.analyze_group(result) : Mongo.group_result({domain : string count: int})
      final_result = match r_analyze with 
      | {~failure} -> 
        do Log.error("nodes_in_domain", "{failure}")
        []
      | {success=group} ->
        do jlog("nodes_in_domain : count={group.count} keys={group.keys} ok={group.ok}")
        group.retval
      end
      do MongoCollection.destroy(collection)
      final_result

}}