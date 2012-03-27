package sigmajs

type sig_inst = external

module Sigmajs {

  function init(dom) {
    %%sigma.init%%(Dom.get_id(dom))
  }

  function add_node(sig, id, label, color) {
    %%sigma.add_node%%(sig,id,label,color)
  }

  function add_edge(sig, id, n1, n2)
  {
    %%sigma.add_edge%%(sig,id,n1,n2)
  }

  function draw(sig)
  {
    %%sigma.draw%%(sig)
  }

  function startForceAtlas2(sig)
  {
    %%sigma.startForceAtlas2%%(sig)
  }

  function stopForceAtlas2(sig)
  {
    %%sigma.stopForceAtlas2%%(sig)
  }
  
  function parseGexf(sig, path)
  {
	%%sigma.parseGexf%%(sig, path)
  }

}
