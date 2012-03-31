package sigmajs

type sig_inst = external

@client Sigmajs(dom) = {{

  sig = %%sigma.init%%(Dom.get_id(dom))

  add_node(id, label, color) = %%sigma.add_node%%(sig,id,label,color)

  add_edge(id, n1, n2) = %%sigma.add_edge%%(sig,id,n1,n2)

  draw() = %%sigma.draw%%(sig)

  startForceAtlas2() = %%sigma.startForceAtlas2%%(sig)

  stopForceAtlas2() = %%sigma.stopForceAtlas2%%(sig)
  
  parseGexf(path) = %%sigma.parseGexf%%(sig, path)

  nodesCount() = %%sigma.nodesCount%%(sig)
  
  edgesCount() =  %%sigma.edgesCount%%(sig)

  drawingProperties(lColor, lSize, lBGColor, lHoverColor, lThreshold, edgeType) =
      %%sigma.drawingProperties%%(sig, lColor, lSize, lBGColor, lHoverColor, lThreshold, edgeType)

  graphProperties(minNodeSize, maxNodeSize, minEdgeSize, maxEdgeSize) = %%sigma.graphProperties%%(sig, minNodeSize, maxNodeSize, minEdgeSize, maxEdgeSize)

  position(x,y,z) = %%sigma.position%%(sig,x,y,z)

}}
