##extern-type sig_inst
##extern-type node
##extern-type edge
##extern-type xml

##register init: string -> sig_inst
##args(dom)
{
		var sigInst = sigma.init(document.getElementById(dom));
		return sigInst;
}

##register add_node: sig_inst, string, string, string -> void
##args(sigInst, id, label, color)
{
		sigInst.addNode(id,{
			'x': Math.random(),
			'y': Math.random(),
			'label': label,
			'color': color
		});
}

##register add_edge: sig_inst, string, string, string -> void
##args(sigInst, id, n1, n2)
{
		sigInst.addEdge(id,n1,n2);
}

##register draw: sig_inst -> void
##args(sigInst)
{
		sigInst.draw();
}

##register startForceAtlas2: sig_inst -> void
##args(sigInst)
{
		sigInst.startForceAtlas2();
}

##register stopForceAtlas2: sig_inst -> void
##args(sigInst)
{
    sigInst.stopForceAtlas2();
}

##register parseGexf: sig_inst, string -> void
##args(sigInst, gexf)
{
	sigInst.parseGexf(gexf);
}

##register forEachNode: sig_inst, (node -> void) -> void
##args(sigInst, f)
{
	sigInst._core.graph.nodes.forEach(f);
}

##register forEachEdge: sig_inst, (edge -> void) -> void
##args(sigInst, f)
{
	sigInst._core.graph.edges.forEach(f);
}

##register nodesCount: sig_inst -> int
##args(sigInst)
{
  return sigInst.getNodesCount();
}
 
##register edgesCount: sig_inst -> int
##args(sigInst)
{
  return sigInst.getEdgesCount();
}

##register drawingProperties: sig_inst, string, int, string, string, int, string -> void
##args(sigInst, lColor, lSize, lBGColor, lHoverColor, lThreshold, edgeType)
{
    sigInst.drawingProperties({
	defaultLabelColor: lColor,
	defaultLabelSize: lSize,
	defaultLabelBGColor: lBGColor,
	defaultLabelHoverColor: lHoverColor,
	labelThreshold: lThreshold,
	defaultEdgeType: edgeType
    });
}

##register graphProperties: sig_inst, int, int, int, int -> void
##args(sigInst, minNodeSize, maxNodeSize, minEdgeSize, maxEdgeSize)
{
    sigInst.graphProperties({
	minNodeSize: minNodeSize,
	maxNodeSize: maxNodeSize,
	minEdgeSize: minEdgeSize,
	maxEdgeSize: maxEdgeSize
    });
}

##register position: sig_inst, int, int, int -> void
##args(sigInst, x, y, z)
{
    sigInst.position(x,y,z);
}