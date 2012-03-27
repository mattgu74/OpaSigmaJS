##extern-type dom_element
##extern-type sig_inst


##register init: dom_element -> sig_inst
##args(dom)
{
		var sigInst = sigma.init(dom);
		return sigInst;
}

##register add_node: sig_inst, string, string, string -> void
##args(sigInst, id, label, color)
{
		sigInst.addNode(id,{
			label: label,
			color: color
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
