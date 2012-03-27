##extern-type sig_inst


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
