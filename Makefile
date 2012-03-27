opp:
	opa-plugin-builder sigmajs/plugin/sigma.concat.js sigmajs/plugin/sigma.js -o sigmajs.opp

plugin:
	opa sigmajs/sigmajs.opa sigmajs.opp

demo:
	opa --parser classic demo_sigma.opa
