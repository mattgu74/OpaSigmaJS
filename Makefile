opp:
	opa-plugin-builder sigmajs/plugin/sigma.concat.js sigmajs/plugin/sigma.forceatlas2.js sigmajs/plugin/sigma.js -o sigmajs.opp

plugin:
	opa sigmajs/sigmajs.opa sigmajs.opp

demo:
	opa --parser classic demo_sigma.opa

clean:
	rm *.opx -rf
	rm *.opp -rf
	rm _build -rf
	rm *.exe
