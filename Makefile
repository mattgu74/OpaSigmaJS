opp:
	opa-plugin-builder sigmajs/plugin/sigma.concat.js sigmajs/plugin/sigma.forceatlas2.js sigmajs/plugin/sigma.parseGexf.js sigmajs/plugin/sigma.js -o sigmajs.opp

plugin:
	opa --parser classic sigmajs/sigmajs.opa sigmajs.opp --no-server

demo:
	opa --parser classic demo_sigma.opa

run:
	./demo_sigma.exe

all: clean opp plugin demo run

clean:
	rm *.opx -rf
	rm *.opp -rf
	rm _build -rf
	rm *.exe -rf
