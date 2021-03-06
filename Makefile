opp:
	opa-plugin-builder sigmajs/plugin/sigma.concat.js sigmajs/plugin/sigma.forceatlas2.js sigmajs/plugin/sigma.parseGexf.js sigmajs/plugin/sigma.js -o sigmajs.opp

plugin:
	opa --parser classic sigmajs/sigmajs.opa sigmajs.opp --no-server

demo:
	opa --parser classic uri_parser.opa demo_sigma.opa

mongo:
	opa --database mongo --parser classic uri_parser.opa db.opa rest.opa demo_sigma.opa

run:
	./demo_sigma.exe --db-local ~/mongodata/opa

all: clean opp plugin mongo run

clean:
	rm *.opx -rf
	rm *.opp -rf
	rm _build -rf
	rm *.exe -rf
