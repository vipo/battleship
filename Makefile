all:
	stack build
	mkdir -p bins
	stack install --local-bin-path=bins
	docker build -t battleship:latest .

clean:
	rm -rf bins
	stack clean
