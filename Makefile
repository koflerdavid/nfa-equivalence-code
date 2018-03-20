build:
	cd cl-data-structures && stack setup --silent && stack build
	cd cl-automata && stack setup --silent && stack build
	cd cl-automata-tool && stack setup --silent && stack build
	cd cl-regexes && stack setup --silent && stack build
	cd cl-regex-tool && stack setup --silent && stack build
	cd cl-website-tool && stack setup --silent && stack build
	cd cl-benchmark && stack setup --silent && stack build

rebuild: clean build

.PHONY: clean doc

doc:
	make -C doc

clean:
	cd cl-data-structures && stack clean --full
	cd cl-automata && stack clean --full
	cd cl-automata-tool && stack clean --full
	cd cl-regexes && stack clean --full
	cd cl-regex-tool && stack clean --full
	cd cl-website-tool && stack clean --full
	cd cl-benchmark && stack clean --full
	make clean -C doc
