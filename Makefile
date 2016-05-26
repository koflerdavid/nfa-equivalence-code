build:
	stack setup
	stack build

rebuild: clean build

.PHONY: clean doc

doc:
	make -C doc

clean:
	stack clean
	make clean -C doc
