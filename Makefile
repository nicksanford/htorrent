install:
	stack build
	stack install

clean:
	stack clean

test:
	stack test

.PHONY: test
