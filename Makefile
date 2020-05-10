.PHONY: all test count

all:
	elm-live src/Main.elm -- --debug

test:
	elm-test

count:
	fd | grep elm$$ | xargs wc -l
