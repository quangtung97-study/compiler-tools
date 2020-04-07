.PHONY: count

test:
	elm-test

count:
	fd | grep elm$$ | xargs wc -l
