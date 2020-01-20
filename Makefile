all:
	raco exe src/chess-game.rkt

clean:
	rm -rf src/chess-game src/*.rkt~ src/components/*.rkt~ src/components/compiled

deps:

run:
	racket src/chess-game.rkt
