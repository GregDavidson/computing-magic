H = ngender.net
D = Network-Volley
B := bin
vpath $B
SRC = sprites-worlds-server.rkt sprites-worlds-client.rkt sprites-worlds-game.rkt
BIN = sprites-worlds-server sprites-worlds-client
help:
	@ echo make all -- make the things: $(BIN)
	@ echo make sprites-worlds-server -- just build that
	@ echo make sprites-worlds-client -- just build that
	@ echo dest-copy -- copy the local sources to $H:$D
	@ echo dest-make -- make the things in directory $D on host $H
	@ echo dest-run -- run sprites-worlds-server on $H
all: sprites-worlds-server sprites-worlds-client
sprites-worlds-server: sprites-worlds-server.rkt sprites-worlds-game.rkt
	raco exe --gui -o $B/$@ $<
sprites-worlds-client: sprites-worlds-client.rkt sprites-worlds-game.rkt
	raco exe --gui -o $B/$@ $<
dest-copy:
	scp $(SRC) ngender.net:$D
dest-make:
	ssh ngender.net make -C Network-Volley all
dest-run:
	ssh -X ngender.net $D/$B/sprites-worlds-server -t
dest-clean:
	ssh $H rm -f $D/$B/sprites-worlds-server $D/$B/sprites-worlds-client
clean:
	rm -f $B/sprites-worlds-server $B/sprites-worlds-client
