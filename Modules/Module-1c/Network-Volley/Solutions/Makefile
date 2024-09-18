# do the clean actions work??
H = ngender.net
D = Network-Volley
B := bin
vpath $B
SERVER = sprites-worlds-server
GAME = network-volley-game
LIB = sprites-worlds-game.rkt
LIBS = $(LIB) sprites-worlds-client.rkt
SRC = $(SERVER).rkt $(LIBS) $(GAME).rkt
BIN = $(SERVER) $(GAME)
help:
	@ echo make all -- make the things: $(BIN)
	@ echo $(SERVER) -- just build that
	@ echo $(GAME) -- just build that
	@ echo dest-copy -- copy sources to $H:$D
	@ echo dest-make -- make all in directory $D on host $H
	@ echo dest-run -- run $(SERVER) on $H
all: $(SERVER) $(GAME)
$(SERVER): $(SERVER).rkt $(LIB)
	raco exe --gui -o $B/$@ $<
$(GAME): $(GAME).rkt $(LIBS)
	raco exe --gui -o $B/$@ $<
dest-copy:
	scp $(SRC) ngender.net:$D
dest-make:
	ssh ngender.net make -C Network-Volley all
dest-run:
	ssh -X ngender.net $D/$B/$(SERVER) -t
dest-clean:
	ssh $H  '( 'cd' "$B"; rm -f $(BINS) )'
clean:
	( 'cd' "$B"; rm -f $(BINS) )
