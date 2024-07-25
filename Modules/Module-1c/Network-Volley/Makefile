M := bin
vpath $M
all: ball-toss-server ball-toss-client
ball-toss-server: ball-toss-server.rkt
	raco exe --gui -o $M/$@ $<
ball-toss-client: ball-toss-client.rkt
	raco exe --gui -o $M/$@ $<
clean:
	rm -f $M/ball-toss-server $M/ball-toss-client
