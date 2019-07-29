.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

ERL = erl

MODS = aux connect dht trade test testA testB

all: compile

compile: ${MODS:%=%.beam}
	
clean:	
	rm -rf *.beam erl_crash.dump 
