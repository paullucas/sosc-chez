prefix=$(HOME)/opt
R6RS=$(prefix)/lib/r6rs

all:
	make clean
	ikarus --r6rs-script mk.scm $(R6RS)

clean:
	rm -f $(R6RS)/sosc.*.sls
