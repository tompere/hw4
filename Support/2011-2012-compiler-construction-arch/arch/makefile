CC=gcc
CFLAGS=-O -g

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

all: hello

hello: hello.o
	$(CC) $(CFLAGS) -o $@ $^

hello.o: hello.c

clean:
	rm -f *.o hello
