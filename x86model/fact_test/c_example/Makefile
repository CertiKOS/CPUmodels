CC = gcc
FLAGS = -m32 -nostdlib -s -static
CFLAGS = -c $(FLAGS)

fac : fac.s stubstart.s
	$(CC) $(FLAGS) $^ -o $@

.PHONY: clean

clean : 
	rm fac
