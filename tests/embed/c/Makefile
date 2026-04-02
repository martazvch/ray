CC      = cc
CFLAGS  = -Wall -Wextra -std=c99

SRC     = $(wildcard *.c)
OBJ     = $(SRC:.c=.o)
TARGET  = tester

$(TARGET): $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -f $(OBJ) $(TARGET)

.PHONY: clean
