CC      := cc
CFLAGS  := -Wall -Wextra -std=c99
LDFLAGS :=

SRC     := $(wildcard *.c)
OBJ     := $(SRC:.c=.o)
TARGET  := tester

$(TARGET): $(OBJ)
	$(CC) $(LDFLAGS) -o $@ $^

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -f $(OBJ) $(TARGET)

debug: CFLAGS += -g -I../../../src/embed
debug: LDFLAGS += ../../../zig-out/lib/libray.dylib -Wl,-rpath,../../../zig-out/lib/
debug: $(TARGET)

.PHONY: clean
