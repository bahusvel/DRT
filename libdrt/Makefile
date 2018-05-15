BUILD_DIR=build
SRC = $(wildcard *.c)
OBJ = $(patsubst %.c, $(BUILD_DIR)/%.o, $(SRC))
CFLAGS=-Iinclude

$(BUILD_DIR)/%.o: %.c
	gcc $(CFLAGS) -c $< -o $@

build/libdrt.a: $(OBJ)
	ar rcs $@ $^

compile:
	mkdir -p build
	make build/libdrt.a
