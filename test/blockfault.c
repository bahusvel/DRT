#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>

int main(int argc, char **argv) {
	if (argc < 2) {
        printf("Usage: %s <device_to_corrupt> <bytes_to_corrupt>", argv[0]);
	}
    int fd = open(argv[1], O_WRONLY);
    if (fd < 0) {
        perror("Could not open device");
        exit(EXIT_FAILURE);
    }
}
