test_fiemap: block_device fiemap.c
	gcc fiemap.c -o fiemap
	cp fiemap.c ext4fs
	./fiemap ext4fs/fiemap.c

block_device:
	stat block.raw > /dev/null || ( \
		dd if=/dev/zero of=block.raw bs=1M count=1024 && \
		mkdir -p ext4fs && \
		mkfs.ext4 block.raw \
	)
	mountpoint ext4fs || sudo mount block.raw ext4fs
