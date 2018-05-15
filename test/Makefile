test_fiemap: block_device fiemap.c
	rm -rf test.drtl
	make -C ../libdrt/ compile
	gcc -I../libdrt/include fiemap.c -o fiemap -L../libdrt/build -ldrt
	cp fiemap.c ext4fs
	./fiemap test.drtl ext4fs/fiemap.c
	hexdump -C test.drtl

block_device:
	stat block.raw > /dev/null || ( \
		dd if=/dev/zero of=block.raw bs=1M count=1024 && \
		mkdir -p ext4fs && \
		mkfs.ext4 block.raw \
	)
	mountpoint ext4fs || sudo mount block.raw ext4fs
