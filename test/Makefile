USB=/dev/sdb

build_fiemap: fiemap.c
	make -C ../libdrt/ compile
	gcc -g -I../libdrt/include fiemap.c -o fiemap -L../libdrt/build -ldrt

test_fiemap: block_device build_fiemap
	rm -rf test.drtl
	cp -rfx ../* ext4fs
	./fiemap test.drtl `find ext4fs/ -type f`
	hexdump -C test.drtl

usb_device:
	sh format_disk.sh $(USB)
	partprobe

corrupt_mbr:
	sudo dd if=/dev/zero of=$(USB) bs=512 count=1
	partprobe

block_device:
	stat block.raw > /dev/null || ( \
		dd if=/dev/zero of=block.raw bs=1M count=1024 && \
		mkdir -p ext4fs && \
		mkfs.ext4 block.raw \
	)
	mountpoint ext4fs || sudo mount block.raw ext4fs

test_crc32:
	make -C ../libdrt/ compile
	gcc crc32_test.c -L../libdrt/build -ldrt -o crc32
	./crc32
