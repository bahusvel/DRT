USB=/dev/sdb
CDEV=block.raw

build_fiemap: fiemap.c
	make -C ../libdrt/ compile
	gcc -g -I../libdrt/include fiemap.c -o fiemap -L../libdrt/build -ldrt

test_fiemap: block_device build_fiemap
	rm -rf test.drtl
	./fiemap test.drtl `find usbfs/ -type f`
	hexdump -C test.drtl

usb_device:
	mkdir -p usbfs || true
	sudo umount $(USB) || true
	sudo mkfs.ext4 -F $(USB)
	sudo mount $(USB) usbfs
	sudo chown `whoami`:`whoami` usbfs

corrupt_random:
	gcc blockfault.c -o blockfault
	sudo umount usbfs || true
	sudo ./blockfault $(CDEV) 1000
	sudo fsck.ext4 -y $(CDEV) || true
	sudo mount $(CDEV) usbfs
	sudo chown `whoami`:`whoami` usbfs
	echo "Lost files:"
	find usbfs/ -type f | sort | comm -1 -3 - old.tree | tee lost.tree

corrupt_mbr:
	sudo umount $(USB)1 || true
	sudo dd if=/dev/zero of=$(USB) bs=1M count=3
	partprobe

block_device:
	sudo umount usbfs
	rm block.raw
	stat block.raw > /dev/null || ( \
		dd if=/dev/zero of=block.raw bs=1M count=1024 && \
		mkdir -p ext4fs && \
		mkfs.ext4 block.raw \
	)
	mountpoint usbfs || sudo mount block.raw usbfs
	sudo chown `whoami`:`whoami` usbfs
	sudo rm -rf usbfs/* || true
	cp -r ../../GO/src/github.com/* usbfs/ || true
	find usbfs/ -type f | sort > old.tree

test_crc32:
	make -C ../libdrt/ compile
	gcc crc32_test.c -L../libdrt/build -ldrt -o crc32
	./crc32
