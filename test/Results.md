# Partitioned disk corruption
Most filesystems today use backup superblocks at known locations. This enables them to recovery from primary superblock corruption errors.

Partition tables MBR and GPT posses no such backup information. As such single sector corruption within a GPT or MBR partition table will cause loss of all partitions on disk.

This is exploited by purposfully filling the MBR sector with zeroes. A utility TestDisk is used to recover these lost partitions. TestDisk performs successfully for well known filesystems even if their superblock is also corrupt, it will look for backup ones. TestDisk did fail to find a btrfs partition with its superblock corrupt. Perhaps simply due to TestDisk implementation.

# Filesystem metdata corruption
This test will asses filesystem capability to present data to the user in case it's internal structures have been severely corrupted beyond repair. Theoretically if the metadata describing user files has become irreperabily corrupt the filesystem can do nothing but fortget that data exists, even though it may still be present on disk.

Metadata corruption will be triggered by randomly corrupting blocks of the block device.
