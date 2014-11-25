#! /usr/bin/env bash
umount /dev/$1
mkfs.vfat -n 'FP&TDD' -I /dev/$1
mkdir -p /media/$1
mount /dev/$1 /media/$1
cp $2/* /media/$1
umount -v /dev/$1
mount /dev/$1 /media/$1
pushd .
cd /media/$1
sha512sum -c fptdddebjessie32bit.ova.sha512sum
popd
umount /dev/$1
