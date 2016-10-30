bin_PROGRAMS = emd2simh
emd2simh_SOURCES = emd2simh.c
AM_CFLAGS = -Wall -g -O2
AM_CPPFLAGS =

clean-local:
	-rm -rf *~

disks: emd2simh ../0 ../1
	./emd2simh ../0 disk2048.bin
	./emd2simh ../1 disk2053.bin
	dd bs=8256 count=32 skip=708 < disk2053.bin > drum2.bin
