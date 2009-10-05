/*
 * Convert BESM-6 disk images from EMD to SIMH.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You can redistribute this program and/or modify it under the terms of
 * the GNU General Public License as published by the Free Software Foundation;
 * either version 2 of the License, or (at your discretion) any later version.
 * See the accompanying file "COPYING" for more details.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include <sys/param.h>
#include <fcntl.h>
#include "config.h"

#define ZBYTES	(6*1024)
enum {
	OPT_START,
	OPT_LENGTH,
};

/* Table of options. */
static struct option longopts[] = {
	/* option	     has arg		integer code */
	{ "help",		0,	0,	'h'		},
	{ "version",		0,	0,	'V'		},
	{ "start",		1,	0,	OPT_START	},
	{ "length",		1,	0,	OPT_LENGTH	},
	{ 0,			0,	0,	0		},
};

/*
 * Отладочная печать зоны.
 */
#if 0
void
dump_zone (int z, unsigned char *buf1, unsigned char *buf2)
{
	int i;
	void dump_word (unsigned char *data) {
		printf (" %02x-%02x-%02x-%02x-%02x-%02x", data[0], data[1],
			data[2], data[3], data[4], data[5]);
	}

	printf ("%4d:", z);
	for (i=0; i<516*6; i+=6)
		dump_word (buf1+i);
	printf ("\n     ");
	for (i=0; i<516*6; i+=6)
		dump_word (buf2+i);
	printf ("\n");
}
#endif

/*
 * Эмулятор Морозова хранит каждую зону как две половины по 516 слов.
 * Машинное слово (50 бит) разбито на пять частей по 10 бит.
 * Каждая 10-битная часть уложена в два байта, младшим вперёд:
 *	биты 50 45, 40 35 30 25 20 15 10 5, - байты 1, 0
 *	биты 49 44, 39 34 29 24 19 14  9 4, - байты 3, 2
 *	биты 48 43, 38 33 28 23 18 13  8 3, - байты 5, 4
 *	биты 47 42, 37 32 27 22 17 12  7 2, - байты 7, 6
 *	биты 46 41, 36 31 26 21 16 11  6 1, - байты 9, 8
 * Свертка у слова числовая, если в 50-м разряде и левой половине слова
 * суммарное число единиц нечетное.
 */
static int
popcnt (unsigned char c)
{
	c = (c & 0x55) + ((c & 0xAA) >> 1);
	c = (c & 0x33) + ((c & 0xCC) >> 2);
	return (c & 0xF) + (c >> 4);
}

static void
repack (unsigned char *from, unsigned char *to, unsigned char *convol)
{
	int n;

	for (n=0; n<516; ++n, from+=10) {
		*convol = (from[1] >> 1) & 1;
		*to++ = (((from[5] >> 1) & 1) << 7) |	/* бит 48 */
			(((from[7] >> 1) & 1) << 6) |	/* бит 47 */
			(((from[9] >> 1) & 1) << 5) |	/* бит 46 */
			(((from[1] >> 0) & 1) << 4) |	/* бит 45 */
			(((from[3] >> 0) & 1) << 3) |	/* бит 44 */
			(((from[5] >> 0) & 1) << 2) |	/* бит 43 */
			(((from[7] >> 0) & 1) << 1) |	/* бит 42 */
			(((from[9] >> 0) & 1) << 0);	/* бит 41 */
		*convol ^= popcnt (to[-1]) & 1;
		*to++ = (((from[0] >> 7) & 1) << 7) |	/* бит 40 */
			(((from[2] >> 7) & 1) << 6) |	/* бит 39 */
			(((from[4] >> 7) & 1) << 5) |	/* бит 38 */
			(((from[6] >> 7) & 1) << 4) |	/* бит 37 */
			(((from[8] >> 7) & 1) << 3) |	/* бит 36 */
			(((from[0] >> 6) & 1) << 2) |	/* бит 35 */
			(((from[2] >> 6) & 1) << 1) |	/* бит 34 */
			(((from[4] >> 6) & 1) << 0);	/* бит 33 */
		*convol ^= popcnt (to[-1]) & 1;
		*to++ = (((from[6] >> 6) & 1) << 7) |	/* бит 32 */
			(((from[8] >> 6) & 1) << 6) |	/* бит 31 */
			(((from[0] >> 5) & 1) << 5) |	/* бит 30 */
			(((from[2] >> 5) & 1) << 4) |	/* бит 29 */
			(((from[4] >> 5) & 1) << 3) |	/* бит 28 */
			(((from[6] >> 5) & 1) << 2) |	/* бит 27 */
			(((from[8] >> 5) & 1) << 1) |	/* бит 26 */
			(((from[0] >> 4) & 1) << 0);	/* бит 25 */
		*convol++ ^= popcnt (to[-1]) & 1;
		*to++ = (((from[2] >> 4) & 1) << 7) |	/* бит 24 */
			(((from[4] >> 4) & 1) << 6) |	/* бит 23 */
			(((from[6] >> 4) & 1) << 5) |	/* бит 22 */
			(((from[8] >> 4) & 1) << 4) |	/* бит 21 */
			(((from[0] >> 3) & 1) << 3) |	/* бит 20 */
			(((from[2] >> 3) & 1) << 2) |	/* бит 19 */
			(((from[4] >> 3) & 1) << 1) |	/* бит 18 */
			(((from[6] >> 3) & 1) << 0);	/* бит 17 */
		*to++ = (((from[8] >> 3) & 1) << 7) |	/* бит 16 */
			(((from[0] >> 2) & 1) << 6) |	/* бит 15 */
			(((from[2] >> 2) & 1) << 5) |	/* бит 14 */
			(((from[4] >> 2) & 1) << 4) |	/* бит 13 */
			(((from[6] >> 2) & 1) << 3) |	/* бит 12 */
			(((from[8] >> 2) & 1) << 2) |	/* бит 11 */
			(((from[0] >> 1) & 1) << 1) |	/* бит 10 */
			(((from[2] >> 1) & 1) << 0);	/* бит 9 */
		*to++ = (((from[4] >> 1) & 1) << 7) |	/* бит 8 */
			(((from[6] >> 1) & 1) << 6) |	/* бит 7 */
			(((from[8] >> 1) & 1) << 5) |	/* бит 6 */
			(((from[0] >> 0) & 1) << 4) |	/* бит 5 */
			(((from[2] >> 0) & 1) << 3) |	/* бит 4 */
			(((from[4] >> 0) & 1) << 2) |	/* бит 3 */
			(((from[6] >> 0) & 1) << 1) |	/* бит 2 */
			(((from[8] >> 0) & 1) << 0);	/* бит 1 */
	}
}

/*
 * Создание образа диска из позонного каталога
 * от эмулятора магнитных дисков Морозова.
 */
void
dir_to_disk (char *from_dir, char *to_file, int start, int length)
{
	FILE *fd;
	int zd, z;
	unsigned char raw [5160], buf1 [516*6], buf2 [516*6];
	unsigned char cvbuf1[516], cvbuf2[516];
	char buf [ZBYTES];
	char cvbuf[1024];
	char check[48];
	char filename [MAXPATHLEN];

	fd = fopen (to_file, "wb");
	if (! fd) {
		perror (to_file);
		exit (-1);
	}
	for (z=0; z<01000; ++z) {
		strcpy (filename, from_dir);
		sprintf (filename + strlen (filename), "/%04d", z);
		zd = open (filename, O_RDONLY);
		if (zd < 0) {
			if (z == 0)
				perror (filename);
			break;
		}
		if (read (zd, raw, sizeof (raw)) != sizeof (raw)) {
			fprintf (stderr, "%s: read failed\n", filename);
			break;
		}
		repack (raw, buf1, cvbuf1);

		if (read (zd, raw, sizeof (raw)) != sizeof (raw)) {
			fprintf (stderr, "%s: read failed\n", filename);
			break;
		}
		repack (raw, buf2, cvbuf2);
		close (zd);

		memcpy (buf,            buf1 + 4*6, ZBYTES/2);
		memcpy (buf + ZBYTES/2, buf2 + 4*6, ZBYTES/2);
		memcpy (cvbuf,		cvbuf1 + 4, 512);
		memcpy (cvbuf + 512,	cvbuf2 + 4, 512);
		memcpy (check,		buf1, 4*6);
		memcpy (check + 4*6,	buf2, 4*6);
		if (fwrite (buf, 8, 1024, fd) != 1024) {
			fprintf (stderr, "Write to %s zone %04o failed\n", to_file, z);
			break;
		}
	}
	fclose (fd);
	printf ("Written %d zones from directory %s to disk %s\n",
		z, from_dir, to_file);
}

void
usage ()
{
	fprintf (stderr, "emd2simh version %s\n", PACKAGE_VERSION);
	fprintf (stderr, "Convert BESM-6 disk images from EMD to SIMH.\n");
	fprintf (stderr, "\n");

	fprintf (stderr, "Usage:\n");
	fprintf (stderr, "\temd2simh <emd-dir-name> <simh-file>\n");
	exit (-1);
}

int
main (int argc, char **argv)
{
	unsigned start = 0, length = 0;
	char *to_file = 0, *from_dir = 0;
	int c;

	for (;;) {
		c = getopt_long (argc, argv, "hV", longopts, 0);
		if (c < 0)
			break;
		switch (c) {
		case 'h':
			usage ();
			break;
		case 'V':
			printf ("Version: %s\n", PACKAGE_VERSION);
			return 0;
		case OPT_START:
			start = strtol (optarg, 0, 0);
			break;
		case OPT_LENGTH:
			length = strtol (optarg, 0, 0);
			break;
		}
	}
	argc -= optind;
	argv += optind;
	if (argc != 2)
		usage ();
	from_dir = argv[0];
	to_file = argv[1];
	dir_to_disk (from_dir, to_file, start, length);
	return 0;
}
