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
#include <unistd.h>
#include "config.h"

#ifndef uint64_t
#  if SIZEOF_LONG == 8
#    define uint64_t unsigned long
#  elif SIZEOF_LONG_LONG == 8
#    define uint64_t unsigned long long
#  endif
#endif

int verbose = 0;

enum {
	OPT_START,
	OPT_LENGTH,
};

/* Table of options. */
struct option longopts[] = {
	/* option	     has arg		integer code */
	{ "help",		0,	0,	'h'		},
	{ "version",		0,	0,	'V'		},
	{ "verbose",		0,	0,	'v'		},
	{ "start",		1,	0,	OPT_START	},
	{ "length",		1,	0,	OPT_LENGTH	},
	{ 0,			0,	0,	0		},
};

/*
 * Отладочная печать зоны.
 */
void
dump_zone (int z, uint64_t *buf1, uint64_t *buf2)
{
	int i;

	printf ("%4d:", z);
	for (i=0; i<516; ++i)
		printf (" %013llx", (long long) buf1 [i]);
	printf ("\n     ");
	for (i=0; i<516; ++i)
		printf (" %013llx", (long long) buf2 [i]);
	printf ("\n");
}

/*
 * Вычисление чётности для 24-битных чисел.
 * Если входное число имеет четное число единиц в 24 младших битах,
 * то возвращается 0, если нечётное - то 1.
 */
int
odd_parity24 (int val)
{
	/* Таблица чётности для 6-битных чисел. */
	static const int parity6 [64] = {
		0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,
		1,0,0,1,0,1,1,0,0,1,1,0,1,0,0,1,
		1,0,0,1,0,1,1,0,0,1,1,0,1,0,0,1,
		0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,
	};

	return parity6 [val & 077] ^ parity6 [val >> 6 & 077] ^
		parity6 [val >> 12 & 077] ^ parity6 [val >> 18 & 077];
}


/*
 * Коррекция битов свёртки.
 * Для команд: 50-й бит дополняет левую половину слова (биты 25-48) до четного,
 * 49-й бит дополняет правую половину слова (биты 1-24) до нечетного.
 * Для чисел: 50-й бит дополняет левую половину слова (биты 25-48) до нечетного,
 * 49-й бит дополняет правую половину слова (биты 1-24) до четного.
 * В эмуляторе SIMH биты 50-49 должны быть установлены в '01' для команд
 * и в '10' для чисел.
 */
uint64_t
fix_parity (uint64_t word)
{
	/* Левая половина, бит 50. */
	if (odd_parity24 ((int) (word >> 24)))
		word ^= (uint64_t) 1 << 49;

	/* Правая половина, бит 49. */
	if (odd_parity24 ((int) word))
		word ^= (uint64_t) 1 << 48;
	return word;
}

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
void
repack (unsigned char *from, uint64_t *to)
{
	/* Таблица положения выходных битов во входном потоке. */
	static const struct {
		int byte, mask;
	} bit [50] = {
/* биты 1-5 */	{8, 0x01}, {6, 0x01}, {4, 0x01}, {2, 0x01}, {0, 0x01},
/* биты 6-10 */	{8, 0x02}, {6, 0x02}, {4, 0x02}, {2, 0x02}, {0, 0x02},
/* биты 11-15 */{8, 0x04}, {6, 0x04}, {4, 0x04}, {2, 0x04}, {0, 0x04},
/* биты 16-20 */{8, 0x08}, {6, 0x08}, {4, 0x08}, {2, 0x08}, {0, 0x08},
/* биты 21-25 */{8, 0x10}, {6, 0x10}, {4, 0x10}, {2, 0x10}, {0, 0x10},
/* биты 26-30 */{8, 0x20}, {6, 0x20}, {4, 0x20}, {2, 0x20}, {0, 0x20},
/* биты 31-35 */{8, 0x40}, {6, 0x40}, {4, 0x40}, {2, 0x40}, {0, 0x40},
/* биты 36-40 */{8, 0x80}, {6, 0x80}, {4, 0x80}, {2, 0x80}, {0, 0x80},
/* биты 41-45 */{9, 0x01}, {7, 0x01}, {5, 0x01}, {3, 0x01}, {1, 0x01},
/* биты 46-50 */{9, 0x02}, {7, 0x02}, {5, 0x02}, {3, 0x02}, {1, 0x02},
	};
	int n, k;

	for (n=0; n<516; ++n, ++to, from+=10) {
		/* Собираем слово из слогов входного файла. */
		*to = 0;
		for (k=0; k<50; ++k) {
			if (from [bit[k].byte] & bit[k].mask)
				*to |= (uint64_t) 1 << k;
		}
		/* Корректируем биты чётности. */
		*to = fix_parity (*to);
	}
}

/*
 * Создание образа диска SIMH из позонного каталога
 * от эмулятора магнитных дисков Морозова.
 */
void
dir_to_disk (char *from_dir, FILE *to_file, int start, int length)
{
	char filename [MAXPATHLEN];
	unsigned char raw [5160];
	uint64_t buf1 [516], buf2 [516];
	int zd, z;

	for (z=0; z<02000; ++z) {
		strcpy (filename, from_dir);
		sprintf (filename + strlen (filename), "/%04d", z);
		zd = open (filename, O_RDONLY);
		if (zd < 0) {
			if (z == 0)
				perror (filename);
			break;
		}

		/* Читаем первую половину зоны. */
		if (read (zd, raw, sizeof (raw)) != sizeof (raw)) {
			fprintf (stderr, "%s: read failed\n", filename);
			break;
		}
		repack (raw, buf1);

		/* Вторая половина зоны. */
		if (read (zd, raw, sizeof (raw)) != sizeof (raw)) {
			fprintf (stderr, "%s: read failed\n", filename);
			break;
		}
		repack (raw, buf2);
		close (zd);
		if (verbose)
			dump_zone (z, buf1, buf2);

		/* Сначала записываем 8 служебных слов,
		 * затем 1024 слова данных. */
		if (fwrite (buf1, 8, 4, to_file) != 4 ||
		    fwrite (buf2, 8, 4, to_file) != 4 ||
		    fwrite (buf1+4, 8, 512, to_file) != 512 ||
		    fwrite (buf2+4, 8, 512, to_file) != 512) {
			fprintf (stderr, "Write to zone %04o failed\n", z);
			break;
		}
	}
	printf ("Written %d zones.\n", z);
}

void
usage ()
{
	fprintf (stderr, "emd2simh version %s\n", PACKAGE_VERSION);
	fprintf (stderr, "Convert BESM-6 disk images from EMD to SIMH.\n");
	fprintf (stderr, "\n");

	fprintf (stderr, "Usage:\n");
	fprintf (stderr, "\temd2simh [-v] <emd-dir-name> <simh-file>\n");
	exit (-1);
}

int
main (int argc, char **argv)
{
	unsigned start = 0, length = 0;
	FILE *to_file;
	int c;

	for (;;) {
		c = getopt_long (argc, argv, "hVv", longopts, 0);
		if (c < 0)
			break;
		switch (c) {
		case 'h':
			usage ();
			break;
		case 'V':
			printf ("Version: %s\n", PACKAGE_VERSION);
			return 0;
		case 'v':
			++verbose;
			break;
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

	to_file = fopen (argv[1], "wb");
	if (! to_file) {
		perror (argv[1]);
		exit (-1);
	}
	dir_to_disk (argv[0], to_file, start, length);
	fclose (to_file);
	return 0;
}
