#include <stdio.h>
#include <stdlib.h>
#include <time.h>

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

int count_ones (int val)
{
	int n, i;

	for (i=0; i<24; ++i)
		if (val >> i & 1)
			++n;
	return n;
}

int main ()
{
	int i, val, odd, nones;

	/* Проверка вычисления чётности. */
	srandom (time (0));
	printf ("Testing parity24()...");
	fflush (stdout);
	for (i=0; i<10000000; ++i) {
		val = random () & 077777777;
		odd = odd_parity24 (val);
		nones = count_ones (val);
		if (odd != (nones & 1))
			printf ("Error on %013x: odd=%d, nones=%d\n",
				val, odd, nones);
	}
	printf (" done.\n");
	return 0;
}
