/*
 * To be deleted after code refactoring.
 */
typedef struct  {
	unsigned	l;		/* left 24 bits */
	unsigned	r;		/* right 24 bits */
	unsigned short	o;		/* exponent (temp storage) */
	unsigned	ml;		/* left part of mantissa */
} alureg_t;				/* ALU register type */

#define NEGATIVE(R)     (((R).ml & 0x10000) != 0)

extern alureg_t acc, accex, enreg, zeroword;

extern t_value fromalu (alureg_t reg);
extern alureg_t toalu (t_value val);
extern alureg_t negate (alureg_t word);

#define UNPCK(R)        { \
	(R).o = ((R).l >> 17) & 0x7f; \
	(R).ml = (R).l & 0x1ffff; \
	(R).ml |= ((R).ml & 0x10000) << 1; \
}

extern void add(), aax(), aex(), arx(), avx(), aox(), b6div(),
	mul(), apx(), aux(), acx(), anx(), epx(), emx(), asx(),
	yta(), b6mod(), ext(), priv();
