/*
 * To be deleted after code refactoring.
 */
typedef struct  {
	unsigned	l;		/* left 24 bits */
	unsigned	r;		/* right 24 bits */
	unsigned short	o;		/* exponent (temp storage) */
	unsigned	ml;		/* left part of mantissa */
} alureg_t;				/* ALU register type */

#define NEGATIVE(R)     (((R).ml & BIT17) != 0)

extern alureg_t zeroword;

extern t_value fromalu (alureg_t reg);
extern alureg_t toalu (t_value val);
extern alureg_t negate (alureg_t word);

#define UNPCK(R)        { \
	(R).o = ((R).l >> 17) & 0177; \
	(R).ml = (R).l & BITS17; \
	(R).ml |= ((R).ml & BIT17) << 1; \
}

#define PACK(R)		{ \
        (R).l = ((unsigned) (R).o << 17) | (R).ml; \
}

extern void besm6_add (alureg_t acc, alureg_t enreg);
extern void besm6_divide (alureg_t acc, alureg_t enreg);
extern void besm6_multiply (alureg_t acc, alureg_t enreg);
extern void apx (alureg_t acc, alureg_t enreg);
extern void aux (alureg_t acc, alureg_t enreg);
extern void anx (alureg_t acc, alureg_t enreg);
extern void shift (alureg_t acc, int toright);
extern int count_ones (t_value word);
void besm6_add_exponent (int val);
void normalize_and_round (alureg_t acc, alureg_t rmr);
