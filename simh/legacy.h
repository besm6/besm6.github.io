#ifndef legacy_h
#define legacy_h                          /* to avoid multiple inclusions */

typedef struct  {
	uint    l;			/* left 24 bits */
	uint    r;			/* right 24 bits */
	ushort  o;			/* exponent (temp storage) */
	uint    ml;			/* left part of mantissa */
#define mr      r
}       alureg_t;                       /* ALU register type            */

#define NEGATIVE(R)     (((R).ml & 0x10000) != 0)

extern alureg_t acc, accex, enreg, zeroword;

extern alureg_t toalu(t_value val);
extern t_value fromalu(alureg_t reg);
 
#define LOAD(reg,addr) reg = toalu(mmu_load(addr))

#define STORE(reg,addr) mmu_store(addr, fromalu(reg))

#define UNPCK(R)        { \
	(R).o = ((R).l >> 17) & 0x7f; \
	(R).ml = (R).l & 0x1ffff; \
/*	(R).mr = (R).r; */ \
	(R).ml |= ((R).ml & 0x10000) << 1; \
}
#define PACK(R) { \
	(R).l = (long) ((R).o << 17) | (R).ml; \
/*	(R).r = (R).mr; */ \
}

#define NEGATE(R) { \
	if (NEGATIVE(R)) \
		(R).ml |= 0x20000; \
	(R).mr = (~(R).mr & 0xffffff) + 1; \
	(R).ml = (~(R).ml + ((R).mr >> 24)) & 0x3ffff; \
	(R).mr &= 0xffffff; \
	if ((((R).ml >> 1) ^ (R).ml) & 0x10000) { \
		(R).mr = (((R).mr >> 1) | ((R).ml << 23)) & 0xffffff; \
		(R).ml >>= 1; \
		++(R).o; \
	} \
	if (NEGATIVE(R)) \
		(R).ml |= 0x20000; \
}

#define CHK_STACK       \
        if (!addr && (reg == STACKREG)) {   \
                M[STACKREG] = ADDR(M[STACKREG] - 1);        \
                corr_stack = 1; \
        } else

#define GET_OP  {\
                LOAD(enreg, effaddr);\
                if (op.o_flags & F_AR) {\
                        accex = zeroword;\
                        UNPCK(enreg);\
                        UNPCK(acc);\
                } else if (op.o_flags & F_AROP) {\
                        UNPCK(enreg);\
                }\
}

#define GET_NAI_OP      {\
        enreg.o = effaddr & 0x7f;\
        enreg.ml = enreg.mr = 0;\
        if (op.o_flags & F_AR) {\
                accex = zeroword;\
                UNPCK(acc);\
        };\
}
#define STK_PUSH        {\
        STORE(acc, M[STACKREG]);\
        M[STACKREG] = ADDR(M[STACKREG] + 1);\
}

#define STK_POP         {\
        M[STACKREG] = ADDR(M[STACKREG] - 1);\
        corr_stack = 1;\
        LOAD(acc, M[STACKREG]);\
}

#define ADDR(x) ((x) & BITS15)

extern int rnd_rq;

#endif  /* legacy_h */
