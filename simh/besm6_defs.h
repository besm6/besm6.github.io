/*
 * besm6_defs.h: BESM-6 simulator definitions
 *
 * Copyright (c) 2009, Serge Vakulenko
 */
#ifndef _BESM6_DEFS_H_
#define _BESM6_DEFS_H_    0

#include "sim_defs.h"				/* simulator defns */
#include <setjmp.h>

#define MAX(x,y)	((x) > (y) ? (x) : (y))

/*
 * Memory
 */
#define NREGS		30			/* number of registers-modifiers */
#define MEMSIZE		(128*1024)		/* memory size, words */
#define DRUM_SIZE	(256*(1024+8))		/* drum size, words */
#define DISK_SIZE	(1024*(1024+8))		/* disk size, words */

/*
 * Simulator stop codes
 */
enum {
	STOP_STOP = 1,				/* STOP */
	STOP_IBKPT,				/* breakpoint */
	STOP_RUNOUT,				/* run out end of memory limits */
	STOP_BADCMD,				/* invalid instruction */
	STOP_INSN_CHECK,			/* not an instruction */
	STOP_INSN_PROT,				/* fetch from blocked page */
	STOP_OPERAND_PROT,			/* load from blocked page */
	STOP_RAM_CHECK,				/* RAM parity error */
	STOP_CACHE_CHECK,			/* data cache parity error */

};

/*
 * Разряды машинного слова.
 */
#define BITS15		077777			/* биты 15..1 */
#define BITS24		077777777		/* биты 24..1 */
#define WORD		07777777777777777LL	/* биты 48..1 */
#if 0
#define BIT46		01000000000000000LL	/* 46-й бит */
#define TAG		00400000000000000LL	/* 45-й бит-признак */
#define SIGN		00200000000000000LL	/* 44-й бит-знак */
#define BIT37		00001000000000000LL	/* 37-й бит */
#define BIT19		00000000001000000LL	/* 19-й бит */
#define MANTISSA	00000777777777777LL	/* биты 36..1 */
#endif

/*
 * Разряды режима АУ.
 */
#define RAU_NORM_DISABLE	001	/* блокировка нормализации */
#define RAU_ROUND_DISABLE	002	/* блокировка округления */
#define RAU_LOG			004	/* признак логической группы */
#define RAU_MULT		010	/* признак группы умножения */
#define RAU_ADD			020	/* признак группы слодения */
#define RAU_OVF_DISABLE		040	/* блокировка переполнения */

#define RAU_MODE		(RAU_LOG | RAU_MULT | RAU_ADD)
#define SET_LOGICAL(x)		(((x) & ~RAU_MODE) | RAU_LOG)
#define SET_MULTIPLICATIVE(x)	(((x) & ~RAU_MODE) | RAU_MULT)
#define SET_ADDITIVE(x)		(((x) & ~RAU_MODE) | RAU_ADD)
#define IS_LOGICAL(x)		(((x) & RAU_MODE) == RAU_LOG)
#define IS_MULTIPLICATIVE(x)	(((x) & (RAU_ADD | RAU_MULT) == RAU_MULT)
#define IS_ADDITIVE(x)		((x) & RAU_ADD)

/*
 * Работа со сверткой. Значение разрядов свертки слова равно значению
 * регистров ПКЛ и ПКП при записи слова.
 * 00 - командная свертка
 * 01 или 10 - контроль числа
 * 11 - числовая свертка
 */
#define SET_CONVOL(x, c)	(((x) & WORD) | (((c) & 3LL) << 48))
#define GET_CONVOL(x)		((x) >> 48)
#define IS_INSN(x)		(GET_CONVOL(x) == 0)
#define	IS_NUMBER(x)		(GET_CONVOL(x) == 0 || GET_CONVOL(x) == 3)

extern uint32 sim_brk_types, sim_brk_dflt, sim_brk_summ; /* breakpoint info */
extern int32 sim_interval, sim_step;
extern FILE *sim_deb;

extern UNIT cpu_unit;
extern t_value memory [MEMSIZE];
extern t_value pult [8];
extern uint32 PC, PPK;
extern uint32 M[NREGS];
extern uint32 supmode, convol_mode;
extern DEVICE drum_dev, mmu_dev;
extern jmp_buf cpu_halt;

/*
 * Режимы УУ
 */
#define sup_mmap (M[17] & 1)
#define prot_disabled ((M[17] & 2) == 2)

/*
 * Процедуры работы с памятью
 */
extern void mmu_store(int addr, t_value word);
extern t_value mmu_load(int addr), mmu_fetch(int addr);

/*
 * Выполнение обращения к барабану.
 * Все параметры находятся в регистрах.
 */
void drum (t_value *sum);

t_stat fprint_sym (FILE *of, t_addr addr, t_value *val,
	UNIT *uptr, int32 sw);

#endif
