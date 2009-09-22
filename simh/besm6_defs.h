/*
 * besm6_defs.h: BESM-6 simulator definitions
 *
 * Copyright (c) 2009, Serge Vakulenko
 */
#ifndef _BESM6_DEFS_H_
#define _BESM6_DEFS_H_    0

#include "sim_defs.h"				/* simulator defns */
#include <setjmp.h>

/*
 * Memory
 */
#define NREGS		17			/* number of registers-modifiers */
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
/*TODO*/
	STOP_ADDOVF,				/* addition overflow */
	STOP_EXPOVF,				/* exponent overflow */
	STOP_MULOVF,				/* multiplication overflow */
	STOP_DIVOVF,				/* division overflow */
	STOP_DIVMOVF,				/* division mantissa overflow */
	STOP_NEGSQRT,				/* division mantissa overflow */
	STOP_SQRTERR,				/* sqrt error */
	STOP_READERR,				/* drum read error */
	STOP_BADRLEN,				/* invalid drum read length */
	STOP_BADWLEN,				/* invalid drum write length */
	STOP_WRERR,				/* drum write error error */
	STOP_DRUMINVAL,				/* invalid drum control word */
	STOP_DRUMINVDATA,			/* reading uninialized drum data */
	STOP_TAPEINVAL,				/* invalid tape control word */
	STOP_TAPEFMTINVAL,			/* invalid tape format word */
	STOP_TAPEUNSUPP,			/* tape not implemented */
	STOP_TAPEFMTUNSUPP,			/* tape formatting not implemented */
	STOP_PUNCHUNSUPP,			/* punch not implemented */
	STOP_RPUNCHUNSUPP,			/* punch reader not implemented */
	STOP_EXTINVAL,				/* invalid control word */
	STOP_INVARG,				/* invalid argument of instruction */
	STOP_ASSERT,				/* assertion failed */
	STOP_MBINVAL,				/* MB command without MA */
};

/*
 * Разряды машинного слова.
 */
#define BIT46		01000000000000000LL	/* 46-й бит */
#define TAG		00400000000000000LL	/* 45-й бит-признак */
#define SIGN		00200000000000000LL	/* 44-й бит-знак */
#define BIT37		00001000000000000LL	/* 37-й бит */
#define BIT19		00000000001000000LL	/* 19-й бит */
#define WORD		00777777777777777LL	/* биты 45..1 */
#define MANTISSA	00000777777777777LL	/* биты 36..1 */

/*
 * Разряды условного числа для обращения к внешнему устройству.
 */
#define EXT_DIS_RAM	04000	/* 36 - БМ - блокировка памяти */
#define EXT_DIS_CHECK	02000   /* 35 - БК - блокировка контроля */
#define EXT_TAPE_REV	01000   /* 34 - ОН - обратное движение ленты */
#define EXT_DIS_STOP	00400   /* 33 - БО - блокировка останова */
#define EXT_PUNCH	00200   /* 32 - Пф - перфорация */
#define EXT_PRINT	00100   /* 31 - Пч - печать */
#define EXT_TAPE_FORMAT	00040   /* 30 - РЛ - разметка ленты */
#define EXT_TAPE	00020   /* 29 - Л - лента */
#define EXT_DRUM	00010   /* 28 - Б - барабан */
#define EXT_WRITE	00004   /* 27 - Зп - запись */
#define EXT_UNIT	00003   /* 26,25 - номер барабана или ленты */

extern uint32 sim_brk_types, sim_brk_dflt, sim_brk_summ; /* breakpoint info */
extern int32 sim_interval, sim_step;
extern FILE *sim_deb;

extern UNIT cpu_unit;
extern t_value memory [MEMSIZE];
extern uint32 PC, PPK;
extern DEVICE drum_dev;
extern jmp_buf cpu_halt;

/* Параметры обмена с внешним устройством. */
extern int ext_op;		/* УЧ - условное число */
extern int ext_disk_addr;	/* А_МЗУ - начальный адрес на барабане/ленте */
extern int ext_ram_start;	/* α_МОЗУ - начальный адрес памяти */
extern int ext_ram_finish;	/* ω_МОЗУ - конечный адрес памяти */

/*
 * Выполнение обращения к барабану.
 * Все параметры находятся в регистрах УЧ, А_МЗУ, α_МОЗУ, ω_МОЗУ.
 */
void drum (t_value *sum);

t_stat fprint_sym (FILE *of, t_addr addr, t_value *val,
	UNIT *uptr, int32 sw);

#endif
