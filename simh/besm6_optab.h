/*
 *      instructions table entry
 */
typedef struct  {
	char    *o_name;
	int     (*o_impl)();
	char    o_inline;
	unsigned o_flags;
	unsigned o_count;
	unsigned o_ticks;
} optab_t;

extern optab_t  optab[];

extern int      add(), aax(), aex(), arx(),
                avx(), aox(), b6div(),  mul(),  apx(), aux(), acx(),
                anx(), epx(), emx(),  asx(), yta(),
                b6mod(), ext(),
                priv();

/*
 *      inline implementation labels
 */
#define I_ATX   1
#define I_STX   2
#define I_XTS   3
#define I_XTA   4
#define I_RTE   5
#define I_XTR   6
#define I_ATI   7
#define I_ITA   8
#define I_MTJ   9
#define I_MPJ   10
#define I_TRAP  11
#define I_UTC   12
#define I_WTC   13
#define I_VTM   14
#define I_UZA   15
#define I_UJ    16
#define I_VJM   17
#define I_STOP  18
#define I_VZM   19
#define I_VLM   20
#define I_SUB   21
#define I_RSUB  22
#define I_ASUB  23
#define I_UIA   24
#define I_ADD   25
#define I_ITS   26
#define I_NTR   27
#define I_UTM   28
#define I_STI   29
#define I_YTA   30
#define I_IRET  31
#define I_MOD   32
#define I_EXT	33

/*
 *      instruction flags
 */
#define F_LG            1
#define F_MG            2
#define F_AG            3
#define F_GRP           3
#define F_PRIV          4
#define F_RSTACK        8
#define F_WSTACK        0x10
#define F_OP            0x20
#define F_AR            0x40
#define F_NAI           0x80
#define F_REG           0x100
#define F_TRAP          0x200
#define F_STACK         0x400
#define F_AROP          0x800
