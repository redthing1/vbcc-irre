/*  Example backend for vbcc, it models a generic 32bit RISC or CISC
    CPU.

    Configurable at build-time are:
    - number of (32bit) general-purpose-registers
    - number of (64bit) floating-point-registers
    - number of (8bit) condition-code-registers
    - mechanism for stack-arguments (moving ot fixed sp)

    It allows to select as run-time-options:
    - two- or three-address code
    - memory operands or load-store-architecture
    - number of register-arguments
    - number of caller-save-registers
*/

#include "supp.h"

static char FILE_[] = __FILE__;

/*  Public data that MUST be there.                             */

/* Name and copyright. */
char cg_copyright[] = "IRRE code-generator V0.1a (c) 2020, xdrie";

#define FIXED_SP 1 // fixed sp mode

/*  Commandline-flags the code-generator accepts:
    0: just a flag
    VALFLAG: a value must be specified
    STRINGFLAG: a string can be specified
    FUNCFLAG: a function will be called
    apart from FUNCFLAG, all other versions can only be specified once */
int g_flags[MAXGF] = {0, 0, VALFLAG, VALFLAG, VALFLAG, 0, 0, VALFLAG, VALFLAG, 0};

/* the flag-name, do not use names beginning with l, L, I, D or U, because
   they collide with the frontend */
char *g_flags_name[MAXGF] = {"default-main", "load-store", "volatile-gprs", "volatile-fprs", "volatile-ccrs",
                             "imm-ind",      "gpr-ind",    "gpr-args",      "fpr-args",      "use-commons"};

/* the results of parsing the command-line-flags will be stored here */
union ppi g_flags_val[MAXGF];

/*  Alignment-requirements for all types in bytes.              */
zmax align[MAX_TYPE + 1];

/*  Alignment that is sufficient for every object.              */
zmax maxalign;

/*  CHAR_BIT for the target machine.                            */
zmax char_bit;

/*  sizes of the basic types (in bytes) */
zmax sizetab[MAX_TYPE + 1];

/*  Minimum and Maximum values each type can have.              */
/*  Must be initialized in init_cg().                           */
zmax t_min[MAX_TYPE + 1];
zumax t_max[MAX_TYPE + 1];
zumax tu_max[MAX_TYPE + 1];

/*  Names of all registers.
    register number 0 is invalid, valid registers start at 1 */
char *regnames[MAXR + 1] = {"noreg", "r0",  "r1",  "r2",  "r3",  "r4",  "r5",  "r6",  "r7",  "r8",  "r9",  "r10", "r11",
                            "r12",   "r13", "r14", "r15", "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23", "r24",
                            "r25",   "r26", "r27", "r28", "r29", "r30", "r31", "lr",  "ad",  "at",  "sp"};

/*  The Size of each register in bytes.                         */
zmax regsize[MAXR + 1];

/*  a type which can store each register. */
struct Typ *regtype[MAXR + 1];

/*  regsa[reg]!=0 if a certain register is allocated and should */
/*  not be used by the compiler pass.                           */
int regsa[MAXR + 1];

/*  Specifies which registers may be scratched by functions.    */
int regscratch[MAXR + 1] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0};

/* specifies the priority for the register-allocator, if the same
   estimated cost-saving can be obtained by several registers, the
   one with the highest priority will be used */
int reg_prio[MAXR + 1];

/* an empty reg-handle representing initial state */
struct reg_handle empty_reg_handle = {0, 0};

/* Names of target-specific variable attributes.                */
char *g_attr_name[] = {"__interrupt", 0};

/****************************************/
/*  Private data and functions.         */
/****************************************/

#define THREE_ADDR 0
#define DEFAULT_MAIN (g_flags[0] & USEDFLAG)
#define LOAD_STORE (g_flags[1] & USEDFLAG)
#define VOL_GPRS ((g_flags[2] & USEDFLAG) ? g_flags_val[2].l : NUM_GPRS / 2)
#define VOL_FPRS ((g_flags[3] & USEDFLAG) ? g_flags_val[3].l : NUM_FPRS / 2)
#define VOL_CCRS ((g_flags[4] & USEDFLAG) ? g_flags_val[4].l : NUM_CCRS / 2)
#define IMM_IND ((g_flags[5] & USEDFLAG) ? 1 : 0)
#define GPR_IND ((g_flags[6] & USEDFLAG) ? 2 : 0)
#define GPR_ARGS ((g_flags[7] & USEDFLAG) ? g_flags_val[7].l : 0)
#define FPR_ARGS ((g_flags[8] & USEDFLAG) ? g_flags_val[8].l : 0)
#define USE_COMMONS (g_flags[9] & USEDFLAG)

/* alignment of basic data-types, used to initialize align[] */
static long malign[MAX_TYPE + 1] = {1, 1, 2, 4, 4, 4, 4, 8, 8, 1, 4, 1, 1, 1, 4, 1};
/* sizes of basic data-types, used to initialize sizetab[] */
static long msizetab[MAX_TYPE + 1] = {1, 1, 2, 4, 4, 8, 4, 8, 8, 0, 4, 0, 0, 0, 4, 0};

/* used to initialize regtype[] */
static struct Typ ltyp = {LONG}, ldbl = {DOUBLE}, lchar = {CHAR};

/* macros defined by the backend */
static char *marray[] = {"__section(x)=__vattr(\"section(\"#x\")\")", "__GENERIC__", 0};

/* special registers */
static int sp = 36;                /*  Stackpointer                        */
static int lr = 33;                /*  Link Register                       */
static int ad = 34, at = 35;       /*  Special Temps                       */
static int t1 = 2, t2 = 3, t3 = 4; /*  Temporaries used by code generator  */
static int f1, f2;                 /*  Temporaries used by code generator  */

/* special constants */
#define RETURN_ADDR_SIZE 4

#define dt(t) (((t)&UNSIGNED) ? udt[(t)&NQ] : sdt[(t)&NQ])
static char *sdt[MAX_TYPE + 1] = {"??", "c", "s", "i", "l", "ll", "f", "d", "ld", "v", "p"};
static char *udt[MAX_TYPE + 1] = {"??", "uc", "us", "ui", "ul", "ull", "f", "d", "ld", "v", "p"};

/* sections */
#define DATA 0
#define BSS 1
#define CODE 2
#define RODATA 3
#define SPECIAL 4

/* helpers */
#define objvar_raw_offset(o) (o->v->offset)

// static long stack;
// static int stack_valid;
static int section = -1, newobj;
static char *codename = "\t; .text\n", *dataname = "\t; .data\n", *bssname = "", *rodataname = "\t.section\t.rodata\n";

/* return-instruction */
static char *ret = "\tret\n";

/* label at the end of the function (if any) */
static int exit_label;

/* assembly-prefixes for labels and external identifiers */
static char *labprefix = "l", *idprefix = "_";

/* stuff to keep track of the stack */
static long pushed; // callee args that have been pushed

static long localsize, rsavesize, callee_argsize;

static void emit_obj(FILE *f, struct obj *p, int t);

// optimization crap
static void peephole(struct IC *p);

/* calculate the actual current offset of an object relative to the
   stack-pointer; we use a stack layout like this:
   <---------- STACK POINTER
   ------------------------------------------------
   | arguments to called functions [size=callee_argsize] |
   ------------------------------------------------
   | return-address [size=4]                      |
   ------------------------------------------------
   | caller-save registers [size=rsavesize]       |
   ------------------------------------------------
   | local variables [size=localsize]             |
   ------------------------------------------------
   <---------- STACK FRAME
   ------------------------------------------------
   | arguments to this function                   |
   ------------------------------------------------
   All sizes will be aligned as necessary.
   In the case of a fixed stack pointer, the stack-pointer will be adjusted at
   function-entry to leave enough space for the arguments and have it
   aligned to 16 bytes. Therefore, when calling a function, the
   stack-pointer is always aligned to 16 bytes.

   This is just an example layout. Other layouts are also possible.
*/

static long real_offset(struct obj *o) {
    long off = objvar_raw_offset(o);
    long dbg1 = off;
    long v_size = zm2l(o->val.vmax);
    if (off < 0) {
        // this is a parameter
        off = localsize + rsavesize - off - zm2l(maxalign);
    }
    long dbg2 = off;
    off += callee_argsize;
    off += RETURN_ADDR_SIZE;
    off += rsavesize;
    off += v_size;
    // printf("real_offset(%ld), nga: %ld, ca: %ld, vs: %ld, adj: %ld\n", dbg1, dbg2, callee_argsize, v_size, off);
    return off;
    // if (off < 0) {
    //     /* function parameter */
    //     printf("parm: %ld, save: %ld, loc: %ld, ma: %ld", off, rsavesize, localsize, zm2l(maxalign));
    //     off = rsavesize + localsize - off - zm2l(maxalign);
    //     printf(", adj: %ld\n", off);
    // }

    // off += callee_argsize;

    // long v_size = zm2l(o->val.vmax);
    // off += v_size;
    // printf("ro: %ld, as: %ld, sz: %ld\n", off, callee_argsize, v_size);
    // return off;
}

/*  Initializes an addressing-mode structure and returns a pointer to
    that object. Will not survive a second call! */
static struct obj *cam(int flags, int base, long offset) {
    static struct obj obj;
    static struct AddressingMode am;
    obj.am = &am;
    am.flags = flags;
    am.base = base;
    am.offset = offset;
    return &obj;
}

/* generate .file title/header */
void title(FILE *f) {
    static int done;
    extern char *inname; /*grmpf*/
    if (!done && f) {
        done = 1;
        if (DEFAULT_MAIN) {
            printf("setting default _main.\n");
            emit(f, "%%entry: _main\n");
        }
        emit(f, "\t; .file\t\"%s\"\n", inname);
    }
}

/* changes to a special section, used for __section() */
static int special_section(FILE *f, struct Var *v) {
    char *sec;
    if (!v->vattr)
        return 0;
    sec = strstr(v->vattr, "section(");
    if (!sec)
        return 0;
    sec += strlen("section(");
    emit(f, "\t.section\t");
    while (*sec && *sec != ')')
        emit_char(f, *sec++);
    emit(f, "\n");
    if (f)
        section = SPECIAL;
    return 1;
}

/* generate code to load the address of a variable into register r */
static void load_address(FILE *f, int r, struct obj *o, int type)
/*  Generates code to load the address of a variable into register r.   */
{
    if (!(o->flags & VAR))
        ierror(0);
    if (o->v->storage_class == AUTO || o->v->storage_class == REGISTER) {
        long off = real_offset(o);
        // var pointer
        emit(f, "\tmov\t%s\t%s\t; &var\n", regnames[r], regnames[sp]);
        if (off)
            emit(f, "\tadi\t%s\t%s\t#%ld\n", regnames[r], regnames[r], off);
    } else if (o->v->storage_class == EXTERN || o->v->storage_class == STATIC) {
        // pointer address
        emit(f, "\tset\t%s\t", regnames[r]);
        emit_obj(f, o, type);
        emit(f, "\n");
    } else {
        emit(f, "\tmov.%s\t%s,", dt(POINTER), regnames[r]);
        emit_obj(f, o, type);
        emit(f, "\n");
    }
}
/* Generates code to load a memory object into register r. tmp is a
   general purpose register which may be used. tmp can be r. */
static void load_reg(FILE *f, int r, struct obj *o, int type) {
    type &= NU;
    if (o->flags & VARADR) {
        load_address(f, r, o, POINTER);
    } else {
        // if operand is REG, skip because it would be redundant
        if ((o->flags & (REG | DREFOBJ)) == REG && o->reg == r)
            return;

        if ((o->flags & KONST) > 0) {
            // constant, use SET
            emit(f, "\tset\t%s\t", regnames[r]);
            emit_obj(f, o, type);
            emit(f, "\n");
        } else {
            if ((o->flags & REG) > 0) {
                // source is register
                // check if it needs to be dereferenced
                if ((o->flags & DREFOBJ) > 0) {
                    // we need to dereference reg val
                    // so use LDW
                    emit(f, "\tldw\t%s\t%s\t#0", regnames[r], regnames[o->reg]);
                    emit(f, "\n");
                } else {
                    // we can MOV value from register
                    emit(f, "\tmov\t%s\t", regnames[r]);
                    emit_obj(f, o, type);
                    emit(f, "\n");
                }
            } else if ((o->flags & VAR) > 0) {
                // source is var, use ldw
                if (o->v->storage_class == EXTERN || o->v->storage_class == STATIC) {
                    // address is var
                    // TODO: maybe don't use AT, use temp instead
                    // 1. load address to temp
                    emit(f, "\tset\t%s\t", regnames[at]);
                    emit_obj(f, o, type);
                    emit(f, "\n");
                    // 2. load from address
                    emit(f, "\tldw\t%s\t%s\t#0\n", regnames[r], regnames[at]);
                } else {
                    // address is register
                    emit(f, "\tldw\t%s\t", regnames[r]);
                    emit_obj(f, o, type);
                    emit(f, "\n");
                }
            } else {
                // unknown
                ierror(0);
            }
        }
    }
}

/*  Generates code to store register r into memory object o. */
static void store_reg(FILE *f, int r, struct obj *o, int type) {
    type &= NQ;
    // emit(f, "\tmov.%s\t", dt(type));
    // emit_obj(f, o, type);
    // emit(f, ",%s\n", regnames[r]);

    // store register into memory
    if ((o->flags & VAR) > 0) {
        // variable, check storage type
        if (o->v->storage_class == AUTO || o->v->storage_class == REGISTER) {
            // dest is reg
            emit(f, "\tstw\t%s\t", regnames[r]);
            emit_obj(f, o, type);
            emit(f, "\n");
        } else if (o->v->storage_class == STATIC || o->v->storage_class == EXTERN) {
            // dest is static/extern
            // put the var address in a temp reg
            load_address(f, at, o, type);
            // TODO: maybe don't use AT, use temp instead
            emit(f, "\tstw\t%s\t%s\t#0", regnames[r], regnames[at]);
            emit(f, "\n");
        } else {
            // unknown
            printf("unknown (VAR) store_reg (storage %d)\n", o->v->storage_class);
            ierror(0);
        }
    } else if ((o->flags & (DREFOBJ)) > 0) {
        if ((o->flags & (REG)) > 0) {
            // use value in register as pointer into memory
            emit(f, "\tstw\t%s\t%s\t#0", regnames[r], regnames[o->reg]);
            emit(f, "\n");
        } else {
            // unknown
            printf("unknown (DREFOBJ) store_reg (flags %d)\n", o->flags);
            ierror(0);
        }
    } else {
        // unknown
        printf("unknown store_reg (flags %d)\n", o->flags);
        ierror(0);
    }
}

/*  Yields log2(x)+1 or 0. */
static long pof2(zumax x) {
    zumax p;
    int ln = 1;
    p = ul2zum(1L);
    while (ln <= 32 && zumleq(p, x)) {
        if (zumeqto(x, p))
            return ln;
        ln++;
        p = zumadd(p, p);
    }
    return 0;
}

static struct IC *preload(FILE *, struct IC *);

static void function_top(FILE *, struct Var *, long);
static void function_bottom(FILE *f, struct Var *, long);

#define isreg(x) ((p->x.flags & (REG | DREFOBJ)) == REG)
#define isconst(x) ((p->x.flags & (KONST | DREFOBJ)) == KONST)

static int q1reg, q2reg, zreg;

static char *ccs[] = {"eq", "ne", "lt", "ge", "le", "gt", ""};
static char *logicals[] = {"or", "xor", "and"};
static char *arithmetics[] = {"slw", "srw", "add", "sub", "mullw", "divw", "mod"};

/* Does some pre-processing like fetching operands from memory to
   registers etc. */
static struct IC *preload(FILE *f, struct IC *p) {
    int r;

    if (isreg(q1))
        q1reg = p->q1.reg;
    else
        q1reg = 0;

    if (isreg(q2))
        q2reg = p->q2.reg;
    else
        q2reg = 0;

    if (isreg(z)) {
        zreg = p->z.reg;
    } else {
        if (ISFLOAT(ztyp(p)))
            zreg = f1;
        else
            zreg = t1;
    }

    if ((p->q1.flags & (DREFOBJ | REG)) == DREFOBJ && !p->q1.am) {
        p->q1.flags &= ~DREFOBJ;
        load_reg(f, t1, &p->q1, q1typ(p));
        p->q1.reg = t1;
        p->q1.flags |= (REG | DREFOBJ);
    }
    if (p->q1.flags && LOAD_STORE && !isreg(q1)) {
        if (ISFLOAT(q1typ(p)))
            q1reg = f1;
        else
            q1reg = t1;
        load_reg(f, q1reg, &p->q1, q1typ(p));
        p->q1.reg = q1reg;
        p->q1.flags = REG;
    }

    if ((p->q2.flags & (DREFOBJ | REG)) == DREFOBJ && !p->q2.am) {
        p->q2.flags &= ~DREFOBJ;
        load_reg(f, t1, &p->q2, q2typ(p));
        p->q2.reg = t1;
        p->q2.flags |= (REG | DREFOBJ);
    }
    if (p->q2.flags && LOAD_STORE && !isreg(q2)) {
        if (ISFLOAT(q2typ(p)))
            q2reg = f2;
        else
            q2reg = t2;
        load_reg(f, q2reg, &p->q2, q2typ(p));
        p->q2.reg = q2reg;
        p->q2.flags = REG;
    }
    return p;
}

/* save the result (in zreg) into p->z */
void save_result(FILE *f, struct IC *p) {
    if ((p->z.flags & (REG | DREFOBJ)) == DREFOBJ && !p->z.am) {
        p->z.flags &= ~DREFOBJ;
        load_reg(f, t2, &p->z, POINTER);
        p->z.reg = t2;
        p->z.flags |= (REG | DREFOBJ);
    }
    if (isreg(z)) {
        // store result reg into register (z)
        if (p->z.reg != zreg) {
            emit(f, "\tmov\t%s\t%s\n", regnames[p->z.reg], regnames[zreg]);
        }
    } else {
        // store result reg into object
        store_reg(f, zreg, &p->z, ztyp(p));
    }
}

/* prints an object */
static void emit_obj(FILE *f, struct obj *p, int t) {
    if (p->am) {
        if (p->am->flags & GPR_IND)
            emit(f, "(%s,%s)", regnames[p->am->offset], regnames[p->am->base]);
        if (p->am->flags & IMM_IND)
            emit(f, "(%ld,%s)", p->am->offset, regnames[p->am->base]);
        return;
    }
    if ((p->flags & (KONST | DREFOBJ)) == (KONST | DREFOBJ)) {
        // emit(f, "#"); // decimal constant ??
        emitval(f, &p->val, p->dtyp & NU);
        return;
    }
    if (p->flags & DREFOBJ) {
        emit(f, "(");
    }
    if (p->flags & REG) {
        emit(f, "%s", regnames[p->reg]);
    } else if (p->flags & VAR) {
        if (p->v->storage_class == AUTO || p->v->storage_class == REGISTER) {
            // stack offset location
            emit(f, "%s\t#%ld", regnames[sp], real_offset(p));
            // is it an arg passed to this function?
            if (objvar_raw_offset(p) < 0) {
                // TODO: can we improve this? this hardcodes arg size
                long argi = (objvar_raw_offset(p) + zm2l(maxalign)) / 4;
                emit(f, "\t; get arg_%ld", argi);
            }

        } else {
            if (p->v->storage_class == STATIC) {
                emit(f, "::%s[??]%ld", labprefix, zm2l(p->v->offset));
            } else {
                emit(f, "::%s%s", idprefix, p->v->identifier);
            }
            if (!zmeqto(l2zm(0L), p->val.vmax)) {
                emit(f, "^#");
                emitval(f, &p->val, LONG);
            }
        }
    }
    if (p->flags & KONST) {
        emit(f, "#"); // decimal constant
        emitval(f, &p->val, t & NU);
    }
    if (p->flags & DREFOBJ)
        emit(f, ")");
}

/* generates the function entry code */
static void function_top(FILE *f, struct Var *v, long offset) {
    rsavesize = 0;
    if (!special_section(f, v) && section != CODE) {
        emit(f, codename);
        if (f)
            section = CODE;
    }
    if (v->storage_class == EXTERN) {
        if ((v->flags & (INLINEFUNC | INLINEEXT)) != INLINEFUNC)
            emit(f, "\t; .global\t%s%s\n", idprefix, v->identifier);
        emit(f, "%s%s:\n", idprefix, v->identifier);
    } else
        emit(f, "%s%ld:\n", labprefix, zm2l(v->offset));
    if (offset) {
        emit(f, "\tsbi\t%s\t%s\t#%ld\n", regnames[sp], regnames[sp], offset);
    }
}

/* generates the function exit code */
static void function_bottom(FILE *f, struct Var *v, long offset) {
    if (offset) {
        emit(f, "\tadi\t%s\t%s\t#%ld\n", regnames[sp], regnames[sp], offset);
    }
    emit(f, ret);
}

/****************************************/
/*  End of private data and functions.  */
/****************************************/

/*  Does necessary initializations for the code-generator. Gets called  */
/*  once at the beginning and should return 0 in case of problems.      */
int init_cg(void) {
    int i;
    /*  Initialize some values which cannot be statically initialized   */
    /*  because they are stored in the target's arithmetic.             */
    maxalign = l2zm(8L);
    char_bit = l2zm(8L);
    stackalign = l2zm(4);

    for (i = 0; i <= MAX_TYPE; i++) {
        sizetab[i] = l2zm(msizetab[i]);
        align[i] = l2zm(malign[i]);
    }

    // init register stuff
    for (i = FIRST_GPR; i <= LAST_GPR; i++) {
        regsize[i] = l2zm(4L); // 4-byte words
        regtype[i] = &ltyp;    // use default initialization
    }

    /*  Use multiple ccs.   */
    multiple_ccs = 0;

    /*  Initialize the min/max-settings. Note that the types of the     */
    /*  host system may be different from the target system and you may */
    /*  only use the smallest maximum values ANSI guarantees if you     */
    /*  want to be portable.                                            */
    /*  That's the reason for the subtraction in t_min[INT]. Long could */
    /*  be unable to represent -2147483648 on the host system.          */
    t_min[CHAR] = l2zm(-128L);
    t_min[SHORT] = l2zm(-32768L);
    t_min[INT] = zmsub(l2zm(-2147483647L), l2zm(1L));
    t_min[LONG] = t_min(INT);
    t_min[LLONG] = zmlshift(l2zm(1L), l2zm(63L));
    t_min[MAXINT] = t_min(LLONG);
    t_max[CHAR] = ul2zum(127L);
    t_max[SHORT] = ul2zum(32767UL);
    t_max[INT] = ul2zum(2147483647UL);
    t_max[LONG] = t_max(INT);
    t_max[LLONG] = zumrshift(zumkompl(ul2zum(0UL)), ul2zum(1UL));
    t_max[MAXINT] = t_max(LLONG);
    tu_max[CHAR] = ul2zum(255UL);
    tu_max[SHORT] = ul2zum(65535UL);
    tu_max[INT] = ul2zum(4294967295UL);
    tu_max[LONG] = t_max(UNSIGNED | INT);
    tu_max[LLONG] = zumkompl(ul2zum(0UL));
    tu_max[MAXINT] = t_max(UNSIGNED | LLONG);

    /*  Reserve a few registers for use by the code-generator.      */
    /*  This is not optimal but simple.                             */
    // sp=FIRST_GPR;
    // t1=FIRST_GPR+1;
    // t2=FIRST_GPR+2;
    f1 = FIRST_FPR + 1;
    f2 = FIRST_FPR + 2;

    // - reserve registers
    // temporaries
    regsa[t1] = regsa[t2] = regsa[t3] = 1;
    regsa[f1] = regsa[f2] = 1;
    // special regs
    regsa[lr] = 1;
    regsa[ad] = 1;
    regsa[at] = 1;
    regsa[sp] = 1;

    // these should all already be set
    // regscratch[t1]=regscratch[t2]=0;
    // regscratch[f1]=regscratch[f2]=0;
    // regscratch[sp]=0;

    for (i = FIRST_GPR; i <= LAST_GPR - VOL_GPRS; i++)
        regscratch[i] = 1;
    for (i = FIRST_FPR; i <= LAST_FPR - VOL_FPRS; i++)
        regscratch[i] = 1;
    for (i = FIRST_CCR; i <= LAST_CCR - VOL_CCRS; i++)
        regscratch[i] = 1;

    target_macros = marray;

    return 1;
}

void init_db(FILE *f) {}

int freturn(struct Typ *t)
/*  Returns the register in which variables of type t are returned. */
/*  If the value cannot be returned in a register returns 0.        */
/*  A pointer MUST be returned in a register. The code-generator    */
/*  has to simulate a pseudo register if necessary.                 */
{
    if (ISFLOAT(t->flags)) {
        // float return
        return FIRST_FPR;
    }
    if (ISSTRUCT(t->flags) || ISUNION(t->flags)) {
        // struct or union return
        return 0;
    }
    if (zmleq(szof(t), l2zm(4L))) {
        // 32-bit word return
        return FIRST_GPR;
    } else
        return 0;
}

int reg_pair(int r, struct rpair *p)
/* Returns 0 if the register is no register pair. If r  */
/* is a register pair non-zero will be returned and the */
/* structure pointed to p will be filled with the two   */
/* elements.                                            */
{
    return 0;
}

/* estimate the cost-saving if object o from IC p is placed in
   register r */
int cost_savings(struct IC *p, int r, struct obj *o) {
    int c = p->code;
    if (o->flags & VKONST) {
        if (!LOAD_STORE)
            return 0;
        if (o == &p->q1 && p->code == ASSIGN && (p->z.flags & DREFOBJ))
            return 4;
        else
            return 2;
    }
    if (o->flags & DREFOBJ)
        return 4;
    if (c == SETRETURN && r == p->z.reg && !(o->flags & DREFOBJ)) {
        return 3;
    }
    if (c == GETRETURN && r == p->q1.reg && !(o->flags & DREFOBJ)) {
        return 3;
    }
    return 2;
}

int regok(int r, int t, int mode)
/*  Returns 0 if register r cannot store variables of   */
/*  type t. If t==POINTER and mode!=0 then it returns   */
/*  non-zero only if the register can store a pointer   */
/*  and dereference a pointer to mode.                  */
{
    if (r == 0)
        return 0;
    t &= NQ;
    if (t == 0 && r >= FIRST_CCR && r <= LAST_CCR)
        return 1;
    if (ISFLOAT(t) && r >= FIRST_FPR && r <= LAST_FPR)
        return 1;
    if (t == POINTER && r >= FIRST_GPR && r <= LAST_GPR)
        return 1;
    if (t >= CHAR && t <= LONG && r >= FIRST_GPR && r <= LAST_GPR)
        return 1;
    return 0;
}

int dangerous_IC(struct IC *p)
/*  Returns zero if the IC p can be safely executed     */
/*  without danger of exceptions or similar things.     */
/*  vbcc may generate code in which non-dangerous ICs   */
/*  are sometimes executed although control-flow may    */
/*  never reach them (mainly when moving computations   */
/*  out of loops).                                      */
/*  Typical ICs that generate exceptions on some        */
/*  machines are:                                       */
/*      - accesses via pointers                         */
/*      - division/modulo                               */
/*      - overflow on signed integer/floats             */
{
    int c = p->code;
    if ((p->q1.flags & DREFOBJ) || (p->q2.flags & DREFOBJ) || (p->z.flags & DREFOBJ))
        return 1;
    if ((c == DIV || c == MOD) && !isconst(q2))
        return 1;
    return 0;
}

int must_convert(int o, int t, int const_expr)
/*  Returns zero if code for converting np to type t    */
/*  can be omitted.                                     */
/*  On the PowerPC cpu pointers and 32bit               */
/*  integers have the same representation and can use   */
/*  the same registers.                                 */
{
    int op = o & NQ, tp = t & NQ;
    if ((op == INT || op == LONG || op == POINTER) && (tp == INT || tp == LONG || tp == POINTER))
        return 0;
    if (op == DOUBLE && tp == LDOUBLE)
        return 0;
    if (op == LDOUBLE && tp == DOUBLE)
        return 0;
    return 1;
}

void gen_ds(FILE *f, zmax size, struct Typ *t)
/*  This function has to create <size> bytes of storage */
/*  initialized with zero.                              */
{
    if (newobj && section != SPECIAL) {
        // generate bytes of storage
        // TODO: rework this
        emit(f, "%ld\n", zm2l(size));                        // raw number
        emit(f, "\t%%d\t\\z\t#%ld\t; zero data\n", zm2l(size)); // zero data directive
    } else {
        emit(f, "\t%%d\t\\z\t#%ld\t; space\n", zm2l(size)); // zero data directive
    }
    newobj = 0;
}

void gen_align(FILE *f, zmax align)
/*  This function has to make sure the next data is     */
/*  aligned to multiples of <align> bytes.              */
{
    if (zm2l(align) > 1)
        emit(f, "\t.align\t2\n");
}

void gen_var_head(FILE *f, struct Var *v)
/*  This function has to create the head of a variable  */
/*  definition, i.e. the label and information for      */
/*  linkage etc.                                        */
{
    int constflag;
    char *sec;
    if (v->clist)
        constflag = is_const(v->vtyp);
    if (v->storage_class == STATIC) {
        if (ISFUNC(v->vtyp->flags))
            return;
        if (!special_section(f, v)) {
            if (v->clist && (!constflag || (g_flags[2] & USEDFLAG)) && section != DATA) {
                emit(f, dataname);
                if (f)
                    section = DATA;
            }
            if (v->clist && constflag && !(g_flags[2] & USEDFLAG) && section != RODATA) {
                emit(f, rodataname);
                if (f)
                    section = RODATA;
            }
            if (!v->clist && section != BSS) {
                emit(f, bssname);
                if (f)
                    section = BSS;
            }
        }
        if (v->clist || section == SPECIAL) {
            gen_align(f, falign(v->vtyp));
            emit(f, "%s%ld:\n", labprefix, zm2l(v->offset));
        } else
            emit(f, "\t; .lcomm\t%s%ld,", labprefix, zm2l(v->offset));
        newobj = 1;
    }
    if (v->storage_class == EXTERN) {
        emit(f, "\t; .globl\t%s%s\n", idprefix, v->identifier);
        if (v->flags & (DEFINED | TENTATIVE)) {
            if (!special_section(f, v)) {
                if (v->clist && (!constflag || (g_flags[2] & USEDFLAG)) && section != DATA) {
                    emit(f, dataname);
                    if (f)
                        section = DATA;
                }
                if (v->clist && constflag && !(g_flags[2] & USEDFLAG) && section != RODATA) {
                    emit(f, rodataname);
                    if (f)
                        section = RODATA;
                }
                if (!v->clist && section != BSS) {
                    emit(f, bssname);
                    if (f)
                        section = BSS;
                }
            }
            if (v->clist || section == SPECIAL) {
                gen_align(f, falign(v->vtyp));
                emit(f, "%s%s:\n", idprefix, v->identifier);
            } else {
                // .global
                emit(f, "\t; .global\t%s%s\n", idprefix, v->identifier);
                // make variable label
                emit(f, "\t %s%s:\t; global variable\n", idprefix, v->identifier);
                // commons (.lcomm, etc.)
                char *commons_pfx = (USE_COMMONS ? "" : "l");
                emit(f, "\t; .%scomm\t%s%s,", commons_pfx, idprefix, v->identifier);
            }
            newobj = 1;
        }
    }
}

void gen_dc(FILE *f, int t, struct const_list *p)
/*  This function has to create static storage          */
/*  initialized with const-list p.                      */
{
    emit(f, "\t%%d\t\\x\t", dt(t & NQ));
    if (!p->tree) {
        // ?
        if (ISFLOAT(t)) {
            emit(f, "[dc?f]");
            /*  auch wieder nicht sehr schoen und IEEE noetig   */
            unsigned char *ip;
            ip = (unsigned char *)&p->val.vdouble;
            emit(f, "0x%02x%02x%02x%02x", ip[0], ip[1], ip[2], ip[3]);
            if ((t & NQ) != FLOAT) {
                emit(f, ",0x%02x%02x%02x%02x", ip[4], ip[5], ip[6], ip[7]);
            }
        } else {
            // raw number
            // TODO: this only supports up to ints (4 bytes)
            // emitval(f, &p->val, t & NU);
            int nut = t & NU;
            long v = p->val.vulong;
            char vbuf[9];
            // convert it to pack format
            if (nut & CHAR)
                sprintf(vbuf, "%02x", v);
            else if (nut & SHORT)
                sprintf(vbuf, "%04x", v);
            else if (nut & INT)
                sprintf(vbuf, "%08x", v);
            else {
                printf("[irre/gen_dc] unsupported width");
                ierror(0);
            }

            emit(f, "%s", vbuf);
        }
    } else {
        emit(f, "[dc?2]");
        emit_obj(f, &p->tree->o, t & NU);
    }
    emit(f, "\n");
    newobj = 0;
}

/*  The main code-generation routine.                   */
/*  f is the stream the code should be written to.      */
/*  p is a pointer to a doubly linked list of ICs       */
/*  containing the function body to generate code for.  */
/*  v is a pointer to the function.                     */
/*  offset is the size of the stackframe the function   */
/*  needs for local variables.                          */

void gen_code(FILE *f, struct IC *p, struct Var *v, zmax frame_offset)
/*  The main code-generation.                                           */
{
    int c, t, i;
    struct IC *m;
    Var *func_var = v;
    callee_argsize = 0;
    if (DEBUG & 1)
        printf("gen_code()\n");
    for (c = 1; c <= MAXR; c++)
        regs[c] = regsa[c];

    title(f);

    for (m = p; m; m = m->next) {
        c = m->code;
        t = m->typf & NU;
        if (c == ALLOCREG) {
            regs[m->q1.reg] = 1;
            continue;
        }
        if (c == FREEREG) {
            regs[m->q1.reg] = 0;
            continue;
        }

        /* convert MULT/DIV/MOD with powers of two */
        if ((t & NQ) <= LONG && (m->q2.flags & (KONST | DREFOBJ)) == KONST && (t & NQ) <= LONG &&
            (c == MULT || ((c == DIV || c == MOD) && (t & UNSIGNED)))) {
            eval_const(&m->q2.val, t);
            i = pof2(vmax);
            if (i) {
                if (c == MOD) {
                    vmax = zmsub(vmax, l2zm(1L));
                    m->code = AND;
                } else {
                    vmax = l2zm(i - 1);
                    if (c == DIV)
                        m->code = RSHIFT;
                    else
                        m->code = LSHIFT;
                }
                c = m->code;
                gval.vmax = vmax;
                eval_const(&gval, MAXINT);
                if (c == AND) {
                    insert_const(&m->q2.val, t);
                } else {
                    insert_const(&m->q2.val, INT);
                    p->typf2 = INT;
                }
            }
        }
        // if there are any function calls
        // set callee_argsize (the size of args sent to called functions)
        // to the highest offset needed to pass args
        if (c == CALL && callee_argsize < pushedargsize(m))
            callee_argsize = pushedargsize(m);
    }
    peephole(p);

    for (c = 1; c <= MAXR; c++) {
        if (regsa[c] || regused[c]) {
            BSET(regs_modified, c);
        }
    }

    // calculate word-aligned frame offset
    localsize = (zm2l(frame_offset) + 3) / 4 * 4;
    // locals, return address, callee args
    long stk_lower_size = localsize + RETURN_ADDR_SIZE + callee_argsize;

    function_top(f, v, stk_lower_size);
    emit(f, "\t; loc_sz=%lu, rs_sz=%lu, ce_sz=%lu\n", localsize, rsavesize, callee_argsize);

    // no callee args have been pushed yet
    pushed = 0;

    for (; p; p = p->next) {
        c = p->code;
        t = p->typf;
        if (c == NOP) {
            p->z.flags = 0;
            continue;
        }
        if (c == ALLOCREG) {
            regs[p->q1.reg] = 1;
            continue;
        }
        if (c == FREEREG) {
            regs[p->q1.reg] = 0;
            continue;
        }
        if (c == LABEL) {
            emit(f, "%s%d:\n", labprefix, t);
            continue;
        }
        if (c == BRA) {
            // // use AT to store jump target
            // emit(f, "\tset\tat\t");
            // emit(f, "::%s%d", labprefix, t);
            // emit(f, "\n");
            // emit(f, "\tjmp\tat");
            // emit(f, "\n");
            // use JMI to directly jump
            emit(f, "\tjmi\t");
            emit(f, "::%s%d", labprefix, t);
            emit(f, "\n");
            // if (0 /*t==exit_label&&framesize==0*/)
            //     emit(f, ret);
            // else
            continue;
        }
        if (c >= BEQ && c < BRA) {
            emit(f, "\tb%s\t", ccs[c - BEQ]);
            if (isreg(q1)) {
                emit(f, "[Q1 IS REG]");
                emit_obj(f, &p->q1, 0);
            }
            emit(f, "::%s%d\n", labprefix, t);
            continue;
        }
        if (c == MOVETOREG) {
            load_reg(f, p->z.reg, &p->q1, p->typf);
            continue;
        }
        if (c == MOVEFROMREG) {
            store_reg(f, p->q1.reg, &p->z, p->typf);
            continue;
        }
        if ((c == ASSIGN || c == PUSH) && ((t & NQ) > POINTER || ((t & NQ) == CHAR && zm2l(p->q2.val.vmax) != 1))) {
            ierror(0);
        }
        p = preload(f, p);
        c = p->code;
        if (c == SUBPFP)
            c = SUB;
        if (c == ADDI2P)
            c = ADD;
        if (c == SUBIFP)
            c = SUB;
        if (c == CONVERT) {
            if (ISFLOAT(q1typ(p)) || ISFLOAT(ztyp(p)))
                ierror(0);
            if (sizetab[q1typ(p) & NQ] < sizetab[ztyp(p) & NQ]) {
                if (q1typ(p) & UNSIGNED)
                    emit(f, "\tzext.%s\t%s\n", dt(q1typ(p)), regnames[zreg]);
                else
                    emit(f, "\tsext.%s\t%s\n", dt(q1typ(p)), regnames[zreg]);
            }
            save_result(f, p);
            continue;
        }
        if (c == KOMPLEMENT) {
            load_reg(f, zreg, &p->q1, t);
            emit(f, "\tcpl.%s\t%s\n", dt(t), regnames[zreg]);
            save_result(f, p);
            continue;
        }
        if (c == SETRETURN) {
            load_reg(f, p->z.reg, &p->q1, t);
            BSET(regs_modified, p->z.reg);
            continue;
        }
        if (c == GETRETURN) {
            if (p->q1.reg) {
                zreg = p->q1.reg;
                save_result(f, p);
            } else
                p->z.flags = 0;
            continue;
        }
        if (c == CALL) {
            int reg;
            /*FIXME*/
#if 0      
      if(stack_valid&&(p->q1.flags&(VAR|DREFOBJ))==VAR&&p->q1.v->fi&&(p->q1.v->fi->flags&ALL_STACK)){
	if(framesize+zum2ul(p->q1.v->fi->stack1)>stack)
	  stack=framesize+zum2ul(p->q1.v->fi->stack1);
      }else
	stack_valid=0;
#endif

            // size of all args pushed
            long args_size = pushedargsize(p);
            emit(f, "\t; sz_passed=%ld\n", args_size);

            if ((p->q1.flags & (VAR | DREFOBJ)) == VAR && p->q1.v->fi && p->q1.v->fi->inline_asm) {
                emit_inline_asm(f, p->q1.v->fi->inline_asm);
            } else {
                // - store return address
                long return_addr_slot = args_size;
                emit(f, "\tstw\tlr\tsp\t#%ld\n", return_addr_slot);
                // use the AT register to store call location
                emit(f, "\tset\tat\t");
                emit_obj(f, &p->q1, t);
                emit(f, "\n");
                emit(f, "\tcal\tat");
                emit(f, "\n");
                // - load return address
                emit(f, "\tldw\tlr\tsp\t#%ld\n", return_addr_slot);
            }

            pushed -= args_size; // decrease pushed by size of all args used to call

            if ((p->q1.flags & (VAR | DREFOBJ)) == VAR && p->q1.v->fi && (p->q1.v->fi->flags & ALL_REGS)) {
                bvunite(regs_modified, p->q1.v->fi->regs_modified, RSIZE);
            } else {
                int i;
                for (i = 1; i <= MAXR; i++) {
                    if (regscratch[i])
                        BSET(regs_modified, i);
                }
            }
            continue;
        }
        if (c == ASSIGN || c == PUSH) {
            if (t == 0)
                ierror(0);
            if (c == PUSH) {
                // size of the thing pushed
                long arg_size = pushsize(p);
                // 1. grab the value into a temp reg
                q1reg = t1;
                load_reg(f, q1reg, &p->q1, t);
                // 2. store that temp reg into the stack
                // TODO: argi hardcodes arg size
                long argi = pushed / 4;
                emit(f, "\tstw\t%s\t%s\t#%ld\t; set arg_%ld", regnames[q1reg], regnames[sp], pushed, argi);
                emit(f, "\n");
                pushed += arg_size; // increase pushed by size of thing pushed
                continue;
            }
            if (c == ASSIGN) {
                load_reg(f, zreg, &p->q1, t);
                save_result(f, p);
            }
            continue;
        }
        if (c == ADDRESS) {
            load_address(f, zreg, &p->q1, POINTER);
            save_result(f, p);
            continue;
        }
        if (c == MINUS) {
            load_reg(f, zreg, &p->q1, t);
            emit(f, "\tneg.%s\t%s\n", dt(t), regnames[zreg]);
            save_result(f, p);
            continue;
        }
        if (c == TEST || c == COMPARE) {
            int is_test = c == TEST;
            // grab q1 into reg
            q1reg = t1;
            load_reg(f, q1reg, &p->q1, t);

            // second arg
            q2reg = t2;
            if (!is_test) {
                load_reg(f, q2reg, &p->q2, t);
            } else {
                // put zero in q2reg
                emit(f, "\tset\t%s\t#0\n", regnames[q2reg]);
            }

            emit(f, "\tcmp\t%s\t%s", regnames[q1reg], regnames[q2reg]);
            emit(f, "\n");
            continue;
        }
        if ((c >= OR && c <= AND) || (c >= LSHIFT && c <= MOD)) {
            // binary arithmetic/logical operations (binop)
            // put load sources into qregs
            q1reg = t1;
            q2reg = t2;
            load_reg(f, q1reg, &p->q1, t);
            load_reg(f, q2reg, &p->q2, t);

            // select dest reg
            if ((p->z.flags & REG) > 0) {
                // if dest is already a register, go directly
                zreg = p->z.reg;
            } else {
                // dest isn't a reg, use a temp
                zreg = t3;
            }

            if (c >= OR && c <= AND) {
                emit(f, "\t%s\t%s\t%s\t%s", logicals[c - OR], regnames[zreg], regnames[q1reg], regnames[q2reg]);
            } else {
                emit(f, "\t%s\t%s\t%s\t%s", arithmetics[c - LSHIFT], regnames[zreg], regnames[q1reg], regnames[q2reg]);
            }
            emit(f, "\n");
            save_result(f, p);
            continue;
        }
        pric2(stdout, p);
        ierror(0);
    }

    function_bottom(f, v, stk_lower_size);
    // if (stack_valid) {
    //     if (!v->fi)
    //         v->fi = new_fi();
    //     v->fi->flags |= ALL_STACK;
    //     v->fi->stack1 = stack; // ??
    // }
    // emit(f, "# stacksize=%lu%s\n", zum2ul(stack), stack_valid ? "" : "+??");
}

int shortcut(int code, int typ) { return 0; }

/* get register to be used for arg passing. zero means none. */
int reg_parm(struct reg_handle *m, struct Typ *t, int vararg, struct Typ *d) {
    int f;
    f = t->flags & NQ;
    // if (f <= LONG || f == POINTER) {
    //     if (m->gregs >= GPR_ARGS)
    //         return 0;
    //     else
    //         return FIRST_GPR + 3 + m->gregs++;
    // }
    // if (ISFLOAT(f)) {
    //     if (m->fregs >= FPR_ARGS)
    //         return 0;
    //     else
    //         return FIRST_FPR + 2 + m->fregs++;
    // }
    return 0;
}

int handle_pragma(const char *s) {}
void cleanup_cg(FILE *f) {}
void cleanup_db(FILE *f) {
    if (f)
        section = -1;
}

/*  Test if there is a sequence of FREEREGs containing FREEREG reg.
    Used by peephole. */
static int exists_freereg(struct IC *p, int reg) {
    while (p && (p->code == FREEREG || p->code == ALLOCREG)) {
        if (p->code == FREEREG && p->q1.reg == reg)
            return 1;
        p = p->next;
    }
    return 0;
}

/* search for possible addressing-modes */
static void peephole(struct IC *p) {
    int c, c2, r;
    struct IC *p2;
    struct AddressingMode *am;

    for (; p; p = p->next) {
        c = p->code;
        if (c != FREEREG && c != ALLOCREG && (c != SETRETURN || !isreg(q1) || p->q1.reg != p->z.reg))
            exit_label = 0;
        if (c == LABEL)
            exit_label = p->typf;

        /* Try const(reg) */
        if (IMM_IND && (c == ADDI2P || c == SUBIFP) && isreg(z) && (p->q2.flags & (KONST | DREFOBJ)) == KONST) {
            int base;
            zmax of;
            struct obj *o;
            eval_const(&p->q2.val, p->typf);
            if (c == SUBIFP)
                of = zmsub(l2zm(0L), vmax);
            else
                of = vmax;
            if (1 /*zmleq(l2zm(-32768L),vmax)&&zmleq(vmax,l2zm(32767L))*/) {
                r = p->z.reg;
                if (isreg(q1))
                    base = p->q1.reg;
                else
                    base = r;
                o = 0;
                for (p2 = p->next; p2; p2 = p2->next) {
                    c2 = p2->code;
                    if (c2 == CALL || c2 == LABEL || (c2 >= BEQ && c2 <= BRA))
                        break;
                    if (c2 != FREEREG && (p2->q1.flags & (REG | DREFOBJ)) == REG && p2->q1.reg == r)
                        break;
                    if (c2 != FREEREG && (p2->q2.flags & (REG | DREFOBJ)) == REG && p2->q2.reg == r)
                        break;
                    if (c2 != CALL && (c2 < LABEL || c2 > BRA) /*&&c2!=ADDRESS*/) {
                        if (!p2->q1.am && (p2->q1.flags & (REG | DREFOBJ)) == (REG | DREFOBJ) && p2->q1.reg == r) {
                            if (o)
                                break;
                            o = &p2->q1;
                        }
                        if (!p2->q2.am && (p2->q2.flags & (REG | DREFOBJ)) == (REG | DREFOBJ) && p2->q2.reg == r) {
                            if (o)
                                break;
                            o = &p2->q2;
                        }
                        if (!p2->z.am && (p2->z.flags & (REG | DREFOBJ)) == (REG | DREFOBJ) && p2->z.reg == r) {
                            if (o)
                                break;
                            o = &p2->z;
                        }
                    }
                    if (c2 == FREEREG || (p2->z.flags & (REG | DREFOBJ)) == REG) {
                        int m;
                        if (c2 == FREEREG)
                            m = p2->q1.reg;
                        else
                            m = p2->z.reg;
                        if (m == r) {
                            if (o) {
                                o->am = am = mymalloc(sizeof(*am));
                                am->flags = IMM_IND;
                                am->base = base;
                                am->offset = zm2l(of);
                                if (isreg(q1)) {
                                    p->code = c = NOP;
                                    p->q1.flags = p->q2.flags = p->z.flags = 0;
                                } else {
                                    p->code = c = ASSIGN;
                                    p->q2.flags = 0;
                                    p->typf = p->typf2;
                                    p->q2.val.vmax = sizetab[p->typf2 & NQ];
                                }
                            }
                            break;
                        }
                        if (c2 != FREEREG && m == base)
                            break;
                        continue;
                    }
                }
            }
        }
        /* Try reg,reg */
        if (GPR_IND && c == ADDI2P && isreg(q2) && isreg(z) && (isreg(q1) || p->q2.reg != p->z.reg)) {
            int base, idx;
            struct obj *o;
            r = p->z.reg;
            idx = p->q2.reg;
            if (isreg(q1))
                base = p->q1.reg;
            else
                base = r;
            o = 0;
            for (p2 = p->next; p2; p2 = p2->next) {
                c2 = p2->code;
                if (c2 == CALL || c2 == LABEL || (c2 >= BEQ && c2 <= BRA))
                    break;
                if (c2 != FREEREG && (p2->q1.flags & (REG | DREFOBJ)) == REG && p2->q1.reg == r)
                    break;
                if (c2 != FREEREG && (p2->q2.flags & (REG | DREFOBJ)) == REG && p2->q2.reg == r)
                    break;
                if ((p2->z.flags & (REG | DREFOBJ)) == REG && p2->z.reg == idx && idx != r)
                    break;

                if (c2 != CALL && (c2 < LABEL || c2 > BRA) /*&&c2!=ADDRESS*/) {
                    if (!p2->q1.am && (p2->q1.flags & (REG | DREFOBJ)) == (REG | DREFOBJ) && p2->q1.reg == r) {
                        if (o || (q1typ(p2) & NQ) == LLONG)
                            break;
                        o = &p2->q1;
                    }
                    if (!p2->q2.am && (p2->q2.flags & (REG | DREFOBJ)) == (REG | DREFOBJ) && p2->q2.reg == r) {
                        if (o || (q2typ(p2) & NQ) == LLONG)
                            break;
                        o = &p2->q2;
                    }
                    if (!p2->z.am && (p2->z.flags & (REG | DREFOBJ)) == (REG | DREFOBJ) && p2->z.reg == r) {
                        if (o || (ztyp(p2) & NQ) == LLONG)
                            break;
                        o = &p2->z;
                    }
                }
                if (c2 == FREEREG || (p2->z.flags & (REG | DREFOBJ)) == REG) {
                    int m;
                    if (c2 == FREEREG)
                        m = p2->q1.reg;
                    else
                        m = p2->z.reg;
                    if (m == r) {
                        if (o) {
                            o->am = am = mymalloc(sizeof(*am));
                            am->flags = GPR_IND;
                            am->base = base;
                            am->offset = idx;
                            if (isreg(q1)) {
                                p->code = c = NOP;
                                p->q1.flags = p->q2.flags = p->z.flags = 0;
                            } else {
                                p->code = c = ASSIGN;
                                p->q2.flags = 0;
                                p->typf = p->typf2;
                                p->q2.val.vmax = sizetab[p->typf2 & NQ];
                            }
                        }
                        break;
                    }
                    if (c2 != FREEREG && m == base)
                        break;
                    continue;
                }
            }
        }
    }
}