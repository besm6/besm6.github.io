# MARS-6 archive system

MARS-6 was an implementation of an external sorted key-value container.
The API mechanism was in form of a micro-program.

Pascal compilers for the BESM-6 included a few MARS-6 operations as part ot the standard library, encoded as micro-programs.
They are:

| Operation | Micro-program | Comment |
| --- | --- | --- |
| GETD | 11-14-22 | Get the value by key, error out if the key does not exist |
| PUTD | 11-15-21-26 | Add a key-value pair, error out if the key elready exists |
| DELD | 11-14-23-27 | Delete a pair by key, error out if the key does not exist |
| MODD | 11-15-00-21-26-40-20 | Modify the value if a pair with the key already exists, add a pair otherwise |
| OPEND | 31-11-14-12-25 | Open an existing container by name, error out if it does not exist in the root catalog |
| NEWD | 31-11-15-21-26-11-14-12-10-21 | Create a container given a name and LUN/starting block/length |

The semantics of micro-instructions (with the range of possible valid codes 00-55 octal), deduced so far:

| Code | Meaning |
| --- | --- |
| 00 | NOP |
| 01 | Position the iterator at the beginning |
| 02 | Position the iterator at the end |
| 03 | Step back | 
| 04 | Step forward |
| 05 | ??? Wants to write to the disk |
| 06 | ??? Wants to write to the disk |
| 07 | ??? Non-writing |
| 10 | Initialize a container/catalog |
| 11 | Find an entry by key |
| 12 | Copy the value to the catalog descriptor |
| 13 | Compute total size of the DB ??? |
| 14 | If the current entry does not match the given key:<br>if the 14 op is the last in the instruction word or is followed by a non-00 instruction, error out,<br>otherwise skip the 00 insn and 3 following insns |
| 15 | The converse of 14 (if the current entry does match  the given key ....) |
| 16 | Compute character string length | 
| 17 | Compute data length in words using bdvec[32] as delimiter | 
| 20 | Copy from the user area to the value part of the current entry |
| 21 | Copy data to the disk (???) | 
| 22 | Copy the value part of the current entry to the user area |
| 23 | Free the memory for the value part of the current entry (???) |
| 24 | ??? Password check |
| 25 | Switch to the catalog pointed to by the descriptor |
| 26 | Add a key (???) wants to write to the disk |
| 27 | Delete a key (???) wants to write to the disk |
| 30 | Restart executing the instruction word |
| 31 | Switch to the root catalog |
| 32 | [aitem] = 0; flush |
| 33 | ??? Non-writing |
| 34 | Converts a file descriptor to a text format |
| 35 | Save dirty duffers and exit |
| 36 | ??? Wants to write to the disk |
| 37 | Skip next 2 instructions |
| 40 | Stop |
| 41 | Compare the user data to the current entry and skip next 2 instructions if no match |
| 42 | ??? Wants to write to the disk |
| 43-44 | ??? Non-writing |
| 45 | Setting the flag "database is busy" |
| 46 | ??? Non-writing |
| 47 | СЧ BDVECT+41B, ПБ BDVECT+1 ??? |
| 50-52 | bdvec manipulations with index increment  |
| 53 | bdvec[next_insn] := bdvec[next_next_insn] ??? |
| 54 | mem[bdvec[next_insn]] := bdvec[10] ??? |
| 55 | ПБ (M16) ??? |
