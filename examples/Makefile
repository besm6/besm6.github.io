all:

install:

run:	../emulator/dispak
	../emulator/dispak --input-encoding=utf8 algol-besm6.b6   > algol-besm6.txt
	../emulator/dispak --input-encoding=utf8 algol-gdr.b6     > algol-gdr.txt
	../emulator/dispak --input-encoding=utf8 autocode-itm.b6  > autocode-itm.txt
	../emulator/dispak --input-encoding=utf8 bemsh-auto.b6    > bemsh-auto.txt
	../emulator/dispak --input-encoding=utf8 bemsh.b6         > bemsh.txt
	../emulator/dispak --input-encoding=utf8 dtran.b6         > dtran.txt
	../emulator/dispak --input-encoding=utf8 forex.b6         > forex.txt
	../emulator/dispak --input-encoding=utf8 fortran-dubna.b6 > fortran-dubna.txt
	../emulator/dispak --input-encoding=utf8 fortran-gdr.b6   > fortran-gdr.txt
	../emulator/dispak --input-encoding=utf8 madlen.b6        > madlen.txt
	../emulator/dispak --input-encoding=utf8 monitor80.b6     > monitor80.txt
	../emulator/dispak --input-encoding=utf8 pascal.b6        > pascal.txt
	../emulator/dispak --input-encoding=utf8 pascal-pretty.b6 > pascal-pretty.txt
	../emulator/dispak --input-encoding=utf8 punchtest.b6     > punchtest.txt
	../emulator/dispak --input-encoding=utf8 whetstone.b6     > whetstone.txt

native: ../emulator/dispak
	../emulator/dispak -x --input-encoding=utf8 algol-besm6.b6   > algol-besm6-native.txt
	../emulator/dispak -x --input-encoding=utf8 algol-gdr.b6     > algol-gdr-native.txt
	../emulator/dispak -x --input-encoding=utf8 autocode-itm.b6  > autocode-itm-native.txt
	../emulator/dispak -x --input-encoding=utf8 bemsh-auto.b6    > bemsh-auto-native.txt
	../emulator/dispak -x --input-encoding=utf8 bemsh.b6         > bemsh-native.txt
	../emulator/dispak -x --input-encoding=utf8 dtran.b6         > dtran-native.txt
	../emulator/dispak -x --input-encoding=utf8 forex.b6         > forex-native.txt
	../emulator/dispak -x --input-encoding=utf8 fortran-dubna.b6 > fortran-dubna-native.txt
	../emulator/dispak -x --input-encoding=utf8 fortran-gdr.b6   > fortran-gdr-native.txt
	../emulator/dispak -x --input-encoding=utf8 madlen.b6        > madlen-native.txt
	../emulator/dispak -x --input-encoding=utf8 monitor80.b6     > monitor80-native.txt
	../emulator/dispak -x --input-encoding=utf8 pascal.b6        > pascal-native.txt
	../emulator/dispak -x --input-encoding=utf8 pascal-pretty.b6 > pascal-pretty-native.txt
	../emulator/dispak -x --input-encoding=utf8 punchtest.b6     > punchtest-native.txt
	../emulator/dispak -x --input-encoding=utf8 whetstone.b6     > whetstone-native.txt

diff:
	-diff -u algol-besm6.txt   algol-besm6-native.txt
	-diff -u algol-gdr.txt     algol-gdr-native.txt
	-diff -u autocode-itm.txt  autocode-itm-native.txt
	-diff -u bemsh-auto.txt    bemsh-auto-native.txt
	-diff -u bemsh.txt         bemsh-native.txt
	-diff -u dtran.txt         dtran-native.txt
	-diff -u forex.txt         forex-native.txt
	-diff -u fortran-dubna.txt fortran-dubna-native.txt
	-diff -u fortran-gdr.txt   fortran-gdr-native.txt
	-diff -u madlen.txt        madlen-native.txt
	-diff -u monitor80.txt     monitor80-native.txt
	-diff -u pascal.txt        pascal-native.txt
	-diff -u pascal-pretty.txt pascal-pretty-native.txt
	-diff -u punchtest.txt     punchtest-native.txt
	-diff -u whetstone.txt     whetstone-native.txt


clean:
	rm -f *~

distclean: clean
