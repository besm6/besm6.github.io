all:

run:	../besm6
	(cd ..; ./besm6 examples/algol-besm6.b6)   > algol-besm6.txt
	(cd ..; ./besm6 examples/algol-gdr.b6)     > algol-gdr.txt
	(cd ..; ./besm6 examples/autocode-itm.b6)  > autocode-itm.txt
	(cd ..; ./besm6 examples/bemsh-auto.b6)    > bemsh-auto.txt
	(cd ..; ./besm6 examples/bemsh.b6)         > bemsh.txt
	(cd ..; ./besm6 examples/dtran.b6)         > dtran.txt
	(cd ..; ./besm6 examples/forex.b6)         > forex.txt
	(cd ..; ./besm6 examples/fortran-dubna.b6) > fortran-dubna.txt
	(cd ..; ./besm6 examples/fortran-gdr.b6)   > fortran-gdr.txt
	(cd ..; ./besm6 examples/madlen.b6)        > madlen.txt
	(cd ..; ./besm6 examples/monitor80.b6)     > monitor80.txt
	(cd ..; ./besm6 examples/pascal.b6)        > pascal.txt
	(cd ..; ./besm6 examples/punchtest.b6)     > punchtest.txt
	(cd ..; ./besm6 examples/whetstone.b6)     > whetstone.txt

native:
	(cd ..; ./besm6 -x examples/algol-besm6.b6)   > algol-besm6-native.txt
	(cd ..; ./besm6 -x examples/algol-gdr.b6)     > algol-gdr-native.txt
	(cd ..; ./besm6 -x examples/autocode-itm.b6)  > autocode-itm-native.txt
	(cd ..; ./besm6 -x examples/bemsh-auto.b6)    > bemsh-auto-native.txt
	(cd ..; ./besm6 -x examples/bemsh.b6)         > bemsh-native.txt
	(cd ..; ./besm6 -x examples/dtran.b6)         > dtran-native.txt
	(cd ..; ./besm6 -x examples/forex.b6)         > forex-native.txt
	(cd ..; ./besm6 -x examples/fortran-dubna.b6) > fortran-dubna-native.txt
	(cd ..; ./besm6 -x examples/fortran-gdr.b6)   > fortran-gdr-native.txt
	(cd ..; ./besm6 -x examples/madlen.b6)        > madlen-native.txt
	(cd ..; ./besm6 -x examples/monitor80.b6)     > monitor80-native.txt
	(cd ..; ./besm6 -x examples/pascal.b6)        > pascal-native.txt
	(cd ..; ./besm6 -x examples/punchtest.b6)     > punchtest-native.txt
	(cd ..; ./besm6 -x examples/whetstone.b6)     > whetstone-native.txt

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
	-diff -u punchtest.txt     punchtest-native.txt
	-diff -u whetstone.txt     whetstone-native.txt


clean:
		rm -f *~
