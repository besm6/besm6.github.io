#!/usr/bin/env python
# -*- encoding: utf-8 -*-
#
# Scan a disk for libraries, using LIBREFER utility.
#
import sys, os, string, subprocess, struct

#
# Parse command line.
#
if len(sys.argv) != 1:
    print "Usage: librefer.py"
    sys.exit(1)
disk_name = "2053"
libraries = [01201,01213,01217,01601,01613,01617,01730]

for lib in libraries:
    #
    # Read input file and generate a task file.
    #
    basename = "%s-%04o" % (disk_name, lib)
    task_name = basename + ".b6"
    task_file = open(task_name, "w")
    task_file.write("""шифр 419999 зс5^
лен 40(2048)^
лен 41(%s)^
eeв1а3
*name %s
*perso: 401350
*call librefer:TAPE=41%04o
*end file
``````
еконец
""" % (disk_name, basename, lib))
    task_file.close()

    #
    # Run dispak simulator.
    #
    dispak = subprocess.Popen('dispak %s.b6' % (basename),
        shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    lst_file = open(basename + ".lst", "w")
    nerrors = -1
    for line in dispak.stdout.readlines():
        lst_file.write(line.rstrip() + "\n")

        # Find status.
        line = line.decode('utf-8')
        #print len(line), line,
        if len(line) >= 44:
            if line[21:43] == u"ССЫЛКИ НА ПОДПРОГРАММЫ":
                nerrors = 0

    lst_file.close()

    retval = dispak.wait()
    #print "retval =", retval
    if retval == 127:
        print "dispak: Command not found"
        lst_file.close()
        os.remove(basename + ".lst")
        continue
    if retval != 0:
        print "dispak: Failed to invoke Librefer assembler"
        os.remove(basename + ".lst")
        continue
    if nerrors != 0:
        print "%04o: Librefer errors detected" % (lib)
        #print "See %s.lst for details" % basename
        os.remove(basename + ".lst")
        continue

    print "Zone %04o of disk %s printed into %s.lst" % (lib, disk_name, basename)
