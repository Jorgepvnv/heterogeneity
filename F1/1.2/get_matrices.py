import numpy as np  


F = open("amean.txt", "w")
for j in range(41):
    for k in range(71):
        if k<10:
            ng = " " + str(k)
        else:
            ng = str(k)

        if j<10:
            npt = " " + str(j)
        else:
            npt = str(j)
        M = open("fa"+ng+"-"+npt+".txt", "r")
        lines=M.readlines()
        for nn,g in enumerate(lines):
            if (nn==0):
                F.write(g.split()[1])
                F.write("   ")
        M.close()
    F.write("\n")
F.close()

F = open("desv.txt", "w")
for j in range(41):
    for k in range(71):
        if k<10:
            ng = " " + str(k)
        else:
            ng = str(k)

        if j<10:
            npt = " " + str(j)
        else:
            npt = str(j)
        M = open("fa"+ng+"-"+npt+".txt", "r")
        lines=M.readlines()
        for nn,g in enumerate(lines):
            if (nn==0):
                F.write(g.split()[2])
                F.write("   ")
        M.close()
    F.write("\n")
F.close()

F = open("cv.txt", "w")
for j in range(41):
    for k in range(71):
        if k<10:
            ng = " " + str(k)
        else:
            ng = str(k)

        if j<10:
            npt = " " + str(j)
        else:
            npt = str(j)
        M = open("fa"+ng+"-"+npt+".txt", "r")
        lines=M.readlines()
        for nn,g in enumerate(lines):
            if (nn==0):
                F.write(g.split()[3])
                F.write("   ")
        M.close()
    F.write("\n")
F.close()