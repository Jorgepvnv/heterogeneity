import matplotlib.pyplot as plt
import numpy as np
import matplotlib

# filename='cv000.txt'
# f=open(filename,"r")
# lines=f.readlines()
# k = []
# for j in lines:
#     k.append(j.split()[0])
# f.close

# PARA TODOS
# c = '0-'
# filename=[c+'137.txt',c+'140.txt',c+'143.txt',c+'146.txt',c+'149.txt',c+'152.txt',c+'155.txt']
# labels=['g=1.37','g=1.40','g=1.43','g=1.46','g=1.49','g=1.52','g=1.55']
fig=plt.figure(1, figsize=(3.8, 3.0))
a = 'ffreq2-'
b = '.txt'
filename=[a+'0'+b, a+'002'+b, a+'005'+b]
labels=[r'$J=0.0$',r'$J=0.001$',r'$J=0.002$',r'$J=0.005$']
binns=[100,200,200,200]
matplotlib.rcParams['mathtext.fontset'] = 'stix'
matplotlib.rcParams['font.family'] = 'STIXGeneral'
# c = '.txt'
# d = '02-'
# filename=[d+'137'+c,d+'140'+c,d+'143'+c,d+'146'+c,d+'149'+c,d+'152'+c,d+'155'+c]
# labels=['g=1.37','g=1.40','g=1.43','g=1.46','g=1.49','g=1.52','g=1.55']
# binns=[600,250,400,300,50,50,50]
ncol = 5
color1 = plt.cm.viridis(np.linspace(0,1,ncol))
for gg,coso in enumerate(filename):
    pct = 0     # porcentaje neur que no reciben input excitador
    count = 0
    f=open(coso,"r")
    lines=f.readlines()
    k = []
    for ind,j in enumerate(lines):
        # if np.mod(ind,18000)<14400:
        count += 1
        if j.split()[0] != '-Infinity':
            k.append(j.split()[0])
        else:
            pct += 1
    pct /= count
    print(pct)
    f.close
    kk=np.array(k, dtype=np.float32)
    n, x = np.histogram(kk, bins=binns[gg], density=True)
    # print(len(x), len(n))
    bin_centers = 0.5*(x[1:]+x[:-1])
    plt.plot(bin_centers,n, label=labels[gg],color = color1[gg+1], linewidth=2.1)

# kk=np.array(k, dtype=np.float32)
# under1=0
# for j in kk:
#     if j<1:
#         under1 += 1
# under1 /= len(kk)
# print(np.mean(kk), np.std(kk), under1)
# n, x = np.histogram(kk, bins=250, density=True)
# bin_centers = 0.5*(x[1:]+x[:-1])    #   está puesto para el centro de los bins, si no poner solo x
# print(n,x)
# plt.xlim([0, 4])
# # plt.ylim([0, 5.5])
# plt.xscale('log')
# # # plt.yscale('log')
plt.xlim([0, 0.16])
plt.xticks([0,0.1])
plt.xlabel(r'$\langle s \rangle$', fontsize = 12)
plt.ylabel('Probability density (a.u.)', fontsize = 12)
# plt.xscale('log')
# plt.yscale('log')
# plt.plot(bin_centers,n)
plt.tight_layout()
plt.legend()
plt.show()