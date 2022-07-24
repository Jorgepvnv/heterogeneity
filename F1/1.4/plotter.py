import matplotlib.pyplot as plt
import numpy as np
import matplotlib
from mpl_toolkits.axes_grid1.inset_locator import inset_axes
import  matplotlib.image as mpimg

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

# c = '-140.txt'
# filename=['0'+c,'005'+c,'02'+c,'1'+c]
# labels=['g=0.0','g=0.05','g=0.2','g=1.0']
# binns=[600,250,400,300]

# REGULAR
# c = '.txt'
# d = '0-'
# filename=[d+'140'+c,d+'143'+c,d+'146'+c,d+'149'+c]
# labels=['g=1.40','g=1.43','g=1.46','g=1.49']
# binns=[50,50,50,50]

c = '.txt'
d = '005-'
filename=[d+'140'+c,d+'143'+c,d+'146'+c,d+'149'+c]
labels=[r'$\gamma=1.40$',r'$\gamma=1.43$',r'$\gamma=1.46$',r'$\gamma=1.49$']
binns=[150,150,120,80]

matplotlib.rcParams['mathtext.fontset'] = 'stix'
matplotlib.rcParams['font.family'] = 'STIXGeneral'
fig, ax1 = plt.subplots(1, 1, figsize=(4.0, 3.2))
color2 = plt.cm.viridis(np.linspace(0,1,6))
for gg,coso in enumerate(filename):
    pct = 0     # porcentaje neur que no reciben input excitador
    count = 0
    f=open(coso,"r")
    lines=f.readlines()
    k = []
    for ind,j in enumerate(lines):
        if np.mod(ind,18000)<14400:
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
    ax1.plot(bin_centers,n, label=labels[gg], color = color2[gg] , linewidth = 2.0)

filename = ['137fin.txt','143fin.txt','149fin.txt']
eps = []
v=[[],[],[]]

for gg,coso in enumerate(filename):
    f=open(coso,"r")
    lines=f.readlines()
    for ind,j in enumerate(lines):
        if (gg == 0):
            eps.append(float(j.split()[1])/2.0)
        v[gg].append(float(j.split()[3]))

axins0 = inset_axes(ax1, width="40%", height="40%", loc=1)
matplotlib.rcParams['mathtext.fontset'] = 'stix'
matplotlib.rcParams['font.family'] = 'STIXGeneral'
tickss = [0,0.5,1]
axins0.tick_params(labelsize=9)
axins0.set_yticks(tickss)
color1 = plt.cm.bone(np.linspace(0,1,6)) # para añadir mas, alterar esto
axins0.plot(eps, v[0], c=color1[1])
axins0.plot(eps, v[1],c=color1[2])
axins0.plot(eps, v[2],c=color1[3])
axins0.set_xlabel(r'$\varepsilon$', fontsize = 11, labelpad=0)
axins0.set_ylabel(r'$\sigma \left( s_{i} \right)$', fontsize = 11)
# print(v[0])
# print(eps)


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
ax1.tick_params(labelsize=12)
ax1.set_xlabel(r'$\langle s \rangle$', fontsize = 12)
ax1.set_ylabel('Probability density (a.u.)', fontsize = 12)
ax1.set_xscale('log')
fig.tight_layout()
fig.legend(ncol=2, bbox_to_anchor=(0.95,0.5), fontsize = 8.2, handlelength=1, labelspacing=0.5)
#frameon=False
plt.show()