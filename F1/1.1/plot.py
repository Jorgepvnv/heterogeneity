import numpy as np
import seaborn as sns 
import matplotlib.pyplot as plt
import matplotlib
import matplotlib.gridspec as gridspec


gs = gridspec.GridSpec(3,1)
fig=plt.figure(1, figsize=(4.5, 5.0))
# matplotlib.rcParams['text.usetex'] = 1
matplotlib.rcParams['mathtext.fontset'] = 'stix'
matplotlib.rcParams['font.family'] = 'STIXGeneral'

# plt.subplot(211)
ax1 = plt.subplot(gs[0, 0])
# plt.xticks(fontsize= 12)
# plt.yticks(fontsize= 9)
plt.ylabel(r'$s$', fontsize=10)
ncol = 5
color1 = plt.cm.viridis(np.linspace(0,1,ncol)) # para añadir mas, alterar esto

names = ['0.txt','005.txt','02.txt','05.txt']  # para añadir mas, meter aqui 
# names.reverse()
labls = [r'$\varepsilon=0.0$',r'$\varepsilon=0.05$',r'$\varepsilon=0.2$',r'$\varepsilon=0.5$',]

for ii, i in enumerate(names):
    gam=[]
    s=[]
    d=[]
    cv=[]
    F = open(i , "r")
    lines=F.readlines()
    for x in lines:
        gam.append(float(x.split()[0]))
        s.append(float(x.split()[1]))
        d.append(float(x.split()[2]))
        cv.append(float(x.split()[3]))
    ax1.plot(gam,s, color = color1[ii+1], linewidth=2.1, zorder = 10-ii)
# plt.subplot(212)
# handles, labels = ax1.get_legend_handles_labels()
# ax1.legend(handles[::-1],labels = labls[::-1], loc='upper left', frameon=False)
# ax1.legend(handles[::-1],labels = labls[::-1], loc='upper left', frameon=False)
ax1.legend(labels = labls, loc='upper left', frameon=False)
ax1.yaxis.set_ticks([0,0.5,1]) 
ax1.axes.xaxis.set_ticklabels([])

ax2 = plt.subplot(gs[1,0])
plt.ylabel(r'$\sigma(s)$', fontsize=10)
for ii, i in enumerate(names):
    gam=[]
    s=[]
    d=[]
    cv=[]
    F = open(i , "r")
    lines=F.readlines()
    for x in lines:
        gam.append(float(x.split()[0]))
        s.append(float(x.split()[1]))
        d.append(float(x.split()[2]))
        cv.append(float(x.split()[3]))
    plt.plot(gam,d, color = color1[ii+1], linewidth=2.1, zorder = 10-ii)
ax2.axes.xaxis.set_ticklabels([])
ax3 = plt.subplot(gs[2,0])
plt.ylabel(r'$CV$', fontsize=10)

# plt.subplot(213)
for ii, i in enumerate(names):
    gam=[]
    s=[]
    d=[]
    cv=[]
    F = open(i , "r")
    lines=F.readlines()
    for x in lines:
        gam.append(float(x.split()[0]))
        s.append(float(x.split()[1]))
        d.append(float(x.split()[2]))
        cv.append(float(x.split()[3]))
    plt.plot(gam,cv, color = color1[ii+1], linewidth=2.1, zorder = 10-ii)
plt.xlabel(r'$\gamma$', fontsize=10)


plt.tight_layout()
plt.savefig("gp2.png", dpi=200)
fig.subplots_adjust(wspace=5, hspace=0.06)
plt.show()
