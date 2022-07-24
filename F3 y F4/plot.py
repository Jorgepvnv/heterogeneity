import numpy as np
import seaborn as sns 
import matplotlib.pyplot as plt
import matplotlib

plt.figure(1, figsize=(4.3, 7.1))
# matplotlib.rcParams['text.usetex'] = 1
matplotlib.rcParams['mathtext.fontset'] = 'stix'
matplotlib.rcParams['font.family'] = 'STIXGeneral'

tickfsize = 11
labelfsize = 12
plt.subplot(211)
plt.xticks(fontsize= tickfsize)
plt.yticks(fontsize= tickfsize)
plt.xlabel('Timestep', fontsize=labelfsize)
plt.ylabel(r'$s$', fontsize=labelfsize)
color1 = plt.cm.PuBuGn(np.linspace(0,1,15)) # para añadir mas, alterar esto

gammas = ['124','125','126','127','128','129','130','131','132']  # para añadir mas, meter aqui 
for ii, i in enumerate(gammas):
    t=[]
    r=[]
    F = open(i + ".txt", "r")
    lines=F.readlines()
    for x in lines:
        t.append(float(x.split()[0]))
        r.append(float(x.split()[1]))
    plt.plot(t,r, color = color1[14-ii], linewidth=0.8)
plt.xscale('log')
plt.yscale('log')
plt.xlim(1,1000000)

plt.subplot(212)
plt.xticks(fontsize= tickfsize)
plt.yticks(fontsize= tickfsize)
color2 = plt.cm.viridis(np.linspace(0,1,4))
plt.xlabel(r'$\gamma$', fontsize=labelfsize)
plt.ylabel(r'$\Delta$', fontsize=labelfsize)
eps = ['0','005','02']
colors = ['navy','purple','firebrick']
labelss = [r'$\varepsilon = 0.0$',r'$\varepsilon = 0.05$',r'$\varepsilon = 0.2$']
for ii, i in enumerate(eps):
    gamma=[]
    rd=[]
    F = open(i + "fino.txt", "r")
    lines=F.readlines()
    for x in lines:
        gamma.append(float(x.split()[0]))
        rd.append(float(x.split()[1]))
    plt.plot(gamma,rd, color = colors[ii], label = labelss[ii])
    # plt.scatter(gamma,rd,c='white', marker='s',edgecolor =colors[ii],s = 20)
    plt.scatter(gamma,rd,c='white', marker='s',edgecolor =colors[ii],s = 20)
plt.tight_layout()
plt.legend(loc='best')
plt.legend(frameon=False)
plt.savefig("gp2.png", dpi=200)
plt.show()
