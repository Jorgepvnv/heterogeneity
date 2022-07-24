from turtle import end_fill
import numpy as np
import seaborn as sns 
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap, LinearSegmentedColormap
import matplotlib.colors as colors
import matplotlib

names = ['cv', 'amean', 'desv']     # se elige una opción. abajo se cambia la línea de plotteo (caso)
amean = open(names[0]+'.txt').read()  
amean = [item.split() for item in amean.split('\n')[:-1]]
amean = np.array(amean)
amean = amean.astype(float)
amean = np.flip(amean, 0)


# def create_color(r, g, b):
#     return [r/256, g/256, b/256]
 
# def get_custom_color_palette():
#     return LinearSegmentedColormap.from_list("", [
#         # create_color(227, 101, 33), create_color(246, 145, 53), create_color(251, 168, 74),
#         # create_color(218, 212, 200),
#         # create_color(141, 193, 223), create_color(114, 167, 208), create_color(43, 92, 138)
#          create_color(0, 0, 0), create_color(246, 145, 53)
#     ])
# def get_custom_color_palette_hash():
#     return LinearSegmentedColormap.from_list("", [
#         '#000004', '#1f0c48', '#550f6d','#88226a','#a83655','#e35933','#f9950a','#f8c932','#fcffa4'
#     ])
# cmap = get_custom_color_palette_hash()
plt.figure(figsize=(4.0,2.9), dpi=160)
matplotlib.rcParams['mathtext.fontset'] = 'stix'
matplotlib.rcParams['font.family'] = 'STIXGeneral'
plt.xticks(fontsize= 12)
plt.yticks(fontsize= 12)
# ax = sns.heatmap(amean, cmap="inferno", cbar_kws={"ticks":[0,0.5,1,1.5]})     caso desv
ax = sns.heatmap(amean, cmap="rocket", linewidths=0, cbar_kws=dict(ticks=[0.0, 0.5, 1.0, 1.5])) # caso CV
# ax = sns.heatmap(amean, cmap="mako", cbar_kws={"ticks":[0,0.1,0.5,1]}, norm=colors.PowerNorm(gamma=0.45))   caso actividad media
plt.ylabel(r'$\varepsilon$', fontsize=15)
plt.xlabel(r'$\gamma$', fontsize=15)
plt.xticks([5.185, 18.148, 31.111, 44.074, 57.037, 70 ], ['1.2', '1.3', '1.4', '1.5', '1.6', '1.7'])
plt.yticks([0, 20.5, 41], ['1', '0.5', '0'])
plt.tight_layout()
# plt.savefig("cva.png", dpi=200)
plt.show()