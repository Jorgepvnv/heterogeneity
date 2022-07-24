# Figuras Paper

Todas las figuras están subidas en vectorial al overleaf:
https://www.overleaf.com/4586811285tmfsdszhrjnk

## Figura 1

La figura de la topología está hecha con un script de python y el inkscape, como está en vectorial se puede cambiar todo.

## Figura 2

La figura 2 tiene 4 partes:  
  
-La primera muestra la actividad media, las fluctuaciones y el CV para varios $\varepsilon$. Se hizo mandando un programa para cada valor de $\gamma$ y uniéndolos todos.  
  
-La segunda muestra también $a$, $desv$ y $CV$ en modo heatmap para valores de $\varepsilon$ y $\gamma$. Un problema con estas medidas es que, al ser hechas durante un periodo finito, en la zona de fase de Griffiths (donde la caía de la actividad es muy lenta) dicha actividad nunca se estabiliza a un valor fijo; para esto sería necesario tomar $t$ infinito.  
  
-La tercera muestra la evolución de la actividad en la red a partir de unas condiciones iniciales aleatorias para la zona de actividad intermedia y para la zona GP. Se hizo capturando una imagen de la actividad para 3 $t$ distintos, y añadiendo un kernel gaussiano alrededor de la posición de cada neurona activa (si no, en vez de verse una densidad de color más homogénea se verían puntos, que estéticamente quedan peor).  
  
-Para la cuarta, se mide la actividad individual de cada neurona durante un largo periodo de tiempo para unos parámetros dados y un numero de realizaciones alto, y se calcula la densidad de probabilidad de actividad individual media. En la figura principal se hace para un nivel de heterogeneidad dado y variando $\gamma$, y en el inset al revés: se calcula la anchura de la distribución para distintos niveles de heterogeneidad (que es lo interesante, pues se ve cómo esta anchura aumenta con $\varepsilon$)  
  
  
## Figura 3

Muestra la caída de la actividad en la zona GP


## Figura 4

Muestra el rango dinámico para varios valores de parámetros. Esta figura fue muy tediosa: tienes que introducir ruido externo a la red, en un rango de niveles de ruido muy extenso (5 o 6 órdenes de magnitud si no recuerdo mal). Luego ver dónde se encontraban los valores de ruido para los cuales la actividad de la red alcanzaba un 10 y un 90% de su valor (puedes ver el paper de kinouchi y copelli), y luego realizar nuevas medidas, bastante largas, para afinar en las posiciones de éstos niveles de ruido.

## Figura 5

En esta figura se muestran las distribuciones de actividad individual para los dos mecanismos de homeostasis probados, y para varios valores internos que modulan la potencia de dichos mecanismos.
