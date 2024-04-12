`El juego`
El Nonograma, también conocido como Pintar por números o Picross, es un rompecabezas
que consiste en colorear las celdas correctas de una cuadrícula, de acuerdo con los números
especificados a los lados de la misma, llamados "pistas", con el fin de revelar una imagen
oculta. En este tipo de rompecabezas, los números constituyen una especie de representación
o codificación concisa o comprimida de la imagen oculta, esto es, los números especifican
cuántos cuadros rellenos contiguos hay en una fila o columna dada de la grilla. Por ejemplo,
una pista de "4 8 3" significa que hay grupos de cuatro, ocho, y tres cuadros rellenos, en ese
orden, con al menos un espacio en blanco entre los grupos sucesivos.

![image](https://github.com/ItsMassi/PrologProject/assets/90329965/86d846ff-cc07-463d-8f93-0c021ced9653)

En cuanto a las dimensiones de la grilla, no hay restricciones, pudiendo no ser cuadrada, con
cualquier número de filas y columnas.
Les recomendamos que bajen la app https://nonogram.com, versión blanco y negro (disponible
para Android y iOS), para familiarizarse con el juego. Tiene un tutorial interactivo muy claro, y
además los requerimientos funcionales para el proyecto están basados en esta aplicación, en
el modo de juego clásico (Settings > Game Mode > Classic) y sin autocompletado de cruces
(Settings > Auto-Crosses > deshabilitar).

`Funcionalidad`
Se debe implementar una aplicación que permita resolver interactivamente un Nonograma, al
estilo de la app Nonogram.com, versión blanco y negro, en modo clásico (Settings > Game
Mode > Classic) y sin autocompletado de cruces (Settings > Auto-Crosses > deshabilitar).

![image](https://github.com/ItsMassi/PrologProject/assets/90329965/1146ec27-01e2-40d9-b4dd-061cc645f7c5)

A continuación se listan requerimientos más específicos:
● Presentar al jugador una grilla interactiva (con celdas clickeables) con las pistas a los
costados, donde es posible que inicialmente ciertas celdas ya estén pintadas o
marcadas como espacios. Esta grilla inicial se especificará en Prolog, como se explicará
en la sección siguiente.

● Permitir elegir, en todo momento, el modo de marcado (checkbox / toggle), entre pintar o
modo cruz. En modo pintar, al clickear una celda esta se pinta, excepto que ya esté
pintada, y en ese caso se borra (queda vacía). En modo cruz, al clickear una celda esta
se marca con una X, indicando explícitamente que la queremos dejar sin pintar (de
ahora en adelante), excepto que ya tenga una X, y en ese caso se borra (queda vacía).
A los efectos de determinar el cumplimiento de las pistas de una fila o columna, o la
conclusión del rompecabezas, una X tiene exactamente el mismo efecto que dejar la celda vacía.
Sin embargo es una herramienta muy útil para recordar que ya tomamos la
decisión o concluímos que dicha celda debe quedar en blanco, y nos facilita inferir luego
el contenido de otras celdas por descarte.

● En todo momento, y para cada fila o columna (diremos línea en general), indicar
visualmente si ésta satisface las pistas asociadas (por ejemplo, pintando de verde el
panel con las pistas, o con un efecto de transparencia como en Nonogram.com).
Observaciones:
○ una línea puede satisfacer las pistas y de todas maneras no ser correcta de
acuerdo a la solución final.
○ una línea puede dejar de satisfacer las pistas si se pintan más celdas, o borran
(o marcan con X) celdas pintadas.

● Detectar que se resolvió el nonograma: todas las líneas cumplen con las pistas.

`Implementación`
El archivo init.pl (módulo Prolog) en pengines_server/apps/proylcc permite especificar la
configuración inicial del juego, que será mostrada por la interfaz. Es importante que no altere
este esquema, y que conserve la representación de la grilla propuesta en el código molde,
dado que asumiremos dicha representación para testear la implementación con diferentes
grillas (casos de test) en la corrección.
La configuración inicial del juego se representará mediante un hecho init/3 con la siguiente
forma:
init(PistasFilas, PistasColumns, Grilla).
donde:
- PistasFilas es una lista de listas de números, representando las pistas de cada fila.
- PistasColumnas es una lista de listas de números, representando las pistas de cada
columna.
- Grilla es una lista de filas, donde una fila es una lista de: '#' (celda pintada), 'X'
(celda marcada como no pintada), o variable sin instanciar (vacía, en blanco, sin valor).

El archivo proylcc.pl contiene el predicado put/8, que debe ser extendido para
implementar el efecto de una movida, y que será consultado desde la implementación React en
el cliente:
put(+Contenido, +Pos, +PistasFilas, +PistasColumnas, +Grilla,
-GrillaRes, -FilaSat, -ColSat)
donde:
- Contenido es "#", "X",
- Pos es una lista [Fila, Columna], indicando la posición donde se desea colocar
Contenido,
- PistasFilas, PistasColumnas, Grilla, con la misma representación que para
init/3,
- GrillaRes, la grilla resultante, con la misma representación que para Grilla,
- FilaSat es 1 si las fila de Pos satisface las pistas asociadas, y 0 en caso contrario.
- ColSat es 1 si las columna de Pos satisface las pistas asociadas, y 0 en caso
contrario.

Observaciones:
● En caso de que Contenido ("#" o "X") coincida con el valor en la celda en Pos,
entonces deberá borrarse, esto es, cambiarse por _ (variable sin instanciar). Esto es, la
lógica de borrar cuando se marca con lo que ya había queda del lado de Prolog.
● Luego de una movida, únicamente puede haber cambiado el estado (satisfecha / no
satisfecha) de la fila y la columna de la celda que se cambió, y es por esto que con
FilaSat y ColSat retornados por put/7 es suficiente para actualizar la interfaz,
incluyendo determinar la conclusión del rompecabezas.

`Documentación`
Se deberá realizar un informe que explique claramente la implementación en Prolog
realizada.
Además, deberá incluirse una sección con casos de test significativos (capturas de pantalla).
El informe debe ser:
● Claro: información bien estructurada y presentada
● Completo: explicando cómo resolvieron cada requerimiento funcional (la parte de
Prolog, y a nivel de estrategia, no a nivel de código), funcionalidades extra
implementadas (si es que alguna), aspectos positivos de la resolución, desafíos que
encontraron y cómo los enfrentaron, casos de test (capturas de pantalla).
● Sintético y relevante: no repetir información que está en el enunciado, como reglas del
juego, no documentar funcionalidad de muy bajo nivel o auxiliar, que no hace al
entendimiento de la estrategia principal
