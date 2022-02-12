# Simulación - Programación Declarativa. Segundo Proyecto: Dirty Cleaners
Carmen Irene Cabrera Rodríguez C412

## Orden del problema

El ambiente en el cual intervienen los agentes es discreto y tiene la forma de un rectángulo de N × M. El ambiente es de información completa, por tanto todos los agentes conocen toda la información sobre el ambiente. El ambiente puede varıiar aleatoriamente cada t unidades de tiempo. El valor de t es conocido.

Las acciones que realizan los agentes ocurren por turnos. En un turno, los agentes realizan sus acciones, una sola por cada agente, y modifican el medio sin que este varı́e, a no ser que cambie por una acción de los agentes. En el siguiente, el ambiente puede variar. Si es el momento de cambio del ambiente, ocurre primero el cambio natural del ambiente y luego la variación aleatoria. En una unidad de tiempo ocurren el turno del agente y el turno de cambio del ambiente.

Los elementos que pueden existir en el ambiente son obstáculos, suciedad, niños, el corral y los agentes que son llamados Robots de Casa. A continuación se precisan las caracterı́sticas de los elementos del ambiente:
- **Obstáculos**: estos ocupan una única casilla en el ambiente. Ellos pueden ser movidos, empujándolos, por los niños, una única casilla. El Robot de Casa sin embargo no puede moverlo. No pueden ser movidos ninguna de las casillas ocupadas por cualquier otro elemento del ambiente.
- **Suciedad**: la suciedad es por cada casilla del ambiente. Solo puede aparecer en casillas que previamente estuvieron vacı́as. Esta, o aparece en el estado inicial o es creada por los niños.
- **Corral**: el corral ocupa casillas adyacentes en número igual al del total de niños presentes en el ambiente. El corral no puede moverse. En una casilla del corral solo puede coexistir un niño. En una casilla del corral, que esté vacı́a, puede entrar un robot. En una misma casilla del corral pueden coexistir un niño y un robot solo si el robot lo carga, o si acaba de dejar al niño.
- **Niño**: los niños ocupan solo una casilla. Ellos en el turno del ambiente se mueven, si es posible (si la casilla no está ocupada: no tiene suciedad, no está el corral, no hay un Robot de Casa), y aleatoriamente (puede que no ocurra movimiento), a una de las casilla adyacentes. Si esa casilla está ocupada por un obstáculo este es empujado por el niño, si en la dirección hay más
de un obstáculo, entonces se desplazan todos. Si el obstáculo está en una posición donde no puede ser empujado y el niño lo intenta, entonces el obstáculo no se mueve y el niño ocupa la misma posición. Los niños son los responsables de que aparezca la suciedad. Si en una cuadrı́cula de 3x3 hay un solo niño, entonces, luego de que él se mueva aleatoriamente, una de las casillas de la cuadrı́cula anterior que esté vacı́a puede haber sido ensuciada. Si hay dos niños se pueden ensuciar hasta 3. Si hay tres niños o más, pueden resultar sucias hasta 6. Los niños cuando están en una casilla del corral, ni se mueven, ni ensucian. Si un niño es capturado por un Robot de Casa tampoco se mueve ni ensucia.
- **Robot de Casa**: El Robot de Casa se encarga de limpiar y de controlar a los niños. El Robot se mueve a una de las casillas adyacentes, las que decida. Solo se mueve una casilla sino carga un niño. Si carga un niño puede moverse hasta dos casillas consecutivas. También puede realizar las acciones de limpiar y cargar niños. Si se mueve a una casilla con suciedad, en el próximo turno puede decidir limpiar o moverse. Si se mueve a una casilla donde está un niño, inmediatamente lo
carga. En ese momento, coexisten en la casilla Robot y niño. Si se mueve a una casilla del corral que está vacı́a, y carga un niño, puede decidir si lo deja en esta casilla o se sigue moviendo. El Robot puede dejar al niño que carga en cualquier casilla. En ese momento cesa el movimiento
del Robot en el turno, y coexisten hasta el próximo turno, en la misma casilla, Robot y niño.

### Objetivos
El objetivo del Robot de Casa es mantener la casa limpia. Se considera la casa limpia si el 60 % de las casillas vacias no están sucias.

## Principales ideas seguidas para la solución
La simulación se desarrolla sobre un tablero de NxM tal como indica el problema, y con las cantidades que asigna el usuario para cada elemento del ambiente. Cada casilla del tablero contiene la información necesaria para llevar a cabo la ejecución, dígase, su posición en el mismo y los agentes que se encuentran sobre ella.

Se considera terminada una simulación en tres casos: se alcanza el tiempo máximo prefijado por el usuario; el porciento de suciedad en las casillas vacías sobrepasa el 60% (en cuyo caso se considera que los robots han fracasado); si los robots consiguen neutralizar a los niños, de modo que estos sean incapaces de seguir produciendo suciedad, aunque varíe el ambiente.

Para la generación del tablero primero se crean todas las casillas vacías, luego en el siguiente orden se añaden:
- el corral: aunque las casillas se eligen de forma aleatoria, debe cumplirse que sean adyacentes; Si se añaden otros elementos previos a este, pudiera darse el caso que no exista una configuración válida de la cantidad de casillas necesarias para construir el corral; por tanto se decidió resolver este elemento de primero
- los obstáculos, los niños y los robots: aunque se añadieron al tablero en ese orden respectivamente, no habría problema en variarlo
- la suciedad inicial (que determina el usuario, esta debe ser menor que el 60%). Dado que el valor de entrada de este parámetro se da de forma porcentual, representando la cantidad de suciedad sobre las casillas vacías, se decide tratar este como último elemento a añadir al tablero para poder realizar los cálculos necesarios y determinar específicamente la cantidad de casillas que tendrán suciedad.

La otra acción importante que ocurre sobre el tablero es el cambio aleatorio que tiene lugar cuando pasan *t* unidades de tiempo. Lo que se decide en este caso es mover las casillas del tablero aleatoriamente, lo que se realiza en el mismo orden descrito anteriormente.

El movimiento de los niños se realiza por separado y ese módulo ya se encarga de mover los obstáculos que haya en el camino y de generar nueva suciedad.

En el caso de los agentes de robot, ambos se desarrollaron sobre la misma idea principal: una secuencia de tareas ordenadas por prioridad:
1. En primer lugar se considera que si el robot se encuentra sobre una casilla con suciedad (ya sea que esté sosteniendo un niño o no), este siempre decidirá limpiarla. Note que en caso contrario deberá realizar al menos dos pasos extras para regresar a esta casilla a limpiarla en otro momento. Además la suciedad contribuye a que aumente el porciento que puede dar por terminada la ejecución de la simulación.
2. En caso de que el robot esté cargando un niño, siempre deberá intentar llevarlo a algún corral, lo que le permitirá luego volver a buscar otro niño; note que un niño en el corral, es un niño que no genera suciedad. Aunque el robot no pueda alcanzar un corral (porque tenga el camino bloqueado), nunca se toma como opción que este pueda dejar al niño; primero porque la acción de dejarlo le cuesta un turno, mientras que lo tenga cargado el niño no genera suciedad (está neutralizado), y además, el tenerlo cargado no le impide llevar a cabo la tarea de limpieza.
3. Si no está cargando a ningún niño, y existe un camino para llegar a alguno, el robot preferirá ir a buscarlo; si en el camino se encuentra con una suciedad, como se expresa en el primer punto, preferirá limpiarla y luego continuar su tarea. De todos los niños que pueda alcanzar, el robot eligirá al más cercano como su objetivo.
4. En caso contrario, se decide buscar la casilla sucia más cercana. Si es alcanzable para el robot, este será su siguiente objetivo.
5. Si ocurre que el robot no puede definir ningún objetivo en su turno, porque no están dadas las condiciones para ello, entonces preferirá quedarse en su lugar.
   
El cómo llevan a cabo estos objetivos es lo que diferencia los modelos implementados, que se explicarán a continuación.

## Modelos de agentes considerados

### Agente de robot reactivo

Este robot en cada turno, de acuerdo al nivel de prioridad de las tareas anteriores, decidirá cuál es su siguiente objetivo, y la acción que lo acerque más al cumplimiento del mismo. Note que este procedimiento se realiza en cada turno, sin consideración de sus acciones anteriores; lo que puede implicar que el objetivo de este robot sea diferente en cada ocasión. Este robot tiene un comportamiento reactivo.

### Agente de robot de comportamiento proactivo

Al igual que el modelo de agente anterior, este tiene en cuenta el mismo orden de prioridades de acuerdo a las tareas a realizar; sin embargo, este una vez que decide su siguiente objetivo, tratará de completarlo en los turnos siguientes llevando a cabo una planificación de la secuencia de pasos que debe tomar. La ejecución de dicha tarea solo será interrumpida si hubiere alguna alteración en el plan del robot, como sucede cuando la casilla a la que desea moverse ya no es una casilla válida para él. Cuando algo como lo anterior ocurre, el robot deberá realizar el mismo proceso de planificación para definir sus siguientes movimientos.

## Ideas seguidas para la implementación

### Módulo de tablero

El tablero se considera una matriz de casillas, donde cada una es un par *<Estado, Posición>*. La posición es un par de enteros *(i, j)* donde *i* es el número de la fila y *j* el de la columna. El *Estado* es un tipo que se definió de la siguiente forma:

```
data State = Empty | O | B | D | R | C | BR | BC | RC | RD | BRC | RDB | RBB deriving (Show, Eq)
```

Cada uno representa los elementos sobre la casilla: *Empty* - casilla vacía, *O* - obstáculo, *B* - niño, *D* - suciedad, *R* - robot, *C* - corral, *BR* - robot que carga a un niño, *BC* - niño en un corral (corral ocupado), *RC* - un robot pasando por un corral, *RD* - robot que se acaba de para sobre una casilla con suciedad, *BRC* - casilla en la que coexisten un niño y un robot sobre un corral, esto solo ocurre al momento de dejar al niño en el corral hasta el turno siguiente, *RDB* - robot que carga un niño que encuentra basura en su camino, *RBB* - robot que ya está cargando a un niño pero pasa por una casilla que contiene a otro; note que el robot no puede cargar a ambos niños, por lo que deberá continuar su camino.

En el módulo del tablero se desarrollaron otros métodos de gran utilidad como el reemplazamiento de casillas después de algún movimiento, el acceso a las casillas del tablero, los métodos de generación de elementos que se utilizaron, no solo en la genración del tablero inicial, sino también cuando se desarrolla la variación del ambiente, para construir el nuevo tablero, entre otros.

### Módulo de niños

En el módulo de los niños, como se menciona anteriormente, también se maneja el movimiento de los obstáculos y la generación de suciedad. En primer lugar, se colectan las posiciones donde se encuentran los niños activos, utilizando los estados de las casillas; y se realiza la operación de movimiento sobre cada uno. Se genera de forma aleatoria la dirección en la que el niño ha de moverse, incluyendo el caso en que se pueda quedar en el mismo lugar; si en esa dirección no es posible el movimiento porque la casilla se encuentra ocupada, entonces el niño continúa en la misma posición.

La casilla a donde va a moverse pudiera estar ocupada por un obstáculo, en cuyo caso se recorre la fila o la columna en la dirección fijada buscando una casilla vacía para empujar la hilera de obstáculos. No es posible empujarlos si se choca en el camino con una casilla que contenga otro tipo de elemento, o si se alcanzan los límites del tablero. De encontrarse la casilla vacía, simplemente se realiza un *swap* entre ella y la casilla que contiene al obstáculo que es adyacente al niño en cuestión, lo que imita el comportamiento del empuje.

En ese punto, antes del movimiento del niño, se calcula la cantidad de niños que se encuntran en la casilla 3x3 centrada en él. Con esta cantidad se determina el número máximo de casillas que podrán tener suciedad en esa cuadrícula, de acuerdo a las restricciones del problema y se genera aleatoriamente el número de suciedad final. Después que el niño se mueve, igual de forma aleatoria, se procede a ubicar la suciedad en las casillas vacías.

### Módulo de robots

En el caso de los robots, sobre todo el agente proactivo, es encesario guardar el plan del robot, que consistirá en una lista de posiciones. Por tanto se creo la estructura `(Position, Bool, [Position])`, la tupla anterior guarda en el primer elemento la posición del robot en el tablero; el segundo es un *flag* que determina si está cargando o no a un niño, que también pudiera interpretarse como un indicador de cuántos pasos puede dar; el tercero sería el plan del robot. Como esta estructura se lleva a parte del tablero, también se implementaron los métodos para la generación de la misma, una vez generado el tablero, y para el cambio aleatorio, una vez que este ocurre en el tablero.

Luego por cada agente se implementó el método *move* que es el que determina la acción a realizar por cada uno en un turno.

Como se menciona anteriormente, el robot reactivo tiene su lista de tareas ordenadas en cuanto a prioridad, y en cada turno decide cuál será su siguiente objetivo, lo que define su siguiente acción. Se había hablado en todo momento de si el elemento era alcanzable o no. Para ello se implementó un método estilo *bfs* que recorre las casillas adyacentes, a partir de la casilla donde se encuentra el robot. Las casillas visitadas se guardan en una lista, asi como las que están por visitar (son listas distintas), pero en vez de solo insertar en esa lista la posición a visitar, se guarda una tupla de dos posiciones: la posición a visitar, y su padre (la casilla por la cual esta fue insertada en la cola). Esto permite, que una vez que se encuentre el objetivo deseado, se pueda generar el camino a través del cual se puede llegar a él. Este camino es el que se almacena en el plan, y del cual se toman las casillas a las que debe moverse el robot, sea cual sea.

El robot proactivo siempre intenta moverse a la primera casilla de su plan. Si el plan está vacío o la casilla a la que debe moverse ya no es válida, entonces deberá generar un nuevo plan. Si la casiila sobre la que se encuentra está sucia, se quedará a limpiarla y luego se encargará de continuar su planificación. Si está cargando a un bebé y arriva a un corral, su flag vuelve a ser False, y se obliga a crear un nuevo plan. Del mismo modo, si en el camino se encuentra con un niño, y no está cargando ninguno, lo recoge y también se le obliga a cambiar su plan; su prioridad en ese momento debe ser llevar al niño a un corral.

## Consideraciones obtenidas a partir de las simulaciones.

Se llevaron a cabo las siguientes simulaciones, 30 de cada una. El tiempo de ejecución máximo es de 200 en cada caso. No se varía el número de robots, que serán 2, puesto que es de suponer que mientras más hayan, más sencillo será neutralizar a los niños; así tampoco se varía la dimensión del tablero (10x10).

| Test ID | Niños | Obstáculos | Tiempo(*t*) | Suciedad (%) |
|:---:|:---:|:---:|:---:|:---:|
| t0 | 6 | 2 | 20 | 40 |
| t1 | 6 | 4 | 10 | 25 |
| t2 | 6 | 8 | 5 | 30 |
| t3 | 6 | 16 | 20 | 40 |
| t4 | 8 | 20 | 30 | 45 |
| t5 | 8 | 20 | 4 | 15 |
| t6 | 8 | 30 | 10 | 10 |
| t7 | 12 | 5 | 20 | 0 |
| t8 | 12 | 8 | 15 | 50 |
| t9 | 12 | 10 | 20 | 45 |

Los resultados obtenidos fueron los siguientes:

| Test ID | Ganados 1 | Ganados 2 | Perdidos 1 | Perdidos 2 | Suciedad(%) 1 | Suciedad(%) 2|
|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| t0 | 24 | 30 | 0 | 0 |17.15  | 20.13 |
| t1 | 25 | 30 | 0 | 0 | 14.02 | 11.39 |
| t2 | 24 | 30 | 0 | 0 | 12.51 | 11.72 |
| t3 | 17 | 30 | 0 | 0 | 12.32 | 11.31 |
| t4 | 10 | 29 | 3 | 1 | 10.53 | 10.54 |
| t5 | 5 | 26 | 1 | 2 | 3.6 | 11.94 |
| t6 | 2 | 28 | 0 | 0 | 0.32 | 2.62 |
| t7 | 7 | 30 | 1 | 0 | 4.8 | 5.35 |
| t8 | 5 | 14 | 12 | 16 | 27.24 | 42.52 |
| t9 | 5 | 21 | 14 | 9 | 30.04 | 27.13 |

Se puede observar que aunque la cantidad de robots sea pequeña en comparación con la cantidad de niños o de obstáculos, esta pequeña cantidad hace muy bien su trabajo, o sea, se puede ver que la cantidad de veces que fracasaron porque los niveles de suciedad sobrepasaron los límites fueron muy pocos.

En cuanto a los modelos de los agentes, se puede observar que en prácticamente todos, el robot proactivo obtiene mejores resultados. Esto podría deberse al mismo hecho de que una vez que decide qué objetivo quiere cumplir, sigue su planificación para conseguirlo; mientras que el robot reactivo, al tener que escoger su objetivo en cada turno, pudiera no terminar ninguna tarea.

Este mismo comportamiento pudiera explicar el porqué el porciento de suciedad cuando se termina la simulación en el robot proactivo es mayor que en el otro. Dado que el primero prioriza la recogida de niños, sobre la suciedad; aunque el segundo también lo haga, eso no significa que lo consiga.

Tiene sentido que una simulación donde el nivel de suciedad es bien alto al inicio, tenga una mayor cantidad de fracasos de los robots; sin embargo note que esot no es en realidad determinante de acuerdo a las pruebas realizadas; por el contrario, la cantidad de niños y obstáculos influyen en gran medida.

Igualmente, valores pequeños para el tiempo de cambio del ambiente también resultan en un mayor número de despidos; la diferencia se puede apreciar en los dos últimos test. En el test 8 es el único donde los robots proactivos fracasan en más ocasione que los reactivos. El cambio del ambiente, en pequeños intervalos de tiempo, impide a los robots completar su plan, y hace más difícil la limpieza de la basura.