BigBrother: un lenguaje autocrático.
====================================

BigBrother (corto conocido BB, o bebé) es un lenguaje imperativo
fuertemente tipeado basado en la sintaxis de C e inspirado con ideas
un poco más modernas provenientes de lenguajes como Rust y D.

Estructura General
==================

Un programa en BB siempre tiene que tener una función principal
`main` que recibe un arreglo de strings y retorna un entero. Antes y
después de esta función se pueden declarar cualquier cantidad
funciones, tipos extendidos y variables estáticas (posiblemente
constantes).

~~~
<declaración de variables, tipos, funciones>
fn main(args: string[]) : int {
    <instrucciones>
}
<declaración de variables, tipos y funciones>
~~~

El tipo de la función de retorno de la función `main` puede ser `int`
o `void`. En el caso de ser `void` el programa siempre retornara un
código de salida exitoso al sistema operativo. En caso contrario
retornara al sistema operativo el valor retornado por la función.

Alcances y Bloques
==================

BB es un lenguaje de alcance estático. El alcance dentro de BB está
definido por bloques demarcados por `{` y `}`. Un caso especial de
esto es la definición global que se encuentra afuera de cualquier
declaración de función.

~~~
// declaraciones globales
const X = 65536:int;
fn main(args: string[]) { // <--- define alcance
    var x=0, y=1, z=1 : int;
    if (x > 0) { // <-- define alcance
        var x = 10 : int;
        print! "El valor de x es ", x; //  10
    }
    print! "El valor de x es ", x; // 0
    print! "El valor de X es ", X; // 65536
}
~~~

Redeclarar una variable en el mismo alcance no está
permitido.

Dentro de funciones, y solo dentro de funciones, se puede referenciar
a variables, funciones o procedimientos que no se han declarado
todavía en el alcance global.

~~~
fn main(args: string[]) {
    var nombre:string;
    print! "Dime tu nombre: ";
    grab! nombre;
    imprime_hola(nombre); // imprime tu nombre
    print! "X = ", X; // 17
}

fn imprime_hola(quien: args) {
    print ! "Hola", quien;
}

const X = 17:int;
~~~

Internamente, el analizador estático realiza una pasada superficial y
manteniendo una bitácora de los identificadores de tipo, variables y
funciones conseguidos en el alcance global.

Tipos
=====

Dentro de BB toda variable declarada siempre tiene un valor y
un tipo. Al declarar una variable se puede inicializar su valor o en
caso contrario se asignara un valor neutro. Los valores neutros para
los distintos tipos se definen en las secciones subsiguientes.

La declaración de una variable se realiza con la siguiente sintaxis:

`var <identificador_1>, <identificador_2>, ..., <identificador_n> : <tipo>;`

Se puede al momento de declaración realizar la inicialización de un
número arbitrario de variables como sigue:

`var <identificador_1> [= <valor_1>], <identificador_2> [= <valor_2>], ..., <identificador_n> [= <valor_n>] : <tipo>;`

Tipos básicos
==============

Los tipos básicos que define el lenguaje son booleanos, enteros,
flotantes y caracteres.

Tipos de variables
--------------------

Al tiempo de declaración de variables, se pueden asignar los
siguientes modificadores antes del tipo. Para cualquier tipo se puede
utilizar:

- `static` para declararlo en memoria estática
- `const` para declararlo constante. Una variable constante no puede
  modificar sus contenidos, mas sin embargo de ser un tipo extendido
  puede modificar alguno de sus campos (a menos que este sea
  constante).

~~~
var hola:int;
static mundo:double;
const fin = "fin":string;

fn correme() {
     static miVariable=0:int;
     miVariable += 1;
    print! miVariable;
}

fn main(args: string[]) {
    print! "Hola Mundo";
    correme(); //prints 1
    correme(); // prints 2
}
~~~


Tipos booleanos
---------------

Los tipos booleanos (identificados con el tipo primitivo bool) solo
pueden tomar valores `true` o `false`. Sus operadores son los
operadores normales `&&` (and), `||` (or), `!` (not), y `^` (xor)
respectivamente. Dos valores booleanos se pueden comparar utilizando
los operadores de comparación `==`, `!=`, `>`, `>=`, `<`, `<=`.

Los siguientes operadores de asignación están definidos: `||=`, `&&=`.


Al momento de declaración, en caso de obviar el valor inicial, se
inicializan a su valor neutro `false`.

En este documento al referirnos a una condición nos referimos siempre
a una expresión booleana.

Tipos numéricos
--------------------------

Los tipos numéricos dentro del lenguaje son los enteros denotados por
int16, int32 e int64 y por números flotantes denotados por float32 y
float64.

Al momento de declaración, en caso de obviar el valor inicial, se
inicializan utilizando su valor neutro `0`.

Para referirnos a números podemos utilizar cualquiera de las
siguientes notaciones:

- Notación decimal: 0, 1, 1, 2, 3, 5, 8, ...

El punto flotante se denota con el `.`. Para referirnos a números
flotantes se puede utilizar:

- Notación decimal: 0, 1, 1, 2.0, 3.0, 5, 8, 17.325...

Los siguientes operadores aritméticos están definidos para todos los
tipos numéricos: `+`, `-`, `*`, `/`, `**` (exponenciación), `%`
(módulo).

Los siguientes operadores de comparación están definidos para todos
los tipos numéricos: `==`, `!=`, `>`, `<`, `>=`, `<=`.

Los siguientes operadores de bits están definidos: `&` (and), `|` (or),
`^` (xor), `~` (not), `>>`, `<<`.

Los siguientes operadores de asignación están definidos: `=`, `*=`,
`/=`, `%=`, `+=`, `-=`, `>>=`, `<<=`, `&=`, `^=`, `|=`, `**=`.

Además, se cuenta con un operador unario prefijo caracter `@` el cual
devuelve el caracter cuya representación en ASCII es el entero,
siempre y cuando este dentro de la tabla ASCII --- de lo contrario,
debe ocurrir un error.

Caracteres
----------

Los caracteres están definidos utilizando la tabla ASCII. Su valor
neutro es `'\0'`. Para referirnos a un carácter utilizamos comillas
simples.

Los carácteres tienen un solo operador unario prefijo de ordenamiento
`#`: dado un caracter, devuelve su posición en la tabla ASCII.


Conversiones entre tipos básicos
--------------------------------

Para realizar una conversión explicita se utiliza la palabra clave
`as`.

~~~
3 as float64
x as int32
~~~

La conversión de flotantes a enteros trunca. La conversión entre
carácteres y tipos numéricos es equivalente a utilizar los operadores
`#` y `@`.

Cuando se trata de operar sobre tipos numéricos distintos puede
ocurrir una promoción al tipo más general.

~~~
var x = 64.32 : float64;
var y = 32 : int32;
print! x+y; // y será promovido a float64

var z = x+y : int16; // ocurrira una promoción y luego una democión
~~~

Algunas conversiones ocurren automáticamente entre tipos numéricos:

~~~
var x = 32 : float64; // 32.0
var y = 15.0 : int64; // 15
~~~


Sobrenombres
============

Fuera de cualquier función se puede declarar un sobrenombre para un
tipo utilizando la siguiente sintaxis:

`type <identificador> = <tipo>;`

Dentro del lenguaje se asignan los siguientes sobrenombres para
facilitar el uso:

~~~
type short = int8;
type int = int32;
type long = int64;
type double = float64;
type float = float32;
~~~

A momento de análisis estático el sobrenombre es elevado al alcance
global, por lo que de existir algún otro identificador con el mismo
nombre esto generará un error estático.


Ejemplos
--------

`type MiEntero = int64;`

Rangos
======

Dentro del lenguaje los rangos se pueden definir como sigue:

`<inicial>..<final>`

Esto define un rango desde inicial (incluyéndolo) hasta final
(excluyéndolo) aumentando con un paso de 1.

Los rangos no representan un tipo concreto dentro del lenguaje. Son un
atajo para operaciones comunes.  En caso de ser asignados, pasados como
parametros, retornados de alguna funcion, o en general usados como un 
tipo concreto serán reinterpretados como un arreglo del tamaño del rango 
donde cada posición tiene cada elemento del rango. Por lo tanto, las cotas
de los rangos no pueden ser declaradas utilizando variables. De igual forma, 
los rangos deben ser declarados utilizando literales del tipo entero. 
Esto es, el siguiente rango es valido:

`1..32`

pero los siguientes rangos no:

`x..32`

`1.3..2.0`

`0..32+1`

Ejemplos
---------

`0..10 // [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]`


Arreglos
========

Los arreglos unidimensionales se pueden declarar con la siguiente sintaxis:

`var <identificador> : <tipo>[<rango>];`

El <rango> define los índices accesibles dentro del arreglo. Al
declarar un arreglo todas las posiciones del arreglo toman su valor
neutro.

Adicionalmente, se puede utilizar la siguiente notación:

`var <identificador> : <tipo>[<entero>];`

que es azúcar sintáctico para:

`var <identificador> : <tipo>[0..<entero>-1];`

En el caso de arreglos multidimensionales la sintaxis es similar:

`var <identificador> : <tipo>[<rango_1>][<rango_2>]...[<rango_n>];`

Con la declaración simplificada

`var <identificador> : <tipo>[<entero_1>][<entero_n>]...[<entero_n>];`

La declaracion utilizando la sintaxis de rango necesariamente
tiene que usar  DOS enteros literales (es decir, no se pueden usar
ni variables ni constantes ni alguna operacion sobre estas). 
En el caso de usar la version simplificada se debe usar UN entero literal.

Por ejemplo, la declaracion:

`int[32]` 

es correcta mientras que

`int[32+32]` 

no lo es. 

Se cuenta con los siguientes operadores para trabajar con arreglos:

- `#arreglo` retorna la dimensión más externa del arreglo
- `@arreglo` retorna la posición actual de iteración. Más sobre esto
  en la sección de iteración determinada.
- Para acceder un indice se utiliza el operador `[]` como en C.

Al momento de declaracion de funciones que tomen parametros de tipo arreglo, 
no se permite obviar ninguna dimension. Para declarar las dimensiones, las mismas
restricciones descritas al momento de declaracion de arreglo aplican: solo 
enteros literales. Adicionalmente, solo se permite utilizar la version atajo.

Ejemplos
---------

~~~
// declaración de un arreglo de enteros de diferentes maneras
var arr1 : int[10];
var arr2 : int[0..10];

// declaración de una matriz 10x10
var arr3 : int[10][10];

// declaración de algo raro
var arr4 : int[365][24][60][60];

print! arr1[0]; // imprime -1
arr1[0] = 1; // asigna 1 en la primera posicion del arreglo.

~~~

Tipos de datos compuestos
===================

En BB se pueden definir un numero arbitrario de tipos de datos
compuestos o extendidos. Sin embargo, como se ha mencionado, solo se
puede hacer de forma global.

Estructuras de datos
--------------------

Dentro de BB se pueden definir estructuras de datos similares
a como ocurre en C.


~~~
struct <nombre> {
    <lista de variables_1>: <type_1>,
    <lista de variables_2>: <type_2>,
    ...
    <lista de variables_n>: <type_n>,
};
~~~

No se permite redeclarar ninguna variable en ningún punto. Durante el
análisis sintáctico se eleva el identificador del tipo al alcance
global, por lo que si existe algún otro identificador de alcance
global con el mismo nombre resultará en un error estático.


En caso de obviar la inicialización, se toma el valor neutro de todos
sus campos. En caso de que el campo sea también una estructura de
datos, se utiliza el valor neutro de sus campos hasta alcanzar el caso
base de un tipo básico.

Los campos de cada estructura se pueden acceder utilizando la sintaxis:

`<identificador>.<campo>`

Los campos una vez inicializado son mutables y se pueden modificar
utilizando la sintaxis conocida:

`<identificador>.<campo> = <valor>;`

Enumeraciones
-------------

Se pueden definir enumeraciones del lenguaje con la palabra clave
`enum` como sigue:

~~~
enum <identificador> {
    <identificador valor 1>,
    <identificador valor 2>,
    ...,
    <identificador valor n>
};
~~~

No se puede repetir ningún identificador en ningún punto.

Al hacer cast de un identificador a entero en este caso su valor
corresponde a su posición en el rango `[0..n]`.

Cada identificador tiene que ser único (no puede repetirse y no puede
existir alguna variable, función o tipo con el mismo
nombre). Opcionalmente, se pueden asignar valores enteros a los
identificadores.

~~~
enum <identificador> {
    <identificador valor 1>[ = <valor_1>],
    <identificador valor 2>[ = <valor_2>],
    ...,
    <identificador valor n>[ = <valor_n>]
}
~~~

Al hacer cast de un identificador a entero en este caso cuando no
tiene asignado un valor, su valor corresponde al valor al hacer cast
del identificador que aparece antes de el más uno. El primer
identificador, de no tener asignado un valor, corresponde al valor 0.

El valor neutro de una enumeración corresponde siempre al valor neutro
del identificador que aparece de primero.

Durante el análisis estático los identificadores son elevados a
alcance global, por lo que en caso de que a momento en que se registra
el identificador ya existe algún otro identificador global (tipo,
función, variable y posiblemente otros identificadores de
enumeraciones) que tengan ese nombre no se registrará ese tipo.


Uniones
-------

Dentro de BB se pueden declarar uniones arbitrariamente anidadas
utilizando la siguiente sintaxis:

~~~
union <identificador> {
    <identificador 1>: <tipo 1>,
    <identificador 2>: <tipo 2>,
    ...
    <identificador n>: <tipo n>,
};
~~~

El valor neutro de una unión corresponde siempre al valor neutro del
tipo del identificador que aparece de primero.

Durante el análisis sintáctico se eleva el identificador del tipo al
alcance global, por lo que de existir algún otro identificador de
alcance global con el mismo nombre, resultará en error estático.


La semántica de la enumeración es funcionar como un una unión como en
C/C++. Una declaración anónima de tipo también es válida. Esto se
puede combinar con la sección anterior para definir uniones que tengan
algunos definiciones de estructuras.



Declaraciones "anónimas"
-------------------------

En cualquier punto de una declaración de un tipo se puede agregar una
declaración anónima. Sin embargo, esta declaración tiene que tener su
identificador y definirá un tipo accesible en cualquier punto del
programa. Al igual que las declaraciones que no son anónimas, el
identificador del tipo es elevado al alcance global por lo que de
existir algún otro identificador de alcance global con el mismo nombre
esto resultará en error.

Ejemplos
-------

~~~
struct point {
    x, y : double;
}

struct rect {
    top_left, bottom_right : point;
    fill_color : enum color {
        red, blue, green
    };
}

struct circle {
    center : point;
    radius : double;
    fill_color : color;
}

struct triangle {
    a,b,c : point;
    fill_color : color;
}

struct weird {
	p : point[10];
}

fn main(args: string[]) {
    point p1(0, 0), p2(1, 1);
    int dx = p1.x-p2.x, dy = p1.y - p2.y;
    print! sqrt(dx*dx+dy*dy);
}
~~~

No está permitido declarar arreglos con el uso de declaraciones
anónimas. Exhortamos al desarrollador a declarar previamente el tipo
antes de intentar usar un arreglo del mismo.


Funciones y procedimientos
===========================

Las funciones se declaran con la palabra clave `fn` como sigue:

~~~
fn <identificador>(<parametro_1>: <tipo_1>,
                   <parametro_2>: <tipo_2>,
                   ...,
                   <parametro_n>: <tipo_n>,
                   <default_1>: <tipo_n+1> = <valor_1>,
                   <default_2>: <tipo_n+2> = <valor_2>,
                   ...,
                   <default_m>: <tipo_m> = <valor_m>) [: <tipo retorno>] {
    <instrucciones>
}
~~~

El único caso en el que se puede (y se debe) obviar el `: <tipo
retorno>` corresponde a funciones `void`.

La ejecución de una función finaliza en cualquier punto el uso de la
palabra clave `return`.

`return [<resultado>];`

El único caso en el que se puede (y se debe) obviar `<resultado>` es
cuando la función es de tipo `void`.

El pasado de parámetros por defecto es siempre por valor. Se puede
forzar el pasado de parámetros por referencia en utilizando los tipos
especiales (y solo utilizables en este contexto) que terminan con un
`&`, por ejemplo, `int&`.

Está permitido hacer llamadas recursivas de procedimientos. Dado que
el lenguaje permite referenciar cualquier cantidad de funciones que
posiblemente no están definidas aún, se permite la recursión entre
funciones mutuamente recursivas.

Al necesitar agregar un parametro de tipo arreglo, todas sus dimensiones
deben estar bien especificadas como literales interpretables por
el lenguaje de tipo ENTERO. 

Selección
=========

La instrucción de selección es de la forma

~~~~~~~
if <condicion> {
    <Instrucción>
}
~~~~~~~

Los corchetes son siempre obligatorios pero la instrucción puede ser
vacía. El primer bloque `if` puede, opcionalmente, tener un número
arbitrario de bloques `else if` y un solo bloque `else` al final.

~~~~~~~
if <condición_1> {
    <instrucción 1>
} else if <condición_2> {
    <instrucción 2>
} else if <condición_3> {
    <instrucción 3>
} else {
    <instrucción 4>
}
~~~~~~~

La semántica de la instrucción de selección es la convencional.

Ejemplos:
---------

~~~~~~
fn sign(x: int) : int {
    if x > 0 {
        return 1;
    } else if x < 0 {
        return -1;
    } else {
        return 0;
    }
}
~~~~~~


Iteración Indeterminada
=======================

El lenguaje provee dos métodos para iteración indeterminada. El
primero y más general es la instrucción `while`:

~~~
while <condición> {
    <instrucción>
}
~~~

Los corchetes son siempre obligatorios pero la instrucción puede ser
vacía. La ejecución del bloque puede interrumpirse utilizando las
palabras claves `break` y `continue`. `break` acaba la ejecución de la
instrucción y ejecuta la siguiente instrucción después del corchete
que cierra la instrucción `while`.  `continue` brinca inmediatamente a
la posición del corchete que abre la instrucción `while`. La semántica
del `while` es la convencional. Cualquier variable declara dentro de
los corchetes es solo accesible dentro de los mismos.

La segunda instrucción de iteración indeterminada consiste en el bloque `loop`:

~~~
loop {
    <instrucción>
}
~~~

Esta instrucción es azúcar sintáctico para

~~~
while true {
    <instrucción>
}
~~~

por lo que el funcionamiento de `break` y `continue` funcionan igual
para la instrucción `loop`.

Ejemplos
--------

~~~
// common human behavior
loop {
    if valid(conflict) {
        throw_some_bombs();
    } else {
        conflict = find_new_conflict();
    }
}

// rock concert
int i = 4;
while i > 0 {
    print! "{}... " , i;
    i -= 1;
}
print! "Rock.";
~~~

Iteración Acotada
==================

La iteración acotada se puede realizar haciendo uso de la instrucción
`for`:

~~~
for <type> <id>: <contents> {
    <instrucción>
}
~~~

Los corchetes son siempre obligatorios pero la instrucción puede ser
vacía. `<type>` representa el tipo de cada elemento dentro de
`<contents>` que será accesible a través del identificador `<id>`
únicamente dentro del alcance definido por los corchetes que abren y
cierran la instrucción. `<contents>` puede ser un arreglo o un rango.

Durante la ejecución de la instrucción `for` con un arreglo o un rango
definido sobre un arreglo como `<contents>` se puede utilizar el
operador `@` para determinar el índice de `<contents>` actual.

De igual manera, se puede utilizar las palabras clave `continue` y
`break`. En el caso de `break` su funcionamiento es exactamente igual
para el caso de iteración indeterminada, sin embargo para el caso de
`continue` además se avanza una posición en el rango o arreglo.

Ejemplos
--------

~~~~
for int i : 2..1000000 {
    if esPrimo(i) {
        print! i," es primo";
    }
}
~~~~

~~~
for string s: args {
    print! @s, " -> ", s;
}
~~~


Tablas de Precedencia
----------------------

| Precedencia | Operador                                                    | Asociatividad       |
|-------------|-------------------------------------------------------------|---------------------|
|           1 | `arreglo[]`, `objeto.valor`, `funcion()`                    | Izquierda a derecha |
|           2 | `**`                                                        | Izquierda a derecha |
|           3 | `-` `+` `~` `!` `@` `#` `as` | Derecha a izquierda |
|           4 | `*` `/` `%`                                                  | Izquierda a Derecha |
|           5 | `+`, `-`                                                    | Izquierda a derecha |
|           6 | `>>`, `<<`                                                  | Izquierda a derecha |
|           7 | `<=`, `<`, `>`, `>=`                                        | Izquierda a derecha |
|           8 | `==`, `!=`                                                  | Izquierda a derecha |
|           9 | `&`                                                         | Izquierda a derecha |
|          10 | `^`                                                         | Izquierda a derecha |
|          11 | OR binario                                                         | Izquierda a derecha |
|          12 | `&&`                                                        | Izquierda a derecha |
|          13 | OR booleano                                                        | Izquierda a derecha |
|          14 | `by`                                                        | No asociativo       |
|          15 | `..`                                                        | Izquierda a derecha |
