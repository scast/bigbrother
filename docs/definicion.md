* Imperativo.
* Tipos fuertes.
* [X] void, bool, enteros, flotantes, caracteres y cadenas.
* Alcance estático de bloques anidados.
* [X] Selector.
* [X] Iteración acotada.
* [X] Iteración indeterminada.
* [X] Pasaje por valor y por referencia
  * [X] Tipos primitivos por valor o referencia.
  * [X] Tipos agregados por referencia.
* [X] Recursión.
* [X] Estructuras arbitariamente anidadas (`struct` de C)
* [X] Uniones arbitrariamente anidadas (`union` de C)
* [X] Arreglos unidimensionales con índices arbitrarios
* Read, write polimórficos para tipos primitivos.

* [X} Arreglos multidimensionales base cero
* [X] `break`, `continue`
* etiquetas para bloques (goto, queremos esto?)
* Seleccion múltiple (`case`) [redeclaracion]
* Funciones con número variable de argumentos - funciones variádicas
* [X] Tipos enumeración / subrango / integrado con iteraciones
* Anidamiento de funciones -- implica cadena estática.
* Pasaje de parámetros "raros".
* Uniones con discriminante oculto.
* Funciones de segunda clase.

BB es un lenguaje imperativo fuertemente tipeado basado en la sintaxis
de C e inspirado con ideas de Rust y D.

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
<declaracón de variables, tipos y funciones>
~~~

El tipo de la función de retorno de la función `main` puede ser `int`
o `void`. En el caso de ser `void` el programa siempre retornara un
código de salida exitoso.

Alcances y Bloques
==================

BB es un lenguaje de alcance estático. El alcance dentro de BB está
definido por bloques demarcados por `{` y `}`.

~~~
const X = 65536:int;
fn main(args: string[]) {
    var x=0, y=1, z=1 : int;
    {
        var x = 10 : int;
        print! "El valor de x es ", x; //  10
    }
    print! "El valor de x es ", x; // 0
    print! "El valor de X es ", X; // 65536
}
~~~

Redeclarar una variable en el mismo alcance no está
permitido.

Para definiciones de alcance global, y solo para definiciones
globales, se puede referenciar a variables, funciones o procedimientos
que no se han declarado todavía.

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


Tipos
=====

Dentro de BB toda variable declarada siempre tiene un valor y
un tipo. Al declarar una variable se puede inicializar su valor o en
caso contrario se asignara un valor neutro. Los valores neutros para
los distintos tipos se definen en las secciones subsiguientes.

La declaración de una variable se realiza con la siguiente sintaxis:

`var <identificador_1>, <identificador_2>, ..., <identificador_n> : <tipo>;`

Se puede al momento de declaración realizar la inicialización de un número arbitrario de variables como sigue:

`var <identificador_1> [= <valor_1>], <identificador_2> [= <valor_2>], ..., <identificador_n> [= <valor_n>] : <tipo>;`

Tipos básicos
==============

Los tipos básicos que define el lenguaje son booleanos, enteros,
flotantes y caracteres.

Modificadores
-------------

Al tiempo de declaración de variables globales, se pueden asignar los
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

fn main(args: string[]) {
    print! "Hola Mundo";
}
~~~

Tipos booleanos
---------------

Los tipos booleanos solo pueden tomar valores `true` o `false`. Sus
operadores son los operadores normales `&&` (and), `||` (or), `!` (not), y `^` (xor)
respectivamente. Dos valores booleanos se pueden comparar utilizando
los operadores de comparación `==`, `!=`, `>`, `>=`, `<`, `<=`.

Al momento de declaración, en caso de obviar el valor inicial, se
inicializan a su valor neutro `false`.

En este documento al referirnos a una condición nos referimos siempre
a una expresión booleana.

Tipos numéricos
---------------

Los tipos numéricos dentro del lenguaje son los enteros denotados por
int16, int32 e int64 y por números flotantes denotados por float32 y
float64.

Al momento de declaración, en caso de obviar el valor inicial, se
inicializan utilizando su valor neutro `0`.

Para referirnos a números podemos utilizar cualquiera de las siguientes notaciones:

- Notación decimal: 0, 1, 1, 2, 3, 5, 8, ...
- Notación científica (redondea al entero más cercano): 6.022e23, ...
- Notación para favorecer lectura: 1\_000\_000
- Notación binaria: 0b0, 0b1, 0b1, 0b10, 0b11, 0b101, 0b1000, ...
- Notación hexadecimal: 0x0, 0x1, 0x1, 0x2, 0x3, 0x5, 0x8

El punto flotante se denota con el `.`. Para referirnos a números flotantes se puede utilizar:

- Notación decimal: 0, 1, 1, 2.0, 3.0, 5, 8, 17.325...
- Notación científica: 6.022e23, 1.5e-4 ...

Los siguientes operadores aritméticos están definidos: `+`, `-`, `*`, `/`,
`\*\*` (exponenciación), `%` (módulo).

Los siguientes operadores de comparación están definidos: `==`, `!=`, `>`,
`<`, `>=`, `<=`.

Los siguientes operadores de bits están definidos: `&` (and), `|` (or),
`^` (xor), `~` (not), `>>`, `<<`.

Los siguientes operadores de asignación están definidos: `=`, `*=`, `/=`, `%=`, `+=`,
`-=`, `>>=`, `<<=`, `&=`, `^=`, `|=`, `\*\*=`.

Además, se cuenta con un operador unario prefijo caracter % el cual
devuelve el caracter cuya representación en ASCII es el entero,
siempre y cuando este dentro de la tabla ASCII --- de lo contrario,
debe ocurrir un error.

Caracteres
----------

Los caracteres están definidos utilizando la tabla ASCII. Su valor
neutro es `'\0'`. Para referirnos a un carácter utilizamos comillas
simples.

Los carácteres tienen un solo operador unario prefijo de ordenamiento
\#: dado un caracter, devuelve su posición en la tabla ASCII.

<!-- Cadenas de Caracteres -->
<!-- --------------------- -->

Tablas de Precedencia
----------------------

Tabla aca.

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
`#` y `%`.

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
var y = 15. : int64; // 15
~~~

Ejemplos
--------

`true or false // == true`

`true and false // == false`

`true == false // == false`

`true > false // == true`

`not true // == false`

Sobrenombres
============

Fuera de cualquier función se puede declarar un sobrenombre para un
tipo utilizando la siguiente sintaxis:

`type <identificador> = <tipo>;`

Dentro del lenguaje se asignan los siguientes sobrenombres para facilitar el uso:

~~~
type short = int8;
type int = int32;
type long = int64;
type double = float64;
type float = float32;
~~~


Ejemplos
--------

`type MiEntero = int64;`

Rangos
======

Dentro del lenguaje los rangos se pueden definir como sigue:

`<inicial>..<final>`

Esto define un rango desde inicial (incluyéndolo) hasta final
(excluyéndolo) aumentando con un paso de 1.

Para variar el paso, se puede utilizar la notación extendida:

`<inicial>, <inicial>+<paso>.., <final>`

Los rangos no representan un tipo concreto dentro del lenguaje. Son un
atajo para operaciones comunes, no se pueden asignar, no se pueden
pasar como parámetro a ninguna función, y no se pueden retornar como
resultado de ninguna función.

Ejemplos
---------

`0..10 // [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]`

`0,2..10 // [0, 2, 4, 6, 8]`


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

`var <identificador> : <tipo>[0..<entero>];`

En el caso de arreglos multidimensionales la sintaxis es similar:

`var <identificador> : <tipo>[<rango_1>][<rango_2>]...[<rango_n>];`

Con la declaración simplificada

`var <identificador> : <tipo>[<entero_1>][<entero_n>]...[<entero_n>];`

Como atajo para operaciones comunes, al momento de inicialización se
puede utilizar la siguiente notación para llenarlo de un valor
particular:

`<arreglo> = <valor>`

Se cuenta con los siguientes operadores para trabajar con arreglos:

- `#arreglo` retorna la dimensión más externa del arreglo
- Para acceder un indice se utiliza el operador `[]` como en C.
- `arreglo[RANGO]` devuelve un rango con los elementos entre los elementos de RANGO.

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

// poner todos los valores de arr1 en -1
arr1 = -1;

// también podemos hacerlo al declarar
var arr5 = -1 : int[10];

print! arr1[0]; // imprime -1
arr1[0] = 1; // asigna 1.

for int i: arr1[2..7] {
    for int j: arr2[5..8] {
        // hacer algo con i y j.
    }
}
~~~



Estructuras de Datos
===================

Dentro de BB se pueden definir estructuras de datos similares
a como ocurre en C.


~~~
struct <nombre> {
    <lista de variables_1>: <type_1>;
    <lista de variables_2>: <type_2>;
    ...
    <lista de variables_n>: <type_n>;
}
~~~

Una vez declarada la estructura, en cualquier punto del programa se
puede declarar y a su vez inicializar sus campos utilizando la
sintaxis común en C++:

~~~
var <id_1>(<valores para cada tipo en orden en que fueron declarados>): <nombre>;
~~~


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
    <identificador valor n>;
}
~~~

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


Uniones
-------

Dentro de BB se pueden declarar uniones arbitrariamente anidadas
utilizando la siguiente sintaxis:

~~~
union <identificador> {
    <identificador 1>: <tipo 1>;
    <identificador 2>: <tipo 2>;
    ...
    <identificador n>: <tipo n>;
}
~~~

El valor neutro de una unión corresponde siempre al valor neutro del
tipo del identificador que aparece de primero.


La semántica de la enumeración es funcionar como un una unión como en
C/C++. Una declaración anónima de tipo también es válida. Esto se
puede combinar con la sección anterior para definir uniones que tengan
algunos definiciones de estructuras.



Declaraciones "anónimas"
-------------------------

En cualquier punto de una declaración de un tipo se puede agregar una
declaración anónima. Sin embargo, esta declaración tiene que tener su
identificador y definirá un tipo accesible en cualquier punto del programa.

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

El pasado de parámetros por defecto es por valor para los tipos
básicos y por referencia para los tipos extendidos y arreglos. Se
puede forzar el pasado de parámetros por referencia en los tipos
básicos utilizando los tipos especiales (y solo utilizables en este
contexto) que terminan con un `&`, por ejemplo, `int&`.

Está permitido hacer llamadas recursivas de procedimientos.

Es posible declarar multiples veces la misma función o procedimiento
siempre y cuando sus parámetros sean diferentes entre cualquier par de
funciones o procedimiento con el mismo nombre.


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
operador `#` para determinar el índice de `<contents>` actual.

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
    print! #{s}, " -> ", s;
}
~~~
