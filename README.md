bigbrother
==========

BB un lenguaje autocrático.

Corriendo
----------

Como mínimo necesitas la plataforma de Haskell y Cabal >= 1.18. Nosotros probamos con un sandbox de Cabal 1.18 por lo que estas instrucciones dependen de eso.

Una vez clonado este repositorio

~~~
cd src/
cabal sandbox init
cabal install 
~~~

No logramos que cabal tomara encuenta nuestras opciones de traducción para Alex por lo que nosotros corremos manualmente `alex` y `happy` como sigue:

~~~
alex -t templates Lexer.x
happy Parser.ly
cabal build
~~~

Buena suerte.
