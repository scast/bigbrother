var fib : int[10000];

fn main() {
	var f:int;
	grab! f;
	fib[0] = 1;
	fib[1] = 1.0;
	if (f<0) {
		var msg = "Oh no..":char[10];
		print! msg;
		return 1;
	} else if f>=2 {
		// Ni lo sueñes, tienes que abrir y cerrar llaves
		var i=2:int;
		while () {
			i += 1;
			fib[i] = fib[i-1] + fib[i-2];
		}
	}
	print! "El numero de Fibonacci de ", f, " es ", fib[f];
}
