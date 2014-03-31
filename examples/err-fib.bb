var fib : int[10000];

fn main() {
	var f:int;
	grab! f;
	fib[0] = 1;
	fib[1] = 1;
	if (f<0) {
		var msg = "Oh no..":string;
		print! msg;
		return 1;
	} else if f>=2 {
		// Ni lo sueñes, tienes que abrir y cerrar llaves
		for int i : 2..(f+1)
			fib[i] = fib[i-1] + fib[i-2];
	}
	print! "El numero de Fibonacci de ", f, " es ", fib[f];
}
