// Funcion de Fibonacci
fn fib (f:int) {
	if (f<2) {
		return 1;
	}
	return fib(f-1)+fib(f-2);
}

fn main() {
	var f:int;
	grab! f;
	if (f) {  // Esto es if true o que ?
		var msg = "Oh no..":char[10];
		print! msg;
		return 1;
	}
	print! "El numero de Fibonacci de ", f, " es ", fib(f);
}
