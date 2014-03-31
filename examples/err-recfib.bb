// Funcion de Fibonacci
fn fib (f:int) {
	if (f<2) {
		return 1;
	}
	return fib(f-1)+fib(f-2);
}

fn main() : int {
	var f:int;
	grab! f;
	if () {      // Esto es if true o que ?
		var f = "Oh no..":string;
		print! msg;
		return 1;
	}}           // Chamo, ya cerraste el if...
	print! "El numero de Fibonacci de ", f, " es ", fib(f);
}
