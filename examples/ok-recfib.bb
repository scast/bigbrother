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
	if (f<0) {
		var msg = "Oh no..":char[10];
		print! msg;
		return 1;
	}
	print! "El numero de Fibonacci de ", f, " es ", fib(f);
}
