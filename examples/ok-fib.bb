var fib : int[10000];

fn main():int {
	var f:int;
	grab! f;
	fib[0] = 1;
	fib[1] = 1;
	if (f<0) {
		var msg = "Oh no..":char[10];
		print! msg;
		return 1;
	} else if f>=2 {
		var i=2:int;
		while (i<=f) {
			fib[i] = fib[i-1] + fib[i-2];
			i += 1;
		}
	}
	print! "El numero de Fibonacci de ", f, " es ", fib[f];
	return 0;
}
