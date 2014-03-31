var x = 1000:int;

fn main() {
	var x = 2:int;
	print! x;             // 2
	print! SumaleUno(x);  // 3
	var x = 77:int;       // La estas embarrando
	print! x;
}

fn SumaleUno(x:int):int {
	return x+1;
}

fn ImprimeElGlobal() {
	print! x;             // 1000
}
