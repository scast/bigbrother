var x = 1000:int;

fn main() {
	var x = 2:int;
	print! x;             // 2
	print! SumaleUno(x);  // 3
	if x>0 {
		var x = 77:int;
		print! x;         // 77
	}
}

fn ImprimeElGlobal() {
	print! x;             // 1000
}

fn SumaleUno(x:int):int {
	return x+1;
}
