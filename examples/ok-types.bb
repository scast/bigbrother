struct rect {
	p1,p2:struct punto {
		x,y:float
	}
};

fn abs(n:float):float {
	if (n<0) {
		return -n;
	} else {
		return n;
	}
}

fn area(r:rect):int {
	return abs(r.p2.x - r.p1.x) * abs(r.p2.y - r.p1.y);
}

fn main() {
	var a:int[4];
	var b:float[4];
	var c:char;
	var p:punto[2];
	var r:rect;

	a[0] = 1;
	p[1].x = b[1]*a[1];
	r.p1.y = 1.0;
	b[0] = area(r);
}
