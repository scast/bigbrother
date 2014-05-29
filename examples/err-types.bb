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

	a = 1;
	p[1].x = b[1]*a*c;
	r.y = 1.0;
	c = area(r);
	b[0] = area(p);
	b[c] = area(r);
}
