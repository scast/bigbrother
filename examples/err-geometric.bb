fn AreaRect(r:rect):int {
	// Seria bueno si defines abs
	return abs(r.p2.x - r.p1.x) * abs(r.p2.y - r.p1.y);
}

struct point {
    x, y : double
};

struct rect {
    p1, p2 : point,
    fill_color : enum color {
        red, blue, green
    },
	blue : float
};

fn main() {
	var p1, p2 : point;
	var p1 : rect;
	
	// Probando rectangulos
	// Just because we can
	for int x1 : 0..6 {
		p1.x = x1;
		for int x2 : 0..6 {
			p2.x = x2;
			for int y1 : 0..6 {
				p1.y = y1;
				for int y2 : 0..6 {
					p2.y = y2;
					r.p1 = p1;
					r.p1 = p2;
					print! AreaRect(r);
				}
			}
		}
	}
}
