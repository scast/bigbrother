fn abs(n:int):int {
	if (n<0) {
		return -n;
	} else {
		return n;
	}
}

struct rect {
    p1, p2 : struct point {
        x, y : float,
        deep: struct raro {
            x, y: float
        }
    },
    fill_color : enum color {
        red, blue, green
    }
};

fn AreaRect(r:rect):int {
	return abs(r.p2.x - r.p1.x) * abs(r.p2.y - r.p1.y);
}

fn main(args: char[][]) {
	var p1, p2 : point;
	var r : rect;
	
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
