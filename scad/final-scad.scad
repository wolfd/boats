$fs=0.1; 
$fa=5;

scale(v = [0.0100000000, 0.0100000000, 0.0100000000]) {
	difference() {
		scale(v = 100) {
			import_stl(filename = "/home/wolf/qea/hulls/boats/bestlemon.stl");
		}
		translate(v = [39.7542000000, 0.0000000000, 6.2879200000]) {
			union() {
				rotate(a = [0, 90, 0]) {
					cylinder(h = 12.8000000000, r = 3.3000000000, center = true);
				}
				translate(v = [0, 0, 50]) {
					cube(center = true, size = [12.8000000000, 6.6000000000, 100]);
				}
			}
		}
		translate(v = [23.7542000000, 0.0000000000, 6.2879200000]) {
			union() {
				rotate(a = [0, 90, 0]) {
					cylinder(h = 12.8000000000, r = 3.3000000000, center = true);
				}
				translate(v = [0, 0, 50]) {
					cube(center = true, size = [12.8000000000, 6.6000000000, 100]);
				}
			}
		}
	}
}
