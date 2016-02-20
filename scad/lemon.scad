sectionLength=25;
difference() {
    for(i = [-12:12]) {
        translate([0,0,-i*sectionLength]) {
            cylinder(sectionLength,150-(i*i),150-((i-1)*(i-1)));
        }
    }
    translate([400,0,0]) cube(800,200,200,center=true);
}