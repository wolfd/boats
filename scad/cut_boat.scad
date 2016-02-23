$fs=0.5; 
$fa=5;


metersToInches = 39.37;



module mast() {
    scale([metersToInches/100,metersToInches/100,metersToInches/100]) {
        translate([31.7542,0.0,26.5879])    cylinder(50,0.5,0.5,center=true);
    }
}

module boat() {
    scale([metersToInches,metersToInches,metersToInches]) import("/home/wolf/qea/bestboat.stl");
}

module bboat() {
    translate([0,5,-5.5]) difference() { 
        boat();
//        mast();
    }
}

module cut(n) {
    difference() {
        translate([0, 0, n*2]) bboat();
        scale([1,1,-1]) cube(100);
        translate([0,0,2]) cube(100);
    }
}

//projection(cut=false) {
//    translate([0,0,5]) {
//        intersection() {
//            cut(1);
//            cube([30,5,2]);
//        }
//    }
//}

translate([0,0,5]) {
    intersection() {
        cut(1);
        cube([30,5,2]);
    }
};

difference() {
    
//    for(i = [0:3]) {
//        translate([0, i * 10, i*2]) bboat();
//    }
//    scale([1,1,-1]) cube(100);
//    translate([0,0,2]) cube(100);
}

