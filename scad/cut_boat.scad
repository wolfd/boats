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
    difference() { 
        boat();
//        mast();
    }
}

bboat();

//for(i = [0:3]) {
//    difference() {
//        translate([0, i * 10, -i*2]) bboat();
//        cube(100);
//    }
//}