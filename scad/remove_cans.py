#!/usr/bin/env python2

from solid import *

can_height = 12.3
can_radius = 3.2

def can():
	return rotate([0, 90, 0])(
		cylinder(h=can_height, r=can_radius, center=True)
	)

def can_hole():
	return cube(size=[can_height,can_radius*2,100])


boat = scale(100)(
		import_stl("/home/wolf/qea/hulls/boats/lemonierboatfixed.stl")
	)

boat - can_hole()
