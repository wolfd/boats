#!/usr/bin/env python2

from solid import *
from solid.utils import *
import subprocess
import json
import os

os.path.dirname(os.path.realpath(__file__))

can_height = 12.3 + 0.5
can_radius = 3.2 + 0.1

def can():
    return rotate([0, 90, 0])(
        cylinder(h=can_height, r=can_radius, center=True)
    )

def can_hole():
    hole_height = 100
    return up(hole_height/2)(
        cube(size=[can_height, can_radius*2, hole_height], center=True)
    )

def can_and_hole(t):
    return translate(t)(can() + can_hole())

def bailing_ports(t, water_height):
    port_length = 100
    port_radius = 1

    port = rotate([120, 0, 0])(
        cylinder(h=port_length,r=port_radius)
    )

    return translate([t[0], t[1], t[2] + can_radius + (port_radius * 2)])(
        port
    )

boat = scale(100)(
    import_stl("/home/wolf/qea/hulls/boats/experimentallemon.stl")
)

with open("boat.json","r") as f:
    boat_def = json.load(f)
    print boat_def
    import sys

    cans = filter(lambda o: o[u"name"] == u"Can", boat_def)

    # can1 = translate(boat_def[])

    with_can_holes = boat - [can_and_hole(c['com']) for c in cans]



    water = filter(lambda o: o[u"name"] == u"Water", boat_def)[0]['waterline']

    ports = [bailing_ports(c['com'], water) for c in cans]
    flipped_ports = scale([1,-1,1])(ports)

    bailed_boat = with_can_holes - ports - flipped_ports

    # final_boat = bailed_boat
    final_boat = scale([1.0/100.0,1.0/100.0,1.0/100.0])(bailed_boat)

    print scad_render(final_boat)

    with open("scad/tmp.scad","w") as tmp_scad:
        tmp_scad.write(scad_render(final_boat))

    subprocess.call(["openscad", "-o", "tmp.stl", "scad/tmp.scad"], cwd=os.getcwd())
