#!/usr/bin/env python

# arclayer.py: Gimp plug-in to warp one layer along an arc of a given radius.

# Copyright 2002,2008 by Akkana Peck, http://www.shallowsky.com/software/
# You may use and distribute this plug-in under the terms of the GPL.
#
# Thanks to Joao Bueno for some excellent tips on speeding up
# pixel ops in gimp-python!

import math
import time
from gimpfu import *
from array import array

def python_arc_layer(img, layer, radius, ontop) :
        gimp.progress_init("Arcing layer" + layer.name + "...")

        # Spinner passes an integer, which will upset later calculations
        radius = float(radius)

        pdb.gimp_image_undo_group_start(img)

        layername = "arc " + layer.name
        # Calculate the size for the new layer
        theta2 = layer.width / radius / 2
        newWidth = int(2 * radius * math.sin(theta2))
        if theta2 <= 1.570795 :   # PI/2: if we're doing less than a semicircle
                newHeight = int(layer.height * math.cos(theta2) 
                                + .5 * radius * math.sin(theta2) *
                                  math.tan(theta2))
        else :
                newHeight = layer.height + int(radius * (1 - math.cos(theta2)))

        #print "Old size: ",layer.width,"x",layer.height
        #print "New size: ",newWidth,"x",newHeight
        #print "r =", radius, ", theta/2 =", theta2

        # Create the new layer:
        destDrawable = gimp.Layer(img, layername, newWidth, newHeight,
                                  layer.type, layer.opacity, layer.mode)
        img.add_layer(destDrawable, 0)
        xoff, yoff = layer.offsets
        destDrawable.translate(xoff - (newWidth - layer.width)/2,
                               yoff - (newHeight - layer.height)/2)

        # No need to clear the layer here --
        # we'll initialize it to zero when we create the pixel array.

        srcWidth, srcHeight = layer.width, layer.height
        srcRgn = layer.get_pixel_rgn(0, 0, srcWidth, srcHeight,
                                     False, False)
        src_pixels = array("B", srcRgn[0:srcWidth, 0:srcHeight])

        dstRgn = destDrawable.get_pixel_rgn(0, 0, newWidth, newHeight,
                                            True, True)
        p_size = len(srcRgn[0,0])               
        dest_pixels = array("B", "\x00" * (newWidth * newHeight * p_size))

        # Finally, loop over the region:                    
        for x in xrange(0, srcWidth - 1) :
                for y in xrange(0, srcHeight) :
                        # Calculate new coordinates
                        phi = theta2 - x/radius
                        if ontop :
                                r = radius - y
                                newy = int(radius - r * math.cos(phi))
                                if (newy < 0) :
                                        continue
                        else :
                                r = radius - layer.height + y
                                newy = newHeight \
                                       + int(r * math.cos(phi) - radius)
                                if (newy > newHeight) :
                                        continue
                        newx = int(newWidth/2 - r * math.sin(phi))

                        src_pos = (x + srcWidth * y) * p_size
                        dest_pos = (newx + newWidth * newy) * p_size
                        
                        newval = src_pixels[src_pos: src_pos + p_size]
                        dest_pixels[dest_pos : dest_pos + p_size] = newval
                        
                        # A fast (but not great) cheat to fill in holes:
                        # Write to two pixel above the new location
                        # and one pixel to the right of it.
                        # If that's a valid location, then later
                        # another set of coordinates will map to it,
                        # and overwrite this value; if not, then it
                        # would have been a hole, and this fills it.
                        if (newy < newHeight-1) :
                                next_row = dest_pos + p_size * newWidth
                                dest_pixels [next_row : next_row + p_size] \
                                    = newval
                        if (newx < newWidth-1) :
                                next_col = dest_pos + p_size
                                dest_pixels [next_col : next_col + p_size] \
                                    = newval

                #print "Progress:", 100.0 * x / layer.width
                # Docs say progress_update takes a percent,
                # but it's really a fraction.
                # GIMP whines "arclayer.py is updating the progress too often"
                # but, well, tough. It's only ten times total.
                progress = float(x)/layer.width
                if (int(progress * 100) % 20 == 0) :
                        gimp.progress_update(progress)

        # Copy the whole array back to the pixel region:
        dstRgn[0:newWidth, 0:newHeight] = dest_pixels.tostring() 

        destDrawable.flush()
        destDrawable.merge_shadow(True)
        destDrawable.update(0, 0, newWidth,newHeight)

        # Remove the old layer
        #img.remove_layer(layer)
        layer.visible = False

        pdb.gimp_selection_none(img)
        pdb.gimp_image_undo_group_end(img)

register(
        "python_fu_arc_layer",
        "Bend a layer in an arc",
        "Bend a layer in an arc",
        "Akkana Peck",
        "Akkana Peck",
        "2002,2008",
        "<Image>/Filters/Distorts/ArcLayer(py)...",
        "*",
        [
                (PF_SPINNER, "Radius", "Arc Radius (pixels)",
                 400, (0, 2000, 50)),
                (PF_TOGGLE, "Top", "Top of circle?", 1),
        ],
        [],
        python_arc_layer)

main()
