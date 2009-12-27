#!/usr/bin/env python
# -*- coding: utf-8 -*-

# GIMP Layer Effects
# Copyright (c) 2008 Jonathan Stipe
# JonStipe@prodigy.net

# ---------------------------------------------------------------------

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import gimp, gimpplugin, math, re
from gimpenums import *
pdb = gimp.pdb
import gtk, gimpui, gimpcolor, gobject
from gimpshelf import shelf

class layerfx_base(object):
  mode_list = (NORMAL_MODE, DISSOLVE_MODE, MULTIPLY_MODE, DIVIDE_MODE, SCREEN_MODE, OVERLAY_MODE, DODGE_MODE, BURN_MODE, HARDLIGHT_MODE, SOFTLIGHT_MODE, GRAIN_EXTRACT_MODE, GRAIN_MERGE_MODE, DIFFERENCE_MODE, ADDITION_MODE, SUBTRACT_MODE, DARKEN_ONLY_MODE, LIGHTEN_ONLY_MODE, HUE_MODE, SATURATION_MODE, COLOR_MODE, VALUE_MODE)
  previewLayer = None
  hiddenLayer = None

  def cond(self, b, t = 1, f = 0):
    if b == True:
      return t
    else:
      return f

  def get_layer_pos(self, layer):
    for i, v in enumerate(self.img.layers):
      if layer == v:
        return i
    return -1

  def add_under_layer(self, newlayer, oldlayer):
    self.img.add_layer(newlayer, self.get_layer_pos(oldlayer) + 1)

  def add_over_layer(self, newlayer, oldlayer):
    self.img.add_layer(newlayer, self.get_layer_pos(oldlayer))

  def layer_exists(self, layer):
    return layer != None and layer in self.img.layers

  def set_hidden_layer(self, layer):
    if self.hiddenLayer == None:
      self.hiddenLayer = layer
    elif type(self.hiddenLayer) == gimp.Layer and self.hiddenLayer != layer:
      self.hiddenLayer = [self.hiddenLayer, layer]
    elif type(self.hiddenLayer) == list and layer not in self.hiddenLayer:
      self.hiddenLayer.append(layer)
    layer.visible = 0

  def unset_hidden_layer(self):
    if self.hiddenLayer != None:
      if type(self.hiddenLayer) == gimp.Layer and self.layer_exists(self.hiddenLayer):
        self.hiddenLayer.visible = 1
      elif type(self.hiddenLayer) == list:
        for i in self.hiddenLayer:
          if self.layer_exists(i):
            i.visible = 1
      self.hiddenLayer = None

  def draw_blurshape(self, drawable, size, initgrowth, sel, invert):
    k = initgrowth
    currshade = 0
    for i in range(size):
      if k > 0:
        pdb.gimp_selection_grow(drawable.image, k)
      elif k < 0:
        pdb.gimp_selection_shrink(drawable.image, abs(k))
      if invert:
        currshade = int(round((float(size - (i + 1)) / float(size)) * 255))
      else:
        currshade = int(round((float(i + 1) / float(size)) * 255))
      gimp.set_foreground(currshade, currshade, currshade)
      if pdb.gimp_selection_is_empty(drawable.image) == 0:
        pdb.gimp_edit_fill(drawable, FOREGROUND_FILL)
      pdb.gimp_selection_load(sel)
      k -= 1

  def apply_contour(self, drawable, channel, contour):
    contourtypes = (0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1)
    contours = ((0, 0, 127, 255, 255, 0),
                (0, 255, 127, 0, 255, 255),
                (0, 64, 94, 74, 150, 115, 179, 179, 191, 255),
                (0, 0, 5, 125, 6, 125, 48, 148, 79, 179, 107, 217, 130, 255),
                (0, 0, 33, 8, 64, 38, 97, 102, 128, 166, 158, 209, 191, 235, 222, 247, 255, 255),
                (0, 0, 28, 71, 87, 166, 194, 240, 255, 255),
                (0, 0, 33, 110, 64, 237, 97, 240, 128, 138, 158, 33, 191, 5, 222, 99, 255, 255),
                (0, 0, 33, 74, 64, 219, 97, 186, 128, 0, 158, 176, 191, 201, 222, 3, 255, 255),
                (3, 255, 54, 99, 97, 107, 179, 153, 252, 0),
                (0, 5, 9, 13, 16, 19, 22, 25, 27, 29, 30, 32, 33, 34, 35, 36, 38, 39, 40, 41, 43, 44, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 55, 56, 56, 57, 57, 58, 58, 59, 59, 59, 60, 60, 60, 61, 61, 61, 61, 62, 62, 62, 62, 62, 63, 63, 63, 63, 63, 63, 64, 64, 64, 64, 64, 71, 75, 78, 81, 84, 86, 89, 91, 93, 95, 96, 98, 99, 101, 102, 103, 104, 105, 107, 107, 108, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 119, 120, 121, 121, 122, 123, 123, 123, 124, 124, 124, 125, 125, 125, 125, 125, 125, 125, 126, 126, 126, 126, 126, 126, 126, 125, 125, 125, 125, 125, 125, 125, 125, 130, 134, 137, 141, 145, 148, 151, 153, 156, 158, 160, 162, 163, 165, 166, 167, 168, 170, 171, 171, 172, 173, 174, 175, 176, 177, 178, 178, 179, 180, 181, 181, 182, 183, 183, 184, 184, 185, 185, 186, 186, 187, 187, 188, 188, 189, 189, 189, 189, 190, 190, 190, 190, 191, 191, 191, 191, 191, 191, 191, 191, 191, 191, 193, 194, 196, 197, 198, 200, 201, 203, 204, 205, 207, 208, 209, 211, 212, 213, 214, 215, 217, 218, 219, 220, 220, 221, 222, 222, 223, 223, 224, 224, 224, 224, 224, 223, 223, 222, 222, 221, 221, 220, 219, 218, 217, 216, 215, 214, 213, 212, 211, 210, 209, 208, 206, 205, 204, 203, 202, 200, 199, 198, 197, 196, 194, 194),
                (0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 52, 54, 56, 58, 60, 62, 64, 66, 68, 70, 72, 74, 76, 78, 80, 82, 84, 86, 88, 90, 92, 94, 96, 98, 100, 102, 104, 106, 108, 110, 112, 114, 116, 118, 120, 122, 124, 126, 127, 125, 123, 121, 119, 117, 115, 113, 111, 109, 107, 105, 103, 101, 99, 97, 95, 93, 91, 89, 87, 85, 83, 81, 79, 77, 75, 73, 71, 69, 67, 65, 63, 61, 59, 57, 55, 53, 51, 49, 47, 45, 43, 41, 39, 37, 35, 33, 31, 29, 27, 25, 23, 21, 19, 17, 15, 13, 11, 9, 7, 5, 3, 1, 1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 45, 47, 49, 51, 53, 55, 57, 59, 61, 63, 65, 67, 69, 71, 73, 75, 77, 79, 81, 83, 85, 87, 89, 91, 93, 95, 97, 99, 101, 103, 105, 107, 109, 111, 113, 115, 117, 119, 121, 123, 125, 127, 128, 126, 124, 122, 120, 118, 116, 114, 112, 110, 108, 106, 104, 102, 100, 98, 96, 94, 92, 90, 88, 86, 84, 82, 80, 78, 76, 74, 72, 70, 68, 66, 64, 62, 60, 58, 56, 54, 52, 50, 48, 46, 44, 42, 40, 38, 36, 34, 32, 30, 28, 26, 24, 22, 20, 18, 16, 14, 12, 10, 8, 6, 4, 2))
    if contourtypes[contour-1] == 0:
      pdb.gimp_curves_spline(drawable, channel, len(contours[contour-1]), contours[contour-1])
    else:
      pdb.gimp_curves_explicit(drawable, channel, len(contours[contour-1]), contours[contour-1])

  def apply_noise(self, drawable, srclayer, noise, uselayer):
    noiselayer = gimp.Layer(drawable.image, "%s-noise" % (drawable.name), srclayer.width, srclayer.height, (RGBA_IMAGE, GRAYA_IMAGE)[drawable.image.base_type], 100.0, NORMAL_MODE)
    blanklayer = gimp.Layer(drawable.image, "%s-blank" % (drawable.name), srclayer.width, srclayer.height, (RGBA_IMAGE, GRAYA_IMAGE)[drawable.image.base_type], 100.0, NORMAL_MODE)
    self.add_over_layer(blanklayer, srclayer)
    self.add_over_layer(noiselayer, blanklayer)
    noiselayer.set_offsets(srclayer.offsets[0], srclayer.offsets[1])
    blanklayer.set_offsets(srclayer.offsets[0], srclayer.offsets[1])
    pdb.gimp_selection_none(drawable.image)
    gimp.set_foreground(0, 0, 0)
    pdb.gimp_edit_fill(noiselayer, FOREGROUND_FILL)
    pdb.gimp_edit_fill(blanklayer, FOREGROUND_FILL)
    gimp.set_foreground(255, 255, 255)
    if uselayer:
      srclayer.add_mask(srclayer.create_mask(ADD_WHITE_MASK))
      pdb.gimp_selection_none(drawable.image)
      pdb.gimp_edit_fill(blanklayer, FOREGROUND_FILL)
      pdb.plug_in_hsv_noise(drawable.image, noiselayer, 1, 0, 0, 255)
    else:
      pdb.gimp_selection_load(srclayer.mask)
      pdb.gimp_edit_fill(blanklayer, FOREGROUND_FILL)
      pdb.gimp_selection_none(drawable.image)
      pdb.plug_in_hsv_noise(drawable.image, noiselayer, 1, 0, 0, 255)
      noiselayer.mode = OVERLAY_MODE
    noiselayer.opacity = noise
    noiselayer = pdb.gimp_image_merge_down(drawable.image, noiselayer, EXPAND_AS_NECESSARY)
    blanklayer = noiselayer.create_mask(ADD_COPY_MASK)
    noiselayer.add_mask(blanklayer)
    pdb.gimp_selection_none(drawable.image)
    gimp.set_foreground(0, 0, 0)
    pdb.gimp_edit_fill(srclayer.mask, FOREGROUND_FILL)
    pdb.gimp_channel_combine_masks(srclayer.mask, blanklayer, CHANNEL_OP_REPLACE, 0, 0)
    drawable.image.remove_layer(noiselayer)

  def removePreviews(self):
    if self.layer_exists(self.previewLayer):
      self.img.remove_layer(self.previewLayer)
      self.previewLayer = None
    self.unset_hidden_layer()
    gimp.displays_flush()
    if pdb.gimp_image_undo_is_enabled(self.img) == 0:
      pdb.gimp_image_undo_thaw(self.img)

  def make_label(self, text, show = True):
    label = gtk.Label(text)
    label.set_use_underline(True)
    label.set_alignment(1.0, 0.5)
    if show:
      label.show()
    return label

  def make_combo_box(self, *vals):
    list_vals = []
    for i, v in enumerate(vals):
      list_vals.append(v)
      list_vals.append(i)
    box = gimpui.IntComboBox(tuple(list_vals))
    return box

  def make_blend_mode_box(self):
    return gimpui.IntComboBox((
      "Normal",        NORMAL_MODE,
      "Dissolve",      DISSOLVE_MODE,
      "Multiply",      MULTIPLY_MODE,
      "Divide",        DIVIDE_MODE,
      "Screen",        SCREEN_MODE,
      "Overlay",       OVERLAY_MODE,
      "Dodge",         DODGE_MODE,
      "Burn",          BURN_MODE,
      "Hard Light",    HARDLIGHT_MODE,
      "Soft Light",    SOFTLIGHT_MODE,
      "Grain Extract", GRAIN_EXTRACT_MODE,
      "Grain Merge",   GRAIN_MERGE_MODE,
      "Difference",    DIFFERENCE_MODE,
      "Addition",      ADDITION_MODE,
      "Subtract",      SUBTRACT_MODE,
      "Darken Only",   DARKEN_ONLY_MODE,
      "Lighten Only",  LIGHTEN_ONLY_MODE,
      "Hue",           HUE_MODE,
      "Saturation",    SATURATION_MODE,
      "Color",         COLOR_MODE,
      "Value",         VALUE_MODE
    ))

  def make_contour_box(self):
    return self.make_combo_box(
      "Linear",
      "Cone",
      "Cone - Inverted",
      "Cove - Deep",
      "Cove-Shallow",
      "Gaussian",
      "Half Round",
      "Ring",
      "Ring - Double",
      "Rolling Slope - Descending",
      "Rounded Steps",
      "Sawtooth 1"
    )

  def make_gradient_type_box(self):
    return self.make_combo_box(
      "Linear",
      "Bi-linear",
      "Radial",
      "Square",
      "Conical (sym)",
      "Conical (asym)",
      "Shaped (angular)",
      "Shaped (spherical)",
      "Shaped (dimpled)",
      "Spiral (cw)",
      "Spiral (ccw)"
    )

  def make_gradient_repeat_box(self):
    return self.make_combo_box("None", "Sawtooth Wave", "Triangular Wave")

  def make_interpolation_box(self):
    return self.make_combo_box("None", "Linear", "Cubic", "Sinc (Lanczos3)")

  def make_spinner(self, init, min, max, step, page, digits, show = True):
    controls = {
      "adj":     gtk.Adjustment(init, min, max, step, page),
      "spinner": gtk.SpinButton()
    }
    controls["spinner"].set_adjustment(controls["adj"])
    controls["spinner"].set_digits(digits)
    if show:
      controls["spinner"].show()
    return controls

  def make_slider_and_spinner(self, init, min, max, step, page, digits, show = True):
    controls = {
      "adj":     gtk.Adjustment(init, min, max, step, page),
      "slider":  gtk.HScale(),
      "spinner": gtk.SpinButton()
    }
    controls["slider"].set_adjustment(controls["adj"])
    controls["slider"].set_draw_value(False)
    controls["spinner"].set_adjustment(controls["adj"])
    controls["spinner"].set_digits(digits)
    if show:
      controls["slider"].show()
      controls["spinner"].show()
    return controls

  def show_error_msg(self, msg, e):
    origMsgHandler = pdb.gimp_message_get_handler()
    pdb.gimp_message_set_handler(ERROR_CONSOLE)
    pdb.gimp_message("Error: %s" % (msg))
    pdb.gimp_message_set_handler(origMsgHandler)
    raise(e(msg))

  def validatedata(self, img, drawable, *params):
    if type(img) != gimp.Image:
      self.show_error_msg("Argument 1 is not an image.", TypeError)
      return False
    elif not img.base_type in (RGB, GRAY):
      self.show_error_msg("Argument 1 must be of type RGB or GRAY.", ValueError)
      return False
    elif type(drawable) != gimp.Layer:
      self.show_error_msg("Argument 2 is not a layer.", TypeError)
      return False
    elif drawable not in img.layers:
      self.show_error_msg("Layer is not part of image.", ValueError)
      return False
    else:
      for i, v in enumerate(params):
        if v[0] == "color":
          if type(v[1]) != gimpcolor.RGB:
            self.show_error_msg("Argument %s must be of type gimpcolor.RGB." % (i), TypeError)
            return False
        elif v[0] == "gradient":
          if type(v[1]) != str:
            self.show_error_msg("Argument %s must be of type string." % (i), TypeError)
            return False
          elif v[1] not in pdb.gimp_gradients_get_list("")[1]:
            self.show_error_msg("Argument %s not found in gradient list." % (i), ValueError)
            return False
        elif v[0] == "color/gradient":
          if type(v[1]) != gimpcolor.RGB and type(v[1]) != str:
            self.show_error_msg("Argument %s must be of type gimpcolor.RGB or string." % (i), TypeError)
            return False
          elif type(v[1]) == str and v[1] not in pdb.gimp_gradients_get_list("")[1]:
            self.show_error_msg("Argument %s not found in gradient list." % (i), ValueError)
            return False
        elif v[0] == "pattern":
          if type(v[1]) != str:
            self.show_error_msg("Argument %s must be of type string." % (i), TypeError)
            return False
          elif v[1] not in pdb.gimp_patterns_get_list("")[1]:
            self.show_error_msg("Argument %s not found in pattern list." % (i), ValueError)
            return False
        elif v[0] == "color/gradientdata/patterndata":
          if type(v[1]) != gimpcolor.RGB and type(v[1]) != list and type(v[1]) != tuple:
            self.show_error_msg("Argument %s must be of type gimpcolor.RGB, list, or tuple." % (i), TypeError)
            return False
          elif type(v[1]) == list or type(v[1]) == tuple:
            if len(v[1]) == 8:
              if type(v[1][0]) != str:
                self.show_error_msg("Argument %s[0] must be of type string." % (i), TypeError)
                return False
              elif v[1][0] not in pdb.gimp_gradients_get_list("")[1]:
                self.show_error_msg("Argument %s[0] not found in gradient list." % (i), ValueError)
                return False
              elif type(v[1][1]) != int:
                self.show_error_msg("Argument %s[1] must be of type int." % (i), TypeError)
                return False
              elif v[1][1] < 0 or v[1][1] > 11:
                self.show_error_msg("Argument %s[1] is out of range (must be between 0 and 11)." % (i), ValueError)
                return False
              elif type(v[1][2]) != int:
                self.show_error_msg("Argument %s[2] must be of type int." % (i), TypeError)
                return False
              elif v[1][2] < 0 or v[1][2] > 3:
                self.show_error_msg("Argument %s[2] is out of range (must be between 0 and 3)." % (i), ValueError)
                return False
              elif type(v[1][3]) != int:
                self.show_error_msg("Argument %s[3] must be of type int." % (i), TypeError)
                return False
              elif v[1][3] < 0 or v[1][3] > 1:
                self.show_error_msg("Argument %s[3] is out of range (must be between 0 and 1)." % (i), ValueError)
                return False
              elif type(v[1][4]) != float:
                self.show_error_msg("Argument %s[4] must be of type float." % (i), TypeError)
                return False
              elif v[1][4] < 0.0 or v[1][4] > img.width:
                self.show_error_msg("Argument %s[4] is out of range (must be between 0 and %s)." % (i, img.width), ValueError)
                return False
              elif type(v[1][5]) != float:
                self.show_error_msg("Argument %s[5] must be of type float." % (i), TypeError)
                return False
              elif v[1][5] < 0.0 or v[1][4] > img.height:
                self.show_error_msg("Argument %s[5] is out of range (must be between 0 and %s)." % (i, img.height), ValueError)
                return False
              elif type(v[1][6]) != float:
                self.show_error_msg("Argument %s[6] must be of type float." % (i), TypeError)
                return False
              elif v[1][6] < -180.0 or v[1][6] > 180.0:
                self.show_error_msg("Argument %s[6] is out of range (must be between -180 and 180)." % (i), ValueError)
                return False
              elif type(v[1][7]) != float:
                self.show_error_msg("Argument %s[7] must be of type float." % (i), TypeError)
                return False
              elif v[1][7] < 0.0 or v[1][7] > 262144.0:
                self.show_error_msg("Argument %s[7] is out of range (must be between 0 and 262144)." % (i), ValueError)
                return False
            elif len(v[1]) == 3:
              if type(v[1][0]) != str:
                self.show_error_msg("Argument %s[0] must be of type string." % (i), TypeError)
                return False
              elif v[1][0] not in pdb.gimp_patterns_get_list("")[1]:
                self.show_error_msg("Argument %s[0] not found in pattern list." % (i), ValueError)
                return False
              elif type(v[1][1]) != float:
                self.show_error_msg("Argument %s[1] must be of type float." % (i), TypeError)
                return False
              elif v[1][1] < 1.0 or v[1][1] > 1000.0:
                self.show_error_msg("Argument %s[1] is out of range (must be between 1 and 1000)." % (i), ValueError)
                return False
              elif type(v[1][2]) != int:
                self.show_error_msg("Argument %s[2] must be of type int." % (i), TypeError)
                return False
              elif v[1][2] < 0 or v[1][2] > 3:
                self.show_error_msg("Argument %s[2] is out of range (must be between 0 and 3)." % (i), ValueError)
                return False
            else:
              self.show_error_msg("Argument %s must contain 8 values for a gradient fill, or 3 values for a pattern fill." % (i), ValueError)
              return False
        elif v[0] == "percent":
          if type(v[1]) != float:
            self.show_error_msg("Argument %s must be of type float." % (i), TypeError)
            return False
          elif v[1] < 0.0 or v[1] > 100.0:
            self.show_error_msg("Argument %s is out of range (must be between 0 and 100)." % (i), ValueError)
            return False
        elif v[0] == "contour":
          if type(v[1]) != int:
            self.show_error_msg("Argument %s must be of type int." % (i), TypeError)
            return False
          elif v[1] < 0 or v[1] > 11:
            self.show_error_msg("Argument %s is out of range (must be between 0 and 11)." % (i), ValueError)
            return False
        elif v[0] == "mode":
          if type(v[1]) != int:
            self.show_error_msg("Argument %s must be of type int." % (i), TypeError)
            return False
          elif v[1] not in self.mode_list:
            self.show_error_msg("Illegal value for argument %s." % (i), ValueError)
            return False
        elif v[0] == "size":
          if type(v[1]) != int:
            self.show_error_msg("Argument %s must be of type int." % (i), TypeError)
            return False
          elif v[1] < 0 or v[1] > 250:
            self.show_error_msg("Argument %s is out of range (must be between 0 and 250)." % (i), ValueError)
            return False
        elif v[0] == "angle":
          if type(v[1]) != float:
            self.show_error_msg("Argument %s must be of type float." % (i), TypeError)
            return False
          elif v[1] < -180.0 or v[1] > 180.0:
            self.show_error_msg("Argument %s is out of range (must be between -180 and 180)." % (i), ValueError)
            return False
        elif v[0] == "boolean":
          if type(v[1]) != int:
            self.show_error_msg("Argument %s must be of type int." % (i), TypeError)
            return False
          elif v[1] < 0 or v[1] > 1:
            self.show_error_msg("Argument %s is out of range (must be between 0 and 1)." % (i), ValueError)
            return False
        elif v[0] == "intrange":
          if type(v[1]) != int:
            self.show_error_msg("Argument %s must be of type int." % (i), TypeError)
            return False
          elif v[1] < v[2] or v[1] > v[3]:
            self.show_error_msg("Argument %s is out of range (must be between %s and %s)." % (i, v[2], v[3]), ValueError)
            return False
        elif v[0] == "floatrange":
          if type(v[1]) != float:
            self.show_error_msg("Argument %s must be of type float." % (i), TypeError)
            return False
          elif v[1] < v[2] or v[1] > v[3]:
            self.show_error_msg("Argument %s is out of range (must be between %s and %s)." % (i, v[2], v[3]), ValueError)
            return False
      return True

  def stringToColor(self, string):
    matchObj = re.match('RGB \((\d+(?:.\d+)?), (\d+(?:.\d+)?), (\d+(?:.\d+)?), (\d+(?:.\d+)?)\)', string)
    if matchObj:
      colorlist = matchObj.group(1, 2, 3, 4)
      return gimpcolor.RGB(float(colorlist[0]), float(colorlist[1]), float(colorlist[2]), float(colorlist[3]))
    else:
      return False

  def writeParasite(self, drawable, fxlayer, *controls):
    dataList = []
    for i in controls:
      if i[0] == "color":
        dataList.append(str(i[1].get_color()))
      elif i[0] == "gradient":
        dataList.append(i[1].get_gradient().encode("string_escape").replace("|", "\\x7c"))
      elif i[0] == "pattern":
        dataList.append(i[1].get_pattern().encode("string_escape").replace("|", "\\x7c"))
      elif i[0] == "intadj":
        dataList.append(str(int(round(i[1].get_value()))))
      elif i[0] == "floatadj":
        dataList.append(str(i[1].get_value()))
      elif i[0] == "combobox":
        dataList.append(str(i[1].get_active()))
      elif i[0] == "modebox":
        dataList.append(str(i[1].get_active()))
      elif i[0] == "check":
        if i[1].get_active():
          dataList.append("1")
        else:
          dataList.append("0")
      elif i[0] == "radio":
        for j, v in enumerate(i[1]):
          if v.get_active():
            dataList.append(str(j))
            break
    data = "|".join(dataList)
    fxlayer.attach_new_parasite(self.shelfkey, 0, data)
    drawable.attach_new_parasite("%s-fxlayer" % (self.shelfkey), 0, fxlayer.name)

  def writeParasiteRaw(self, drawable, fxlayer, *values):
    dataList = []
    for i in values:
      if type(i) == str:
        dataList.append(i.encode("string_escape").replace("|", "\\x7c"))
      else:
        dataList.append(str(i))
    data = "|".join(dataList)
    fxlayer.attach_new_parasite(self.shelfkey, 0, data)
    drawable.attach_new_parasite("%s-fxlayer" % (self.shelfkey), 0, fxlayer.name)

  def readParasite(self, img, drawable, keyname, *keysntypes):
    fxlayername = "%s-fxlayer" % (keyname)
    if fxlayername in pdb.gimp_drawable_parasite_list(drawable)[1]:
      fxlayername = pdb.gimp_drawable_parasite_find(drawable, fxlayername).data
      for i in img.layers:
        if i.name == fxlayername:
          if keyname in pdb.gimp_drawable_parasite_list(i)[1]:
            datalist = pdb.gimp_drawable_parasite_find(i, keyname).data.split("|")
            keys = []
            vals = []
            for j, v in enumerate(datalist):
              keys.append(keysntypes[j][0])
              if keysntypes[j][1] == "color":
                vals.append(self.stringToColor(v))
              elif keysntypes[j][1] == "int":
                vals.append(int(v))
              elif keysntypes[j][1] == "float":
                vals.append(float(v))
              elif keysntypes[j][1] == "string":
                vals.append(v.decode("string_escape"))
            keys.append("oldid")
            vals.append(i)
            data = dict(zip(keys, vals))
            return data
          else:
            return False
    else:
      return False

  def removeOldLayer(self):
    if not hasattr(self, "parasitedata"):
      self.parasitedata = self.readParasite(self.img, self.drawable)
    if self.parasitedata:
      self.img.remove_layer(self.parasitedata["oldid"])

  def makeDialogButtons(self):
    reset_button = gtk.Button("_Reset")
    reset_button.connect("clicked", self.resetbutton)
    reset_button.show()
    if gtk.alternative_dialog_button_order():
      ok_button = self.dialog.add_button(gtk.STOCK_OK, gtk.RESPONSE_OK)
      cancel_button = self.dialog.add_button(gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL)
      self.dialog.action_area.add(reset_button)
    else:
      self.dialog.action_area.add(reset_button)
      cancel_button = self.dialog.add_button(gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL)
      ok_button = self.dialog.add_button(gtk.STOCK_OK, gtk.RESPONSE_OK)
    ok_button.connect("clicked", self.okbutton)

  def getGradientMeasurements(self, drawoffsetx, drawoffsety, gradienttype, centerx, centery, angle, width):
    ang = (angle * -1) * (math.pi / 180.0)
    if gradienttype == 0:
      offset = ((width / 2.0) * math.cos(ang), (width / 2.0) * math.sin(ang))
      gradstart = (centerx - offset[0] - drawoffsetx, centery - offset[1] - drawoffsety)
      gradend = (centerx + offset[0] - drawoffsetx, centery + offset[1] - drawoffsety)
    elif gradienttype >= 1 and gradienttype <= 8:
      offset = ((width / 2.0) * math.cos(ang), (width / 2.0) * math.sin(ang))
      gradstart = (centerx - drawoffsetx, centery - drawoffsety)
      gradend = (centerx + offset[0] - drawoffsetx, centery + offset[1] - drawoffsety)
    else:
      offset = (width * math.cos(ang), width * math.sin(ang))
      gradstart = (centerx - drawoffsetx, centery - drawoffsety)
      gradend = (centerx + offset[0] - drawoffsetx, centery + offset[1] - drawoffsety)
    return { "ang": ang, "offset": offset, "start": gradstart, "end": gradend }

class layerfx_drop_shadow(layerfx_base):
  shelfkey = "layerfx-drop-shadow"

  def __init__(self, runmode, img, drawable, color, opacity, contour, noise, mode, spread, size, offsetangle, offsetdist, knockout, merge):
    self.origMsgHandler = pdb.gimp_message_get_handler()
    pdb.gimp_message_set_handler(ERROR_CONSOLE)
    self.img = img
    self.drawable = drawable
    if runmode == RUN_INTERACTIVE:
      self.showDialog()
    elif runmode == RUN_NONINTERACTIVE:
      if self.validatedata(img, drawable,
        ("color",      color),
        ("percent",    opacity),
        ("contour",    contour),
        ("percent",    noise),
        ("mode",       mode),
        ("percent",    spread),
        ("size",       size),
        ("angle",      offsetangle),
        ("floatrange", offsetdist, 0.0, 30000.0),
        ("boolean",    knockout),
        ("boolean",    merge)
      ):
        self.removeOldLayer()
        fxlayer = self.makeShadow(img, drawable, color, opacity, contour, noise, mode, spread, size, offsetangle, offsetdist, knockout, merge)
        if merge == 0:
          self.writeParasiteRaw(drawable, fxlayer, color, opacity, contour, noise, mode, spread, size, offsetangle, offsetdist, knockout, merge)
        shelf[self.shelfkey] = {
          "color":       color,
          "opacity":     opacity,
          "contour":     contour,
          "noise":       noise,
          "mode":        mode,
          "spread":      spread,
          "size":        size,
          "offsetangle": offsetangle,
          "offsetdist":  offsetdist,
          "knockout":    knockout,
          "merge":       merge
        }
    elif runmode == RUN_WITH_LAST_VALS and shelf.has_key(self.shelfkey) == 1:
      self.removeOldLayer()
      fxlayer = self.makeShadow(
        img,
        drawable,
        shelf[self.shelfkey]["color"],
        shelf[self.shelfkey]["opacity"],
        shelf[self.shelfkey]["contour"],
        shelf[self.shelfkey]["noise"],
        shelf[self.shelfkey]["mode"],
        shelf[self.shelfkey]["spread"],
        shelf[self.shelfkey]["size"],
        shelf[self.shelfkey]["offsetangle"],
        shelf[self.shelfkey]["offsetdist"],
        shelf[self.shelfkey]["knockout"],
        shelf[self.shelfkey]["merge"]
      )
      if shelf[self.shelfkey]["merge"] == 0:
        self.writeParasiteRaw(drawable, fxlayer,
          shelf[self.shelfkey]["color"],
          shelf[self.shelfkey]["opacity"],
          shelf[self.shelfkey]["contour"],
          shelf[self.shelfkey]["noise"],
          shelf[self.shelfkey]["mode"],
          shelf[self.shelfkey]["spread"],
          shelf[self.shelfkey]["size"],
          shelf[self.shelfkey]["offsetangle"],
          shelf[self.shelfkey]["offsetdist"],
          shelf[self.shelfkey]["knockout"],
          shelf[self.shelfkey]["merge"]
        )
    else:
      pdb.gimp_message("unknown runmode")
    pdb.gimp_message_set_handler(self.origMsgHandler)

  def showDialog(self):
    self.dialog = gimpui.Dialog("Drop Shadow", "dropshadowdialog")

    self.parasitedata = self.readParasite(self.img, self.drawable)

    self.table = gtk.Table(9, 5, True)
    self.table.set_homogeneous(True)
    self.table.set_row_spacings(3)
    self.table.set_col_spacings(3)
    self.table.show()

    self.color_label = self.make_label("_Color:")
    self.table.attach(self.color_label, 0, 1, 0, 1)

    self.color_button = gimpui.ColorButton("Shadow Color", 10, 10, gimpcolor.RGB(0, 0, 0, 255))
    if self.parasitedata:
      self.color_button.set_color(self.parasitedata["color"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.color_button.set_color(shelf[self.shelfkey]["color"])
    self.color_label.set_mnemonic_widget(self.color_button)
    self.color_button.show()
    self.table.attach(self.color_button, 1, 2, 0, 1)
    self.color_button.connect("color-changed", self.preview)

    self.mode_label = self.make_label("_Blend Mode:")
    self.table.attach(self.mode_label, 0, 1, 1, 2)

    self.mode_box = self.make_blend_mode_box()
    if self.parasitedata:
      self.mode_box.set_active(self.parasitedata["mode"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.mode_box.set_active(shelf[self.shelfkey]["mode"])
    else:
      self.mode_box.set_active(MULTIPLY_MODE)
    self.mode_label.set_mnemonic_widget(self.mode_box)
    self.mode_box.show()
    self.table.attach(self.mode_box, 1, 5, 1, 2)
    self.mode_box.connect("changed", self.preview)

    self.opacity_label = self.make_label("_Opacity:")
    self.table.attach(self.opacity_label, 0, 1, 2, 3)

    self.opacity_slider = self.make_slider_and_spinner(75.0, 0.0, 100.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.opacity_slider["adj"].set_value(self.parasitedata["opacity"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.opacity_slider["adj"].set_value(shelf[self.shelfkey]["opacity"])
    self.opacity_label.set_mnemonic_widget(self.opacity_slider["spinner"])
    self.table.attach(self.opacity_slider["slider"], 1, 4, 2, 3)
    self.table.attach(self.opacity_slider["spinner"], 4, 5, 2, 3)
    self.opacity_slider["adj"].connect("value-changed", self.preview)

    self.angle_label = self.make_label("_Angle:")
    self.table.attach(self.angle_label, 0, 1, 3, 4)

    self.angle_slider = self.make_slider_and_spinner(120.0, -180.0, 180.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.angle_slider["adj"].set_value(self.parasitedata["offsetangle"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.angle_slider["adj"].set_value(shelf[self.shelfkey]["offsetangle"])
    self.angle_label.set_mnemonic_widget(self.angle_slider["spinner"])
    self.table.attach(self.angle_slider["slider"], 1, 4, 3, 4)
    self.table.attach(self.angle_slider["spinner"], 4, 5, 3, 4)
    self.angle_slider["adj"].connect("value-changed", self.preview)

    self.distance_label = self.make_label("_Distance:")
    self.table.attach(self.distance_label, 0, 1, 4, 5)

    self.distance_spinner = self.make_spinner(5.0, 0.0, 30000.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.distance_spinner["adj"].set_value(self.parasitedata["offsetdist"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.distance_spinner["adj"].set_value(shelf[self.shelfkey]["offsetdist"])
    self.distance_label.set_mnemonic_widget(self.distance_spinner["spinner"])
    self.table.attach(self.distance_spinner["spinner"], 1, 2, 4, 5)
    self.distance_spinner["adj"].connect("value-changed", self.preview)

    self.spread_label = self.make_label("_Spread:")
    self.table.attach(self.spread_label, 0, 1, 5, 6)

    self.spread_slider = self.make_slider_and_spinner(0.0, 0.0, 100.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.spread_slider["adj"].set_value(self.parasitedata["spread"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.spread_slider["adj"].set_value(shelf[self.shelfkey]["spread"])
    self.spread_label.set_mnemonic_widget(self.spread_slider["spinner"])
    self.table.attach(self.spread_slider["slider"], 1, 4, 5, 6)
    self.table.attach(self.spread_slider["spinner"], 4, 5, 5, 6)
    self.spread_slider["adj"].connect("value-changed", self.preview)

    self.size_label = self.make_label("S_ize:")
    self.table.attach(self.size_label, 0, 1, 6, 7)

    self.size_slider = self.make_slider_and_spinner(5, 0, 250, 1, 10, 0)
    if self.parasitedata:
      self.size_slider["adj"].set_value(self.parasitedata["size"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.size_slider["adj"].set_value(shelf[self.shelfkey]["size"])
    self.size_label.set_mnemonic_widget(self.size_slider["spinner"])
    self.table.attach(self.size_slider["slider"], 1, 4, 6, 7)
    self.table.attach(self.size_slider["spinner"], 4, 5, 6, 7)
    self.size_slider["adj"].connect("value-changed", self.preview)

    self.contour_label = self.make_label("Con_tour:")
    self.table.attach(self.contour_label, 0, 1, 7, 8)

    self.contour_box = self.make_contour_box()
    if self.parasitedata:
      self.contour_box.set_active(self.parasitedata["contour"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.contour_box.set_active(shelf[self.shelfkey]["contour"])
    else:
      self.contour_box.set_active(0)
    self.contour_label.set_mnemonic_widget(self.contour_box)
    self.contour_box.show()
    self.table.attach(self.contour_box, 1, 5, 7, 8)
    self.contour_box.connect("changed", self.preview)

    self.noise_label = self.make_label("_Noise:")
    self.table.attach(self.noise_label, 0, 1, 8, 9)

    self.noise_slider = self.make_slider_and_spinner(0.0, 0.0, 100.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.noise_slider["adj"].set_value(self.parasitedata["noise"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.noise_slider["adj"].set_value(shelf[self.shelfkey]["noise"])
    self.noise_label.set_mnemonic_widget(self.noise_slider["spinner"])
    self.table.attach(self.noise_slider["slider"], 1, 4, 8, 9)
    self.table.attach(self.noise_slider["spinner"], 4, 5, 8, 9)
    self.noise_slider["adj"].connect("value-changed", self.preview)

    self.knockout_check = gtk.CheckButton("Layer _knocks out Drop Shadow")
    if self.parasitedata:
      if self.parasitedata["knockout"] == 1:
        self.knockout_check.set_active(True)
    elif shelf.has_key(self.shelfkey) == 1 and shelf[self.shelfkey]["knockout"] == 1:
      self.knockout_check.set_active(True)
    self.knockout_check.show()
    self.knockout_check.connect("toggled", self.preview)

    self.merge_check = gtk.CheckButton("_Merge with layer")
    if self.parasitedata:
      if self.parasitedata["merge"] == 1:
        self.merge_check.set_active(True)
    elif shelf.has_key(self.shelfkey) == 1 and shelf[self.shelfkey]["merge"] == 1:
      self.merge_check.set_active(True)
    self.merge_check.show()
    self.merge_check.connect("toggled", self.preview)

    self.preview_check = gtk.CheckButton("_Preview")
    self.preview_check.show()
    self.preview_check.connect("toggled", self.preview)

    self.dialog.vbox.hbox1 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox1.show()
    self.dialog.vbox.pack_start(self.dialog.vbox.hbox1, True, True, 7)
    self.dialog.vbox.hbox1.pack_start(self.table, True, True, 7)
    self.dialog.vbox.hbox2 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox2.show()
    self.dialog.vbox.add(self.dialog.vbox.hbox2)
    self.dialog.vbox.hbox2.pack_start(self.knockout_check, True, True, 10)
    self.dialog.vbox.hbox3 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox3.show()
    self.dialog.vbox.add(self.dialog.vbox.hbox3)
    self.dialog.vbox.hbox3.pack_start(self.merge_check, True, True, 10)
    self.dialog.vbox.hbox4 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox4.show()
    self.dialog.vbox.add(self.dialog.vbox.hbox4)
    self.dialog.vbox.hbox4.pack_start(self.preview_check, True, True, 10)

    self.makeDialogButtons()

    self.dialog.show()
    self.dialog.run()
    self.removePreviews()

  def okbutton(self, widget):
    if self.layer_exists(self.previewLayer):
      self.img.remove_layer(self.previewLayer)
      self.previewLayer = None
    if pdb.gimp_image_undo_is_enabled(self.img) == 0:
      pdb.gimp_image_undo_thaw(self.img)
    params = {
      "color":       self.color_button.get_color(),
      "opacity":     self.opacity_slider["adj"].get_value(),
      "contour":     self.contour_box.get_active(),
      "noise":       self.noise_slider["adj"].get_value(),
      "mode":        self.mode_box.get_active(),
      "spread":      self.spread_slider["adj"].get_value(),
      "size":        int(round(self.size_slider["adj"].get_value())),
      "offsetangle": self.angle_slider["adj"].get_value(),
      "offsetdist":  self.distance_spinner["adj"].get_value(),
      "knockout":    self.cond(self.knockout_check.get_active()),
      "merge":       self.cond(self.merge_check.get_active())
    }
    self.removeOldLayer()
    fxlayer = self.makeShadow(
      self.img,
      self.drawable,
      params["color"],
      params["opacity"],
      params["contour"],
      params["noise"],
      params["mode"],
      params["spread"],
      params["size"],
      params["offsetangle"],
      params["offsetdist"],
      params["knockout"],
      params["merge"]
    )
    if params["merge"] == 0:
      self.writeParasite(self.drawable, fxlayer,
        ("color",    self.color_button),
        ("floatadj", self.opacity_slider["adj"]),
        ("combobox", self.contour_box),
        ("floatadj", self.noise_slider["adj"]),
        ("modebox",  self.mode_box),
        ("floatadj", self.spread_slider["adj"]),
        ("intadj",   self.size_slider["adj"]),
        ("floatadj", self.angle_slider["adj"]),
        ("floatadj", self.distance_spinner["adj"]),
        ("check",    self.knockout_check),
        ("check",    self.merge_check)
      )
    shelf[self.shelfkey] = params

  def readParasite(self, img, drawable):
    return layerfx_base.readParasite(self, img, drawable,
      layerfx_drop_shadow.shelfkey,
      ("color",       "color"),
      ("opacity",     "float"),
      ("contour",     "int"),
      ("noise",       "float"),
      ("mode",        "int"),
      ("spread",      "float"),
      ("size",        "int"),
      ("offsetangle", "float"),
      ("offsetdist",  "float"),
      ("knockout",    "int"),
      ("merge",       "int")
    )

  def resetbutton(self, widget):
    self.color_button.set_color(gimpcolor.RGB(0, 0, 0, 255))
    self.mode_box.set_active(MULTIPLY_MODE)
    self.opacity_slider["adj"].set_value(75.0)
    self.angle_slider["adj"].set_value(120.0)
    self.distance_spinner["adj"].set_value(5.0)
    self.spread_slider["adj"].set_value(0.0)
    self.size_slider["adj"].set_value(5)
    self.contour_box.set_active(0)
    self.noise_slider["adj"].set_value(0.0)
    self.knockout_check.set_active(False)
    self.merge_check.set_active(False)

  def preview(self, widget):
    if self.layer_exists(self.previewLayer):
      self.img.remove_layer(self.previewLayer)
    if self.preview_check.get_active():
      if pdb.gimp_image_undo_is_enabled(self.img) == 1:
        pdb.gimp_image_undo_freeze(self.img)
      if self.parasitedata:
        self.set_hidden_layer(self.parasitedata["oldid"])
      self.previewLayer = self.makeShadow(
        self.img,
        self.drawable,
        self.color_button.get_color(),
        self.opacity_slider["adj"].get_value(),
        self.contour_box.get_active(),
        self.noise_slider["adj"].get_value(),
        self.mode_box.get_active(),
        self.spread_slider["adj"].get_value(),
        int(round(self.size_slider["adj"].get_value())),
        self.angle_slider["adj"].get_value(),
        self.distance_spinner["adj"].get_value(),
        self.cond(self.knockout_check.get_active()),
        0
      )
    else:
      gimp.displays_flush()

  def makeShadow(self, img, drawable, color, opacity, contour, noise, mode, spread, size, offsetangle, offsetdist, knockout, merge):
    pdb.gimp_image_undo_group_start(img)
    origfgcolor = gimp.get_foreground()
    origselection = pdb.gimp_selection_save(img)
    growamt = int(math.ceil(size / 2.0))
    steps = int(round(size - ((spread / 100.0) * size)))
    lyrgrowamt = int(round(growamt * 1.2))
    shadowlayer = gimp.Layer(img, "%s-dropshadow" % (drawable.name), drawable.width + (lyrgrowamt * 2), drawable.height + (lyrgrowamt * 2), (RGBA_IMAGE, GRAYA_IMAGE)[img.base_type], opacity, mode)
    ang = ((offsetangle + 180) * -1) * (math.pi / 180.0)
    offset = (int(round(offsetdist * math.cos(ang))), int(round(offsetdist * math.sin(ang))))
    self.add_under_layer(shadowlayer, drawable)
    shadowlayer.set_offsets(drawable.offsets[0] + offset[0] - lyrgrowamt, drawable.offsets[1] + offset[1] - lyrgrowamt)
    pdb.gimp_selection_none(img)
    gimp.set_foreground(color)
    pdb.gimp_edit_fill(shadowlayer, FOREGROUND_FILL)
    shadowmask = shadowlayer.create_mask(ADD_BLACK_MASK)
    shadowlayer.add_mask(shadowmask)
    pdb.gimp_selection_layer_alpha(drawable)
    if drawable.mask != None:
      pdb.gimp_selection_combine(drawable.mask, CHANNEL_OP_INTERSECT)
    pdb.gimp_selection_translate(img, offset[0], offset[1])
    alphaSel = pdb.gimp_selection_save(img)
    if (steps > 0):
      self.draw_blurshape(shadowmask, steps, growamt, alphaSel, False)
    else:
      pdb.gimp_selection_grow(img, growamt)
      gimp.set_foreground(255, 255, 255)
      pdb.gimp_edit_fill(shadowmask, FOREGROUND_FILL)
    pdb.gimp_selection_none(img)
    if contour > 0:
      self.apply_contour(shadowmask, HISTOGRAM_VALUE, contour)
      pdb.gimp_selection_load(alphaSel)
      pdb.gimp_selection_grow(img, growamt)
      pdb.gimp_selection_invert(img)
      gimp.set_foreground(0, 0, 0)
      pdb.gimp_edit_fill(shadowmask, FOREGROUND_FILL)
      pdb.gimp_selection_none(img)
    if noise > 0:
      self.apply_noise(drawable, shadowlayer, noise, False)
    if knockout == 1:
      gimp.set_foreground(0, 0, 0)
      pdb.gimp_selection_layer_alpha(drawable)
      pdb.gimp_edit_fill(shadowmask, FOREGROUND_FILL)
    shadowlayer.remove_mask(MASK_APPLY)
    pdb.gimp_selection_none(img)
    if merge == 1:
      origmask = drawable.mask
      layername = drawable.name
      if origmask != None:
        drawable.remove_mask(MASK_APPLY)
      shadowlayer = pdb.gimp_image_merge_down(img, drawable, EXPAND_AS_NECESSARY)
      shadowlayer.name = layername
    else:
      pdb.gimp_image_set_active_layer(img, drawable)
    gimp.set_foreground(origfgcolor)
    pdb.gimp_selection_load(origselection)
    img.remove_channel(alphaSel)
    img.remove_channel(origselection)
    gimp.displays_flush()
    pdb.gimp_image_undo_group_end(img)
    return shadowlayer

class layerfx_inner_shadow(layerfx_base):
  shelfkey = "layerfx-inner-shadow"

  def __init__(self, runmode, img, drawable, color, opacity, contour, noise, mode, source, choke, size, offsetangle, offsetdist, merge):
    self.origMsgHandler = pdb.gimp_message_get_handler()
    pdb.gimp_message_set_handler(ERROR_CONSOLE)
    self.img = img
    self.drawable = drawable
    if runmode == RUN_INTERACTIVE:
      self.showDialog()
    elif runmode == RUN_NONINTERACTIVE:
      if self.validatedata(img, drawable,
        ("color",      color),
        ("percent",    opacity),
        ("contour",    contour),
        ("percent",    noise),
        ("mode",       mode),
        ("boolean",    source),
        ("percent",    choke),
        ("size",       size),
        ("angle",      offsetangle),
        ("floatrange", offsetdist, 0.0, 30000.0),
        ("boolean",    merge)
      ):
        self.removeOldLayer()
        fxlayer = self.makeShadow(img, drawable, color, opacity, contour, noise, mode, source, choke, size, offsetangle, offsetdist, merge)
        if merge == 0:
          self.writeParasiteRaw(drawable, fxlayer, color, opacity, contour, noise, mode, source, choke, size, offsetangle, offsetdist, merge)
        shelf[self.shelfkey] = {
          "color":       color,
          "opacity":     opacity,
          "contour":     contour,
          "noise":       noise,
          "mode":        mode,
          "source":      source,
          "choke":       choke,
          "size":        size,
          "offsetangle": offsetangle,
          "offsetdist":  offsetdist,
          "merge":       merge
        }
    elif runmode == RUN_WITH_LAST_VALS and shelf.has_key(self.shelfkey) == 1:
      self.removeOldLayer()
      fxlayer = self.makeShadow(
        img,
        drawable,
        shelf[self.shelfkey]["color"],
        shelf[self.shelfkey]["opacity"],
        shelf[self.shelfkey]["contour"],
        shelf[self.shelfkey]["noise"],
        shelf[self.shelfkey]["mode"],
        shelf[self.shelfkey]["source"],
        shelf[self.shelfkey]["choke"],
        shelf[self.shelfkey]["size"],
        shelf[self.shelfkey]["offsetangle"],
        shelf[self.shelfkey]["offsetdist"],
        shelf[self.shelfkey]["merge"]
      )
      if shelf[self.shelfkey]["merge"] == 0:
        self.writeParasiteRaw(drawable, fxlayer,
          shelf[self.shelfkey]["color"],
          shelf[self.shelfkey]["opacity"],
          shelf[self.shelfkey]["contour"],
          shelf[self.shelfkey]["noise"],
          shelf[self.shelfkey]["mode"],
          shelf[self.shelfkey]["source"],
          shelf[self.shelfkey]["choke"],
          shelf[self.shelfkey]["size"],
          shelf[self.shelfkey]["offsetangle"],
          shelf[self.shelfkey]["offsetdist"],
          shelf[self.shelfkey]["merge"]
        )
    else:
      pdb.gimp_message("unknown runmode")
    pdb.gimp_message_set_handler(self.origMsgHandler)

  def showDialog(self):
    self.dialog = gimpui.Dialog("Inner Shadow", "innershadowdialog")

    self.parasitedata = self.readParasite(self.img, self.drawable)

    self.table = gtk.Table(10, 5, True)
    self.table.set_homogeneous(True)
    self.table.set_row_spacings(3)
    self.table.set_col_spacings(3)
    self.table.show()

    self.color_label = self.make_label("_Color:")
    self.table.attach(self.color_label, 0, 1, 0, 1)

    self.color_button = gimpui.ColorButton("Shadow Color", 10, 10, gimpcolor.RGB(0, 0, 0, 255))
    if self.parasitedata:
      self.color_button.set_color(self.parasitedata["color"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.color_button.set_color(shelf[self.shelfkey]["color"])
    self.color_label.set_mnemonic_widget(self.color_button)
    self.color_button.show()
    self.table.attach(self.color_button, 1, 2, 0, 1)
    self.color_button.connect("color-changed", self.preview)

    self.mode_label = self.make_label("_Blend Mode:")
    self.table.attach(self.mode_label, 0, 1, 1, 2)

    self.mode_box = self.make_blend_mode_box()
    if self.parasitedata:
      self.mode_box.set_active(self.parasitedata["mode"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.mode_box.set_active(shelf[self.shelfkey]["mode"])
    else:
      self.mode_box.set_active(MULTIPLY_MODE)
    self.mode_label.set_mnemonic_widget(self.mode_box)
    self.mode_box.show()
    self.table.attach(self.mode_box, 1, 5, 1, 2)
    self.mode_box.connect("changed", self.preview)

    self.opacity_label = self.make_label("_Opacity:")
    self.table.attach(self.opacity_label, 0, 1, 2, 3)

    self.opacity_slider = self.make_slider_and_spinner(75.0, 0.0, 100.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.opacity_slider["adj"].set_value(self.parasitedata["opacity"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.opacity_slider["adj"].set_value(shelf[self.shelfkey]["opacity"])
    self.opacity_label.set_mnemonic_widget(self.opacity_slider["spinner"])
    self.table.attach(self.opacity_slider["slider"], 1, 4, 2, 3)
    self.table.attach(self.opacity_slider["spinner"], 4, 5, 2, 3)
    self.opacity_slider["adj"].connect("value-changed", self.preview)

    self.angle_label = self.make_label("_Angle:")
    self.table.attach(self.angle_label, 0, 1, 3, 4)

    self.angle_slider = self.make_slider_and_spinner(120.0, -180.0, 180.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.angle_slider["adj"].set_value(self.parasitedata["offsetangle"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.angle_slider["adj"].set_value(shelf[self.shelfkey]["offsetangle"])
    self.angle_label.set_mnemonic_widget(self.angle_slider["spinner"])
    self.table.attach(self.angle_slider["slider"], 1, 4, 3, 4)
    self.table.attach(self.angle_slider["spinner"], 4, 5, 3, 4)
    self.angle_slider["adj"].connect("value-changed", self.preview)

    self.distance_label = self.make_label("_Distance:")
    self.table.attach(self.distance_label, 0, 1, 4, 5)

    self.distance_spinner = self.make_spinner(5.0, 0.0, 30000.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.distance_spinner["adj"].set_value(self.parasitedata["offsetdist"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.distance_spinner["adj"].set_value(shelf[self.shelfkey]["offsetdist"])
    self.distance_label.set_mnemonic_widget(self.distance_spinner["spinner"])
    self.table.attach(self.distance_spinner["spinner"], 1, 2, 4, 5)
    self.distance_spinner["adj"].connect("value-changed", self.preview)

    self.source_label = self.make_label("Source:")
    self.table.attach(self.source_label, 0, 1, 5, 6)

    self.source_center_radio = gtk.RadioButton(None, "Cent_er", True)
    self.source_edge_radio = gtk.RadioButton(self.source_center_radio, "Ed_ge", True)
    if self.parasitedata:
      if self.parasitedata["source"] == 0:
        self.source_center_radio.set_active(True)
      elif self.parasitedata["source"] == 1:
        self.source_edge_radio.set_active(True)
    elif shelf.has_key(self.shelfkey) == 1:
      if shelf[self.shelfkey]["source"] == 0:
        self.source_center_radio.set_active(True)
      elif shelf[self.shelfkey]["source"] == 1:
        self.source_edge_radio.set_active(True)
    else:
      self.source_edge_radio.set_active(True)
    self.source_center_radio.show()
    self.table.attach(self.source_center_radio, 1, 2, 5, 6)
    self.source_edge_radio.show()
    self.table.attach(self.source_edge_radio, 2, 3, 5, 6)
    self.source_center_radio.connect("toggled", self.preview)
    self.source_edge_radio.connect("toggled", self.preview)

    self.choke_label = self.make_label("C_hoke:")
    self.table.attach(self.choke_label, 0, 1, 6, 7)

    self.choke_slider = self.make_slider_and_spinner(0.0, 0.0, 100.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.choke_slider["adj"].set_value(self.parasitedata["choke"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.choke_slider["adj"].set_value(shelf[self.shelfkey]["choke"])
    self.choke_label.set_mnemonic_widget(self.choke_slider["spinner"])
    self.table.attach(self.choke_slider["slider"], 1, 4, 6, 7)
    self.table.attach(self.choke_slider["spinner"], 4, 5, 6, 7)
    self.choke_slider["adj"].connect("value-changed", self.preview)

    self.size_label = self.make_label("S_ize:")
    self.table.attach(self.size_label, 0, 1, 7, 8)

    self.size_slider = self.make_slider_and_spinner(5, 0, 250, 1, 10, 0)
    if self.parasitedata:
      self.size_slider["adj"].set_value(self.parasitedata["size"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.size_slider["adj"].set_value(shelf[self.shelfkey]["size"])
    self.size_label.set_mnemonic_widget(self.size_slider["spinner"])
    self.table.attach(self.size_slider["slider"], 1, 4, 7, 8)
    self.table.attach(self.size_slider["spinner"], 4, 5, 7, 8)
    self.size_slider["adj"].connect("value-changed", self.preview)

    self.contour_label = self.make_label("Con_tour:")
    self.table.attach(self.contour_label, 0, 1, 8, 9)

    self.contour_box = self.make_contour_box()
    if self.parasitedata:
      self.contour_box.set_active(self.parasitedata["contour"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.contour_box.set_active(shelf[self.shelfkey]["contour"])
    else:
      self.contour_box.set_active(0)
    self.contour_label.set_mnemonic_widget(self.contour_box)
    self.contour_box.show()
    self.table.attach(self.contour_box, 1, 5, 8, 9)
    self.contour_box.connect("changed", self.preview)

    self.noise_label = self.make_label("_Noise:")
    self.table.attach(self.noise_label, 0, 1, 9, 10)

    self.noise_slider = self.make_slider_and_spinner(0.0, 0.0, 100.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.noise_slider["adj"].set_value(self.parasitedata["noise"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.noise_slider["adj"].set_value(shelf[self.shelfkey]["noise"])
    self.noise_label.set_mnemonic_widget(self.noise_slider["spinner"])
    self.table.attach(self.noise_slider["slider"], 1, 4, 9, 10)
    self.table.attach(self.noise_slider["spinner"], 4, 5, 9, 10)
    self.noise_slider["adj"].connect("value-changed", self.preview)

    self.merge_check = gtk.CheckButton("_Merge with layer")
    if self.parasitedata:
      if self.parasitedata["merge"] == 1:
        self.merge_check.set_active(True)
    elif shelf.has_key(self.shelfkey) == 1 and shelf[self.shelfkey]["merge"] == 1:
      self.merge_check.set_active(True)
    self.merge_check.show()
    self.merge_check.connect("toggled", self.preview)

    self.preview_check = gtk.CheckButton("_Preview")
    self.preview_check.show()
    self.preview_check.connect("toggled", self.preview)

    self.dialog.vbox.hbox1 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox1.show()
    self.dialog.vbox.pack_start(self.dialog.vbox.hbox1, True, True, 7)
    self.dialog.vbox.hbox1.pack_start(self.table, True, True, 7)
    self.dialog.vbox.hbox2 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox2.show()
    self.dialog.vbox.add(self.dialog.vbox.hbox2)
    self.dialog.vbox.hbox2.pack_start(self.merge_check, True, True, 10)
    self.dialog.vbox.hbox3 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox3.show()
    self.dialog.vbox.add(self.dialog.vbox.hbox3)
    self.dialog.vbox.hbox3.pack_start(self.preview_check, True, True, 10)

    self.makeDialogButtons()

    self.dialog.show()
    self.dialog.run()
    self.removePreviews()

  def okbutton(self, widget):
    if self.layer_exists(self.previewLayer):
      self.img.remove_layer(self.previewLayer)
      self.previewLayer = None
    self.unset_hidden_layer()
    if pdb.gimp_image_undo_is_enabled(self.img) == 0:
      pdb.gimp_image_undo_thaw(self.img)
    if self.source_center_radio.get_active():
      source = 0
    elif self.source_edge_radio.get_active():
      source = 1
    params = {
      "color":       self.color_button.get_color(),
      "opacity":     self.opacity_slider["adj"].get_value(),
      "contour":     self.contour_box.get_active(),
      "noise":       self.noise_slider["adj"].get_value(),
      "mode":        self.mode_box.get_active(),
      "source":      source,
      "choke":       self.choke_slider["adj"].get_value(),
      "size":        int(round(self.size_slider["adj"].get_value())),
      "offsetangle": self.angle_slider["adj"].get_value(),
      "offsetdist":  self.distance_spinner["adj"].get_value(),
      "merge":       self.cond(self.merge_check.get_active())
    }
    self.removeOldLayer()
    fxlayer = self.makeShadow(
      self.img,
      self.drawable,
      params["color"],
      params["opacity"],
      params["contour"],
      params["noise"],
      params["mode"],
      params["source"],
      params["choke"],
      params["size"],
      params["offsetangle"],
      params["offsetdist"],
      params["merge"]
    )
    if params["merge"] == 0:
      self.writeParasite(self.drawable, fxlayer,
        ("color",    self.color_button),
        ("floatadj", self.opacity_slider["adj"]),
        ("combobox", self.contour_box),
        ("floatadj", self.noise_slider["adj"]),
        ("modebox",  self.mode_box),
        ("radio",    (self.source_center_radio, self.source_edge_radio)),
        ("floatadj", self.choke_slider["adj"]),
        ("intadj",   self.size_slider["adj"]),
        ("floatadj", self.angle_slider["adj"]),
        ("floatadj", self.distance_spinner["adj"]),
        ("check",    self.merge_check),
      )
    shelf[self.shelfkey] = params

  def resetbutton(self, widget):
    self.color_button.set_color(gimpcolor.RGB(0, 0, 0, 255))
    self.mode_box.set_active(MULTIPLY_MODE)
    self.opacity_slider["adj"].set_value(75.0)
    self.angle_slider["adj"].set_value(120.0)
    self.distance_spinner["adj"].set_value(5.0)
    self.source_edge_radio.set_active(True)
    self.choke_slider["adj"].set_value(0.0)
    self.size_slider["adj"].set_value(5)
    self.contour_box.set_active(0)
    self.noise_slider["adj"].set_value(0.0)
    self.merge_check.set_active(False)

  def readParasite(self, img, drawable):
    return layerfx_base.readParasite(self, img, drawable,
      layerfx_inner_shadow.shelfkey,
      ("color",       "color"),
      ("opacity",     "float"),
      ("contour",     "int"),
      ("noise",       "float"),
      ("mode",        "int"),
      ("source",      "int"),
      ("choke",       "float"),
      ("size",        "int"),
      ("offsetangle", "float"),
      ("offsetdist",  "float"),
      ("merge",       "int")
    )

  def preview(self, widget):
    if self.layer_exists(self.previewLayer):
      self.img.remove_layer(self.previewLayer)
    if self.preview_check.get_active():
      if pdb.gimp_image_undo_is_enabled(self.img) == 1:
        pdb.gimp_image_undo_freeze(self.img)
      self.unset_hidden_layer()
      if self.parasitedata:
        self.set_hidden_layer(self.parasitedata["oldid"])
      if self.source_center_radio.get_active():
        source = 0
      elif self.source_edge_radio.get_active():
        source = 1
      if self.merge_check.get_active():
        self.previewLayer = self.drawable.copy()
        self.add_over_layer(self.previewLayer, self.drawable)
        self.set_hidden_layer(self.drawable)
        layer = self.previewLayer
        merge = 1
      else:
        layer = self.drawable
        merge = 0
      self.previewLayer = self.makeShadow(
        self.img,
        layer,
        self.color_button.get_color(),
        self.opacity_slider["adj"].get_value(),
        self.contour_box.get_active(),
        self.noise_slider["adj"].get_value(),
        self.mode_box.get_active(),
        source,
        self.choke_slider["adj"].get_value(),
        int(round(self.size_slider["adj"].get_value())),
        self.angle_slider["adj"].get_value(),
        self.distance_spinner["adj"].get_value(),
        merge
      )
    else:
      gimp.displays_flush()

  def makeShadow(self, img, drawable, color, opacity, contour, noise, mode, source, choke, size, offsetangle, offsetdist, merge):
    pdb.gimp_image_undo_group_start(img)
    origfgcolor = gimp.get_foreground()
    origselection = pdb.gimp_selection_save(img)
    growamt = int(math.ceil(size / 2.0))
    chokeamt = (choke / 100.0) * size
    steps = int(round(size - chokeamt))
    shadowlayer = gimp.Layer(img, "%s-innershadow" % (drawable.name), drawable.width, drawable.height, (RGBA_IMAGE, GRAYA_IMAGE)[img.base_type], opacity, mode)
    ang = ((offsetangle + 180) * -1) * (math.pi / 180.0)
    offset = (int(round(offsetdist * math.cos(ang))), int(round(offsetdist * math.sin(ang))))
    self.add_over_layer(shadowlayer, drawable)
    shadowlayer.set_offsets(drawable.offsets[0], drawable.offsets[1])
    pdb.gimp_selection_none(img)
    gimp.set_foreground(color)
    pdb.gimp_edit_fill(shadowlayer, FOREGROUND_FILL)
    shadowmask = shadowlayer.create_mask(ADD_BLACK_MASK)
    shadowlayer.add_mask(shadowmask)
    pdb.gimp_selection_layer_alpha(drawable)
    if drawable.mask != None:
      pdb.gimp_selection_combine(drawable.mask, CHANNEL_OP_INTERSECT)
    pdb.gimp_selection_translate(img, offset[0], offset[1])
    alphaSel = pdb.gimp_selection_save(img)
    if source == 1:
      pdb.gimp_selection_none(img)
      gimp.set_foreground(255, 255, 255)
      pdb.gimp_edit_fill(shadowmask, FOREGROUND_FILL)
      pdb.gimp_selection_load(alphaSel)
      if steps > 0:
        self.draw_blurshape(shadowmask, steps, growamt - chokeamt, alphaSel, True)
      else:
        pdb.gimp_selection_shrink(img, growamt)
        gimp.set_foreground(0, 0, 0)
        pdb.gimp_edit_fill(shadowmask, FOREGROUND_FILL)
    else:
      if steps > 0:
        self.draw_blurshape(shadowmask, steps, growamt - chokeamt, alphaSel, False)
      else:
        pdb.gimp_selection_shrink(img, growamt)
        gimp.set_foreground(255, 255, 255)
        pdb.gimp_edit_fill(shadowmask, FOREGROUND_FILL)
    pdb.gimp_selection_none(img)
    if contour > 0:
      self.apply_contour(shadowmask, HISTOGRAM_VALUE, contour)
    if merge == 0:
      pdb.gimp_selection_layer_alpha(drawable)
      pdb.gimp_selection_invert(img)
      gimp.set_foreground(0, 0, 0)
      pdb.gimp_edit_fill(shadowmask, FOREGROUND_FILL)
    if noise > 0:
      self.apply_noise(drawable, shadowlayer, noise, False)
    shadowlayer.remove_mask(MASK_APPLY)
    if merge == 1:
      if source == 1:
        origmask = drawable.mask
        layername = drawable.name
        if origmask != None:
          origmask = drawable.mask.copy()
          drawable.remove_mask(MASK_DISCARD)
        alphamask = drawable.create_mask(ADD_ALPHA_TRANSFER_MASK)
        shadowlayer = pdb.gimp_image_merge_down(img, shadowlayer, EXPAND_AS_NECESSARY)
        shadowlayer.name = layername
        shadowlayer.add_mask(alphamask)
        shadowlayer.remove_mask(MASK_APPLY)
        if origmask != None:
          shadowlayer.add_mask(origmask)
      else:
        origmask = drawable.mask
        layername = drawable.name
        if origmask != None:
          origmask = drawable.mask.copy()
          drawable.remove_mask(MASK_DISCARD)
        shadowlayer = pdb.gimp_image_merge_down(img, shadowlayer, EXPAND_AS_NECESSARY)
        shadowlayer.name = layername
        if origmask != None:
          shadowlayer.add_mask(origmask)
    else:
      pdb.gimp_image_set_active_layer(img, drawable)
    gimp.set_foreground(origfgcolor)
    pdb.gimp_selection_load(origselection)
    img.remove_channel(alphaSel)
    img.remove_channel(origselection)
    gimp.displays_flush()
    pdb.gimp_image_undo_group_end(img)
    return shadowlayer

class layerfx_outer_glow(layerfx_base):
  shelfkey = "layerfx-outer-glow"

  def __init__(self, runmode, img, drawable, color, opacity, contour, noise, mode, spread, size, knockout, merge):
    self.origMsgHandler = pdb.gimp_message_get_handler()
    pdb.gimp_message_set_handler(ERROR_CONSOLE)
    self.img = img
    self.drawable = drawable
    if runmode == RUN_INTERACTIVE:
      self.showDialog()
    elif runmode == RUN_NONINTERACTIVE:
      if self.validatedata(img, drawable,
        ("color/gradient", color),
        ("percent",        opacity),
        ("contour",        contour),
        ("percent",        noise),
        ("mode",           mode),
        ("percent",        spread),
        ("size",           size),
        ("boolean",        knockout),
        ("boolean",        merge)
      ):
        self.removeOldLayer()
        fxlayer = self.makeGlow(img, drawable, color, opacity, contour, noise, mode, spread, size, knockout, merge)
        if merge == 0:
          if type(color) == gimpcolor.RGB:
            self.writeParasiteRaw(drawable, fxlayer, 0, color, "FG to BG (RGB)", opacity, contour, noise, mode, spread, size, knockout, merge)
          else:
            self.writeParasiteRaw(drawable, fxlayer, 1, gimpcolor.RGB(255, 255, 190, 255), color, opacity, contour, noise, mode, spread, size, knockout, merge)
        if type(color) == gimpcolor.RGB:
          shelf[self.shelfkey] = {
            "filltype": 0,
            "color":    color,
            "gradient": "FG to BG (RGB)",
            "opacity":  opacity,
            "contour":  contour,
            "noise":    noise,
            "mode":     mode,
            "spread":   spread,
            "size":     size,
            "knockout": knockout,
            "merge":    merge
          }
        else:
          shelf[self.shelfkey] = {
            "filltype": 1,
            "color":    gimpcolor.RGB(255, 255, 190, 255),
            "gradient": color,
            "opacity":  opacity,
            "contour":  contour,
            "noise":    noise,
            "mode":     mode,
            "spread":   spread,
            "size":     size,
            "knockout": knockout,
            "merge":    merge
          }
    elif runmode == RUN_WITH_LAST_VALS and shelf.has_key(self.shelfkey) == 1:
      self.removeOldLayer()
      fxlayer = self.makeGlow(
        img,
        drawable,
        self.cond(shelf[self.shelfkey]["filltype"] == 0, shelf[self.shelfkey]["color"], shelf[self.shelfkey]["gradient"]),
        shelf[self.shelfkey]["opacity"],
        shelf[self.shelfkey]["contour"],
        shelf[self.shelfkey]["noise"],
        shelf[self.shelfkey]["mode"],
        shelf[self.shelfkey]["spread"],
        shelf[self.shelfkey]["size"],
        shelf[self.shelfkey]["knockout"],
        shelf[self.shelfkey]["merge"]
      )
      if shelf[self.shelfkey]["merge"] == 0:
        self.writeParasiteRaw(drawable, fxlayer,
          shelf[self.shelfkey]["filltype"],
          shelf[self.shelfkey]["color"],
          shelf[self.shelfkey]["gradient"],
          shelf[self.shelfkey]["opacity"],
          shelf[self.shelfkey]["contour"],
          shelf[self.shelfkey]["noise"],
          shelf[self.shelfkey]["mode"],
          shelf[self.shelfkey]["spread"],
          shelf[self.shelfkey]["size"],
          shelf[self.shelfkey]["knockout"],
          shelf[self.shelfkey]["merge"]
        )
    else:
      pdb.gimp_message("unknown runmode")
    pdb.gimp_message_set_handler(self.origMsgHandler)

  def showDialog(self):
    self.dialog = gimpui.Dialog("Outer Glow", "outerglowdialog")

    self.parasitedata = self.readParasite(self.img, self.drawable)

    self.table = gtk.Table(7, 5, True)
    self.table.set_homogeneous(True)
    self.table.set_row_spacings(3)
    self.table.set_col_spacings(3)
    self.table.show()

    self.color_label = self.make_label("_Color:")
    self.table.attach(self.color_label, 0, 1, 0, 1)

    self.color_radio = gtk.RadioButton(None, None)
    self.color_radio.set_alignment(1.0, 0.5)
    self.gradient_radio = gtk.RadioButton(self.color_radio, None)
    self.gradient_radio.set_alignment(1.0, 0.5)
    if self.parasitedata:
      if self.parasitedata["filltype"] == 1:
        self.gradient_radio.set_active(True)
      else:
        self.color_radio.set_active(True)
    elif shelf.has_key(self.shelfkey) == 1:
      if shelf[self.shelfkey]["filltype"] == 1:
        self.gradient_radio.set_active(True)
      else:
        self.color_radio.set_active(True)
    else:
      self.color_radio.set_active(True)
    self.color_radio.show()
    self.gradient_radio.show()
    self.color_radio.connect("toggled", self.preview)
    self.gradient_radio.connect("toggled", self.preview)

    self.color_button = gimpui.ColorButton("Glow Color", 80, 0, gimpcolor.RGB(255, 255, 190, 255))
    if self.parasitedata:
      self.color_button.set_color(self.parasitedata["color"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.color_button.set_color(shelf[self.shelfkey]["color"])
    self.color_label.set_mnemonic_widget(self.color_button)
    self.color_button.show()
    self.color_button.connect("color-changed", self.preview)

    self.gradient_button = gimpui.GradientSelector()
    if self.parasitedata:
      self.gradient_button.set_gradient(self.parasitedata["gradient"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.gradient_button.set_gradient(shelf[self.shelfkey]["gradient"])
    self.gradient_button.show()
    self.gradient_button.connect("gradient-set", self.preview)

    self.color_hbox = gtk.HBox(False, 3)
    self.color_hbox.add(self.color_radio)
    self.color_hbox.add(self.color_button)
    self.color_hbox.show()

    self.gradient_hbox = gtk.HBox(False, 3)
    self.gradient_hbox.add(self.gradient_radio)
    self.gradient_hbox.add(self.gradient_button)
    self.gradient_hbox.show()

    self.table.attach(self.color_hbox, 1, 3, 0, 1)
    self.table.attach(self.gradient_hbox, 3, 5, 0, 1)

    self.mode_label = self.make_label("_Blend Mode:")
    self.table.attach(self.mode_label, 0, 1, 1, 2)

    self.mode_box = self.make_blend_mode_box()
    if self.parasitedata:
      self.mode_box.set_active(self.parasitedata["mode"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.mode_box.set_active(shelf[self.shelfkey]["mode"])
    else:
      self.mode_box.set_active(SCREEN_MODE)
    self.mode_label.set_mnemonic_widget(self.mode_box)
    self.mode_box.show()
    self.table.attach(self.mode_box, 1, 5, 1, 2)
    self.mode_box.connect("changed", self.preview)

    self.opacity_label = self.make_label("_Opacity:")
    self.table.attach(self.opacity_label, 0, 1, 2, 3)

    self.opacity_slider = self.make_slider_and_spinner(75.0, 0.0, 100.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.opacity_slider["adj"].set_value(self.parasitedata["opacity"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.opacity_slider["adj"].set_value(shelf[self.shelfkey]["opacity"])
    self.opacity_label.set_mnemonic_widget(self.opacity_slider["spinner"])
    self.table.attach(self.opacity_slider["slider"], 1, 4, 2, 3)
    self.table.attach(self.opacity_slider["spinner"], 4, 5, 2, 3)
    self.opacity_slider["adj"].connect("value-changed", self.preview)

    self.spread_label = self.make_label("_Spread:")
    self.table.attach(self.spread_label, 0, 1, 3, 4)

    self.spread_slider = self.make_slider_and_spinner(0.0, 0.0, 100.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.spread_slider["adj"].set_value(self.parasitedata["spread"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.spread_slider["adj"].set_value(shelf[self.shelfkey]["spread"])
    self.spread_label.set_mnemonic_widget(self.spread_slider["spinner"])
    self.table.attach(self.spread_slider["slider"], 1, 4, 3, 4)
    self.table.attach(self.spread_slider["spinner"], 4, 5, 3, 4)
    self.spread_slider["adj"].connect("value-changed", self.preview)

    self.size_label = self.make_label("S_ize:")
    self.table.attach(self.size_label, 0, 1, 4, 5)

    self.size_slider = self.make_slider_and_spinner(5, 0, 250, 1, 10, 0)
    if self.parasitedata:
      self.size_slider["adj"].set_value(self.parasitedata["size"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.size_slider["adj"].set_value(shelf[self.shelfkey]["size"])
    self.size_label.set_mnemonic_widget(self.size_slider["spinner"])
    self.table.attach(self.size_slider["slider"], 1, 4, 4, 5)
    self.table.attach(self.size_slider["spinner"], 4, 5, 4, 5)
    self.size_slider["adj"].connect("value-changed", self.preview)

    self.contour_label = self.make_label("Con_tour:")
    self.table.attach(self.contour_label, 0, 1, 5, 6)

    self.contour_box = self.make_contour_box()
    if self.parasitedata:
      self.contour_box.set_active(self.parasitedata["contour"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.contour_box.set_active(shelf[self.shelfkey]["contour"])
    else:
      self.contour_box.set_active(0)
    self.contour_label.set_mnemonic_widget(self.contour_box)
    self.contour_box.show()
    self.table.attach(self.contour_box, 1, 5, 5, 6)
    self.contour_box.connect("changed", self.preview)

    self.noise_label = self.make_label("_Noise:")
    self.table.attach(self.noise_label, 0, 1, 6, 7)

    self.noise_slider = self.make_slider_and_spinner(0.0, 0.0, 100.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.noise_slider["adj"].set_value(self.parasitedata["noise"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.noise_slider["adj"].set_value(shelf[self.shelfkey]["noise"])
    self.noise_label.set_mnemonic_widget(self.noise_slider["spinner"])
    self.table.attach(self.noise_slider["slider"], 1, 4, 6, 7)
    self.table.attach(self.noise_slider["spinner"], 4, 5, 6, 7)
    self.noise_slider["adj"].connect("value-changed", self.preview)

    self.knockout_check = gtk.CheckButton("Layer _knocks out Outer Glow")
    if self.parasitedata:
      if self.parasitedata["knockout"] == 1:
        self.knockout_check.set_active(True)
    elif shelf.has_key(self.shelfkey) == 1 and shelf[self.shelfkey]["knockout"] == 1:
      self.knockout_check.set_active(True)
    self.knockout_check.show()
    self.knockout_check.connect("toggled", self.preview)

    self.merge_check = gtk.CheckButton("_Merge with layer")
    if self.parasitedata:
      if self.parasitedata["merge"] == 1:
        self.merge_check.set_active(True)
    elif shelf.has_key(self.shelfkey) == 1 and shelf[self.shelfkey]["merge"] == 1:
      self.merge_check.set_active(True)
    self.merge_check.show()
    self.merge_check.connect("toggled", self.preview)

    self.preview_check = gtk.CheckButton("_Preview")
    self.preview_check.show()
    self.preview_check.connect("toggled", self.preview)

    self.dialog.vbox.hbox1 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox1.show()
    self.dialog.vbox.pack_start(self.dialog.vbox.hbox1, True, True, 7)
    self.dialog.vbox.hbox1.pack_start(self.table, True, True, 7)
    self.dialog.vbox.hbox2 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox2.show()
    self.dialog.vbox.add(self.dialog.vbox.hbox2)
    self.dialog.vbox.hbox2.pack_start(self.knockout_check, True, True, 10)
    self.dialog.vbox.hbox3 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox3.show()
    self.dialog.vbox.add(self.dialog.vbox.hbox3)
    self.dialog.vbox.hbox3.pack_start(self.merge_check, True, True, 10)
    self.dialog.vbox.hbox4 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox4.show()
    self.dialog.vbox.add(self.dialog.vbox.hbox4)
    self.dialog.vbox.hbox4.pack_start(self.preview_check, True, True, 10)

    self.makeDialogButtons()

    self.dialog.show()
    self.dialog.run()
    self.removePreviews()

  def okbutton(self, widget):
    if self.layer_exists(self.previewLayer):
      self.img.remove_layer(self.previewLayer)
      self.previewLayer = None
    if pdb.gimp_image_undo_is_enabled(self.img) == 0:
      pdb.gimp_image_undo_thaw(self.img)
    if self.color_radio.get_active():
      filltype = 0
    elif self.gradient_radio.get_active():
      filltype = 1
    params = {
      "filltype": filltype,
      "color":    self.color_button.get_color(),
      "gradient": self.gradient_button.get_gradient(),
      "opacity":  self.opacity_slider["adj"].get_value(),
      "contour":  self.contour_box.get_active(),
      "noise":    self.noise_slider["adj"].get_value(),
      "mode":     self.mode_box.get_active(),
      "spread":   self.spread_slider["adj"].get_value(),
      "size":     int(round(self.size_slider["adj"].get_value())),
      "knockout": self.cond(self.knockout_check.get_active()),
      "merge":    self.cond(self.merge_check.get_active())
    }
    self.removeOldLayer()
    fxlayer = self.makeGlow(
      self.img,
      self.drawable,
      self.cond(filltype == 0, params["color"], params["gradient"]),
      params["opacity"],
      params["contour"],
      params["noise"],
      params["mode"],
      params["spread"],
      params["size"],
      params["knockout"],
      params["merge"]
    )
    if params["merge"] == 0:
      self.writeParasite(self.drawable, fxlayer,
        ("radio",    (self.color_radio, self.gradient_radio)),
        ("color",    self.color_button),
        ("gradient", self.gradient_button),
        ("floatadj", self.opacity_slider["adj"]),
        ("combobox", self.contour_box),
        ("floatadj", self.noise_slider["adj"]),
        ("modebox",  self.mode_box),
        ("floatadj", self.spread_slider["adj"]),
        ("intadj",   self.size_slider["adj"]),
        ("check",    self.knockout_check),
        ("check",    self.merge_check)
      )
    shelf[self.shelfkey] = params

  def resetbutton(self, widget):
    self.color_radio.set_active(True)
    self.color_button.set_color(gimpcolor.RGB(255, 255, 190, 255))
    self.gradient_button.set_gradient("FG to BG (RGB)")
    self.mode_box.set_active(SCREEN_MODE)
    self.opacity_slider["adj"].set_value(75.0)
    self.spread_slider["adj"].set_value(0.0)
    self.size_slider["adj"].set_value(5)
    self.contour_box.set_active(0)
    self.noise_slider["adj"].set_value(0.0)
    self.knockout_check.set_active(False)
    self.merge_check.set_active(False)

  def readParasite(self, img, drawable):
    return layerfx_base.readParasite(self, img, drawable,
      layerfx_outer_glow.shelfkey,
      ("filltype", "int"),
      ("color",    "color"),
      ("gradient", "string"),
      ("opacity",  "float"),
      ("contour",  "int"),
      ("noise",    "float"),
      ("mode",     "int"),
      ("spread",   "float"),
      ("size",     "int"),
      ("knockout", "int"),
      ("merge",    "int")
    )

  def preview(self, widget, *extra):
    if self.layer_exists(self.previewLayer):
      self.img.remove_layer(self.previewLayer)
    if self.preview_check.get_active():
      if pdb.gimp_image_undo_is_enabled(self.img) == 1:
        pdb.gimp_image_undo_freeze(self.img)
      if self.parasitedata:
        self.set_hidden_layer(self.parasitedata["oldid"])
      if self.color_radio.get_active():
        filltype = 0
      elif self.gradient_radio.get_active():
        filltype = 1
      self.previewLayer = self.makeGlow(
        self.img,
        self.drawable,
        self.cond(filltype == 0, self.color_button.get_color(), self.gradient_button.get_gradient()),
        self.opacity_slider["adj"].get_value(),
        self.contour_box.get_active(),
        self.noise_slider["adj"].get_value(),
        self.mode_box.get_active(),
        self.spread_slider["adj"].get_value(),
        int(round(self.size_slider["adj"].get_value())),
        self.cond(self.knockout_check.get_active()),
        0
      )
    else:
      gimp.displays_flush()

  def makeGlow(self, img, drawable, color, opacity, contour, noise, mode, spread, size, knockout, merge):
    pdb.gimp_image_undo_group_start(img)
    origfgcolor = gimp.get_foreground()
    origgradient = pdb.gimp_context_get_gradient()
    origselection = pdb.gimp_selection_save(img)
    growamt = (spread / 100.0) * size
    steps = int(round(size - growamt))
    lyrgrowamt = int(round(size * 1.2))
    glowlayer = gimp.Layer(img, "%s-outerglow" % (drawable.name), drawable.width + (lyrgrowamt * 2), drawable.height + (lyrgrowamt * 2), (RGBA_IMAGE, GRAYA_IMAGE)[img.base_type], opacity, mode)
    self.add_under_layer(glowlayer, drawable)
    glowlayer.set_offsets(drawable.offsets[0] - lyrgrowamt, drawable.offsets[1] - lyrgrowamt)
    pdb.gimp_selection_none(img)
    if type(color) == gimpcolor.RGB:
      gimp.set_foreground(color)
      pdb.gimp_edit_fill(glowlayer, FOREGROUND_FILL)
      glowmask = glowlayer.create_mask(ADD_BLACK_MASK)
      glowlayer.add_mask(glowmask)
    else:
      gimp.set_foreground(0, 0, 0)
      pdb.gimp_edit_fill(glowlayer, FOREGROUND_FILL)
      glowmask = glowlayer
    pdb.gimp_selection_layer_alpha(drawable)
    if drawable.mask != None:
      pdb.gimp_selection_combine(drawable.mask, CHANNEL_OP_INTERSECT)
    alphaSel = pdb.gimp_selection_save(img)
    if steps > 0:
      self.draw_blurshape(glowmask, steps, size, alphaSel, False)
    else:
      pdb.gimp_selection_grow(img, growamt)
      gimp.set_foreground(255, 255, 255)
      pdb.gimp_edit_fill(glowmask, FOREGROUND_FILL)
    pdb.gimp_selection_none(img)
    if contour > 0:
      self.apply_contour(glowmask, HISTOGRAM_VALUE, contour)
      pdb.gimp_selection_load(alphaSel)
      pdb.gimp_selection_grow(img, size)
      pdb.gimp_selection_invert(img)
      gimp.set_foreground(0, 0, 0)
      pdb.gimp_edit_fill(glowmask, FOREGROUND_FILL)
      pdb.gimp_selection_none(img)
    if noise > 0:
      self.apply_noise(drawable, glowlayer, noise, type(color) != gimpcolor.RGB)
    if knockout == 1 and type(color) == gimpcolor.RGB:
      gimp.set_foreground(0, 0, 0)
      pdb.gimp_selection_load(alphaSel)
      pdb.gimp_edit_fill(glowmask, FOREGROUND_FILL)
    if type(color) != gimpcolor.RGB:
      gimp.set_foreground(origfgcolor)
      pdb.gimp_context_set_gradient(color)
      pdb.gimp_selection_none(img)
      pdb.gimp_invert(glowlayer)
      pdb.plug_in_gradmap(img, glowlayer)
      if glowlayer.mask != None:
        glowlayer.remove_mask(MASK_APPLY)
      glowlayer.add_mask(glowlayer.create_mask(ADD_BLACK_MASK))
      pdb.gimp_selection_all(img)
      gimp.set_foreground(255, 255, 255)
      pdb.gimp_edit_fill(glowlayer.mask, FOREGROUND_FILL)
      glowlayer.remove_mask(MASK_APPLY)
      pdb.gimp_context_set_gradient(origgradient)
      pdb.gimp_selection_load(alphaSel)
      if knockout == 1:
        pdb.gimp_edit_clear(glowlayer)
      pdb.gimp_selection_grow(img, size)
      pdb.gimp_selection_invert(img)
      pdb.gimp_edit_clear(glowlayer)
    else:
      glowlayer.remove_mask(MASK_APPLY)
    pdb.gimp_selection_none(img)
    if merge == 1:
      origmask = drawable.mask
      layername = drawable.name
      if origmask != None:
        drawable.remove_mask(MASK_APPLY)
      glowlayer = pdb.gimp_image_merge_down(img, drawable, EXPAND_AS_NECESSARY)
      glowlayer.name = layername
    else:
      pdb.gimp_image_set_active_layer(img, drawable)
    gimp.set_foreground(origfgcolor)
    pdb.gimp_selection_load(origselection)
    img.remove_channel(alphaSel)
    img.remove_channel(origselection)
    gimp.displays_flush()
    pdb.gimp_image_undo_group_end(img)
    return glowlayer

class layerfx_inner_glow(layerfx_base):
  shelfkey = "layerfx-inner-glow"

  def __init__(self, runmode, img, drawable, color, opacity, contour, noise, mode, source, choke, size, merge):
    self.origMsgHandler = pdb.gimp_message_get_handler()
    pdb.gimp_message_set_handler(ERROR_CONSOLE)
    self.img = img
    self.drawable = drawable
    if runmode == RUN_INTERACTIVE:
      self.showDialog()
    elif runmode == RUN_NONINTERACTIVE:
      if self.validatedata(img, drawable,
        ("color/gradient", color),
        ("percent",        opacity),
        ("contour",        contour),
        ("percent",        noise),
        ("mode",           mode),
        ("boolean",        source),
        ("percent",        choke),
        ("size",           size),
        ("boolean",        merge)
      ):
        self.removeOldLayer()
        fxlayer = self.makeGlow(img, drawable, color, opacity, contour, noise, mode, source, choke, size, merge)
        if merge == 0:
          if type(color) == gimpcolor.RGB:
            self.writeParasiteRaw(drawable, fxlayer, 0, color, "FG to BG (RGB)", opacity, contour, noise, mode, source, choke, size, merge)
          else:
            self.writeParasiteRaw(drawable, fxlayer, 1, gimpcolor.RGB(255, 255, 190, 255), color, opacity, contour, noise, mode, source, choke, size, merge)
        if type(color) == gimpcolor.RGB:
          shelf[self.shelfkey] = {
            "filltype": 0,
            "color":    color,
            "gradient": "FG to BG (RGB)",
            "opacity":  opacity,
            "contour":  contour,
            "noise":    noise,
            "mode":     mode,
            "source":   source,
            "choke":    choke,
            "size":     size,
            "merge":    merge
          }
        else:
          shelf[self.shelfkey] = {
            "filltype": 1,
            "color":    gimpcolor.RGB(255, 255, 190, 255),
            "gradient": color,
            "opacity":  opacity,
            "contour":  contour,
            "noise":    noise,
            "mode":     mode,
            "source":   source,
            "choke":    choke,
            "size":     size,
            "merge":    merge
          }
    elif runmode == RUN_WITH_LAST_VALS and shelf.has_key(self.shelfkey) == 1:
      self.removeOldLayer()
      fxlayer = self.makeGlow(
        img,
        drawable,
        self.cond(shelf[self.shelfkey]["filltype"] == 0, shelf[self.shelfkey]["color"], shelf[self.shelfkey]["gradient"]),
        shelf[self.shelfkey]["opacity"],
        shelf[self.shelfkey]["contour"],
        shelf[self.shelfkey]["noise"],
        shelf[self.shelfkey]["mode"],
        shelf[self.shelfkey]["source"],
        shelf[self.shelfkey]["choke"],
        shelf[self.shelfkey]["size"],
        shelf[self.shelfkey]["merge"]
      )
      if shelf[self.shelfkey]["merge"] == 0:
        self.writeParasiteRaw(drawable, fxlayer,
          shelf[self.shelfkey]["filltype"],
          shelf[self.shelfkey]["color"],
          shelf[self.shelfkey]["gradient"],
          shelf[self.shelfkey]["opacity"],
          shelf[self.shelfkey]["contour"],
          shelf[self.shelfkey]["noise"],
          shelf[self.shelfkey]["mode"],
          shelf[self.shelfkey]["source"],
          shelf[self.shelfkey]["choke"],
          shelf[self.shelfkey]["size"],
          shelf[self.shelfkey]["merge"]
        )
    else:
      pdb.gimp_message("unknown runmode")
    pdb.gimp_message_set_handler(self.origMsgHandler)

  def showDialog(self):
    self.dialog = gimpui.Dialog("Inner Glow", "innerglowdialog")

    self.parasitedata = self.readParasite(self.img, self.drawable)

    self.table = gtk.Table(8, 5, True)
    self.table.set_homogeneous(True)
    self.table.set_row_spacings(3)
    self.table.set_col_spacings(3)
    self.table.show()

    self.color_label = self.make_label("_Color:")
    self.table.attach(self.color_label, 0, 1, 0, 1)

    self.color_radio = gtk.RadioButton(None, None)
    self.color_radio.set_alignment(1.0, 0.5)
    self.gradient_radio = gtk.RadioButton(self.color_radio, None)
    self.gradient_radio.set_alignment(1.0, 0.5)
    if self.parasitedata:
      if self.parasitedata["filltype"] == 1:
        self.gradient_radio.set_active(True)
      else:
        self.color_radio.set_active(True)
    elif shelf.has_key(self.shelfkey) == 1:
      if shelf[self.shelfkey]["filltype"] == 1:
        self.gradient_radio.set_active(True)
      else:
        self.color_radio.set_active(True)
    else:
      self.color_radio.set_active(True)
    self.color_radio.show()
    self.gradient_radio.show()
    self.color_radio.connect("toggled", self.preview)
    self.gradient_radio.connect("toggled", self.preview)

    self.color_button = gimpui.ColorButton("Glow Color", 80, 0, gimpcolor.RGB(255, 255, 190, 255))
    if self.parasitedata:
      self.color_button.set_color(self.parasitedata["color"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.color_button.set_color(shelf[self.shelfkey]["color"])
    self.color_label.set_mnemonic_widget(self.color_button)
    self.color_button.show()
    self.color_button.connect("color-changed", self.preview)

    self.gradient_button = gimpui.GradientSelector()
    if self.parasitedata:
      self.gradient_button.set_gradient(self.parasitedata["gradient"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.gradient_button.set_gradient(shelf[self.shelfkey]["gradient"])
    self.gradient_button.show()
    self.gradient_button.connect("gradient-set", self.preview)

    self.color_hbox = gtk.HBox(False, 3)
    self.color_hbox.add(self.color_radio)
    self.color_hbox.add(self.color_button)
    self.color_hbox.show()

    self.gradient_hbox = gtk.HBox(False, 3)
    self.gradient_hbox.add(self.gradient_radio)
    self.gradient_hbox.add(self.gradient_button)
    self.gradient_hbox.show()

    self.table.attach(self.color_hbox, 1, 3, 0, 1)
    self.table.attach(self.gradient_hbox, 3, 5, 0, 1)

    self.mode_label = self.make_label("_Blend Mode:")
    self.table.attach(self.mode_label, 0, 1, 1, 2)

    self.mode_box = self.make_blend_mode_box()
    if self.parasitedata:
      self.mode_box.set_active(self.parasitedata["mode"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.mode_box.set_active(shelf[self.shelfkey]["mode"])
    else:
      self.mode_box.set_active(SCREEN_MODE)
    self.mode_label.set_mnemonic_widget(self.mode_box)
    self.mode_box.show()
    self.table.attach(self.mode_box, 1, 5, 1, 2)
    self.mode_box.connect("changed", self.preview)

    self.opacity_label = self.make_label("_Opacity:")
    self.table.attach(self.opacity_label, 0, 1, 2, 3)

    self.opacity_slider = self.make_slider_and_spinner(75.0, 0.0, 100.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.opacity_slider["adj"].set_value(self.parasitedata["opacity"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.opacity_slider["adj"].set_value(shelf[self.shelfkey]["opacity"])
    self.opacity_label.set_mnemonic_widget(self.opacity_slider["spinner"])
    self.table.attach(self.opacity_slider["slider"], 1, 4, 2, 3)
    self.table.attach(self.opacity_slider["spinner"], 4, 5, 2, 3)
    self.opacity_slider["adj"].connect("value-changed", self.preview)

    self.source_label = self.make_label("Source:")
    self.table.attach(self.source_label, 0, 1, 3, 4)

    self.source_center_radio = gtk.RadioButton(None, "Cent_er", True)
    self.source_edge_radio = gtk.RadioButton(self.source_center_radio, "Ed_ge", True)
    if self.parasitedata:
      if self.parasitedata["source"] == 0:
        self.source_center_radio.set_active(True)
      elif self.parasitedata["source"] == 1:
        self.source_edge_radio.set_active(True)
    elif shelf.has_key(self.shelfkey) == 1:
      if shelf[self.shelfkey]["source"] == 0:
        self.source_center_radio.set_active(True)
      elif shelf[self.shelfkey]["source"] == 1:
        self.source_edge_radio.set_active(True)
    else:
      self.source_edge_radio.set_active(True)
    self.source_center_radio.show()
    self.table.attach(self.source_center_radio, 1, 2, 3, 4)
    self.source_edge_radio.show()
    self.table.attach(self.source_edge_radio, 2, 3, 3, 4)
    self.source_center_radio.connect("toggled", self.preview)
    self.source_edge_radio.connect("toggled", self.preview)

    self.choke_label = self.make_label("C_hoke:")
    self.table.attach(self.choke_label, 0, 1, 4, 5)

    self.choke_slider = self.make_slider_and_spinner(0.0, 0.0, 100.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.choke_slider["adj"].set_value(self.parasitedata["choke"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.choke_slider["adj"].set_value(shelf[self.shelfkey]["choke"])
    self.choke_label.set_mnemonic_widget(self.choke_slider["spinner"])
    self.table.attach(self.choke_slider["slider"], 1, 4, 4, 5)
    self.table.attach(self.choke_slider["spinner"], 4, 5, 4, 5)
    self.choke_slider["adj"].connect("value-changed", self.preview)

    self.size_label = self.make_label("S_ize:")
    self.table.attach(self.size_label, 0, 1, 5, 6)

    self.size_slider = self.make_slider_and_spinner(5, 0, 250, 1, 10, 0)
    if self.parasitedata:
      self.size_slider["adj"].set_value(self.parasitedata["size"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.size_slider["adj"].set_value(shelf[self.shelfkey]["size"])
    self.size_label.set_mnemonic_widget(self.size_slider["spinner"])
    self.table.attach(self.size_slider["slider"], 1, 4, 5, 6)
    self.table.attach(self.size_slider["spinner"], 4, 5, 5, 6)
    self.size_slider["adj"].connect("value-changed", self.preview)

    self.contour_label = self.make_label("Con_tour:")
    self.table.attach(self.contour_label, 0, 1, 6, 7)

    self.contour_box = self.make_contour_box()
    if self.parasitedata:
      self.contour_box.set_active(self.parasitedata["contour"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.contour_box.set_active(shelf[self.shelfkey]["contour"])
    else:
      self.contour_box.set_active(0)
    self.contour_label.set_mnemonic_widget(self.contour_box)
    self.contour_box.show()
    self.table.attach(self.contour_box, 1, 5, 6, 7)
    self.contour_box.connect("changed", self.preview)

    self.noise_label = self.make_label("_Noise:")
    self.table.attach(self.noise_label, 0, 1, 7, 8)

    self.noise_slider = self.make_slider_and_spinner(0.0, 0.0, 100.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.noise_slider["adj"].set_value(self.parasitedata["noise"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.noise_slider["adj"].set_value(shelf[self.shelfkey]["noise"])
    self.noise_label.set_mnemonic_widget(self.noise_slider["spinner"])
    self.table.attach(self.noise_slider["slider"], 1, 4, 7, 8)
    self.table.attach(self.noise_slider["spinner"], 4, 5, 7, 8)
    self.noise_slider["adj"].connect("value-changed", self.preview)

    self.merge_check = gtk.CheckButton("_Merge with layer")
    if self.parasitedata:
      if self.parasitedata["merge"] == 1:
        self.merge_check.set_active(True)
    elif shelf.has_key(self.shelfkey) == 1 and shelf[self.shelfkey]["merge"] == 1:
      self.merge_check.set_active(True)
    self.merge_check.show()
    self.merge_check.connect("toggled", self.preview)

    self.preview_check = gtk.CheckButton("_Preview")
    self.preview_check.show()
    self.preview_check.connect("toggled", self.preview)

    self.dialog.vbox.hbox1 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox1.show()
    self.dialog.vbox.pack_start(self.dialog.vbox.hbox1, True, True, 7)
    self.dialog.vbox.hbox1.pack_start(self.table, True, True, 7)
    self.dialog.vbox.hbox2 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox2.show()
    self.dialog.vbox.add(self.dialog.vbox.hbox2)
    self.dialog.vbox.hbox2.pack_start(self.merge_check, True, True, 10)
    self.dialog.vbox.hbox3 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox3.show()
    self.dialog.vbox.add(self.dialog.vbox.hbox3)
    self.dialog.vbox.hbox3.pack_start(self.preview_check, True, True, 10)

    self.makeDialogButtons()

    self.dialog.show()
    self.dialog.run()
    self.removePreviews()

  def okbutton(self, widget):
    if self.layer_exists(self.previewLayer):
      self.img.remove_layer(self.previewLayer)
      self.previewLayer = None
    self.unset_hidden_layer()
    if pdb.gimp_image_undo_is_enabled(self.img) == 0:
      pdb.gimp_image_undo_thaw(self.img)
    if self.color_radio.get_active():
      filltype = 0
    elif self.gradient_radio.get_active():
      filltype = 1
    if self.source_center_radio.get_active():
      source = 0
    elif self.source_edge_radio.get_active():
      source = 1
    params = {
      "filltype": filltype,
      "color":    self.color_button.get_color(),
      "gradient": self.gradient_button.get_gradient(),
      "opacity":  self.opacity_slider["adj"].get_value(),
      "contour":  self.contour_box.get_active(),
      "noise":    self.noise_slider["adj"].get_value(),
      "mode":     self.mode_box.get_active(),
      "source":   source,
      "choke":    self.choke_slider["adj"].get_value(),
      "size":     int(round(self.size_slider["adj"].get_value())),
      "merge":    self.cond(self.merge_check.get_active())
    }
    if self.parasitedata:
      self.img.remove_layer(self.parasitedata["oldid"])
    fxlayer = self.makeGlow(
      self.img,
      self.drawable,
      self.cond(filltype == 0, params["color"], params["gradient"]),
      params["opacity"],
      params["contour"],
      params["noise"],
      params["mode"],
      params["source"],
      params["choke"],
      params["size"],
      params["merge"]
    )
    if params["merge"] == 0:
      self.writeParasite(self.drawable, fxlayer,
        ("radio",    (self.color_radio, self.gradient_radio)),
        ("color",    self.color_button),
        ("gradient", self.gradient_button),
        ("floatadj", self.opacity_slider["adj"]),
        ("combobox", self.contour_box),
        ("floatadj", self.noise_slider["adj"]),
        ("modebox",  self.mode_box),
        ("radio",    (self.source_center_radio, self.source_edge_radio)),
        ("floatadj", self.choke_slider["adj"]),
        ("intadj",   self.size_slider["adj"]),
        ("check",    self.merge_check),
      )
    shelf[self.shelfkey] = params

  def resetbutton(self, widget):
    self.color_radio.set_active(True)
    self.color_button.set_color(gimpcolor.RGB(255, 255, 190, 255))
    self.gradient_button.set_gradient("FG to BG (RGB)")
    self.mode_box.set_active(SCREEN_MODE)
    self.opacity_slider["adj"].set_value(75.0)
    self.source_edge_radio.set_active(True)
    self.choke_slider["adj"].set_value(0.0)
    self.size_slider["adj"].set_value(5)
    self.contour_box.set_active(0)
    self.noise_slider["adj"].set_value(0.0)
    self.merge_check.set_active(False)

  def readParasite(self, img, drawable):
    return layerfx_base.readParasite(self, img, drawable,
      layerfx_inner_glow.shelfkey,
      ("filltype", "int"),
      ("color",    "color"),
      ("gradient", "string"),
      ("opacity",  "float"),
      ("contour",  "int"),
      ("noise",    "float"),
      ("mode",     "int"),
      ("source",   "int"),
      ("choke",    "float"),
      ("size",     "int"),
      ("merge",    "int")
    )

  def preview(self, widget, *extra):
    if self.layer_exists(self.previewLayer):
      self.img.remove_layer(self.previewLayer)
    if self.preview_check.get_active():
      self.unset_hidden_layer()
      if pdb.gimp_image_undo_is_enabled(self.img) == 1:
        pdb.gimp_image_undo_freeze(self.img)
      if self.parasitedata:
        self.set_hidden_layer(self.parasitedata["oldid"])
      if self.color_radio.get_active():
        filltype = 0
      elif self.gradient_radio.get_active():
        filltype = 1
      if self.source_center_radio.get_active():
        source = 0
      elif self.source_edge_radio.get_active():
        source = 1
      if self.merge_check.get_active():
        self.previewLayer = self.drawable.copy()
        self.add_over_layer(self.previewLayer, self.drawable)
        self.set_hidden_layer(self.drawable)
        layer = self.previewLayer
        merge = 1
      else:
        layer = self.drawable
        merge = 0
      self.previewLayer = self.makeGlow(
        self.img,
        layer,
        self.cond(filltype == 0, self.color_button.get_color(), self.gradient_button.get_gradient()),
        self.opacity_slider["adj"].get_value(),
        self.contour_box.get_active(),
        self.noise_slider["adj"].get_value(),
        self.mode_box.get_active(),
        source,
        self.choke_slider["adj"].get_value(),
        int(round(self.size_slider["adj"].get_value())),
        merge
      )
    else:
      gimp.displays_flush()

  def makeGlow(self, img, drawable, color, opacity, contour, noise, mode, source, choke, size, merge):
    pdb.gimp_image_undo_group_start(img)
    origfgcolor = gimp.get_foreground()
    origgradient = pdb.gimp_context_get_gradient()
    origselection = pdb.gimp_selection_save(img)
    chokeamt = (choke / 100.0) * size
    steps = int(round(size - chokeamt))
    glowlayer = gimp.Layer(img, "%s-innerglow" % (drawable.name), drawable.width, drawable.height, (RGBA_IMAGE, GRAYA_IMAGE)[img.base_type], opacity, mode)
    self.add_over_layer(glowlayer, drawable)
    glowlayer.set_offsets(drawable.offsets[0], drawable.offsets[1])
    pdb.gimp_selection_none(img)
    if type(color) == gimpcolor.RGB:
      gimp.set_foreground(color)
      pdb.gimp_edit_fill(glowlayer, FOREGROUND_FILL)
      glowmask = glowlayer.create_mask(ADD_BLACK_MASK)
      glowlayer.add_mask(glowmask)
    else:
      if source == 0:
        gimp.set_foreground(0, 0, 0)
      else:
        gimp.set_foreground(255, 255, 255)
      pdb.gimp_edit_fill(glowlayer, FOREGROUND_FILL)
      glowmask = glowlayer
    pdb.gimp_selection_layer_alpha(drawable)
    if drawable.mask != None:
      pdb.gimp_selection_combine(drawable.mask, CHANNEL_OP_INTERSECT)
    alphaSel = pdb.gimp_selection_save(img)
    if source == 1:
      pdb.gimp_selection_none(img)
      gimp.set_foreground(255, 255, 255)
      pdb.gimp_edit_fill(glowmask, FOREGROUND_FILL)
      pdb.gimp_selection_load(alphaSel)
      if steps > 0:
        self.draw_blurshape(glowmask, steps, (chokeamt * -1) - 1, alphaSel, True)
      else:
        pdb.gimp_selection_shrink(img, chokeamt)
        gimp.set_foreground(0, 0, 0)
        pdb.gimp_edit_fill(glowmask, FOREGROUND_FILL)
    else:
      if steps > 0:
        self.draw_blurshape(glowmask, steps, chokeamt * -1, alphaSel, False)
      else:
        pdb.gimp_selection_shrink(img, chokeamt)
        gimp.set_foreground(255, 255, 255)
        pdb.gimp_edit_fill(glowmask, FOREGROUND_FILL)
    pdb.gimp_selection_none(img)
    if contour > 0:
      self.apply_contour(glowmask, HISTOGRAM_VALUE, contour)
    if type(color) == gimpcolor.RGB and source == 1 and merge == 0:
      pdb.gimp_selection_load(alphaSel)
      pdb.gimp_selection_invert(img)
      gimp.set_foreground(0, 0, 0)
      pdb.gimp_edit_fill(glowmask, FOREGROUND_FILL)
    if noise > 0:
      self.apply_noise(drawable, glowlayer, noise, type(color) != gimpcolor.RGB)
    if type(color) != gimpcolor.RGB:
      gimp.set_foreground(origfgcolor)
      pdb.gimp_context_set_gradient(color)
      pdb.gimp_selection_none(img)
      pdb.gimp_invert(glowlayer)
      pdb.plug_in_gradmap(img, glowlayer)
      pdb.gimp_context_set_gradient(origgradient)
      pdb.gimp_selection_load(alphaSel)
      if merge == 0:
        pdb.gimp_selection_invert(img)
        pdb.gimp_edit_clear(glowlayer)
        pdb.gimp_selection_invert(img)
      pdb.gimp_selection_shrink(img, size)
      pdb.gimp_edit_clear(glowlayer)
      if glowlayer.mask != None:
        glowlayer.remove_mask(MASK_APPLY)
    else:
      glowlayer.remove_mask(MASK_APPLY)
    if merge == 1:
      origmask = drawable.mask
      layername = drawable.name
      if origmask != None:
        origmask = drawable.mask.copy()
        drawable.remove_mask(MASK_DISCARD)
      if source == 1 or type(color) != gimpcolor.RGB:
        alphamask = drawable.create_mask(ADD_ALPHA_TRANSFER_MASK)
      glowlayer = pdb.gimp_image_merge_down(img, glowlayer, EXPAND_AS_NECESSARY)
      glowlayer.name = layername
      if source == 1 or type(color) != gimpcolor.RGB:
        glowlayer.add_mask(alphamask)
        glowlayer.remove_mask(MASK_APPLY)
      if origmask != None:
        glowlayer.add_mask(origmask)
    else:
      pdb.gimp_image_set_active_layer(img, drawable)
    gimp.set_foreground(origfgcolor)
    pdb.gimp_selection_load(origselection)
    img.remove_channel(alphaSel)
    img.remove_channel(origselection)
    gimp.displays_flush()
    pdb.gimp_image_undo_group_end(img)
    return glowlayer

class layerfx_bevel_emboss(layerfx_base):
  shelfkey = "layerfx-bevel-emboss"

  def __init__(self, runmode, img, drawable, style, depth, direction, size, soften, angle, altitude, glosscontour, highlightcolor, highlightmode, highlightopacity, shadowcolor, shadowmode, shadowopacity, surfacecontour, use_texture, pattern, scale, tex_depth, invert, merge):
    self.origMsgHandler = pdb.gimp_message_get_handler()
    pdb.gimp_message_set_handler(ERROR_CONSOLE)
    self.img = img
    self.drawable = drawable
    if runmode == RUN_INTERACTIVE:
      self.showDialog()
    elif runmode == RUN_NONINTERACTIVE:
      if self.validatedata(img, drawable,
        ("intrange",   style, 0, 3),
        ("intrange",   depth, 1, 65),
        ("boolean",    direction),
        ("size",       size),
        ("intrange",   soften, 0, 16),
        ("angle",      angle),
        ("floatrange", altitude, 0.0, 90.0),
        ("contour",    glosscontour),
        ("color",      highlightcolor),
        ("mode",       highlightmode),
        ("percent",    highlightopacity),
        ("color",      shadowcolor),
        ("mode",       shadowmode),
        ("percent",    shadowopacity),
        ("contour",    surfacecontour),
        ("boolean",    use_texture),
        ("pattern",    pattern),
        ("floatrange", scale, 1.0, 1000.0),
        ("floatrange", tex_depth, -1000.0, 1000.0),
        ("boolean",    invert),
        ("boolean",    merge)
      ):
        self.removeOldLayer()
        fxlayer = self.makeBevel(img, drawable, style, depth, direction, size, soften, angle, altitude, glosscontour, highlightcolor, highlightmode, highlightopacity, shadowcolor, shadowmode, shadowopacity, surfacecontour, use_texture, pattern, scale, tex_depth, invert, merge)
        if merge == 0:
          self.writeParasiteRaw(drawable, fxlayer, style, depth, direction, size, soften, angle, altitude, glosscontour, highlightcolor, highlightmode, highlightopacity, shadowcolor, shadowmode, shadowopacity, surfacecontour, use_texture, pattern, scale, tex_depth, invert, merge)
        shelf[self.shelfkey] = {
          "style":            style,
          "depth":            depth,
          "direction":        direction,
          "size":             size,
          "soften":           soften,
          "angle":            angle,
          "altitude":         altitude,
          "glosscontour":     glosscontour,
          "highlightcolor":   highlightcolor,
          "highlightmode":    highlightmode,
          "highlightopacity": highlightopacity,
          "shadowcolor":      shadowcolor,
          "shadowmode":       shadowmode,
          "shadowopacity":    shadowopacity,
          "surfacecontour":   surfacecontour,
          "use_texture":      use_texture,
          "pattern":          pattern,
          "scale":            scale,
          "tex_depth":        tex_depth,
          "invert":           invert,
          "merge":            merge
        }
    elif runmode == RUN_WITH_LAST_VALS and shelf.has_key(self.shelfkey) == 1:
      self.removeOldLayer()
      fxlayer = self.makeBevel(
        img,
        drawable,
        shelf[self.shelfkey]["style"],
        shelf[self.shelfkey]["depth"],
        shelf[self.shelfkey]["direction"],
        shelf[self.shelfkey]["size"],
        shelf[self.shelfkey]["soften"],
        shelf[self.shelfkey]["angle"],
        shelf[self.shelfkey]["altitude"],
        shelf[self.shelfkey]["glosscontour"],
        shelf[self.shelfkey]["highlightcolor"],
        shelf[self.shelfkey]["highlightmode"],
        shelf[self.shelfkey]["highlightopacity"],
        shelf[self.shelfkey]["shadowcolor"],
        shelf[self.shelfkey]["shadowmode"],
        shelf[self.shelfkey]["shadowopacity"],
        shelf[self.shelfkey]["surfacecontour"],
        shelf[self.shelfkey]["use_texture"],
        shelf[self.shelfkey]["pattern"],
        shelf[self.shelfkey]["scale"],
        shelf[self.shelfkey]["tex_depth"],
        shelf[self.shelfkey]["invert"],
        shelf[self.shelfkey]["merge"]
      )
      if shelf[self.shelfkey]["merge"] == 0:
        self.writeParasiteRaw(drawable, fxlayer,
          shelf[self.shelfkey]["style"],
          shelf[self.shelfkey]["depth"],
          shelf[self.shelfkey]["direction"],
          shelf[self.shelfkey]["size"],
          shelf[self.shelfkey]["soften"],
          shelf[self.shelfkey]["angle"],
          shelf[self.shelfkey]["altitude"],
          shelf[self.shelfkey]["glosscontour"],
          shelf[self.shelfkey]["highlightcolor"],
          shelf[self.shelfkey]["highlightmode"],
          shelf[self.shelfkey]["highlightopacity"],
          shelf[self.shelfkey]["shadowcolor"],
          shelf[self.shelfkey]["shadowmode"],
          shelf[self.shelfkey]["shadowopacity"],
          shelf[self.shelfkey]["surfacecontour"],
          shelf[self.shelfkey]["use_texture"],
          shelf[self.shelfkey]["pattern"],
          shelf[self.shelfkey]["scale"],
          shelf[self.shelfkey]["tex_depth"],
          shelf[self.shelfkey]["invert"],
          shelf[self.shelfkey]["merge"]
        )
    else:
      pdb.gimp_message("unknown runmode")
    pdb.gimp_message_set_handler(self.origMsgHandler)

  def showDialog(self):
    self.dialog = gimpui.Dialog("Bevel and Emboss", "bevelembossdialog")

    self.parasitedata = self.readParasite(self.img, self.drawable)

    self.structframe = gimpui.Frame("Structure")
    self.structframe.show()

    self.structtable = gtk.Table(6, 6, True)
    self.structtable.set_homogeneous(True)
    self.structtable.set_row_spacings(3)
    self.structtable.set_col_spacings(3)
    self.structtable.show()
    self.structframe.add(self.structtable)

    self.style_label = self.make_label("S_tyle:")
    self.structtable.attach(self.style_label, 0, 2, 0, 1)

    self.style_box = self.make_combo_box("Outer Bevel", "Inner Bevel", "Emboss", "Pillow Emboss")
    if self.parasitedata:
      self.style_box.set_active(self.parasitedata["style"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.style_box.set_active(shelf[self.shelfkey]["style"])
    else:
      self.style_box.set_active(0)
    self.style_box.show()
    self.style_label.set_mnemonic_widget(self.style_box)
    self.structtable.attach(self.style_box, 2, 6, 0, 1)
    self.style_box.connect("changed", self.preview)

    self.depth_label = self.make_label("_Depth:")
    self.structtable.attach(self.depth_label, 0, 2, 1, 2)

    self.depth_slider = self.make_slider_and_spinner(3, 1, 65, 1, 10, 0)
    if self.parasitedata:
      self.depth_slider["adj"].set_value(self.parasitedata["depth"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.depth_slider["adj"].set_value(shelf[self.shelfkey]["depth"])
    self.depth_label.set_mnemonic_widget(self.depth_slider["spinner"])
    self.structtable.attach(self.depth_slider["slider"], 2, 5, 1, 2)
    self.structtable.attach(self.depth_slider["spinner"], 5, 6, 1, 2)
    self.depth_slider["adj"].connect("value-changed", self.preview)

    self.direction_label = self.make_label("Direction:")
    self.structtable.attach(self.direction_label, 0, 2, 2, 3)

    self.direction_up_radio = gtk.RadioButton(None, "Up", True)
    self.direction_down_radio = gtk.RadioButton(self.direction_up_radio, "Down", True)
    if self.parasitedata:
      if self.parasitedata["direction"] == 0:
        self.direction_up_radio.set_active(True)
      elif self.parasitedata["direction"] == 1:
        self.direction_down_radio.set_active(True)
    elif shelf.has_key(self.shelfkey) == 1:
      if shelf[self.shelfkey]["direction"] == 0:
        self.direction_up_radio.set_active(True)
      elif shelf[self.shelfkey]["direction"] == 1:
        self.direction_down_radio.set_active(True)
    else:
      self.direction_up_radio.set_active(True)
    self.direction_up_radio.show()
    self.structtable.attach(self.direction_up_radio, 2, 3, 2, 3)
    self.direction_down_radio.show()
    self.structtable.attach(self.direction_down_radio, 3, 4, 2, 3)
    self.direction_up_radio.connect("toggled", self.preview)
    self.direction_down_radio.connect("toggled", self.preview)

    self.size_label = self.make_label("S_ize:")
    self.structtable.attach(self.size_label, 0, 2, 3, 4)

    self.size_slider = self.make_slider_and_spinner(5, 0, 250, 1, 10, 0)
    if self.parasitedata:
      self.size_slider["adj"].set_value(self.parasitedata["size"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.size_slider["adj"].set_value(shelf[self.shelfkey]["size"])
    self.size_label.set_mnemonic_widget(self.size_slider["spinner"])
    self.structtable.attach(self.size_slider["slider"], 2, 5, 3, 4)
    self.structtable.attach(self.size_slider["spinner"], 5, 6, 3, 4)
    self.size_slider["adj"].connect("value-changed", self.preview)

    self.soften_label = self.make_label("So_ften:")
    self.structtable.attach(self.soften_label, 0, 2, 4, 5)

    self.soften_slider = self.make_slider_and_spinner(0, 0, 16, 1, 2, 0)
    if self.parasitedata:
      self.soften_slider["adj"].set_value(self.parasitedata["soften"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.soften_slider["adj"].set_value(shelf[self.shelfkey]["soften"])
    self.soften_label.set_mnemonic_widget(self.soften_slider["spinner"])
    self.structtable.attach(self.soften_slider["slider"], 2, 5, 4, 5)
    self.structtable.attach(self.soften_slider["spinner"], 5, 6, 4, 5)
    self.soften_slider["adj"].connect("value-changed", self.preview)

    self.surfacecontour_label = self.make_label("Surface Con_tour:")
    self.structtable.attach(self.surfacecontour_label, 0, 2, 5, 6)

    self.surfacecontour_box = self.make_contour_box()
    if self.parasitedata:
      self.surfacecontour_box.set_active(self.parasitedata["surfacecontour"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.surfacecontour_box.set_active(shelf[self.shelfkey]["surfacecontour"])
    else:
      self.surfacecontour_box.set_active(0)
    self.surfacecontour_label.set_mnemonic_widget(self.surfacecontour_box)
    self.surfacecontour_box.show()
    self.structtable.attach(self.surfacecontour_box, 2, 6, 5, 6)
    self.surfacecontour_box.connect("changed", self.preview)

    self.shadeframe = gimpui.Frame("Shading")
    self.shadeframe.show()

    self.shadetable = gtk.Table(7, 6, True)
    self.shadetable.set_homogeneous(True)
    self.shadetable.set_row_spacings(3)
    self.shadetable.set_col_spacings(3)
    self.shadetable.show()
    self.shadeframe.add(self.shadetable)

    self.angle_label = self.make_label("_Angle:")
    self.shadetable.attach(self.angle_label, 0, 2, 0, 1)

    self.angle_slider = self.make_slider_and_spinner(120.0, -180.0, 180.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.angle_slider["adj"].set_value(self.parasitedata["angle"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.angle_slider["adj"].set_value(shelf[self.shelfkey]["angle"])
    self.angle_label.set_mnemonic_widget(self.angle_slider["spinner"])
    self.shadetable.attach(self.angle_slider["slider"], 2, 5, 0, 1)
    self.shadetable.attach(self.angle_slider["spinner"], 5, 6, 0, 1)
    self.angle_slider["adj"].connect("value-changed", self.preview)

    self.altitude_label = self.make_label("_Altitude:")
    self.shadetable.attach(self.altitude_label, 0, 2, 1, 2)

    self.altitude_slider = self.make_slider_and_spinner(30.0, 0.0, 90.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.altitude_slider["adj"].set_value(self.parasitedata["altitude"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.altitude_slider["adj"].set_value(shelf[self.shelfkey]["altitude"])
    self.altitude_label.set_mnemonic_widget(self.altitude_slider["spinner"])
    self.shadetable.attach(self.altitude_slider["slider"], 2, 5, 1, 2)
    self.shadetable.attach(self.altitude_slider["spinner"], 5, 6, 1, 2)
    self.altitude_slider["adj"].connect("value-changed", self.preview)

    self.glosscontour_label = self.make_label("Gloss Con_tour:")
    self.shadetable.attach(self.glosscontour_label, 0, 2, 2, 3)

    self.glosscontour_box = self.make_contour_box()
    if self.parasitedata:
      self.glosscontour_box.set_active(self.parasitedata["glosscontour"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.glosscontour_box.set_active(shelf[self.shelfkey]["glosscontour"])
    else:
      self.glosscontour_box.set_active(0)
    self.glosscontour_label.set_mnemonic_widget(self.glosscontour_box)
    self.glosscontour_box.show()
    self.shadetable.attach(self.glosscontour_box, 2, 6, 2, 3)
    self.glosscontour_box.connect("changed", self.preview)

    self.highlightmode_label = self.make_label("Highlight Mode:")
    self.shadetable.attach(self.highlightmode_label, 0, 2, 3, 4)

    self.highlightmode_box = self.make_blend_mode_box()
    if self.parasitedata:
      self.highlightmode_box.set_active(self.parasitedata["highlightmode"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.highlightmode_box.set_active(shelf[self.shelfkey]["highlightmode"])
    else:
      self.highlightmode_box.set_active(SCREEN_MODE)
    self.highlightmode_label.set_mnemonic_widget(self.highlightmode_box)
    self.highlightmode_box.show()
    self.shadetable.attach(self.highlightmode_box, 2, 4, 3, 4)
    self.highlightmode_box.connect("changed", self.preview)

    self.highlightcolor_label = self.make_label("Color:")
    self.shadetable.attach(self.highlightcolor_label, 4, 5, 3, 4)

    self.highlightcolor_button = gimpui.ColorButton("Highlight Color", 10, 10, gimpcolor.RGB(255, 255, 255, 255))
    if self.parasitedata:
      self.highlightcolor_button.set_color(self.parasitedata["highlightcolor"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.highlightcolor_button.set_color(shelf[self.shelfkey]["highlightcolor"])
    self.highlightcolor_label.set_mnemonic_widget(self.highlightcolor_button)
    self.highlightcolor_button.show()
    self.shadetable.attach(self.highlightcolor_button, 5, 6, 3, 4)
    self.highlightcolor_button.connect("color-changed", self.preview)

    self.highlightopacity_label = self.make_label("Highlight Opacity:")
    self.shadetable.attach(self.highlightopacity_label, 0, 2, 4, 5)

    self.highlightopacity_slider = self.make_slider_and_spinner(75.0, 0.0, 100.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.highlightopacity_slider["adj"].set_value(self.parasitedata["highlightopacity"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.highlightopacity_slider["adj"].set_value(shelf[self.shelfkey]["highlightopacity"])
    self.highlightopacity_label.set_mnemonic_widget(self.highlightopacity_slider["spinner"])
    self.shadetable.attach(self.highlightopacity_slider["slider"], 2, 5, 4, 5)
    self.shadetable.attach(self.highlightopacity_slider["spinner"], 5, 6, 4, 5)
    self.highlightopacity_slider["adj"].connect("value-changed", self.preview)

    self.shadowmode_label = self.make_label("Shadow Mode:")
    self.shadetable.attach(self.shadowmode_label, 0, 2, 5, 6)

    self.shadowmode_box = self.make_blend_mode_box()
    if self.parasitedata:
      self.shadowmode_box.set_active(self.parasitedata["shadowmode"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.shadowmode_box.set_active(shelf[self.shelfkey]["shadowmode"])
    else:
      self.shadowmode_box.set_active(MULTIPLY_MODE)
    self.shadowmode_label.set_mnemonic_widget(self.shadowmode_box)
    self.shadowmode_box.show()
    self.shadetable.attach(self.shadowmode_box, 2, 4, 5, 6)
    self.shadowmode_box.connect("changed", self.preview)

    self.shadowcolor_label = self.make_label("Color:")
    self.shadetable.attach(self.shadowcolor_label, 4, 5, 5, 6)

    self.shadowcolor_button = gimpui.ColorButton("Shadow Color", 10, 10, gimpcolor.RGB(0, 0, 0, 255))
    if self.parasitedata:
      self.shadowcolor_button.set_color(self.parasitedata["shadowcolor"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.shadowcolor_button.set_color(shelf[self.shelfkey]["shadowcolor"])
    self.shadowcolor_label.set_mnemonic_widget(self.shadowcolor_button)
    self.shadowcolor_button.show()
    self.shadetable.attach(self.shadowcolor_button, 5, 6, 5, 6)
    self.shadowcolor_button.connect("color-changed", self.preview)

    self.shadowopacity_label = self.make_label("Shadow Opacity:")
    self.shadetable.attach(self.shadowopacity_label, 0, 2, 6, 7)

    self.shadowopacity_slider = self.make_slider_and_spinner(75.0, 0.0, 100.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.shadowopacity_slider["adj"].set_value(self.parasitedata["shadowopacity"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.shadowopacity_slider["adj"].set_value(shelf[self.shelfkey]["shadowopacity"])
    self.shadowopacity_label.set_mnemonic_widget(self.shadowopacity_slider["spinner"])
    self.shadetable.attach(self.shadowopacity_slider["slider"], 2, 5, 6, 7)
    self.shadetable.attach(self.shadowopacity_slider["spinner"], 5, 6, 6, 7)
    self.shadowopacity_slider["adj"].connect("value-changed", self.preview)

    self.textureframe = gimpui.Frame("Texture")
    self.textureframe.show()

    self.texturetable = gtk.Table(4, 6, True)
    self.texturetable.set_homogeneous(True)
    self.texturetable.set_row_spacings(3)
    self.texturetable.set_col_spacings(3)
    self.texturetable.show()
    self.textureframe.add(self.texturetable)

    self.use_texture_check = gtk.CheckButton("Use Texture")
    if self.parasitedata:
      if self.parasitedata["use_texture"] == 1:
        self.use_texture_check.set_active(True)
    elif shelf.has_key(self.shelfkey) == 1 and shelf[self.shelfkey]["use_texture"] == 1:
      self.use_texture_check.set_active(True)
    self.use_texture_check.show()
    self.texturetable.attach(self.use_texture_check, 1, 3, 0, 1)
    self.use_texture_check.connect("toggled", self.preview)

    self.pattern_label = self.make_label("_Pattern:")
    self.texturetable.attach(self.pattern_label, 0, 1, 1, 2)

    self.pattern_hbox = gtk.HBox(False, 15)
    self.pattern_hbox.show()
    self.texturetable.attach(self.pattern_hbox, 1, 4, 1, 2)

    self.pattern_button = gimpui.PatternSelector()
    if self.parasitedata:
      self.pattern_button.set_pattern(self.parasitedata["pattern"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.pattern_button.set_pattern(shelf[self.shelfkey]["pattern"])
    self.pattern_label.set_mnemonic_widget(self.pattern_button)
    self.pattern_button.show()
    self.pattern_hbox.add(self.pattern_button)
    self.pattern_button.connect("pattern-set", self.preview)

    self.invert_check = gtk.CheckButton("_Invert")
    if self.parasitedata:
      if self.parasitedata["invert"] == 1:
        self.invert_check.set_active(True)
    elif shelf.has_key(self.shelfkey) == 1 and shelf[self.shelfkey]["invert"] == 1:
      self.invert_check.set_active(True)
    self.invert_check.show()
    self.pattern_hbox.add(self.invert_check)
    self.invert_check.connect("toggled", self.preview)

    self.scale_label = self.make_label("Scale:")
    self.texturetable.attach(self.scale_label, 0, 1, 2, 3)

    self.scale_slider = self.make_slider_and_spinner(100.0, 1.0, 1000.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.scale_slider["adj"].set_value(self.parasitedata["scale"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.scale_slider["adj"].set_value(shelf[self.shelfkey]["scale"])
    self.scale_label.set_mnemonic_widget(self.scale_slider["spinner"])
    self.texturetable.attach(self.scale_slider["slider"], 1, 5, 2, 3)
    self.texturetable.attach(self.scale_slider["spinner"], 5, 6, 2, 3)
    self.scale_slider["adj"].connect("value-changed", self.preview)

    self.tex_depth_label = self.make_label("Depth:")
    self.texturetable.attach(self.tex_depth_label, 0, 1, 3, 4)

    self.tex_depth_slider = self.make_slider_and_spinner(100.0, -1000.0, 1000.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.tex_depth_slider["adj"].set_value(self.parasitedata["tex_depth"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.tex_depth_slider["adj"].set_value(shelf[self.shelfkey]["tex_depth"])
    self.tex_depth_label.set_mnemonic_widget(self.tex_depth_slider["spinner"])
    self.texturetable.attach(self.tex_depth_slider["slider"], 1, 5, 3, 4)
    self.texturetable.attach(self.tex_depth_slider["spinner"], 5, 6, 3, 4)
    self.tex_depth_slider["adj"].connect("value-changed", self.preview)

    self.merge_check = gtk.CheckButton("_Merge with layer")
    if self.parasitedata:
      if self.parasitedata["merge"] == 1:
        self.merge_check.set_active(True)
    elif shelf.has_key(self.shelfkey) == 1 and shelf[self.shelfkey]["merge"] == 1:
      self.merge_check.set_active(True)
    self.merge_check.show()
    self.merge_check.connect("toggled", self.preview)

    self.preview_check = gtk.CheckButton("_Preview")
    self.preview_check.show()
    self.preview_check.connect("toggled", self.preview)

    self.dialog.vbox.hbox1 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox1.show()
    self.dialog.vbox.pack_start(self.dialog.vbox.hbox1, True, True, 7)
    self.dialog.vbox.hbox1.pack_start(self.structframe, True, True, 7)
    self.dialog.vbox.hbox2 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox2.show()
    self.dialog.vbox.add(self.dialog.vbox.hbox2)
    self.dialog.vbox.hbox2.pack_start(self.shadeframe, True, True, 7)
    self.dialog.vbox.hbox3 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox3.show()
    self.dialog.vbox.add(self.dialog.vbox.hbox3)
    self.dialog.vbox.hbox3.pack_start(self.textureframe, True, True, 7)
    self.dialog.vbox.hbox4 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox4.show()
    self.dialog.vbox.add(self.dialog.vbox.hbox4)
    self.dialog.vbox.hbox4.pack_start(self.merge_check, True, True, 10)
    self.dialog.vbox.hbox5 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox5.show()
    self.dialog.vbox.add(self.dialog.vbox.hbox5)
    self.dialog.vbox.hbox5.pack_start(self.preview_check, True, True, 10)

    self.makeDialogButtons()

    self.dialog.show()
    self.dialog.run()
    self.removePreviews()

  def okbutton(self, widget):
    if self.previewLayer != None:
      if type(self.previewLayer) == tuple:
        if self.layer_exists(self.previewLayer[0]):
          self.img.remove_layer(self.previewLayer[0])
        if self.layer_exists(self.previewLayer[1]):
          self.img.remove_layer(self.previewLayer[1])
      else:
        if self.layer_exists(self.previewLayer):
          self.img.remove_layer(self.previewLayer)
      self.previewLayer = None
    self.unset_hidden_layer()
    if pdb.gimp_image_undo_is_enabled(self.img) == 0:
      pdb.gimp_image_undo_thaw(self.img)
    if self.direction_up_radio.get_active():
      direction = 0
    elif self.direction_down_radio.get_active():
      direction = 1
    params = {
      "style":            self.style_box.get_active(),
      "depth":            int(round(self.depth_slider["adj"].get_value())),
      "direction":        direction,
      "size":             int(round(self.size_slider["adj"].get_value())),
      "soften":           int(round(self.soften_slider["adj"].get_value())),
      "angle":            self.angle_slider["adj"].get_value(),
      "altitude":         self.altitude_slider["adj"].get_value(),
      "glosscontour":     self.glosscontour_box.get_active(),
      "highlightcolor":   self.highlightcolor_button.get_color(),
      "highlightmode":    self.highlightmode_box.get_active(),
      "highlightopacity": self.highlightopacity_slider["adj"].get_value(),
      "shadowcolor":      self.shadowcolor_button.get_color(),
      "shadowmode":       self.shadowmode_box.get_active(),
      "shadowopacity":    self.shadowopacity_slider["adj"].get_value(),
      "surfacecontour":   self.surfacecontour_box.get_active(),
      "use_texture":      self.cond(self.use_texture_check.get_active()),
      "pattern":          self.pattern_button.get_pattern(),
      "scale":            self.scale_slider["adj"].get_value(),
      "tex_depth":        self.tex_depth_slider["adj"].get_value(),
      "invert":           self.cond(self.invert_check.get_active()),
      "merge":            self.cond(self.merge_check.get_active())
    }
    if self.parasitedata:
      self.img.remove_layer(self.parasitedata["oldid"])
      self.img.remove_layer(self.parasitedata["oldid2"])
    fxlayer = self.makeBevel(
      self.img,
      self.drawable,
      params["style"],
      params["depth"],
      params["direction"],
      params["size"],
      params["soften"],
      params["angle"],
      params["altitude"],
      params["glosscontour"],
      params["highlightcolor"],
      params["highlightmode"],
      params["highlightopacity"],
      params["shadowcolor"],
      params["shadowmode"],
      params["shadowopacity"],
      params["surfacecontour"],
      params["use_texture"],
      params["pattern"],
      params["scale"],
      params["tex_depth"],
      params["invert"],
      params["merge"]
    )
    if params["merge"] == 0:
      self.writeParasite(self.drawable, fxlayer,
        ("combobox", self.style_box),
        ("intadj",   self.depth_slider["adj"]),
        ("radio",    (self.direction_up_radio, self.direction_down_radio)),
        ("intadj",   self.size_slider["adj"]),
        ("intadj",   self.soften_slider["adj"]),
        ("floatadj", self.angle_slider["adj"]),
        ("floatadj", self.altitude_slider["adj"]),
        ("combobox", self.glosscontour_box),
        ("color",    self.highlightcolor_button),
        ("modebox",  self.highlightmode_box),
        ("floatadj", self.highlightopacity_slider["adj"]),
        ("color",    self.shadowcolor_button),
        ("modebox",  self.shadowmode_box),
        ("floatadj", self.shadowopacity_slider["adj"]),
        ("combobox", self.surfacecontour_box),
        ("check",    self.use_texture_check),
        ("pattern",  self.pattern_button),
        ("floatadj", self.scale_slider["adj"]),
        ("floatadj", self.tex_depth_slider["adj"]),
        ("check",    self.invert_check),
        ("check",    self.merge_check)
      )
    shelf[self.shelfkey] = params

  def resetbutton(self, widget):
    self.style_box.set_active(0)
    self.depth_slider["adj"].set_value(3)
    self.direction_up_radio.set_active(True)
    self.size_slider["adj"].set_value(5)
    self.soften_slider["adj"].set_value(0)
    self.surfacecontour_box.set_active(0)
    self.angle_slider["adj"].set_value(120.0)
    self.altitude_slider["adj"].set_value(30.0)
    self.glosscontour_box.set_active(0)
    self.highlightcolor_button.set_color(gimpcolor.RGB(255, 255, 255, 255))
    self.highlightmode_box.set_active(SCREEN_MODE)
    self.highlightopacity_slider["adj"].set_value(75.0)
    self.shadowcolor_button.set_color(gimpcolor.RGB(0, 0, 0, 255))
    self.shadowmode_box.set_active(MULTIPLY_MODE)
    self.shadowopacity_slider["adj"].set_value(75.0)
    self.use_texture_check.set_active(False)
    self.pattern_button.set_pattern(pdb.gimp_context_get_pattern())
    self.invert_check.set_active(False)
    self.scale_slider["adj"].set_value(100.0)
    self.tex_depth_slider["adj"].set_value(100.0)
    self.merge_check.set_active(False)

  def removeOldLayer(self):
    if not hasattr(self, "parasitedata"):
      self.parasitedata = self.readParasite(self.img, self.drawable)
    if self.parasitedata:
      self.img.remove_layer(self.parasitedata["oldid"])
      self.img.remove_layer(self.parasitedata["oldid2"])

  def preview(self, widget, *extra):
    if self.previewLayer != None:
      if type(self.previewLayer) == tuple:
        for i in self.previewLayer:
          if self.layer_exists(i):
            self.img.remove_layer(i)
      else:
        if self.layer_exists(self.previewLayer):
          self.img.remove_layer(self.previewLayer)
    if self.preview_check.get_active():
      self.unset_hidden_layer()
      if pdb.gimp_image_undo_is_enabled(self.img) == 1:
        pdb.gimp_image_undo_freeze(self.img)
      if self.parasitedata:
        self.set_hidden_layer(self.parasitedata["oldid"])
        self.set_hidden_layer(self.parasitedata["oldid2"])
      if self.direction_up_radio.get_active():
        direction = 0
      elif self.direction_down_radio.get_active():
        direction = 1
      if self.merge_check.get_active():
        self.previewLayer = self.drawable.copy()
        self.add_over_layer(self.previewLayer, self.drawable)
        self.set_hidden_layer(self.drawable)
        layer = self.previewLayer
        merge = 1
      else:
        layer = self.drawable
        merge = 0
      self.previewLayer = self.makeBevel(
        self.img,
        layer,
        self.style_box.get_active(),
        int(round(self.depth_slider["adj"].get_value())),
        direction,
        int(round(self.size_slider["adj"].get_value())),
        int(round(self.soften_slider["adj"].get_value())),
        self.angle_slider["adj"].get_value(),
        self.altitude_slider["adj"].get_value(),
        self.glosscontour_box.get_active(),
        self.highlightcolor_button.get_color(),
        self.highlightmode_box.get_active(),
        self.highlightopacity_slider["adj"].get_value(),
        self.shadowcolor_button.get_color(),
        self.shadowmode_box.get_active(),
        self.shadowopacity_slider["adj"].get_value(),
        self.surfacecontour_box.get_active(),
        self.cond(self.use_texture_check.get_active()),
        self.pattern_button.get_pattern(),
        self.scale_slider["adj"].get_value(),
        self.tex_depth_slider["adj"].get_value(),
        self.cond(self.invert_check.get_active()),
        merge
      )
    else:
      gimp.displays_flush()

  def removePreviews(self):
    if self.previewLayer != None:
      if type(self.previewLayer) == tuple:
        for i in self.previewLayer:
          if self.layer_exists(i):
            self.img.remove_layer(i)
      else:
        if self.layer_exists(self.previewLayer):
          self.img.remove_layer(self.previewLayer)
      self.previewLayer = None
    self.unset_hidden_layer()
    gimp.displays_flush()

  def writeParasite(self, drawable, fxlayer, *controls):
    layerfx_base.writeParasite(self, drawable, fxlayer[0], *controls)
    drawable.attach_new_parasite("%s-fxlayer-s" % (self.shelfkey), 0, fxlayer[1].name)

  def writeParasiteRaw(self, drawable, fxlayer, *values):
    layerfx_base.writeParasiteRaw(self, drawable, fxlayer[0], *values)
    drawable.attach_new_parasite("%s-fxlayer-s" % (self.shelfkey), 0, fxlayer[1].name)

  def readParasite(self, img, drawable):
    parasitedata = layerfx_base.readParasite(self, img, drawable,
      layerfx_bevel_emboss.shelfkey,
      ("style",            "int"),
      ("depth",            "int"),
      ("direction",        "int"),
      ("size",             "int"),
      ("soften",           "int"),
      ("angle",            "float"),
      ("altitude",         "float"),
      ("glosscontour",     "int"),
      ("highlightcolor",   "color"),
      ("highlightmode",    "int"),
      ("highlightopacity", "float"),
      ("shadowcolor",      "color"),
      ("shadowmode",       "int"),
      ("shadowopacity",    "float"),
      ("surfacecontour",   "int"),
      ("use_texture",      "int"),
      ("pattern",          "string"),
      ("scale",            "float"),
      ("tex_depth",        "float"),
      ("invert",           "int"),
      ("merge",            "int")
    )
    if parasitedata:
      fxlayername = "%s-fxlayer-s" % (self.shelfkey)
      if fxlayername in pdb.gimp_drawable_parasite_list(drawable)[1]:
        fxlayername = pdb.gimp_drawable_parasite_find(drawable, fxlayername).data
        for i in img.layers:
          if i.name == fxlayername:
            parasitedata.update({"oldid2": i})
            break
    return parasitedata

  def makeBevel(self, img, drawable, style, depth, direction, size, soften, angle, altitude, glosscontour, highlightcolor, highlightmode, highlightopacity, shadowcolor, shadowmode, shadowopacity, surfacecontour, use_texture, pattern, scale, tex_depth, invert, merge):
    pdb.gimp_image_undo_group_start(img)
    origfgcolor = gimp.get_foreground()
    origselection = pdb.gimp_selection_save(img)
    imgtype = (RGBA_IMAGE, GRAYA_IMAGE)[img.base_type]
    lyrgrowamt = int(round(size * 1.2))
    if style == 0:
      layersize = {
        "width":   drawable.width + (lyrgrowamt * 2),
        "height":  drawable.height + (lyrgrowamt * 2),
        "offsetx": drawable.offsets[0] - lyrgrowamt,
        "offsety": drawable.offsets[1] - lyrgrowamt
      }
    elif style == 1:
      layersize = {
        "width":   drawable.width,
        "height":  drawable.height,
        "offsetx": drawable.offsets[0],
        "offsety": drawable.offsets[1]
      }
    elif style == 2 or style == 3:
      layersize = {
        "width":   drawable.width + lyrgrowamt,
        "height":  drawable.height + lyrgrowamt,
        "offsetx": drawable.offsets[0] - int(lyrgrowamt/2),
        "offsety": drawable.offsets[1] - int(lyrgrowamt/2)
      }
    bumpmaplayer = gimp.Layer(img, "%s-bumpmap" % (drawable.name), layersize["width"], layersize["height"], imgtype, 100.0, NORMAL_MODE)
    highlightlayer = gimp.Layer(img, "%s-highlight" % (drawable.name), layersize["width"], layersize["height"], imgtype, highlightopacity, highlightmode)
    shadowlayer = gimp.Layer(img, "%s-shadow" % (drawable.name), layersize["width"], layersize["height"], imgtype, shadowopacity, shadowmode)
    self.add_over_layer(bumpmaplayer, drawable)
    self.add_over_layer(shadowlayer, bumpmaplayer)
    self.add_over_layer(highlightlayer, shadowlayer)
    bumpmaplayer.set_offsets(layersize["offsetx"], layersize["offsety"])
    shadowlayer.set_offsets(layersize["offsetx"], layersize["offsety"])
    highlightlayer.set_offsets(layersize["offsetx"], layersize["offsety"])
    pdb.gimp_selection_none(img)
    gimp.set_foreground(highlightcolor)
    pdb.gimp_edit_fill(highlightlayer, FOREGROUND_FILL)
    gimp.set_foreground(shadowcolor)
    pdb.gimp_edit_fill(shadowlayer, FOREGROUND_FILL)
    gimp.set_foreground(0, 0, 0)
    pdb.gimp_edit_fill(bumpmaplayer, FOREGROUND_FILL)
    highlightmask = highlightlayer.create_mask(ADD_BLACK_MASK)
    shadowmask = shadowlayer.create_mask(ADD_BLACK_MASK)
    highlightlayer.add_mask(highlightmask)
    shadowlayer.add_mask(shadowmask)
    pdb.gimp_selection_layer_alpha(drawable)
    if drawable.mask != None:
      pdb.gimp_selection_combine(drawable.mask, CHANNEL_OP_INTERSECT)
    alphaSel = pdb.gimp_selection_save(img)
    if style == 0:
      self.draw_blurshape(bumpmaplayer, size, size, alphaSel, False)
    elif style == 1:
      self.draw_blurshape(bumpmaplayer, size, 0, alphaSel, False)
    elif style == 2:
      halfsizef = int(math.floor(size/2.0))
      halfsizec = size - halfsizef
      self.draw_blurshape(bumpmaplayer, size, int(math.ceil(size/2.0)), alphaSel, False)
    elif style == 3:
      halfsizef = int(math.floor(size/2.0))
      halfsizec = size - halfsizef
      pdb.gimp_selection_none(img)
      gimp.set_foreground(255, 255, 255)
      pdb.gimp_edit_fill(bumpmaplayer, FOREGROUND_FILL)
      self.draw_blurshape(bumpmaplayer, halfsizec, halfsizec, alphaSel, True)
      self.draw_blurshape(bumpmaplayer, halfsizef, 0, alphaSel, False)
    pdb.gimp_selection_none(img)
    if use_texture == 1:
      texturelayer = gimp.Layer(img, "%s-texture" % (drawable.name), int(round(layersize["width"]/(scale/100.0))), int(round(layersize["height"]/(scale/100.0))), imgtype, 100.0, MULTIPLY_MODE)
      self.add_over_layer(texturelayer, bumpmaplayer)
      texturelayer.set_offsets(bumpmaplayer.offsets[0], bumpmaplayer.offsets[1])
      origpattern = pdb.gimp_context_get_pattern()
      pdb.gimp_context_set_pattern(pattern)
      texturelayer.fill(PATTERN_FILL)
      pdb.gimp_context_set_pattern(origpattern)
      if img.base_type == RGB:
        pdb.gimp_desaturate_full(texturelayer, DESATURATE_LUMINOSITY)
      if tex_depth >= 0.0:
        if tex_depth <= 100.0:
          contrastadj = int(round((1-(tex_depth/100.0)) * -127))
        else:
          contrastadj = int(round(((tex_depth-100.0)/900.0) * 127))
      else:
        pdb.gimp_invert(texturelayer)
        if tex_depth >= -100.0:
          contrastadj = int(round((1-(abs(tex_depth)/100.0)) * -127))
        else:
          contrastadj = int(round(((abs(tex_depth)-100.0)/900.0) * 127))
      pdb.gimp_brightness_contrast(texturelayer, 0, contrastadj)
      if scale != 100.0:
        pdb.gimp_drawable_transform_scale(texturelayer, bumpmaplayer.offsets[0], bumpmaplayer.offsets[1], bumpmaplayer.offsets[0] + layersize["width"], bumpmaplayer.offsets[1] + layersize["height"], TRANSFORM_FORWARD, INTERPOLATION_LANCZOS, 1, 3, TRANSFORM_RESIZE_ADJUST)
      bumpmaplayer = pdb.gimp_image_merge_down(img, texturelayer, EXPAND_AS_NECESSARY)
    gimp.set_foreground(127, 127, 127)
    pdb.gimp_edit_fill(highlightmask, FOREGROUND_FILL)
    if surfacecontour > 0:
      self.apply_contour(bumpmaplayer, HISTOGRAM_VALUE, surfacecontour)
    if angle < 0:
      angle += 360.0
    pdb.plug_in_bump_map(img, highlightmask, bumpmaplayer, angle, altitude, depth, 0, 0, 0, 0, 1, direction, 0)
    if glosscontour > 0:
      self.apply_contour(highlightmask, HISTOGRAM_VALUE, glosscontour)
    if soften > 0:
      pdb.plug_in_gauss_rle(img, highlightmask, soften, 1, 1)
    if use_texture == 1 and invert > 0:
      pdb.gimp_invert(highlightmask)
    pdb.gimp_channel_combine_masks(shadowmask, highlightmask, CHANNEL_OP_REPLACE, 0, 0)
    pdb.gimp_levels(highlightmask, HISTOGRAM_VALUE, 127, 255, 1.0, 0, 255)
    pdb.gimp_levels(shadowmask, HISTOGRAM_VALUE, 0, 127, 1.0, 255, 0)
    pdb.gimp_selection_load(alphaSel)
    if style == 0:
      pdb.gimp_selection_grow(img, size)
    elif style == 2 or style == 3:
      pdb.gimp_selection_grow(img, halfsizec)
    pdb.gimp_selection_invert(img)
    gimp.set_foreground(0, 0, 0)
    pdb.gimp_edit_fill(shadowmask, FOREGROUND_FILL)
    pdb.gimp_selection_none(img)
    img.remove_layer(bumpmaplayer)
    if merge == 1:
      if style == 1:
        origmask = drawable.mask
        layername = drawable.name
        if origmask != None:
          origmask = drawable.mask.copy()
          drawable.remove_mask(MASK_DISCARD)
        alphamask = drawable.create_mask(ADD_ALPHA_TRANSFER_MASK)
        shadowlayer = pdb.gimp_image_merge_down(img, shadowlayer, EXPAND_AS_NECESSARY)
        highlightlayer = pdb.gimp_image_merge_down(img, highlightlayer, EXPAND_AS_NECESSARY)
        highlightlayer.name = layername
        highlightlayer.add_mask(alphamask)
        highlightlayer.remove_mask(MASK_APPLY)
        if origmask != None:
          highlightlayer.add_mask(origmask)
      else:
        origmask = drawable.mask
        layername = drawable.name
        if origmask != None:
          drawable.remove_mask(MASK_APPLY)
        shadowlayer = pdb.gimp_image_merge_down(img, shadowlayer, EXPAND_AS_NECESSARY)
        highlightlayer = pdb.gimp_image_merge_down(img, highlightlayer, EXPAND_AS_NECESSARY)
        highlightlayer.name = layername
    else:
      highlightlayer.remove_mask(MASK_APPLY)
      shadowlayer.remove_mask(MASK_APPLY)
      pdb.gimp_image_set_active_layer(img, drawable)
    gimp.set_foreground(origfgcolor)
    pdb.gimp_selection_load(origselection)
    img.remove_channel(alphaSel)
    img.remove_channel(origselection)
    gimp.displays_flush()
    pdb.gimp_image_undo_group_end(img)
    if merge == 0:
      return (highlightlayer, shadowlayer)
    else:
      return highlightlayer

class layerfx_satin(layerfx_base):
  shelfkey = "layerfx-satin"

  def __init__(self, runmode, img, drawable, color, opacity, mode, offsetangle, offsetdist, size, contour, invert, merge):
    self.origMsgHandler = pdb.gimp_message_get_handler()
    pdb.gimp_message_set_handler(ERROR_CONSOLE)
    self.img = img
    self.drawable = drawable
    if runmode == RUN_INTERACTIVE:
      self.showDialog()
    elif runmode == RUN_NONINTERACTIVE:
      if self.validatedata(img, drawable,
        ("color",      color),
        ("percent",    opacity),
        ("mode",       mode),
        ("angle",      offsetangle),
        ("floatrange", offsetdist, 0.0, 30000.0),
        ("size",       size),
        ("contour",    contour),
        ("boolean",    invert),
        ("boolean",    merge)
      ):
        self.removeOldLayer()
        fxlayer = self.makeSatin(img, drawable, color, opacity, mode, offsetangle, offsetdist, size, contour, invert, merge)
        if merge == 0:
          self.writeParasiteRaw(drawable, fxlayer, color, opacity, mode, offsetangle, offsetdist, size, contour, invert, merge)
        shelf[self.shelfkey] = {
          "color":       color,
          "opacity":     opacity,
          "mode":        mode,
          "offsetangle": offsetangle,
          "offsetdist":  offsetdist,
          "size":        size,
          "contour":     contour,
          "invert":      invert,
          "merge":       merge
        }
    elif runmode == RUN_WITH_LAST_VALS and shelf.has_key(self.shelfkey) == 1:
      self.removeOldLayer()
      fxlayer = self.makeSatin(
        img,
        drawable,
        shelf[self.shelfkey]["color"],
        shelf[self.shelfkey]["opacity"],
        shelf[self.shelfkey]["mode"],
        shelf[self.shelfkey]["offsetangle"],
        shelf[self.shelfkey]["offsetdist"],
        shelf[self.shelfkey]["size"],
        shelf[self.shelfkey]["contour"],
        shelf[self.shelfkey]["invert"],
        shelf[self.shelfkey]["merge"]
      )
      if shelf[self.shelfkey]["merge"] == 0:
        self.writeParasiteRaw(drawable, fxlayer,
          shelf[self.shelfkey]["color"],
          shelf[self.shelfkey]["opacity"],
          shelf[self.shelfkey]["mode"],
          shelf[self.shelfkey]["offsetangle"],
          shelf[self.shelfkey]["offsetdist"],
          shelf[self.shelfkey]["size"],
          shelf[self.shelfkey]["contour"],
          shelf[self.shelfkey]["invert"],
          shelf[self.shelfkey]["merge"]
        )
    else:
      pdb.gimp_message("unknown runmode")
    pdb.gimp_message_set_handler(self.origMsgHandler)

  def showDialog(self):
    self.dialog = gimpui.Dialog("Satin", "satindialog")

    self.parasitedata = self.readParasite(self.img, self.drawable)

    self.table = gtk.Table(7, 5, True)
    self.table.set_homogeneous(True)
    self.table.set_row_spacings(3)
    self.table.set_col_spacings(3)
    self.table.show()

    self.color_label = self.make_label("_Color:")
    self.table.attach(self.color_label, 0, 1, 0, 1)

    self.color_button = gimpui.ColorButton("Satin Color", 10, 10, gimpcolor.RGB(0, 0, 0, 255))
    if self.parasitedata:
      self.color_button.set_color(self.parasitedata["color"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.color_button.set_color(shelf[self.shelfkey]["color"])
    self.color_label.set_mnemonic_widget(self.color_button)
    self.color_button.show()
    self.table.attach(self.color_button, 1, 2, 0, 1)
    self.color_button.connect("color-changed", self.preview)

    self.mode_label = self.make_label("_Blend Mode:")
    self.table.attach(self.mode_label, 0, 1, 1, 2)

    self.mode_box = self.make_blend_mode_box()
    if self.parasitedata:
      self.mode_box.set_active(self.parasitedata["mode"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.mode_box.set_active(shelf[self.shelfkey]["mode"])
    else:
      self.mode_box.set_active(MULTIPLY_MODE)
    self.mode_label.set_mnemonic_widget(self.mode_box)
    self.mode_box.show()
    self.table.attach(self.mode_box, 1, 5, 1, 2)
    self.mode_box.connect("changed", self.preview)

    self.opacity_label = self.make_label("_Opacity:")
    self.table.attach(self.opacity_label, 0, 1, 2, 3)

    self.opacity_slider = self.make_slider_and_spinner(75.0, 0.0, 100.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.opacity_slider["adj"].set_value(self.parasitedata["opacity"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.opacity_slider["adj"].set_value(shelf[self.shelfkey]["opacity"])
    self.opacity_label.set_mnemonic_widget(self.opacity_slider["spinner"])
    self.table.attach(self.opacity_slider["slider"], 1, 4, 2, 3)
    self.table.attach(self.opacity_slider["spinner"], 4, 5, 2, 3)
    self.opacity_slider["adj"].connect("value-changed", self.preview)

    self.angle_label = self.make_label("_Angle:")
    self.table.attach(self.angle_label, 0, 1, 3, 4)

    self.angle_slider = self.make_slider_and_spinner(19.0, -180.0, 180.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.angle_slider["adj"].set_value(self.parasitedata["offsetangle"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.angle_slider["adj"].set_value(shelf[self.shelfkey]["offsetangle"])
    self.angle_label.set_mnemonic_widget(self.angle_slider["spinner"])
    self.table.attach(self.angle_slider["slider"], 1, 4, 3, 4)
    self.table.attach(self.angle_slider["spinner"], 4, 5, 3, 4)
    self.angle_slider["adj"].connect("value-changed", self.preview)

    self.distance_label = self.make_label("_Distance:")
    self.table.attach(self.distance_label, 0, 1, 4, 5)

    self.distance_spinner = self.make_spinner(11.0, 0.0, 30000.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.distance_spinner["adj"].set_value(self.parasitedata["offsetdist"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.distance_spinner["adj"].set_value(shelf[self.shelfkey]["offsetdist"])
    self.distance_label.set_mnemonic_widget(self.distance_spinner["spinner"])
    self.table.attach(self.distance_spinner["spinner"], 1, 2, 4, 5)
    self.distance_spinner["adj"].connect("value-changed", self.preview)

    self.size_label = self.make_label("S_ize:")
    self.table.attach(self.size_label, 0, 1, 5, 6)

    self.size_slider = self.make_slider_and_spinner(14, 0, 250, 1, 10, 0)
    if self.parasitedata:
      self.size_slider["adj"].set_value(self.parasitedata["size"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.size_slider["adj"].set_value(shelf[self.shelfkey]["size"])
    self.size_label.set_mnemonic_widget(self.size_slider["spinner"])
    self.table.attach(self.size_slider["slider"], 1, 4, 5, 6)
    self.table.attach(self.size_slider["spinner"], 4, 5, 5, 6)
    self.size_slider["adj"].connect("value-changed", self.preview)

    self.contour_label = self.make_label("Con_tour:")
    self.table.attach(self.contour_label, 0, 1, 6, 7)

    self.contour_box = self.make_contour_box()
    if self.parasitedata:
      self.contour_box.set_active(self.parasitedata["contour"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.contour_box.set_active(shelf[self.shelfkey]["contour"])
    else:
      self.contour_box.set_active(5)
    self.contour_label.set_mnemonic_widget(self.contour_box)
    self.contour_box.show()
    self.table.attach(self.contour_box, 1, 5, 6, 7)
    self.contour_box.connect("changed", self.preview)

    self.invert_check = gtk.CheckButton("Invert")
    if self.parasitedata:
      if self.parasitedata["invert"] == 0:
        self.invert_check.set_active(False)
      else:
        self.invert_check.set_active(True)
    elif shelf.has_key(self.shelfkey) == 1 and shelf[self.shelfkey]["invert"] == 0:
      self.invert_check.set_active(False)
    else:
      self.invert_check.set_active(True)
    self.invert_check.show()
    self.invert_check.connect("toggled", self.preview)

    self.merge_check = gtk.CheckButton("_Merge with layer")
    if self.parasitedata:
      if self.parasitedata["merge"] == 1:
        self.merge_check.set_active(True)
    elif shelf.has_key(self.shelfkey) == 1 and shelf[self.shelfkey]["merge"] == 1:
      self.merge_check.set_active(True)
    self.merge_check.show()
    self.merge_check.connect("toggled", self.preview)

    self.preview_check = gtk.CheckButton("_Preview")
    self.preview_check.show()
    self.preview_check.connect("toggled", self.preview)

    self.dialog.vbox.hbox1 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox1.show()
    self.dialog.vbox.pack_start(self.dialog.vbox.hbox1, True, True, 7)
    self.dialog.vbox.hbox1.pack_start(self.table, True, True, 7)
    self.dialog.vbox.hbox2 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox2.show()
    self.dialog.vbox.add(self.dialog.vbox.hbox2)
    self.dialog.vbox.hbox2.pack_start(self.invert_check, True, True, 10)
    self.dialog.vbox.hbox3 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox3.show()
    self.dialog.vbox.add(self.dialog.vbox.hbox3)
    self.dialog.vbox.hbox3.pack_start(self.merge_check, True, True, 10)
    self.dialog.vbox.hbox4 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox4.show()
    self.dialog.vbox.add(self.dialog.vbox.hbox4)
    self.dialog.vbox.hbox4.pack_start(self.preview_check, True, True, 10)

    self.makeDialogButtons()

    self.dialog.show()
    self.dialog.run()
    self.removePreviews()

  def okbutton(self, widget):
    if self.layer_exists(self.previewLayer):
      self.img.remove_layer(self.previewLayer)
      self.previewLayer = None
    self.unset_hidden_layer()
    if pdb.gimp_image_undo_is_enabled(self.img) == 0:
      pdb.gimp_image_undo_thaw(self.img)
    params = {
      "color":       self.color_button.get_color(),
      "opacity":     self.opacity_slider["adj"].get_value(),
      "mode":        self.mode_box.get_active(),
      "offsetangle": self.angle_slider["adj"].get_value(),
      "offsetdist":  self.distance_spinner["adj"].get_value(),
      "size":        int(round(self.size_slider["adj"].get_value())),
      "contour":     self.contour_box.get_active(),
      "invert":      self.cond(self.invert_check.get_active()),
      "merge":       self.cond(self.merge_check.get_active())
    }
    if self.parasitedata:
      self.img.remove_layer(self.parasitedata["oldid"])
    fxlayer = self.makeSatin(
      self.img,
      self.drawable,
      params["color"],
      params["opacity"],
      params["mode"],
      params["offsetangle"],
      params["offsetdist"],
      params["size"],
      params["contour"],
      params["invert"],
      params["merge"]
    )
    if params["merge"] == 0:
      self.writeParasite(self.drawable, fxlayer,
        ("color",    self.color_button),
        ("floatadj", self.opacity_slider["adj"]),
        ("modebox",  self.mode_box),
        ("floatadj", self.angle_slider["adj"]),
        ("floatadj", self.distance_spinner["adj"]),
        ("intadj",   self.size_slider["adj"]),
        ("combobox", self.contour_box),
        ("check",    self.invert_check),
        ("check",    self.merge_check)
      )
    shelf[self.shelfkey] = params

  def resetbutton(self, widget):
    self.color_button.set_color(gimpcolor.RGB(0, 0, 0, 255))
    self.mode_box.set_active(MULTIPLY_MODE)
    self.opacity_slider["adj"].set_value(75.0)
    self.angle_slider["adj"].set_value(19.0)
    self.distance_spinner["adj"].set_value(11.0)
    self.size_slider["adj"].set_value(14)
    self.contour_box.set_active(5)
    self.invert_check.set_active(True)
    self.merge_check.set_active(False)

  def readParasite(self, img, drawable):
    return layerfx_base.readParasite(self, img, drawable,
      layerfx_satin.shelfkey,
      ("color",       "color"),
      ("opacity",     "float"),
      ("mode",        "int"),
      ("offsetangle", "float"),
      ("offsetdist",  "float"),
      ("size",        "int"),
      ("contour",     "int"),
      ("invert",      "int"),
      ("merge",       "int")
    )

  def preview(self, widget):
    if self.layer_exists(self.previewLayer):
      self.img.remove_layer(self.previewLayer)
    if self.preview_check.get_active():
      self.unset_hidden_layer()
      if pdb.gimp_image_undo_is_enabled(self.img) == 1:
        pdb.gimp_image_undo_freeze(self.img)
      if self.parasitedata:
        self.set_hidden_layer(self.parasitedata["oldid"])
      if self.merge_check.get_active():
        self.previewLayer = self.drawable.copy()
        self.add_over_layer(self.previewLayer, self.drawable)
        self.set_hidden_layer(self.drawable)
        layer = self.previewLayer
        merge = 1
      else:
        layer = self.drawable
        merge = 0
      self.previewLayer = self.makeSatin(
        self.img,
        layer,
        self.color_button.get_color(),
        self.opacity_slider["adj"].get_value(),
        self.mode_box.get_active(),
        self.angle_slider["adj"].get_value(),
        self.distance_spinner["adj"].get_value(),
        int(round(self.size_slider["adj"].get_value())),
        self.contour_box.get_active(),
        self.cond(self.invert_check.get_active()),
        merge
      )
    else:
      gimp.displays_flush()

  def makeSatin(self, img, drawable, color, opacity, mode, offsetangle, offsetdist, size, contour, invert, merge):
    pdb.gimp_image_undo_group_start(img)
    origfgcolor = gimp.get_foreground()
    origselection = pdb.gimp_selection_save(img)
    growamt = int(math.ceil(size / 2.0))
    lyrgrowamt = int(round(growamt * 1.2))
    satinlayer = gimp.Layer(img, "%s-satin" % (drawable.name), drawable.width + (lyrgrowamt * 2), drawable.height + (lyrgrowamt * 2), (RGBA_IMAGE, GRAYA_IMAGE)[img.base_type], 100, NORMAL_MODE)
    ang = ((offsetangle + 180) * -1) * (math.pi / 180.0)
    offset = (int(round(offsetdist * math.cos(ang))), int(round(offsetdist * math.sin(ang))))
    self.add_over_layer(satinlayer, drawable)
    satinlayer.set_offsets(drawable.offsets[0] - lyrgrowamt, drawable.offsets[1] - lyrgrowamt)
    pdb.gimp_selection_none(img)
    gimp.set_foreground(0, 0, 0)
    pdb.gimp_edit_fill(satinlayer, FOREGROUND_FILL)
    pdb.gimp_selection_layer_alpha(drawable)
    if drawable.mask != None:
      pdb.gimp_selection_combine(drawable.mask, CHANNEL_OP_INTERSECT)
    alphaSel = pdb.gimp_selection_save(img)
    self.draw_blurshape(satinlayer, size, growamt, alphaSel, False)
    pdb.plug_in_autocrop_layer(img, satinlayer)
    satinmask = satinlayer.copy(0)
    self.add_over_layer(satinmask, satinlayer)
    satinlayer.translate(offset[0], offset[1])
    satinmask.translate(offset[0] * -1, offset[1] * -1)
    dx = max(satinlayer.offsets[0], satinmask.offsets[0]) - min(satinlayer.offsets[0], satinmask.offsets[0])
    dy = max(satinlayer.offsets[1], satinmask.offsets[1]) - min(satinlayer.offsets[1], satinmask.offsets[1])
    blacklayer = gimp.Layer(img, "%s-satinblank" % (drawable.name), satinlayer.width + dx, satinlayer.height + dy, (RGBA_IMAGE, GRAYA_IMAGE)[img.base_type], 100, NORMAL_MODE)
    self.add_under_layer(blacklayer, satinlayer)
    blacklayer.set_offsets(min(satinlayer.offsets[0], satinmask.offsets[0]), min(satinlayer.offsets[1], satinmask.offsets[1]))
    pdb.gimp_selection_none(img)
    gimp.set_foreground(0, 0, 0)
    pdb.gimp_edit_fill(blacklayer, FOREGROUND_FILL)
    satinmask.mode = DIFFERENCE_MODE
    satinlayer = pdb.gimp_image_merge_down(img, satinlayer, EXPAND_AS_NECESSARY)
    satinlayer = pdb.gimp_image_merge_down(img, satinmask, EXPAND_AS_NECESSARY)
    satinlayer.name = "%s-satin" % (drawable.name)
    if contour > 0:
      self.apply_contour(satinlayer, HISTOGRAM_VALUE, contour)
      pdb.gimp_selection_load(alphaSel)
      pdb.gimp_selection_grow(img, size)
      pdb.gimp_selection_invert(img)
      gimp.set_foreground(0, 0, 0)
      pdb.gimp_edit_fill(satinlayer, FOREGROUND_FILL)
      pdb.gimp_selection_none(img)
    if invert == 1:
      pdb.gimp_invert(satinlayer)
    satinmask = satinlayer.create_mask(ADD_COPY_MASK)
    satinlayer.add_mask(satinmask)
    pdb.gimp_selection_none(img)
    gimp.set_foreground(color)
    pdb.gimp_edit_fill(satinlayer, FOREGROUND_FILL)
    satinlayer.opacity = opacity
    pdb.gimp_layer_set_mode(satinlayer, mode)
    satinlayer.resize(drawable.width, drawable.height, satinlayer.offsets[0] - drawable.offsets[0], satinlayer.offsets[1] - drawable.offsets[1])
    if merge == 1:
      origmask = drawable.mask
      layername = drawable.name
      if origmask != None:
        origmask = drawable.mask.copy()
        drawable.remove_mask(MASK_DISCARD)
      alphamask = drawable.create_mask(ADD_ALPHA_TRANSFER_MASK)
      satinlayer = pdb.gimp_image_merge_down(img, satinlayer, EXPAND_AS_NECESSARY)
      satinlayer.name = layername
      satinlayer.add_mask(alphamask)
      satinlayer.remove_mask(MASK_APPLY)
      if origmask != None:
        satinlayer.add_mask(origmask)
    else:
      pdb.gimp_selection_load(alphaSel)
      pdb.gimp_selection_invert(img)
      gimp.set_foreground(0, 0, 0)
      pdb.gimp_edit_fill(satinmask, FOREGROUND_FILL)
      satinlayer.remove_mask(MASK_APPLY)
      pdb.gimp_image_set_active_layer(img, drawable)
    gimp.set_foreground(origfgcolor)
    pdb.gimp_selection_load(origselection)
    img.remove_channel(alphaSel)
    img.remove_channel(origselection)
    gimp.displays_flush()
    pdb.gimp_image_undo_group_end(img)
    return satinlayer

class layerfx_stroke(layerfx_base):
  shelfkey = "layerfx-stroke"

  def __init__(self, runmode, img, drawable, fill, opacity, mode, size, position, merge):
    self.origMsgHandler = pdb.gimp_message_get_handler()
    pdb.gimp_message_set_handler(ERROR_CONSOLE)
    self.img = img
    self.drawable = drawable
    if runmode == RUN_INTERACTIVE:
      self.showDialog()
    elif runmode == RUN_NONINTERACTIVE:
      if self.validatedata(img, drawable,
        ("color/gradientdata/patterndata", fill),
        ("percent",                        opacity),
        ("mode",                           mode),
        ("size",                           size),
        ("percent",                        position),
        ("boolean",                        merge)
      ):
        self.removeOldLayer()
        fxlayer = self.makeStroke(img, drawable, fill, opacity, mode, size, position, merge)
        if type(fill) == gimpcolor.RGB:
          params = {
            "filltype":           0,
            "color":              fill,
            "gradient":           "FG to BG (RGB)",
            "gradienttype":       0,
            "repeat":             0,
            "reverse":            False,
            "centerx":            0.0,
            "centery":            0.0,
            "angle":              90.0,
            "width":              0.0,
            "pattern":            pdb.gimp_context_get_pattern(),
            "scale":              100.0,
            "interpolation_type": 0
          }
        elif (type(fill) == tuple or type(fill) == list) and len(fill) == 8:
          params = {
            "filltype":           1,
            "color":              gimpcolor.RGB(255, 0, 0, 255),
            "gradient":           fill[0],
            "gradienttype":       fill[1],
            "repeat":             fill[2],
            "reverse":            fill[3],
            "centerx":            fill[4],
            "centery":            fill[5],
            "angle":              fill[6],
            "width":              fill[7],
            "pattern":            pdb.gimp_context_get_pattern(),
            "scale":              100.0,
            "interpolation_type": 0
          }
        elif (type(fill) == tuple or type(fill) == list) and len(fill) == 3:
          params = {
            "filltype":           2,
            "color":              gimpcolor.RGB(255, 0, 0, 255),
            "gradient":           "FG to BG (RGB)",
            "gradienttype":       0,
            "repeat":             0,
            "reverse":            False,
            "centerx":            0.0,
            "centery":            0.0,
            "angle":              90.0,
            "width":              0.0,
            "pattern":            fill[0],
            "scale":              fill[1],
            "interpolation_type": fill[2]
          }
        params.update({
          "opacity":  opacity,
          "mode":     mode,
          "size":     size,
          "position": position,
          "merge":    merge
        })
        if merge == 0:
          self.writeParasiteRaw(drawable, fxlayer,
            params["filltype"],
            params["color"],
            params["gradient"],
            params["gradienttype"],
            params["repeat"],
            params["reverse"],
            params["centerx"],
            params["centery"],
            params["angle"],
            params["width"],
            params["pattern"],
            params["scale"],
            params["interpolation_type"],
            opacity,
            mode,
            size,
            position,
            merge
          )
        shelf[self.shelfkey] = params
    elif runmode == RUN_WITH_LAST_VALS and shelf.has_key(self.shelfkey) == 1:
      self.removeOldLayer()
      if shelf[self.shelfkey]["filltype"] == 0:
        fill = shelf[self.shelfkey]["color"]
      elif shelf[self.shelfkey]["filltype"] == 1:
        fill = (
          shelf[self.shelfkey]["gradient"],
          shelf[self.shelfkey]["gradienttype"],
          shelf[self.shelfkey]["repeat"],
          shelf[self.shelfkey]["reverse"],
          shelf[self.shelfkey]["centerx"],
          shelf[self.shelfkey]["centery"],
          shelf[self.shelfkey]["angle"],
          shelf[self.shelfkey]["width"]
        )
      elif shelf[self.shelfkey]["filltype"] == 2:
        fill = (
          shelf[self.shelfkey]["pattern"],
          shelf[self.shelfkey]["scale"],
          shelf[self.shelfkey]["interpolation_type"]
        )
      fxlayer = self.makeStroke(
        img,
        drawable,
        fill,
        shelf[self.shelfkey]["opacity"],
        shelf[self.shelfkey]["mode"],
        shelf[self.shelfkey]["size"],
        shelf[self.shelfkey]["position"],
        shelf[self.shelfkey]["merge"]
      )
      if shelf[self.shelfkey]["merge"] == 0:
        self.writeParasiteRaw(drawable, fxlayer,
          shelf[self.shelfkey]["filltype"],
          shelf[self.shelfkey]["color"],
          shelf[self.shelfkey]["gradient"],
          shelf[self.shelfkey]["gradienttype"],
          shelf[self.shelfkey]["repeat"],
          shelf[self.shelfkey]["reverse"],
          shelf[self.shelfkey]["centerx"],
          shelf[self.shelfkey]["centery"],
          shelf[self.shelfkey]["angle"],
          shelf[self.shelfkey]["width"],
          shelf[self.shelfkey]["pattern"],
          shelf[self.shelfkey]["scale"],
          shelf[self.shelfkey]["interpolation_type"],
          shelf[self.shelfkey]["opacity"],
          shelf[self.shelfkey]["mode"],
          shelf[self.shelfkey]["size"],
          shelf[self.shelfkey]["position"],
          shelf[self.shelfkey]["merge"]
        )
    else:
      pdb.gimp_message("unknown runmode")
    pdb.gimp_message_set_handler(self.origMsgHandler)

  def showDialog(self):
    self.dialog = gimpui.Dialog("Stroke", "strokedialog")

    self.parasitedata = self.readParasite(self.img, self.drawable)

    self.table = gtk.Table(5, 5, True)
    self.table.set_homogeneous(True)
    self.table.set_row_spacings(3)
    self.table.set_col_spacings(3)
    self.table.show()

    self.size_label = self.make_label("S_ize:")
    self.table.attach(self.size_label, 0, 1, 0, 1)

    self.size_slider = self.make_slider_and_spinner(3, 1, 250, 1, 10, 0)
    if self.parasitedata:
      self.size_slider["adj"].set_value(self.parasitedata["size"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.size_slider["adj"].set_value(shelf[self.shelfkey]["size"])
    self.size_label.set_mnemonic_widget(self.size_slider["spinner"])
    self.table.attach(self.size_slider["slider"], 1, 4, 0, 1)
    self.table.attach(self.size_slider["spinner"], 4, 5, 0, 1)
    self.size_slider["adj"].connect("value-changed", self.preview)

    self.position_label = self.make_label("Position:")
    self.table.attach(self.position_label, 0, 1, 1, 2)

    self.position_slider = self.make_slider_and_spinner(50.0, 0.0, 100.0, 1, 10, 1)
    if self.parasitedata:
      self.position_slider["adj"].set_value(self.parasitedata["position"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.position_slider["adj"].set_value(shelf[self.shelfkey]["position"])
    self.position_label.set_mnemonic_widget(self.position_slider["spinner"])
    self.table.attach(self.position_slider["slider"], 1, 4, 1, 2)
    self.table.attach(self.position_slider["spinner"], 4, 5, 1, 2)
    self.position_slider["adj"].connect("value-changed", self.preview)

    self.position_label2 = gtk.Label("0 = inside, 100 = outside")
    self.position_label2.set_alignment(0.5, 0.5)
    self.position_label2.show()
    self.table.attach(self.position_label2, 1, 5, 2, 3)

    self.mode_label = self.make_label("_Blend Mode:")
    self.table.attach(self.mode_label, 0, 1, 3, 4)

    self.mode_box = self.make_blend_mode_box()
    if self.parasitedata:
      self.mode_box.set_active(self.parasitedata["mode"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.mode_box.set_active(shelf[self.shelfkey]["mode"])
    else:
      self.mode_box.set_active(NORMAL_MODE)
    self.mode_label.set_mnemonic_widget(self.mode_box)
    self.mode_box.show()
    self.table.attach(self.mode_box, 1, 5, 3, 4)
    self.mode_box.connect("changed", self.preview)

    self.opacity_label = self.make_label("_Opacity:")
    self.table.attach(self.opacity_label, 0, 1, 4, 5)

    self.opacity_slider = self.make_slider_and_spinner(75.0, 0.0, 100.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.opacity_slider["adj"].set_value(self.parasitedata["opacity"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.opacity_slider["adj"].set_value(shelf[self.shelfkey]["opacity"])
    else:
      self.opacity_slider["adj"].set_value(100.0)
    self.opacity_label.set_mnemonic_widget(self.opacity_slider["spinner"])
    self.table.attach(self.opacity_slider["slider"], 1, 4, 4, 5)
    self.table.attach(self.opacity_slider["spinner"], 4, 5, 4, 5)
    self.opacity_slider["adj"].connect("value-changed", self.preview)

    self.fill_type_frame = gimpui.Frame("Fill Type")
    self.fill_type_frame.show()
    self.fill_type_vbox = gtk.VBox(False, 7)
    self.fill_type_vbox.show()
    self.fill_type_frame.add(self.fill_type_vbox)

    self.fill_type_radio_hbox = gtk.HBox(False, 7)
    self.fill_type_color_radio = gtk.RadioButton(None, "Color", True)
    self.fill_type_gradient_radio = gtk.RadioButton(self.fill_type_color_radio, "Gradient", True)
    self.fill_type_pattern_radio = gtk.RadioButton(self.fill_type_color_radio, "Pattern", True)
    self.fill_type_color_radio.show()
    self.fill_type_gradient_radio.show()
    self.fill_type_pattern_radio.show()
    self.fill_type_radio_hbox.pack_start(self.fill_type_color_radio, True, True, 0)
    self.fill_type_radio_hbox.pack_start(self.fill_type_gradient_radio, True, True, 0)
    self.fill_type_radio_hbox.pack_start(self.fill_type_pattern_radio, True, True, 0)
    self.fill_type_radio_hbox.show()
    self.fill_type_vbox.pack_start(self.fill_type_radio_hbox, True, True, 7)
    self.fill_type_color_radio.connect("toggled", self.switch_fill_panes)
    self.fill_type_gradient_radio.connect("toggled", self.switch_fill_panes)
    self.fill_type_pattern_radio.connect("toggled", self.switch_fill_panes)

    self.fill_type_hbox = gtk.HBox(False, 0)

    self.fill_type_table = gtk.Table(6, 4, True)
    self.fill_type_table.set_homogeneous(True)
    self.fill_type_table.set_row_spacings(3)
    self.fill_type_table.set_col_spacings(3)
    self.fill_type_table.show()

    self.color_label = self.make_label("_Color:")
    self.fill_type_table.attach(self.color_label, 0, 1, 0, 1)

    self.color_button = gimpui.ColorButton("Stroke Color", 10, 10, gimpcolor.RGB(255, 0, 0, 255))
    if self.parasitedata:
      self.color_button.set_color(self.parasitedata["color"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.color_button.set_color(shelf[self.shelfkey]["color"])
    self.color_label.set_mnemonic_widget(self.color_button)
    self.fill_type_table.attach(self.color_button, 1, 2, 0, 1)
    self.color_button.show()
    self.color_button.connect("color-changed", self.preview)

    self.gradient_label = self.make_label("_Gradient:", False)
    self.fill_type_table.attach(self.gradient_label, 0, 1, 0, 1)

    self.gradient_button = gimpui.GradientSelector()
    if self.parasitedata:
      self.gradient_button.set_gradient(self.parasitedata["gradient"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.gradient_button.set_gradient(shelf[self.shelfkey]["gradient"])
    self.gradient_label.set_mnemonic_widget(self.gradient_button)
    self.fill_type_table.attach(self.gradient_button, 1, 2, 0, 1)
    self.gradient_button.connect("gradient-set", self.preview)

    self.gradient_type_label = self.make_label("Gradient _Type:", False)
    self.fill_type_table.attach(self.gradient_type_label, 0, 1, 1, 2)

    self.gradient_type_box = self.make_gradient_type_box()
    if self.parasitedata:
      self.gradient_type_box.set_active(self.parasitedata["gradienttype"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.gradient_type_box.set_active(shelf[self.shelfkey]["gradienttype"])
    else:
      self.gradient_type_box.set_active(0)
    self.gradient_type_label.set_mnemonic_widget(self.gradient_type_box)
    self.fill_type_table.attach(self.gradient_type_box, 1, 4, 1, 2)
    self.gradient_type_box.connect("changed", self.preview)

    self.gradient_repeat_label = self.make_label("Repeat:", False)
    self.fill_type_table.attach(self.gradient_repeat_label, 0, 1, 2, 3)

    self.gradient_repeat_box = self.make_gradient_repeat_box()
    if self.parasitedata:
      self.gradient_repeat_box.set_active(self.parasitedata["repeat"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.gradient_repeat_box.set_active(shelf[self.shelfkey]["repeat"])
    else:
      self.gradient_repeat_box.set_active(0)
    self.gradient_repeat_label.set_mnemonic_widget(self.gradient_repeat_box)
    self.fill_type_table.attach(self.gradient_repeat_box, 1, 4, 2, 3)
    self.gradient_repeat_box.connect("changed", self.preview)

    self.gradient_reverse_check = gtk.CheckButton("_Reverse")
    if self.parasitedata:
      if self.parasitedata["reverse"] == 1:
        self.gradient_reverse_check.set_active(True)
    elif shelf.has_key(self.shelfkey) == 1 and shelf[self.shelfkey]["reverse"] == 1:
      self.gradient_reverse_check.set_active(True)
    self.fill_type_table.attach(self.gradient_reverse_check, 2, 3, 0, 1)
    self.gradient_reverse_check.connect("toggled", self.preview)

    self.gradient_center_label = self.make_label("_Center:", False)
    self.fill_type_table.attach(self.gradient_center_label, 0, 1, 3, 4)

    self.gradient_centerx_spinner = self.make_spinner(0.0, 0.0, self.img.width, 1.0, 10.0, 1, False)
    if self.parasitedata:
      self.gradient_centerx_spinner["adj"].set_value(self.parasitedata["centerx"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.gradient_centerx_spinner["adj"].set_value(shelf[self.shelfkey]["centerx"])
    self.gradient_center_label.set_mnemonic_widget(self.gradient_centerx_spinner["spinner"])
    self.fill_type_table.attach(self.gradient_centerx_spinner["spinner"], 1, 2, 3, 4)
    self.gradient_centerx_spinner["adj"].connect("value-changed", self.preview)

    self.gradient_centery_spinner = self.make_spinner(0.0, 0.0, self.img.height, 1.0, 10.0, 1, False)
    if self.parasitedata:
      self.gradient_centery_spinner["adj"].set_value(self.parasitedata["centery"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.gradient_centery_spinner["adj"].set_value(shelf[self.shelfkey]["centery"])
    self.fill_type_table.attach(self.gradient_centery_spinner["spinner"], 2, 3, 3, 4)
    self.gradient_centery_spinner["adj"].connect("value-changed", self.preview)

    self.gradient_angle_label = self.make_label("_Gradient Angle:", False)
    self.fill_type_table.attach(self.gradient_angle_label, 0, 1, 4, 5)

    self.gradient_angle_slider = self.make_slider_and_spinner(90.0, -180.0, 180.0, 1.0, 10.0, 1, False)
    if self.parasitedata:
      self.gradient_angle_slider["adj"].set_value(self.parasitedata["angle"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.gradient_angle_slider["adj"].set_value(shelf[self.shelfkey]["angle"])
    self.gradient_angle_label.set_mnemonic_widget(self.gradient_angle_slider["spinner"])
    self.fill_type_table.attach(self.gradient_angle_slider["slider"], 1, 3, 4, 5)
    self.fill_type_table.attach(self.gradient_angle_slider["spinner"], 3, 4, 4, 5)
    self.gradient_angle_slider["adj"].connect("value-changed", self.preview)

    self.gradient_width_label = self.make_label("_Gradient Width:", False)
    self.fill_type_table.attach(self.gradient_width_label, 0, 1, 5, 6)

    self.gradient_width_spinner = self.make_spinner(0.0, 0.0, 262144.0, 1.0, 10.0, 1, False)
    if self.parasitedata:
      self.gradient_width_spinner["adj"].set_value(self.parasitedata["width"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.gradient_width_spinner["adj"].set_value(shelf[self.shelfkey]["width"])
    self.gradient_width_label.set_mnemonic_widget(self.gradient_width_spinner["spinner"])
    self.fill_type_table.attach(self.gradient_width_spinner["spinner"], 1, 2, 5, 6)
    self.gradient_width_spinner["adj"].connect("value-changed", self.preview)

    self.pattern_label = self.make_label("_Pattern:", False)
    self.fill_type_table.attach(self.pattern_label, 0, 1, 0, 1)

    self.pattern_button = gimpui.PatternSelector()
    if self.parasitedata:
      self.pattern_button.set_pattern(self.parasitedata["pattern"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.pattern_button.set_pattern(shelf[self.shelfkey]["pattern"])
    self.pattern_label.set_mnemonic_widget(self.pattern_button)
    self.fill_type_table.attach(self.pattern_button, 1, 3, 0, 1)
    self.pattern_button.connect("pattern-set", self.preview)

    self.pattern_scale_label = self.make_label("Scale:", False)
    self.fill_type_table.attach(self.pattern_scale_label, 0, 1, 1, 2)

    self.pattern_scale_slider = self.make_slider_and_spinner(100.0, 1.0, 1000.0, 1.0, 10.0, 1, False)
    if self.parasitedata:
      self.pattern_scale_slider["adj"].set_value(self.parasitedata["scale"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.pattern_scale_slider["adj"].set_value(shelf[self.shelfkey]["scale"])
    self.pattern_scale_label.set_mnemonic_widget(self.pattern_scale_slider["spinner"])
    self.fill_type_table.attach(self.pattern_scale_slider["slider"], 1, 3, 1, 2)
    self.fill_type_table.attach(self.pattern_scale_slider["spinner"], 3, 4, 1, 2)
    self.pattern_scale_slider["adj"].connect("value-changed", self.preview)

    self.pattern_interpolation_type_label = self.make_label("Interpolation:", False)
    self.fill_type_table.attach(self.pattern_interpolation_type_label, 0, 1, 2, 3)

    self.pattern_interpolation_type_box = self.make_interpolation_box()
    if self.parasitedata:
      self.pattern_interpolation_type_box.set_active(self.parasitedata["interpolation_type"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.pattern_interpolation_type_box.set_active(shelf[self.shelfkey]["interpolation_type"])
    else:
      self.pattern_interpolation_type_box.set_active(0)
    self.pattern_interpolation_type_label.set_mnemonic_widget(self.pattern_interpolation_type_box)
    self.fill_type_table.attach(self.pattern_interpolation_type_box, 1, 4, 2, 3)
    self.pattern_interpolation_type_box.connect("changed", self.preview)

    self.fill_type_hbox.pack_start(self.fill_type_table, True, True, 0)
    self.fill_type_vbox.pack_start(self.fill_type_hbox, True, True, 0)
    self.fill_type_hbox.show()

    if self.parasitedata:
      if self.parasitedata["filltype"] == 0:
        self.fill_type_color_radio.set_active(True)
      elif self.parasitedata["filltype"] == 1:
        self.fill_type_gradient_radio.set_active(True)
      elif self.parasitedata["filltype"] == 2:
        self.fill_type_pattern_radio.set_active(True)
    elif shelf.has_key(self.shelfkey) == 1:
      if shelf[self.shelfkey]["filltype"] == 0:
        self.fill_type_color_radio.set_active(True)
      elif shelf[self.shelfkey]["filltype"] == 1:
        self.fill_type_gradient_radio.set_active(True)
      elif shelf[self.shelfkey]["filltype"] == 2:
        self.fill_type_pattern_radio.set_active(True)
    else:
      self.fill_type_color_radio.set_active(True)

    self.merge_check = gtk.CheckButton("_Merge with layer")
    if self.parasitedata:
      if self.parasitedata["merge"] == 1:
        self.merge_check.set_active(True)
    elif shelf.has_key(self.shelfkey) == 1 and shelf[self.shelfkey]["merge"] == 1:
      self.merge_check.set_active(True)
    self.merge_check.show()
    self.merge_check.connect("toggled", self.preview)

    self.preview_check = gtk.CheckButton("_Preview")
    self.preview_check.show()
    self.preview_check.connect("toggled", self.preview)

    self.dialog.vbox.hbox1 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox1.show()
    self.dialog.vbox.pack_start(self.dialog.vbox.hbox1, True, True, 7)
    self.dialog.vbox.hbox1.pack_start(self.table, True, True, 7)
    self.dialog.vbox.hbox2 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox2.show()
    self.dialog.vbox.add(self.dialog.vbox.hbox2)
    self.dialog.vbox.hbox2.pack_start(self.fill_type_frame, True, True, 7)
    self.dialog.vbox.hbox3 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox3.show()
    self.dialog.vbox.add(self.dialog.vbox.hbox3)
    self.dialog.vbox.hbox3.pack_start(self.merge_check, True, True, 10)
    self.dialog.vbox.hbox4 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox4.show()
    self.dialog.vbox.add(self.dialog.vbox.hbox4)
    self.dialog.vbox.hbox4.pack_start(self.preview_check, True, True, 10)

    self.makeDialogButtons()

    self.dialog.show()
    self.dialog.run()
    self.removePreviews()

  def okbutton(self, widget):
    if self.layer_exists(self.previewLayer):
      self.img.remove_layer(self.previewLayer)
      self.previewLayer = None
    self.unset_hidden_layer()
    if pdb.gimp_image_undo_is_enabled(self.img) == 0:
      pdb.gimp_image_undo_thaw(self.img)
    if self.fill_type_color_radio.get_active():
      fill_type = 0
    elif self.fill_type_gradient_radio.get_active():
      fill_type = 1
    elif self.fill_type_pattern_radio.get_active():
      fill_type = 2
    params = {
      "filltype":           fill_type,
      "color":              self.color_button.get_color(),
      "gradient":           self.gradient_button.get_gradient(),
      "gradienttype":       self.gradient_type_box.get_active(),
      "repeat":             self.gradient_repeat_box.get_active(),
      "reverse":            self.cond(self.gradient_reverse_check.get_active()),
      "centerx":            self.gradient_centerx_spinner["adj"].get_value(),
      "centery":            self.gradient_centery_spinner["adj"].get_value(),
      "angle":              self.gradient_angle_slider["adj"].get_value(),
      "width":              self.gradient_width_spinner["adj"].get_value(),
      "pattern":            self.pattern_button.get_pattern(),
      "scale":              self.pattern_scale_slider["adj"].get_value(),
      "interpolation_type": self.pattern_interpolation_type_box.get_active(),
      "opacity":            self.opacity_slider["adj"].get_value(),
      "mode":               self.mode_box.get_active(),
      "size":               int(round(self.size_slider["adj"].get_value())),
      "position":           self.position_slider["adj"].get_value(),
      "merge":              self.cond(self.merge_check.get_active())
    }
    if self.parasitedata:
      self.img.remove_layer(self.parasitedata["oldid"])
    if params["filltype"] == 0:
      fill = params["color"]
    elif params["filltype"] == 1:
      fill = (
        params["gradient"],
        params["gradienttype"],
        params["repeat"],
        params["reverse"],
        params["centerx"],
        params["centery"],
        params["angle"],
        params["width"]
      )
    elif params["filltype"] == 2:
      fill = (
        params["pattern"],
        params["scale"],
        params["interpolation_type"]
      )
    fxlayer = self.makeStroke(
      self.img,
      self.drawable,
      fill,
      params["opacity"],
      params["mode"],
      params["size"],
      params["position"],
      params["merge"]
    )
    if params["merge"] == 0:
      self.writeParasite(self.drawable, fxlayer,
        ("radio",    (self.fill_type_color_radio, self.fill_type_gradient_radio, self.fill_type_pattern_radio)),
        ("color",    self.color_button),
        ("gradient", self.gradient_button),
        ("combobox", self.gradient_type_box),
        ("combobox", self.gradient_repeat_box),
        ("check",    self.gradient_reverse_check),
        ("floatadj", self.gradient_centerx_spinner["adj"]),
        ("floatadj", self.gradient_centery_spinner["adj"]),
        ("floatadj", self.gradient_angle_slider["adj"]),
        ("floatadj", self.gradient_width_spinner["adj"]),
        ("pattern",  self.pattern_button),
        ("floatadj", self.pattern_scale_slider["adj"]),
        ("combobox", self.pattern_interpolation_type_box),
        ("floatadj", self.opacity_slider["adj"]),
        ("modebox",  self.mode_box),
        ("intadj",   self.size_slider["adj"]),
        ("floatadj", self.position_slider["adj"]),
        ("check",    self.merge_check)
      )
    shelf[self.shelfkey] = params

  def resetbutton(self, widget):
    self.fill_type_color_radio.set_active(True)
    self.color_button.set_color(gimpcolor.RGB(255, 0, 0, 255))
    self.gradient_button.set_gradient("FG to BG (RGB)")
    self.gradient_type_box.set_active(0)
    self.gradient_repeat_box.set_active(0)
    self.gradient_reverse_check.set_active(False)
    self.gradient_centerx_spinner["adj"].set_value(0.0)
    self.gradient_centery_spinner["adj"].set_value(0.0)
    self.gradient_angle_slider["adj"].set_value(90.0)
    self.gradient_width_spinner["adj"].set_value(0.0)
    self.pattern_button.set_pattern(pdb.gimp_context_get_pattern())
    self.pattern_scale_slider["adj"].set_value(100.0)
    self.pattern_interpolation_type_box.set_active(0)
    self.size_slider["adj"].set_value(3)
    self.position_slider["adj"].set_value(50.0)
    self.mode_box.set_active(NORMAL_MODE)
    self.opacity_slider["adj"].set_value(100.0)
    self.merge_check.set_active(False)

  def readParasite(self, img, drawable):
    return layerfx_base.readParasite(self, img, drawable,
      layerfx_stroke.shelfkey,
      ("filltype",           "int"),
      ("color",              "color"),
      ("gradient",           "string"),
      ("gradienttype",       "int"),
      ("repeat",             "int"),
      ("reverse",            "int"),
      ("centerx",            "float"),
      ("centery",            "float"),
      ("angle",              "float"),
      ("width",              "float"),
      ("pattern",            "string"),
      ("scale",              "float"),
      ("interpolation_type", "int"),
      ("opacity",            "float"),
      ("mode",               "int"),
      ("size",               "int"),
      ("position",           "float"),
      ("merge",              "int")
    )

  def preview(self, widget, *extra):
    if self.layer_exists(self.previewLayer):
      self.img.remove_layer(self.previewLayer)
    if self.preview_check.get_active():
      self.unset_hidden_layer()
      if pdb.gimp_image_undo_is_enabled(self.img) == 1:
        pdb.gimp_image_undo_freeze(self.img)
      if self.parasitedata:
        self.set_hidden_layer(self.parasitedata["oldid"])
      if self.merge_check.get_active():
        self.previewLayer = self.drawable.copy()
        self.add_over_layer(self.previewLayer, self.drawable)
        self.set_hidden_layer(self.drawable)
        layer = self.previewLayer
        merge = 1
      else:
        layer = self.drawable
        merge = 0
      if self.fill_type_color_radio.get_active():
        fill = self.color_button.get_color()
      elif self.fill_type_gradient_radio.get_active():
        fill = (
          self.gradient_button.get_gradient(),
          self.gradient_type_box.get_active(),
          self.gradient_repeat_box.get_active(),
          self.cond(self.gradient_reverse_check.get_active()),
          self.gradient_centerx_spinner["adj"].get_value(),
          self.gradient_centery_spinner["adj"].get_value(),
          self.gradient_angle_slider["adj"].get_value(),
          self.gradient_width_spinner["adj"].get_value()
        )
      elif self.fill_type_pattern_radio.get_active():
        fill = (
          self.pattern_button.get_pattern(),
          self.pattern_scale_slider["adj"].get_value(),
          self.pattern_interpolation_type_box.get_active()
        )
      self.previewLayer = self.makeStroke(
        self.img,
        layer,
        fill,
        self.opacity_slider["adj"].get_value(),
        self.mode_box.get_active(),
        int(round(self.size_slider["adj"].get_value())),
        self.position_slider["adj"].get_value(),
        merge
      )
    else:
      gimp.displays_flush()

  def switch_fill_panes(self, widget, *extra):
    color_widgets = (self.color_label, self.color_button)
    gradient_widgets = (
      self.gradient_label,
      self.gradient_button,
      self.gradient_type_label,
      self.gradient_type_box,
      self.gradient_repeat_label,
      self.gradient_repeat_box,
      self.gradient_reverse_check,
      self.gradient_center_label,
      self.gradient_centerx_spinner["spinner"],
      self.gradient_centery_spinner["spinner"],
      self.gradient_angle_label,
      self.gradient_angle_slider["slider"],
      self.gradient_angle_slider["spinner"],
      self.gradient_width_label,
      self.gradient_width_spinner["spinner"]
    )
    pattern_widgets = (
      self.pattern_label,
      self.pattern_button,
      self.pattern_scale_label,
      self.pattern_scale_slider["slider"],
      self.pattern_scale_slider["spinner"],
      self.pattern_interpolation_type_label,
      self.pattern_interpolation_type_box
    )
    if widget.get_active():
      if widget == self.fill_type_color_radio:
        for i in gradient_widgets:
          i.hide()
        for i in pattern_widgets:
          i.hide()
        for i in color_widgets:
          i.show()
      elif widget == self.fill_type_gradient_radio:
        for i in color_widgets:
          i.hide()
        for i in pattern_widgets:
          i.hide()
        for i in gradient_widgets:
          i.show()
      elif widget == self.fill_type_pattern_radio:
        for i in color_widgets:
          i.hide()
        for i in gradient_widgets:
          i.hide()
        for i in pattern_widgets:
          i.show()
      if hasattr(self, "preview_check"):
        self.preview(widget, *extra)

  def makeStroke(self, img, drawable, fill, opacity, mode, size, position, merge):
    pdb.gimp_image_undo_group_start(img)
    origselection = pdb.gimp_selection_save(img)
    if position == 0:
      strokelayer = gimp.Layer(img, "%s-stroke" % (drawable.name), drawable.width, drawable.height, (RGBA_IMAGE, GRAYA_IMAGE)[img.base_type], opacity, mode)
      self.add_over_layer(strokelayer, drawable)
      strokelayer.set_offsets(drawable.offsets[0], drawable.offsets[1])
    elif position == 100:
      growamt = int(round(size * 1.2))
      strokelayer = gimp.Layer(img, "%s-stroke" % (drawable.name), drawable.width + (growamt * 2), drawable.height + (growamt * 2), (RGBA_IMAGE, GRAYA_IMAGE)[img.base_type], opacity, mode)
      self.add_under_layer(strokelayer, drawable)
      strokelayer.set_offsets(drawable.offsets[0] - growamt, drawable.offsets[1] - growamt)
    else:
      outerwidth = int(round((position / 100.0) * size))
      innerwidth = size - outerwidth
      growamt = int(round(outerwidth * 1.2))
      strokelayer = gimp.Layer(img, "%s-stroke" % (drawable.name), drawable.width + (growamt * 2), drawable.height + (growamt * 2), (RGBA_IMAGE, GRAYA_IMAGE)[img.base_type], opacity, mode)
      self.add_over_layer(strokelayer, drawable)
      strokelayer.set_offsets(drawable.offsets[0] - growamt, drawable.offsets[1] - growamt)
    pdb.gimp_selection_none(img)
    pdb.gimp_edit_clear(strokelayer)
    pdb.gimp_selection_layer_alpha(drawable)
    if drawable.mask != None:
      pdb.gimp_selection_combine(drawable.mask, CHANNEL_OP_INTERSECT)
    alphaSel = pdb.gimp_selection_save(img)
    if position == 0:
      pdb.gimp_selection_shrink(img, size)
    elif position != 100:
      pdb.gimp_selection_shrink(img, innerwidth)
    innerSel = pdb.gimp_selection_save(img)
    if position == 0:
      if merge == 1:
        origmask = drawable.mask
        layername = drawable.name
        if origmask != None:
          origmask = drawable.mask.copy()
          drawable.remove_mask(MASK_DISCARD)
        alphamask = drawable.create_mask(ADD_ALPHA_TRANSFER_MASK)
        pdb.gimp_selection_none(img)
        pdb.gimp_threshold(alphaSel, 1, 255)
      pdb.gimp_selection_load(alphaSel)
      pdb.gimp_selection_combine(innerSel, CHANNEL_OP_SUBTRACT)
    elif position == 100:
      pdb.gimp_selection_none(img)
      pdb.gimp_threshold(innerSel, 255, 255)
      pdb.gimp_selection_load(alphaSel)
      pdb.gimp_selection_grow(img, size)
      pdb.gimp_selection_combine(innerSel, CHANNEL_OP_SUBTRACT)
    else:
      pdb.gimp_selection_load(alphaSel)
      pdb.gimp_selection_grow(img, outerwidth)
      pdb.gimp_selection_combine(innerSel, CHANNEL_OP_SUBTRACT)
    if type(fill) == gimpcolor.RGB:
      origfgcolor = gimp.get_foreground()
      gimp.set_foreground(fill)
      pdb.gimp_edit_fill(strokelayer, FOREGROUND_FILL)
      gimp.set_foreground(origfgcolor)
    elif (type(fill) == tuple or type(fill) == list) and len(fill) == 8:
      origgradient = pdb.gimp_context_get_gradient()
      measures = self.getGradientMeasurements(drawable.offsets[0], drawable.offsets[1], fill[1], fill[4], fill[5], fill[6], fill[7])
      pdb.gimp_context_set_gradient(fill[0])
      pdb.gimp_edit_blend(strokelayer, CUSTOM_MODE, NORMAL_MODE, fill[1], 100.0, 1.0, fill[2], fill[3], False, 1, 0.0, False, measures["start"][0], measures["start"][1], measures["end"][0], measures["end"][1])
      pdb.gimp_context_set_gradient(origgradient)
    elif (type(fill) == tuple or type(fill) == list) and len(fill) == 3:
      img.remove_channel(innerSel)
      innerSel = pdb.gimp_selection_save(img)
      pdb.gimp_selection_none(img)
      origpattern = pdb.gimp_context_get_pattern()
      if fill[1] == 100.0:
        layerwidth = drawable.width
        layerheight = drawable.height
      else:
        layerwidth = int(round(drawable.width/(fill[1]/100.0)))
        layerheight = int(round(drawable.height/(fill[1]/100.0)))
      pdb.gimp_layer_resize(strokelayer, layerwidth, layerheight, 0, 0)
      pdb.gimp_context_set_pattern(fill[0])
      pdb.gimp_edit_fill(strokelayer, PATTERN_FILL)
      pdb.gimp_context_set_pattern(origpattern)
      if fill[1] != 100.0:
        pdb.gimp_drawable_transform_scale(strokelayer, drawable.offsets[0], drawable.offsets[1], drawable.offsets[0] + drawable.width, drawable.offsets[1] + drawable.height, TRANSFORM_FORWARD, fill[2], 1, 3, TRANSFORM_RESIZE_ADJUST)
      pdb.gimp_selection_load(innerSel)
      pdb.gimp_selection_invert(img)
      pdb.gimp_edit_clear(strokelayer)
    if merge == 1:
      if position == 0:
        strokelayer = pdb.gimp_image_merge_down(img, strokelayer, EXPAND_AS_NECESSARY)
        strokelayer.name = layername
        strokelayer.add_mask(alphamask)
        strokelayer.remove_mask(MASK_APPLY)
        if origmask != None:
          strokelayer.add_mask(origmask)
      elif position == 100:
        origmask = drawable.mask
        layername = drawable.name
        if origmask != None:
          drawable.remove_mask(MASK_APPLY)
        strokelayer = pdb.gimp_image_merge_down(img, drawable, EXPAND_AS_NECESSARY)
        strokelayer.name = layername
      else:
        origmask = drawable.mask
        layername = drawable.name
        if origmask != None:
          drawable.remove_mask(MASK_APPLY)
        strokelayer = pdb.gimp_image_merge_down(img, strokelayer, EXPAND_AS_NECESSARY)
        strokelayer.name = layername
    else:
      pdb.gimp_image_set_active_layer(img, drawable)
    pdb.gimp_selection_load(origselection)
    img.remove_channel(alphaSel)
    img.remove_channel(innerSel)
    img.remove_channel(origselection)
    gimp.displays_flush()
    pdb.gimp_image_undo_group_end(img)
    return strokelayer

class layerfx_color_overlay(layerfx_base):
  shelfkey = "layerfx-color-overlay"

  def __init__(self, runmode, img, drawable, color, opacity, mode, merge):
    self.origMsgHandler = pdb.gimp_message_get_handler()
    pdb.gimp_message_set_handler(ERROR_CONSOLE)
    self.img = img
    self.drawable = drawable
    if runmode == RUN_INTERACTIVE:
      self.showDialog()
    elif runmode == RUN_NONINTERACTIVE:
      if self.validatedata(img, drawable,
        ("color",   color),
        ("percent", opacity),
        ("mode",    mode),
        ("boolean", merge)
      ):
        self.removeOldLayer()
        fxlayer = self.makeOverlay(img, drawable, color, opacity, mode, merge)
        if merge == 0:
          self.writeParasiteRaw(drawable, fxlayer, color, opacity, mode, merge)
        shelf[self.shelfkey] = {
          "color":   color,
          "opacity": opacity,
          "mode":    mode,
          "merge":   merge
        }
    elif runmode == RUN_WITH_LAST_VALS and shelf.has_key(self.shelfkey) == 1:
      self.removeOldLayer()
      fxlayer = self.makeOverlay(
        img,
        drawable,
        shelf[self.shelfkey]["color"],
        shelf[self.shelfkey]["opacity"],
        shelf[self.shelfkey]["mode"],
        shelf[self.shelfkey]["merge"]
      )
      if shelf[self.shelfkey]["merge"] == 0:
        self.writeParasiteRaw(drawable, fxlayer,
          shelf[self.shelfkey]["color"],
          shelf[self.shelfkey]["opacity"],
          shelf[self.shelfkey]["mode"],
          shelf[self.shelfkey]["merge"]
        )
    else:
      pdb.gimp_message("unknown runmode")
    pdb.gimp_message_set_handler(self.origMsgHandler)

  def showDialog(self):
    self.dialog = gimpui.Dialog("Color Overlay", "coloroverlaydialog")

    self.parasitedata = self.readParasite(self.img, self.drawable)

    self.table = gtk.Table(3, 5, True)
    self.table.set_homogeneous(True)
    self.table.set_row_spacings(3)
    self.table.set_col_spacings(3)
    self.table.show()

    self.color_label = self.make_label("_Color:")
    self.table.attach(self.color_label, 0, 1, 0, 1)

    self.color_button = gimpui.ColorButton("Overlay Color", 10, 10, gimpcolor.RGB(255, 255, 255, 255))
    if self.parasitedata:
      self.color_button.set_color(self.parasitedata["color"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.color_button.set_color(shelf[self.shelfkey]["color"])
    self.color_label.set_mnemonic_widget(self.color_button)
    self.color_button.show()
    self.table.attach(self.color_button, 1, 2, 0, 1)
    self.color_button.connect("color-changed", self.preview)

    self.mode_label = self.make_label("_Blend Mode:")
    self.table.attach(self.mode_label, 0, 1, 1, 2)

    self.mode_box = self.make_blend_mode_box()
    if self.parasitedata:
      self.mode_box.set_active(self.parasitedata["mode"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.mode_box.set_active(shelf[self.shelfkey]["mode"])
    else:
      self.mode_box.set_active(NORMAL_MODE)
    self.mode_label.set_mnemonic_widget(self.mode_box)
    self.mode_box.show()
    self.table.attach(self.mode_box, 1, 5, 1, 2)
    self.mode_box.connect("changed", self.preview)

    self.opacity_label = self.make_label("_Opacity:")
    self.table.attach(self.opacity_label, 0, 1, 2, 3)

    self.opacity_slider = self.make_slider_and_spinner(75.0, 0.0, 100.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.opacity_slider["adj"].set_value(self.parasitedata["opacity"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.opacity_slider["adj"].set_value(shelf[self.shelfkey]["opacity"])
    else:
      self.opacity_slider["adj"].set_value(100.0)
    self.opacity_label.set_mnemonic_widget(self.opacity_slider["spinner"])
    self.table.attach(self.opacity_slider["slider"], 1, 4, 2, 3)
    self.table.attach(self.opacity_slider["spinner"], 4, 5, 2, 3)
    self.opacity_slider["adj"].connect("value-changed", self.preview)

    self.merge_check = gtk.CheckButton("_Merge with layer")
    if self.parasitedata:
      if self.parasitedata["merge"] == 1:
        self.merge_check.set_active(True)
    elif shelf.has_key(self.shelfkey) == 1 and shelf[self.shelfkey]["merge"] == 1:
      self.merge_check.set_active(True)
    self.merge_check.show()
    self.merge_check.connect("toggled", self.preview)

    self.preview_check = gtk.CheckButton("_Preview")
    self.preview_check.show()
    self.preview_check.connect("toggled", self.preview)

    self.dialog.vbox.hbox1 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox1.show()
    self.dialog.vbox.pack_start(self.dialog.vbox.hbox1, True, True, 7)
    self.dialog.vbox.hbox1.pack_start(self.table, True, True, 7)
    self.dialog.vbox.hbox2 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox2.show()
    self.dialog.vbox.add(self.dialog.vbox.hbox2)
    self.dialog.vbox.hbox2.pack_start(self.merge_check, True, True, 10)
    self.dialog.vbox.hbox3 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox3.show()
    self.dialog.vbox.add(self.dialog.vbox.hbox3)
    self.dialog.vbox.hbox3.pack_start(self.preview_check, True, True, 10)

    self.makeDialogButtons()

    self.dialog.show()
    self.dialog.run()
    self.removePreviews()

  def okbutton(self, widget):
    if self.layer_exists(self.previewLayer):
      self.img.remove_layer(self.previewLayer)
      self.previewLayer = None
    self.unset_hidden_layer()
    if pdb.gimp_image_undo_is_enabled(self.img) == 0:
      pdb.gimp_image_undo_thaw(self.img)
    params = {
      "color":   self.color_button.get_color(),
      "opacity": self.opacity_slider["adj"].get_value(),
      "mode":    self.mode_box.get_active(),
      "merge":   self.cond(self.merge_check.get_active())
    }
    if self.parasitedata:
      self.img.remove_layer(self.parasitedata["oldid"])
    fxlayer = self.makeOverlay(
      self.img,
      self.drawable,
      params["color"],
      params["opacity"],
      params["mode"],
      params["merge"]
    )
    if params["merge"] == 0:
      self.writeParasite(self.drawable, fxlayer,
        ("color", self.color_button),
        ("floatadj", self.opacity_slider["adj"]),
        ("modebox", self.mode_box),
        ("check", self.merge_check)
      )
    shelf[self.shelfkey] = params

  def resetbutton(self, widget):
    self.color_button.set_color(gimpcolor.RGB(255, 255, 255, 255))
    self.mode_box.set_active(NORMAL_MODE)
    self.opacity_slider["adj"].set_value(100.0)
    self.merge_check.set_active(False)

  def readParasite(self, img, drawable):
    return layerfx_base.readParasite(self, img, drawable,
      layerfx_color_overlay.shelfkey,
      ("color",   "color"),
      ("opacity", "float"),
      ("mode",    "int"),
      ("merge",   "int")
    )

  def preview(self, widget):
    if self.layer_exists(self.previewLayer):
      self.img.remove_layer(self.previewLayer)
    if self.preview_check.get_active():
      self.unset_hidden_layer()
      if pdb.gimp_image_undo_is_enabled(self.img) == 1:
        pdb.gimp_image_undo_freeze(self.img)
      if self.parasitedata:
        self.set_hidden_layer(self.parasitedata["oldid"])
      if self.merge_check.get_active():
        self.previewLayer = self.drawable.copy()
        self.add_over_layer(self.previewLayer, self.drawable)
        self.set_hidden_layer(self.drawable)
        layer = self.previewLayer
        merge = 1
      else:
        layer = self.drawable
        merge = 0
      self.previewLayer = self.makeOverlay(
        self.img,
        layer,
        self.color_button.get_color(),
        self.opacity_slider["adj"].get_value(),
        self.mode_box.get_active(),
        merge
      )
    else:
      gimp.displays_flush()

  def makeOverlay(self, img, drawable, color, opacity, mode, merge):
    pdb.gimp_image_undo_group_start(img)
    origfgcolor = gimp.get_foreground()
    origselection = pdb.gimp_selection_save(img)
    colorlayer = gimp.Layer(img, "%s-color" % (drawable.name), drawable.width, drawable.height, (RGBA_IMAGE, GRAYA_IMAGE)[img.base_type], opacity, mode)
    self.add_over_layer(colorlayer, drawable)
    colorlayer.set_offsets(drawable.offsets[0], drawable.offsets[1])
    pdb.gimp_selection_none(img)
    gimp.set_foreground(color)
    pdb.gimp_edit_fill(colorlayer, FOREGROUND_FILL)
    if merge == 1:
      origmask = drawable.mask
      layername = drawable.name
      if origmask != None:
        origmask = drawable.mask.copy()
        drawable.remove_mask(MASK_DISCARD)
      alphamask = drawable.create_mask(ADD_ALPHA_TRANSFER_MASK)
      colorlayer = pdb.gimp_image_merge_down(img, colorlayer, EXPAND_AS_NECESSARY)
      colorlayer.name = layername
      colorlayer.add_mask(alphamask)
      colorlayer.remove_mask(MASK_APPLY)
      if origmask != None:
        colorlayer.add_mask(origmask)
    else:
      pdb.gimp_selection_layer_alpha(drawable)
      if drawable.mask != None:
        pdb.gimp_selection_combine(drawable.mask, CHANNEL_OP_INTERSECT)
      alphamask = colorlayer.create_mask(ADD_SELECTION_MASK)
      colorlayer.add_mask(alphamask)
      colorlayer.remove_mask(MASK_APPLY)
      pdb.gimp_image_set_active_layer(img, drawable)
    gimp.set_foreground(origfgcolor)
    pdb.gimp_selection_load(origselection)
    img.remove_channel(origselection)
    gimp.displays_flush()
    pdb.gimp_image_undo_group_end(img)
    return colorlayer

class layerfx_gradient_overlay(layerfx_base):
  shelfkey = "layerfx-gradient-overlay"

  def __init__(self, runmode, img, drawable, gradient, gradienttype, repeat, reverse, opacity, mode, centerx, centery, angle, width, merge):
    self.origMsgHandler = pdb.gimp_message_get_handler()
    pdb.gimp_message_set_handler(ERROR_CONSOLE)
    self.img = img
    self.drawable = drawable
    if runmode == RUN_INTERACTIVE:
      self.showDialog()
    elif runmode == RUN_NONINTERACTIVE:
      if self.validatedata(img, drawable,
        ("gradient",   gradient),
        ("intrange",   gradienttype, 0, 10),
        ("intrange",   repeat, 0, 2),
        ("boolean",    reverse),
        ("percent",    opacity),
        ("mode",       mode),
        ("floatrange", centerx, 0.0, img.width),
        ("floatrange", centery, 0.0, img.height),
        ("angle",      angle),
        ("floatrange", width, 0.0, 262144.0),
        ("boolean",    merge)
      ):
        self.removeOldLayer()
        fxlayer = self.makeOverlay(img, drawable, gradient, gradienttype, repeat, reverse, opacity, mode, centerx, centery, angle, width, merge)
        if merge == 0:
          self.writeParasiteRaw(drawable, fxlayer, gradient, gradienttype, repeat, reverse, opacity, mode, centerx, centery, angle, width, merge)
        shelf[self.shelfkey] = {
          "gradient":     gradient,
          "gradienttype": gradienttype,
          "repeat":       repeat,
          "reverse":      reverse,
          "opacity":      opacity,
          "mode":         mode,
          "centerx":      centerx,
          "centery":      centery,
          "angle":        angle,
          "width":        width,
          "merge":        merge
        }
    elif runmode == RUN_WITH_LAST_VALS and shelf.has_key(self.shelfkey) == 1:
      self.removeOldLayer()
      fxlayer = self.makeOverlay(
        img,
        drawable,
        shelf[self.shelfkey]["gradient"],
        shelf[self.shelfkey]["gradienttype"],
        shelf[self.shelfkey]["repeat"],
        shelf[self.shelfkey]["reverse"],
        shelf[self.shelfkey]["opacity"],
        shelf[self.shelfkey]["mode"],
        shelf[self.shelfkey]["centerx"],
        shelf[self.shelfkey]["centery"],
        shelf[self.shelfkey]["angle"],
        shelf[self.shelfkey]["width"],
        shelf[self.shelfkey]["merge"]
      )
      if shelf[self.shelfkey]["merge"] == 0:
        self.writeParasiteRaw(drawable, fxlayer,
          shelf[self.shelfkey]["gradient"],
          shelf[self.shelfkey]["gradienttype"],
          shelf[self.shelfkey]["repeat"],
          shelf[self.shelfkey]["reverse"],
          shelf[self.shelfkey]["opacity"],
          shelf[self.shelfkey]["mode"],
          shelf[self.shelfkey]["centerx"],
          shelf[self.shelfkey]["centery"],
          shelf[self.shelfkey]["angle"],
          shelf[self.shelfkey]["width"],
          shelf[self.shelfkey]["merge"]
        )
    else:
      pdb.gimp_message("unknown runmode")
    pdb.gimp_message_set_handler(self.origMsgHandler)

  def showDialog(self):
    self.dialog = gimpui.Dialog("Gradient Overlay", "gradientoverlaydialog")

    self.parasitedata = self.readParasite(self.img, self.drawable)

    self.table = gtk.Table(8, 4, True)
    self.table.set_homogeneous(True)
    self.table.set_row_spacings(3)
    self.table.set_col_spacings(3)
    self.table.show()

    self.gradient_label = self.make_label("_Gradient:")
    self.table.attach(self.gradient_label, 0, 1, 0, 1)

    self.gradient_button = gimpui.GradientSelector()
    if self.parasitedata:
      self.gradient_button.set_gradient(self.parasitedata["gradient"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.gradient_button.set_gradient(shelf[self.shelfkey]["gradient"])
    self.gradient_label.set_mnemonic_widget(self.gradient_button)
    self.gradient_button.show()
    self.table.attach(self.gradient_button, 1, 2, 0, 1)
    self.gradient_button.connect("gradient-set", self.preview)

    self.gradienttype_label = self.make_label("Gradient _Type:")
    self.table.attach(self.gradienttype_label, 0, 1, 1, 2)

    self.gradienttype_box = self.make_gradient_type_box()
    if self.parasitedata:
      self.gradienttype_box.set_active(self.parasitedata["gradienttype"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.gradienttype_box.set_active(shelf[self.shelfkey]["gradienttype"])
    else:
      self.gradienttype_box.set_active(0)
    self.gradienttype_box.show()
    self.gradienttype_label.set_mnemonic_widget(self.gradienttype_box)
    self.table.attach(self.gradienttype_box, 1, 4, 1, 2)
    self.gradienttype_box.connect("changed", self.preview)

    self.repeat_label = self.make_label("Repeat:")
    self.table.attach(self.repeat_label, 0, 1, 2, 3)

    self.repeat_box = self.make_gradient_repeat_box()
    if self.parasitedata:
      self.repeat_box.set_active(self.parasitedata["repeat"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.repeat_box.set_active(shelf[self.shelfkey]["repeat"])
    else:
      self.repeat_box.set_active(0)
    self.repeat_box.show()
    self.repeat_label.set_mnemonic_widget(self.repeat_box)
    self.table.attach(self.repeat_box, 1, 4, 2, 3)
    self.repeat_box.connect("changed", self.preview)

    self.reverse_check = gtk.CheckButton("_Reverse")
    if self.parasitedata:
      if self.parasitedata["reverse"] == 1:
        self.reverse_check.set_active(True)
    elif shelf.has_key(self.shelfkey) == 1 and shelf[self.shelfkey]["reverse"] == 1:
      self.reverse_check.set_active(True)
    self.reverse_check.show()
    self.table.attach(self.reverse_check, 2, 3, 0, 1)
    self.reverse_check.connect("toggled", self.preview)

    self.mode_label = self.make_label("_Blend Mode:")
    self.table.attach(self.mode_label, 0, 1, 3, 4)

    self.mode_box = self.make_blend_mode_box()
    if self.parasitedata:
      self.mode_box.set_active(self.parasitedata["mode"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.mode_box.set_active(shelf[self.shelfkey]["mode"])
    else:
      self.mode_box.set_active(NORMAL_MODE)
    self.mode_label.set_mnemonic_widget(self.mode_box)
    self.mode_box.show()
    self.table.attach(self.mode_box, 1, 4, 3, 4)
    self.mode_box.connect("changed", self.preview)

    self.opacity_label = self.make_label("_Opacity:")
    self.table.attach(self.opacity_label, 0, 1, 4, 5)

    self.opacity_slider = self.make_slider_and_spinner(100.0, 0.0, 100.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.opacity_slider["adj"].set_value(self.parasitedata["opacity"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.opacity_slider["adj"].set_value(shelf[self.shelfkey]["opacity"])
    self.opacity_label.set_mnemonic_widget(self.opacity_slider["spinner"])
    self.table.attach(self.opacity_slider["slider"], 1, 3, 4, 5)
    self.table.attach(self.opacity_slider["spinner"], 3, 4, 4, 5)
    self.opacity_slider["adj"].connect("value-changed", self.preview)

    self.center_label = self.make_label("_Center:")
    self.table.attach(self.center_label, 0, 1, 5, 6)

    self.centerx_spinner = self.make_spinner(0.0, 0.0, self.img.width, 1.0, 10.0, 1)
    if self.parasitedata:
      self.centerx_spinner["adj"].set_value(self.parasitedata["centerx"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.centerx_spinner["adj"].set_value(shelf[self.shelfkey]["centerx"])
    self.center_label.set_mnemonic_widget(self.centerx_spinner["spinner"])
    self.table.attach(self.centerx_spinner["spinner"], 1, 2, 5, 6)
    self.centerx_spinner["adj"].connect("value-changed", self.preview)

    self.centery_spinner = self.make_spinner(0.0, 0.0, self.img.height, 1.0, 10.0, 1)
    if self.parasitedata:
      self.centery_spinner["adj"].set_value(self.parasitedata["centery"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.centery_spinner["adj"].set_value(shelf[self.shelfkey]["centery"])
    self.table.attach(self.centery_spinner["spinner"], 2, 3, 5, 6)
    self.centery_spinner["adj"].connect("value-changed", self.preview)

    self.angle_label = self.make_label("_Gradient Angle:")
    self.table.attach(self.angle_label, 0, 1, 6, 7)

    self.angle_slider = self.make_slider_and_spinner(90.0, -180.0, 180.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.angle_slider["adj"].set_value(self.parasitedata["angle"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.angle_slider["adj"].set_value(shelf[self.shelfkey]["angle"])
    self.angle_label.set_mnemonic_widget(self.angle_slider["spinner"])
    self.table.attach(self.angle_slider["slider"], 1, 3, 6, 7)
    self.table.attach(self.angle_slider["spinner"], 3, 4, 6, 7)
    self.angle_slider["adj"].connect("value-changed", self.preview)

    self.width_label = self.make_label("_Gradient Width:")
    self.table.attach(self.width_label, 0, 1, 7, 8)

    self.width_spinner = self.make_spinner(0.0, 0.0, 262144.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.width_spinner["adj"].set_value(self.parasitedata["width"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.width_spinner["adj"].set_value(shelf[self.shelfkey]["width"])
    self.width_label.set_mnemonic_widget(self.width_spinner["spinner"])
    self.table.attach(self.width_spinner["spinner"], 1, 2, 7, 8)
    self.width_spinner["adj"].connect("value-changed", self.preview)

    self.merge_check = gtk.CheckButton("_Merge with layer")
    if self.parasitedata:
      if self.parasitedata["merge"] == 1:
        self.merge_check.set_active(True)
    elif shelf.has_key(self.shelfkey) == 1 and shelf[self.shelfkey]["merge"] == 1:
      self.merge_check.set_active(True)
    self.merge_check.show()
    self.merge_check.connect("toggled", self.preview)

    self.preview_check = gtk.CheckButton("_Preview")
    self.preview_check.show()
    self.preview_check.connect("toggled", self.preview)

    self.dialog.vbox.hbox1 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox1.show()
    self.dialog.vbox.pack_start(self.dialog.vbox.hbox1, True, True, 7)
    self.dialog.vbox.hbox1.pack_start(self.table, True, True, 7)
    self.dialog.vbox.hbox2 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox2.show()
    self.dialog.vbox.add(self.dialog.vbox.hbox2)
    self.dialog.vbox.hbox2.pack_start(self.merge_check, True, True, 10)
    self.dialog.vbox.hbox3 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox3.show()
    self.dialog.vbox.add(self.dialog.vbox.hbox3)
    self.dialog.vbox.hbox3.pack_start(self.preview_check, True, True, 10)

    self.makeDialogButtons()

    self.dialog.show()
    self.dialog.run()
    self.removePreviews()

  def okbutton(self, widget):
    if self.layer_exists(self.previewLayer):
      self.img.remove_layer(self.previewLayer)
      self.previewLayer = None
    self.unset_hidden_layer()
    if pdb.gimp_image_undo_is_enabled(self.img) == 0:
      pdb.gimp_image_undo_thaw(self.img)
    params = {
      "gradient":     self.gradient_button.get_gradient(),
      "gradienttype": self.gradienttype_box.get_active(),
      "repeat":       self.repeat_box.get_active(),
      "reverse":      self.cond(self.reverse_check.get_active()),
      "opacity":      self.opacity_slider["adj"].get_value(),
      "mode":         self.mode_box.get_active(),
      "centerx":      self.centerx_spinner["adj"].get_value(),
      "centery":      self.centery_spinner["adj"].get_value(),
      "angle":        self.angle_slider["adj"].get_value(),
      "width":        self.width_spinner["adj"].get_value(),
      "merge":        self.cond(self.merge_check.get_active())
    }
    if self.parasitedata:
      self.img.remove_layer(self.parasitedata["oldid"])
    fxlayer = self.makeOverlay(
      self.img,
      self.drawable,
      params["gradient"],
      params["gradienttype"],
      params["repeat"],
      params["reverse"],
      params["opacity"],
      params["mode"],
      params["centerx"],
      params["centery"],
      params["angle"],
      params["width"],
      params["merge"]
    )
    if params["merge"] == 0:
      self.writeParasite(self.drawable, fxlayer,
        ("gradient", self.gradient_button),
        ("combobox", self.gradienttype_box),
        ("combobox", self.repeat_box),
        ("check",    self.reverse_check),
        ("floatadj", self.opacity_slider["adj"]),
        ("modebox",  self.mode_box),
        ("floatadj", self.centerx_spinner["adj"]),
        ("floatadj", self.centery_spinner["adj"]),
        ("floatadj", self.angle_slider["adj"]),
        ("floatadj", self.width_spinner["adj"]),
        ("check",    self.merge_check)
      )
    shelf[self.shelfkey] = params

  def resetbutton(self, widget):
    self.gradient_button.set_gradient("FG to BG (RGB)")
    self.gradienttype_box.set_active(0)
    self.repeat_box.set_active(0)
    self.reverse_check.set_active(False)
    self.mode_box.set_active(NORMAL_MODE)
    self.opacity_slider["adj"].set_value(100.0)
    self.centerx_spinner["adj"].set_value(0.0)
    self.centery_spinner["adj"].set_value(0.0)
    self.angle_slider["adj"].set_value(90.0)
    self.width_spinner["adj"].set_value(0.0)
    self.merge_check.set_active(False)

  def readParasite(self, img, drawable):
    return layerfx_base.readParasite(self, img, drawable,
      layerfx_gradient_overlay.shelfkey,
      ("gradient",     "string"),
      ("gradienttype", "int"),
      ("repeat",       "int"),
      ("reverse",      "int"),
      ("opacity",      "float"),
      ("mode",         "int"),
      ("centerx",      "float"),
      ("centery",      "float"),
      ("angle",        "float"),
      ("width",        "float"),
      ("merge",        "int")
    )

  def preview(self, widget, *extra):
    if self.layer_exists(self.previewLayer):
      self.img.remove_layer(self.previewLayer)
    if self.preview_check.get_active():
      self.unset_hidden_layer()
      if pdb.gimp_image_undo_is_enabled(self.img) == 1:
        pdb.gimp_image_undo_freeze(self.img)
      if self.parasitedata:
        self.set_hidden_layer(self.parasitedata["oldid"])
      if self.merge_check.get_active():
        self.previewLayer = self.drawable.copy()
        self.add_over_layer(self.previewLayer, self.drawable)
        self.set_hidden_layer(self.drawable)
        layer = self.previewLayer
        merge = 1
      else:
        layer = self.drawable
        merge = 0
      self.previewLayer = self.makeOverlay(
        self.img,
        layer,
        self.gradient_button.get_gradient(),
        self.gradienttype_box.get_active(),
        self.repeat_box.get_active(),
        self.cond(self.reverse_check.get_active()),
        self.opacity_slider["adj"].get_value(),
        self.mode_box.get_active(),
        self.centerx_spinner["adj"].get_value(),
        self.centery_spinner["adj"].get_value(),
        self.angle_slider["adj"].get_value(),
        self.width_spinner["adj"].get_value(),
        merge
      )
    else:
      gimp.displays_flush()

  def makeOverlay(self, img, drawable, gradient, gradienttype, repeat, reverse, opacity, mode, centerx, centery, angle, width, merge):
    pdb.gimp_image_undo_group_start(img)
    origgradient = pdb.gimp_context_get_gradient()
    origselection = pdb.gimp_selection_save(img)
    gradientlayer = gimp.Layer(img, "%s-gradient" % (drawable.name), drawable.width, drawable.height, (RGBA_IMAGE, GRAYA_IMAGE)[img.base_type], opacity, mode)
    self.add_over_layer(gradientlayer, drawable)
    gradientlayer.set_offsets(drawable.offsets[0], drawable.offsets[1])
    measures = self.getGradientMeasurements(drawable.offsets[0], drawable.offsets[1], gradienttype, centerx, centery, angle, width)
    pdb.gimp_selection_none(img)
    pdb.gimp_edit_clear(gradientlayer)
    pdb.gimp_context_set_gradient(gradient)
    if gradienttype >= 6 and gradienttype <= 8:
      pdb.gimp_selection_layer_alpha(drawable)
    pdb.gimp_edit_blend(gradientlayer, CUSTOM_MODE, NORMAL_MODE, gradienttype, 100.0, 1.0, repeat, reverse, False, 1, 0.0, False, measures["start"][0], measures["start"][1], measures["end"][0], measures["end"][1])
    pdb.gimp_selection_none(img)
    if merge == 1:
      origmask = drawable.mask
      layername = drawable.name
      if origmask != None:
        origmask = drawable.mask.copy()
        drawable.remove_mask(MASK_DISCARD)
      alphamask = drawable.create_mask(ADD_ALPHA_TRANSFER_MASK)
      gradientlayer = pdb.gimp_image_merge_down(img, gradientlayer, EXPAND_AS_NECESSARY)
      gradientlayer.name = layername
      gradientlayer.add_mask(alphamask)
      gradientlayer.remove_mask(MASK_APPLY)
      if origmask != None:
        gradientlayer.add_mask(origmask)
    else:
      pdb.gimp_selection_layer_alpha(drawable)
      if drawable.mask != None:
        pdb.gimp_selection_combine(drawable.mask, CHANNEL_OP_INTERSECT)
      alphamask = gradientlayer.create_mask(ADD_SELECTION_MASK)
      gradientlayer.add_mask(alphamask)
      gradientlayer.remove_mask(MASK_APPLY)
      pdb.gimp_image_set_active_layer(img, drawable)
    pdb.gimp_context_set_gradient(origgradient)
    pdb.gimp_selection_load(origselection)
    img.remove_channel(origselection)
    gimp.displays_flush()
    pdb.gimp_image_undo_group_end(img)
    return gradientlayer

class layerfx_pattern_overlay(layerfx_base):
  shelfkey = "layerfx-pattern-overlay"

  def __init__(self, runmode, img, drawable, pattern, opacity, mode, scale, interpolation_type, merge):
    self.origMsgHandler = pdb.gimp_message_get_handler()
    pdb.gimp_message_set_handler(ERROR_CONSOLE)
    self.img = img
    self.drawable = drawable
    if runmode == RUN_INTERACTIVE:
      self.showDialog()
    elif runmode == RUN_NONINTERACTIVE:
      if self.validatedata(img, drawable,
        ("pattern",    pattern),
        ("percent",    opacity),
        ("mode",       mode),
        ("floatrange", scale, 1.0, 1000.0),
        ("intrange",   interpolation_type, 0, 3),
        ("boolean",    merge)
      ):
        self.removeOldLayer()
        fxlayer = self.makeOverlay(img, drawable, pattern, opacity, mode, scale, interpolation_type, merge)
        if merge == 0:
          self.writeParasiteRaw(drawable, fxlayer, pattern, opacity, mode, scale, interpolation_type, merge)
        shelf[self.shelfkey] = {
          "pattern":            pattern,
          "opacity":            opacity,
          "mode":               mode,
          "scale":              scale,
          "interpolation_type": interpolation_type,
          "merge":              merge
        }
    elif runmode == RUN_WITH_LAST_VALS and shelf.has_key(self.shelfkey) == 1:
      self.removeOldLayer()
      fxlayer = self.makeOverlay(
        img,
        drawable,
        shelf[self.shelfkey]["pattern"],
        shelf[self.shelfkey]["opacity"],
        shelf[self.shelfkey]["mode"],
        shelf[self.shelfkey]["scale"],
        shelf[self.shelfkey]["interpolation_type"],
        shelf[self.shelfkey]["merge"]
      )
      if shelf[self.shelfkey]["merge"] == 0:
        self.writeParasiteRaw(drawable, fxlayer,
          shelf[self.shelfkey]["pattern"],
          shelf[self.shelfkey]["opacity"],
          shelf[self.shelfkey]["mode"],
          shelf[self.shelfkey]["scale"],
          shelf[self.shelfkey]["interpolation_type"],
          shelf[self.shelfkey]["merge"]
        )
    else:
      pdb.gimp_message("unknown runmode")
    pdb.gimp_message_set_handler(self.origMsgHandler)

  def showDialog(self):
    self.dialog = gimpui.Dialog("Pattern Overlay", "patternoverlaydialog")

    self.parasitedata = self.readParasite(self.img, self.drawable)

    self.table = gtk.Table(5, 6, True)
    self.table.set_homogeneous(True)
    self.table.set_row_spacings(3)
    self.table.set_col_spacings(3)
    self.table.show()

    self.pattern_label = self.make_label("_Pattern:")
    self.table.attach(self.pattern_label, 0, 2, 0, 1)

    self.pattern_button = gimpui.PatternSelector()
    if self.parasitedata:
      self.pattern_button.set_pattern(self.parasitedata["pattern"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.pattern_button.set_pattern(shelf[self.shelfkey]["pattern"])
    self.pattern_label.set_mnemonic_widget(self.pattern_button)
    self.pattern_button.show()
    self.table.attach(self.pattern_button, 2, 4, 0, 1)
    self.pattern_button.connect("pattern-set", self.preview)

    self.mode_label = self.make_label("_Blend Mode:")
    self.table.attach(self.mode_label, 0, 2, 1, 2)

    self.mode_box = self.make_blend_mode_box()
    if self.parasitedata:
      self.mode_box.set_active(self.parasitedata["mode"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.mode_box.set_active(shelf[self.shelfkey]["mode"])
    else:
      self.mode_box.set_active(NORMAL_MODE)
    self.mode_label.set_mnemonic_widget(self.mode_box)
    self.mode_box.show()
    self.table.attach(self.mode_box, 2, 6, 1, 2)
    self.mode_box.connect("changed", self.preview)

    self.opacity_label = self.make_label("_Opacity:")
    self.table.attach(self.opacity_label, 0, 2, 2, 3)

    self.opacity_slider = self.make_slider_and_spinner(100.0, 0.0, 100.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.opacity_slider["adj"].set_value(self.parasitedata["opacity"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.opacity_slider["adj"].set_value(shelf[self.shelfkey]["opacity"])
    self.opacity_label.set_mnemonic_widget(self.opacity_slider["spinner"])
    self.table.attach(self.opacity_slider["slider"], 2, 5, 2, 3)
    self.table.attach(self.opacity_slider["spinner"], 5, 6, 2, 3)
    self.opacity_slider["adj"].connect("value-changed", self.preview)

    self.scale_label = self.make_label("Scale:")
    self.table.attach(self.scale_label, 0, 2, 3, 4)

    self.scale_slider = self.make_slider_and_spinner(100.0, 1.0, 1000.0, 1.0, 10.0, 1)
    if self.parasitedata:
      self.scale_slider["adj"].set_value(self.parasitedata["scale"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.scale_slider["adj"].set_value(shelf[self.shelfkey]["scale"])
    self.scale_label.set_mnemonic_widget(self.scale_slider["spinner"])
    self.table.attach(self.scale_slider["slider"], 2, 5, 3, 4)
    self.table.attach(self.scale_slider["spinner"], 5, 6, 3, 4)
    self.scale_slider["adj"].connect("value-changed", self.preview)

    self.interpolation_type_label = self.make_label("Interpolation:")
    self.table.attach(self.interpolation_type_label, 0, 2, 4, 5)

    self.interpolation_type_box = self.make_interpolation_box()
    if self.parasitedata:
      self.interpolation_type_box.set_active(self.parasitedata["interpolation_type"])
    elif shelf.has_key(self.shelfkey) == 1:
      self.interpolation_type_box.set_active(shelf[self.shelfkey]["interpolation_type"])
    else:
      self.interpolation_type_box.set_active(0)
    self.interpolation_type_box.show()
    self.interpolation_type_label.set_mnemonic_widget(self.interpolation_type_box)
    self.table.attach(self.interpolation_type_box, 2, 6, 4, 5)
    self.interpolation_type_box.connect("changed", self.preview)

    self.merge_check = gtk.CheckButton("_Merge with layer")
    if self.parasitedata:
      if self.parasitedata["merge"] == 1:
        self.merge_check.set_active(True)
    elif shelf.has_key(self.shelfkey) == 1 and shelf[self.shelfkey]["merge"] == 1:
      self.merge_check.set_active(True)
    self.merge_check.show()
    self.merge_check.connect("toggled", self.preview)

    self.preview_check = gtk.CheckButton("_Preview")
    self.preview_check.show()
    self.preview_check.connect("toggled", self.preview)

    self.dialog.vbox.hbox1 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox1.show()
    self.dialog.vbox.pack_start(self.dialog.vbox.hbox1, True, True, 7)
    self.dialog.vbox.hbox1.pack_start(self.table, True, True, 7)
    self.dialog.vbox.hbox2 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox2.show()
    self.dialog.vbox.add(self.dialog.vbox.hbox2)
    self.dialog.vbox.hbox2.pack_start(self.merge_check, True, True, 10)
    self.dialog.vbox.hbox3 = gtk.HBox(False, 7)
    self.dialog.vbox.hbox3.show()
    self.dialog.vbox.add(self.dialog.vbox.hbox3)
    self.dialog.vbox.hbox3.pack_start(self.preview_check, True, True, 10)

    self.makeDialogButtons()

    self.dialog.show()
    self.dialog.run()
    self.removePreviews()

  def okbutton(self, widget):
    if self.layer_exists(self.previewLayer):
      self.img.remove_layer(self.previewLayer)
      self.previewLayer = None
    self.unset_hidden_layer()
    if pdb.gimp_image_undo_is_enabled(self.img) == 0:
      pdb.gimp_image_undo_thaw(self.img)
    params = {
      "pattern":            self.pattern_button.get_pattern(),
      "opacity":            self.opacity_slider["adj"].get_value(),
      "mode":               self.mode_box.get_active(),
      "scale":              self.scale_slider["adj"].get_value(),
      "interpolation_type": self.interpolation_type_box.get_active(),
      "merge":              self.cond(self.merge_check.get_active())
    }
    if self.parasitedata:
      self.img.remove_layer(self.parasitedata["oldid"])
    fxlayer = self.makeOverlay(
      self.img,
      self.drawable,
      params["pattern"],
      params["opacity"],
      params["mode"],
      params["scale"],
      params["interpolation_type"],
      params["merge"]
    )
    if params["merge"] == 0:
      self.writeParasite(self.drawable, fxlayer,
        ("pattern",  self.pattern_button),
        ("floatadj", self.opacity_slider["adj"]),
        ("modebox",  self.mode_box),
        ("floatadj", self.scale_slider["adj"]),
        ("combobox", self.interpolation_type_box),
        ("check",    self.merge_check)
      )
    shelf[self.shelfkey] = params

  def resetbutton(self, widget):
    self.pattern_button.set_pattern(pdb.gimp_context_get_pattern())
    self.mode_box.set_active(NORMAL_MODE)
    self.opacity_slider["adj"].set_value(100.0)
    self.scale_slider["adj"].set_value(100.0)
    self.interpolation_type_box.set_active(0)
    self.merge_check.set_active(False)

  def readParasite(self, img, drawable):
    return layerfx_base.readParasite(self, img, drawable,
      layerfx_pattern_overlay.shelfkey,
      ("pattern",            "string"),
      ("opacity",            "float"),
      ("mode",               "int"),
      ("scale",              "float"),
      ("interpolation_type", "int"),
      ("merge",              "int")
    )

  def preview(self, widget, *extra):
    if self.layer_exists(self.previewLayer):
      self.img.remove_layer(self.previewLayer)
    if self.preview_check.get_active():
      self.unset_hidden_layer()
      if pdb.gimp_image_undo_is_enabled(self.img) == 1:
        pdb.gimp_image_undo_freeze(self.img)
      if self.parasitedata:
        self.set_hidden_layer(self.parasitedata["oldid"])
      if self.merge_check.get_active():
        self.previewLayer = self.drawable.copy()
        self.add_over_layer(self.previewLayer, self.drawable)
        self.unset_hidden_layer()
        layer = self.previewLayer
        merge = 1
      else:
        layer = self.drawable
        merge = 0
      self.previewLayer = self.makeOverlay(
        self.img,
        layer,
        self.pattern_button.get_pattern(),
        self.opacity_slider["adj"].get_value(),
        self.mode_box.get_active(),
        self.scale_slider["adj"].get_value(),
        self.interpolation_type_box.get_active(),
        merge
      )
    else:
      gimp.displays_flush()

  def makeOverlay(self, img, drawable, pattern, opacity, mode, scale, interpolation_type, merge):
    pdb.gimp_image_undo_group_start(img)
    origpattern = pdb.gimp_context_get_pattern()
    origselection = pdb.gimp_selection_save(img)
    if scale == 100.0:
      layerwidth = drawable.width
      layerheight = drawable.height
    else:
      layerwidth = int(round(drawable.width/(scale/100.0)))
      layerheight = int(round(drawable.height/(scale/100.0)))
    patternlayer = gimp.Layer(img, "%s-pattern" % (drawable.name), layerwidth, layerheight, (RGBA_IMAGE, GRAYA_IMAGE)[img.base_type], opacity, mode)
    self.add_over_layer(patternlayer, drawable)
    patternlayer.set_offsets(drawable.offsets[0], drawable.offsets[1])
    pdb.gimp_selection_none(img)
    pdb.gimp_context_set_pattern(pattern)
    patternlayer.fill(PATTERN_FILL)
    if scale != 100.0:
      pdb.gimp_drawable_transform_scale(patternlayer, drawable.offsets[0], drawable.offsets[1], drawable.offsets[0] + drawable.width, drawable.offsets[1] + drawable.height, TRANSFORM_FORWARD, interpolation_type, 1, 3, TRANSFORM_RESIZE_ADJUST)
    if merge == 1:
      origmask = drawable.mask
      layername = drawable.name
      if origmask != None:
        origmask = drawable.mask.copy()
        drawable.remove_mask(MASK_DISCARD)
      alphamask = drawable.create_mask(ADD_ALPHA_TRANSFER_MASK)
      patternlayer = pdb.gimp_image_merge_down(img, patternlayer, EXPAND_AS_NECESSARY)
      patternlayer.name = layername
      patternlayer.add_mask(alphamask)
      patternlayer.remove_mask(MASK_APPLY)
      if origmask != None:
        patternlayer.add_mask(origmask)
    else:
      pdb.gimp_selection_layer_alpha(drawable)
      if drawable.mask != None:
        pdb.gimp_selection_combine(drawable.mask, CHANNEL_OP_INTERSECT)
      alphamask = patternlayer.create_mask(ADD_SELECTION_MASK)
      patternlayer.add_mask(alphamask)
      patternlayer.remove_mask(MASK_APPLY)
      pdb.gimp_image_set_active_layer(img, drawable)
    pdb.gimp_context_set_pattern(origpattern)
    pdb.gimp_selection_load(origselection)
    img.remove_channel(origselection)
    gimp.displays_flush()
    pdb.gimp_image_undo_group_end(img)
    return patternlayer

class layerfx_reapply_effects(layerfx_drop_shadow, layerfx_inner_shadow, layerfx_outer_glow, layerfx_inner_glow, layerfx_bevel_emboss, layerfx_satin, layerfx_stroke, layerfx_color_overlay, layerfx_gradient_overlay, layerfx_pattern_overlay):
  def __init__(self, runmode, img, drawable):
    self.origMsgHandler = pdb.gimp_message_get_handler()
    pdb.gimp_message_set_handler(ERROR_CONSOLE)
    self.img = img
    self.drawable = drawable
    if runmode == RUN_INTERACTIVE or runmode == RUN_WITH_LAST_VALS:
      self.reapplyEffects(img, drawable)
    elif runmode == RUN_NONINTERACTIVE:
      if self.validatedata(img, drawable):
        self.reapplyEffects(img, drawable)
    else:
      pdb.gimp_message("unknown runmode")
    pdb.gimp_message_set_handler(self.origMsgHandler)

  def reapplyEffects(self, img, drawable):
    pdb.gimp_image_undo_group_start(img)

    self.parasitedata = {
      "drop-shadow":      layerfx_drop_shadow.readParasite(self, img, drawable),
      "inner-shadow":     layerfx_inner_shadow.readParasite(self, img, drawable),
      "outer-glow":       layerfx_outer_glow.readParasite(self, img, drawable),
      "inner-glow":       layerfx_inner_glow.readParasite(self, img, drawable),
      "bevel-emboss":     layerfx_bevel_emboss.readParasite(self, img, drawable),
      "satin":            layerfx_satin.readParasite(self, img, drawable),
      "stroke":           layerfx_stroke.readParasite(self, img, drawable),
      "color-overlay":    layerfx_color_overlay.readParasite(self, img, drawable),
      "gradient-overlay": layerfx_gradient_overlay.readParasite(self, img, drawable),
      "pattern-overlay":  layerfx_pattern_overlay.readParasite(self, img, drawable)
    }
    active_effects = []
    for k, v in self.parasitedata.iteritems():
      if v:
        active_effects.append(k)
    active_effects.sort(cmp=lambda a, b: self.get_layer_pos(self.parasitedata[a]["oldid"]) - self.get_layer_pos(self.parasitedata[b]["oldid"]))
    fx_detected = False
    for j in active_effects:
      if j == "drop-shadow":
        fx_detected = True
        layerfx_drop_shadow(
          RUN_NONINTERACTIVE,
          img,
          drawable,
          self.parasitedata["drop-shadow"]["color"],
          self.parasitedata["drop-shadow"]["opacity"],
          self.parasitedata["drop-shadow"]["contour"],
          self.parasitedata["drop-shadow"]["noise"],
          self.parasitedata["drop-shadow"]["mode"],
          self.parasitedata["drop-shadow"]["spread"],
          self.parasitedata["drop-shadow"]["size"],
          self.parasitedata["drop-shadow"]["offsetangle"],
          self.parasitedata["drop-shadow"]["offsetdist"],
          self.parasitedata["drop-shadow"]["knockout"],
          self.parasitedata["drop-shadow"]["merge"]
        )
      elif j == "inner-shadow":
        fx_detected = True
        layerfx_inner_shadow(
          RUN_NONINTERACTIVE,
          img,
          drawable,
          self.parasitedata["inner-shadow"]["color"],
          self.parasitedata["inner-shadow"]["opacity"],
          self.parasitedata["inner-shadow"]["contour"],
          self.parasitedata["inner-shadow"]["noise"],
          self.parasitedata["inner-shadow"]["mode"],
          self.parasitedata["inner-shadow"]["source"],
          self.parasitedata["inner-shadow"]["choke"],
          self.parasitedata["inner-shadow"]["size"],
          self.parasitedata["inner-shadow"]["offsetangle"],
          self.parasitedata["inner-shadow"]["offsetdist"],
          self.parasitedata["inner-shadow"]["merge"]
        )
      elif j == "outer-glow":
        fx_detected = True
        if self.parasitedata["outer-glow"]["filltype"] == 0:
          fill = self.parasitedata["outer-glow"]["color"]
        elif self.parasitedata["outer-glow"]["filltype"] == 1:
          fill = self.parasitedata["outer-glow"]["gradient"]
        layerfx_outer_glow(
          RUN_NONINTERACTIVE,
          img,
          drawable,
          fill,
          self.parasitedata["outer-glow"]["opacity"],
          self.parasitedata["outer-glow"]["contour"],
          self.parasitedata["outer-glow"]["noise"],
          self.parasitedata["outer-glow"]["mode"],
          self.parasitedata["outer-glow"]["spread"],
          self.parasitedata["outer-glow"]["size"],
          self.parasitedata["outer-glow"]["knockout"],
          self.parasitedata["outer-glow"]["merge"]
        )
      elif j == "inner-glow":
        fx_detected = True
        if self.parasitedata["inner-glow"]["filltype"] == 0:
          fill = self.parasitedata["inner-glow"]["color"]
        elif self.parasitedata["inner-glow"]["filltype"] == 1:
          fill = self.parasitedata["inner-glow"]["gradient"]
        layerfx_inner_glow(
          RUN_NONINTERACTIVE,
          img,
          drawable,
          fill,
          self.parasitedata["inner-glow"]["opacity"],
          self.parasitedata["inner-glow"]["contour"],
          self.parasitedata["inner-glow"]["noise"],
          self.parasitedata["inner-glow"]["mode"],
          self.parasitedata["inner-glow"]["source"],
          self.parasitedata["inner-glow"]["choke"],
          self.parasitedata["inner-glow"]["size"],
          self.parasitedata["inner-glow"]["merge"]
        )
      elif j == "bevel-emboss":
        fx_detected = True
        layerfx_bevel_emboss(
          RUN_NONINTERACTIVE,
          img,
          drawable,
          self.parasitedata["bevel-emboss"]["style"],
          self.parasitedata["bevel-emboss"]["depth"],
          self.parasitedata["bevel-emboss"]["direction"],
          self.parasitedata["bevel-emboss"]["size"],
          self.parasitedata["bevel-emboss"]["soften"],
          self.parasitedata["bevel-emboss"]["angle"],
          self.parasitedata["bevel-emboss"]["altitude"],
          self.parasitedata["bevel-emboss"]["glosscontour"],
          self.parasitedata["bevel-emboss"]["highlightcolor"],
          self.parasitedata["bevel-emboss"]["highlightmode"],
          self.parasitedata["bevel-emboss"]["highlightopacity"],
          self.parasitedata["bevel-emboss"]["shadowcolor"],
          self.parasitedata["bevel-emboss"]["shadowmode"],
          self.parasitedata["bevel-emboss"]["shadowopacity"],
          self.parasitedata["bevel-emboss"]["surfacecontour"],
          self.parasitedata["bevel-emboss"]["use_texture"],
          self.parasitedata["bevel-emboss"]["pattern"],
          self.parasitedata["bevel-emboss"]["scale"],
          self.parasitedata["bevel-emboss"]["tex_depth"],
          self.parasitedata["bevel-emboss"]["invert"],
          self.parasitedata["bevel-emboss"]["merge"]
        )
      elif j == "satin":
        fx_detected = True
        layerfx_satin(
          RUN_NONINTERACTIVE,
          img,
          drawable,
          self.parasitedata["satin"]["color"],
          self.parasitedata["satin"]["opacity"],
          self.parasitedata["satin"]["mode"],
          self.parasitedata["satin"]["offsetangle"],
          self.parasitedata["satin"]["offsetdist"],
          self.parasitedata["satin"]["size"],
          self.parasitedata["satin"]["contour"],
          self.parasitedata["satin"]["invert"],
          self.parasitedata["satin"]["merge"]
        )
      elif j == "stroke":
        fx_detected = True
        if self.parasitedata["stroke"]["filltype"] == 0:
          fill = self.parasitedata["stroke"]["color"]
        elif self.parasitedata["stroke"]["filltype"] == 1:
          fill = (
            self.parasitedata["stroke"]["gradient"],
            self.parasitedata["stroke"]["gradienttype"],
            self.parasitedata["stroke"]["repeat"],
            self.parasitedata["stroke"]["reverse"],
            self.parasitedata["stroke"]["centerx"],
            self.parasitedata["stroke"]["centery"],
            self.parasitedata["stroke"]["angle"],
            self.parasitedata["stroke"]["width"]
          )
        elif self.parasitedata["stroke"]["filltype"] == 2:
          fill = (
            self.parasitedata["stroke"]["pattern"],
            self.parasitedata["stroke"]["scale"],
            self.parasitedata["stroke"]["interpolation_type"]
          )
        layerfx_stroke(
          RUN_NONINTERACTIVE,
          img,
          drawable,
          fill,
          self.parasitedata["stroke"]["opacity"],
          self.parasitedata["stroke"]["mode"],
          self.parasitedata["stroke"]["size"],
          self.parasitedata["stroke"]["position"],
          self.parasitedata["stroke"]["merge"]
        )
      elif j == "color-overlay":
        fx_detected = True
        layerfx_color_overlay(
          RUN_NONINTERACTIVE,
          img,
          drawable,
          self.parasitedata["color-overlay"]["color"],
          self.parasitedata["color-overlay"]["opacity"],
          self.parasitedata["color-overlay"]["mode"],
          self.parasitedata["color-overlay"]["merge"]
        )
      elif j == "gradient-overlay":
        fx_detected = True
        layerfx_gradient_overlay(
          RUN_NONINTERACTIVE,
          img,
          drawable,
          self.parasitedata["gradient-overlay"]["gradient"],
          self.parasitedata["gradient-overlay"]["gradienttype"],
          self.parasitedata["gradient-overlay"]["repeat"],
          self.parasitedata["gradient-overlay"]["reverse"],
          self.parasitedata["gradient-overlay"]["opacity"],
          self.parasitedata["gradient-overlay"]["mode"],
          self.parasitedata["gradient-overlay"]["centerx"],
          self.parasitedata["gradient-overlay"]["centery"],
          self.parasitedata["gradient-overlay"]["angle"],
          self.parasitedata["gradient-overlay"]["width"],
          self.parasitedata["gradient-overlay"]["merge"]
        )
      elif j == "pattern-overlay":
        fx_detected = True
        layerfx_pattern_overlay(
          RUN_NONINTERACTIVE,
          img,
          drawable,
          self.parasitedata["pattern-overlay"]["pattern"],
          self.parasitedata["pattern-overlay"]["opacity"],
          self.parasitedata["pattern-overlay"]["mode"],
          self.parasitedata["pattern-overlay"]["scale"],
          self.parasitedata["pattern-overlay"]["interpolation_type"],
          self.parasitedata["pattern-overlay"]["merge"]
        )
    if fx_detected == False:
      self.show_error_msg("No effects found on this layer.", ValueError)
    gimp.displays_flush()
    pdb.gimp_image_undo_group_end(img)

class layerfxplugin(gimpplugin.plugin):
  def start(self):
    gimp.main(self.init, self.quit, self.query, self._run)

  def init(self):
    pass

  def quit(self):
    pass

  def query(self):
    authorname = "Jonathan Stipe"
    copyrightname = "Jonathan Stipe"
    imgmenupath = "<Image>/Layer/La_yer Effects/"
    layersmenupath = "<Layers>/_Layer Effects/"
    date = "July 2008"
    modevals = "{ NORMAL-MODE (0), DISSOLVE-MODE (1), MULTIPLY-MODE (3), SCREEN-MODE (4), OVERLAY-MODE (5), DIFFERENCE-MODE (6), ADDITION-MODE (7), SUBTRACT-MODE (8), DARKEN-ONLY-MODE (9), LIGHTEN-ONLY-MODE (10), HUE-MODE (11), SATURATION-MODE (12), COLOR-MODE (13), VALUE-MODE (14), DIVIDE-MODE (15), DODGE-MODE (16), BURN-MODE (17), HARDLIGHT-MODE (18), SOFTLIGHT-MODE (19), GRAIN-EXTRACT-MODE (20), GRAIN-MERGE-MODE (21), COLOR-ERASE-MODE (22), ERASE-MODE (23), REPLACE-MODE (24), ANTI-ERASE-MODE (25) }"

    drop_shadow_description = "Adds a drop shadow to a layer."
    drop_shadow_help = "Adds a drop shadow to a layer."
    drop_shadow_params = (
      (PDB_INT32,    "run_mode",        "Run mode"),
      (PDB_IMAGE,    "image",           "Input image"),
      (PDB_DRAWABLE, "drawable",        "Input drawable"),
      (PDB_COLOR,    "color",           "The shadow's color"),
      (PDB_FLOAT,    "opacity",         "The shadow's opacity (0 <= opacity <= 100)"),
      (PDB_INT32,    "contour",         "A contour used to modify the shadow's intensity curve (0 <= contour <= 11)"),
      (PDB_FLOAT,    "noise",           "The amount of noise applied to the shadow (0 <= noise <= 100)"),
      (PDB_INT32,    "mode",            "The shadow layer's combination mode %s" % (modevals)),
      (PDB_FLOAT,    "spread",          "Spread (0 <= spread <= 100)"),
      (PDB_INT32,    "size",            "The size of the shadow's blur (0 <= size <= 250)"),
      (PDB_FLOAT,    "offset_angle",    "The angle the shadow is cast in (-180 <= offset_angle <= 180)"),
      (PDB_FLOAT,    "offset_distance", "The distance between the layer and the shadow (0 <= offset_distance <= 30000)"),
      (PDB_INT32,    "knockout",        "Layer knocks out Drop Shadow (TRUE or FALSE)"),
      (PDB_INT32,    "merge",           "Merge with layer (TRUE or FALSE)")
    )
    gimp.install_procedure(
      "python_layerfx_drop_shadow",
      drop_shadow_description,
      drop_shadow_help,
      authorname,
      copyrightname,
      date,
      "%s_Drop Shadow..." % (imgmenupath),
      "RGBA, GRAYA",
      PLUGIN,
      drop_shadow_params,
      []
    )
    gimp.install_procedure(
      "python_layer_fx_drop_shadow",
      drop_shadow_description,
      drop_shadow_help,
      authorname,
      copyrightname,
      date,
      "%s_Drop Shadow..." % (layersmenupath),
      "RGBA, GRAYA",
      PLUGIN,
      drop_shadow_params,
      []
    )
    inner_shadow_description = "Adds an inner shadow to a layer"
    inner_shadow_help = "Adds an inner shadow to a layer"
    inner_shadow_params = (
      (PDB_INT32,    "run_mode",        "Run mode"),
      (PDB_IMAGE,    "image",           "Input image"),
      (PDB_DRAWABLE, "drawable",        "Input drawable"),
      (PDB_COLOR,    "color",           "The shadow's color"),
      (PDB_FLOAT,    "opacity",         "The shadow's opacity (0 <= opacity <= 100)"),
      (PDB_INT32,    "contour",         "A contour used to modify the shadow's intensity curve (0 <= contour <= 11)"),
      (PDB_FLOAT,    "noise",           "The amount of noise applied to the shadow (0 <= noise <= 100)"),
      (PDB_INT32,    "mode",            "The shadow layer's combination mode %s" % (modevals)),
      (PDB_INT32,    "source",          "Source (0 = Center, 1 = Edge)"),
      (PDB_FLOAT,    "choke",           "Choke (0 <= choke <= 100)"),
      (PDB_INT32,    "size",            "The size of the shadow's blur (0 <= size <= 250)"),
      (PDB_FLOAT,    "offset_angle",    "The angle the shadow is cast in (-180 <= offset_angle <= 180)"),
      (PDB_FLOAT,    "offset_distance", "The distance between the layer and the shadow (0 <= offset_distance <= 30000)"),
      (PDB_INT32,    "merge",           "Merge with layer (TRUE or FALSE)")
    )
    gimp.install_procedure(
      "python_layerfx_inner_shadow",
      inner_shadow_description,
      inner_shadow_help,
      authorname,
      copyrightname,
      date,
      "%sI_nner Shadow..." % (imgmenupath),
      "RGB*, GRAY*",
      PLUGIN,
      inner_shadow_params,
      []
    )
    gimp.install_procedure(
      "python_layer_fx_inner_shadow",
      inner_shadow_description,
      inner_shadow_help,
      authorname,
      copyrightname,
      date,
      "%sI_nner Shadow..." % (layersmenupath),
      "RGB*, GRAY*",
      PLUGIN,
      inner_shadow_params,
      []
    )
    outer_glow_description = "Creates an outer glow effect around a layer."
    outer_glow_help = "Creates an outer glow effect around a layer."
    outer_glow_params = (
      (PDB_INT32,    "run_mode", "Run mode"),
      (PDB_IMAGE,    "image",    "Input image"),
      (PDB_DRAWABLE, "drawable", "Input drawable"),
      (PDB_COLOR,    "color",    "The glow's color. You can also use a gradient name here."),
      (PDB_FLOAT,    "opacity",  "The glow's opacity (0 <= opacity <= 100)"),
      (PDB_INT32,    "contour",  "A contour used to modify the glow's intensity curve (0 <= contour <= 11)"),
      (PDB_FLOAT,    "noise",    "The amount of noise applied to the glow (0 <= noise <= 100)"),
      (PDB_INT32,    "mode",     "The glow layer's combination mode %s" % (modevals)),
      (PDB_FLOAT,    "spread",   "Spread (0 <= spread <= 100)"),
      (PDB_INT32,    "size",     "The size of the glow's blur (0 <= size <= 250)"),
      (PDB_INT32,    "knockout", "Layer knocks out Outer Glow (TRUE or FALSE)"),
      (PDB_INT32,    "merge",    "Merge with layer (TRUE or FALSE)")
    )
    gimp.install_procedure(
      "python_layerfx_outer_glow",
      outer_glow_description,
      outer_glow_help,
      authorname,
      copyrightname,
      date,
      "%s_Outer Glow..." % (imgmenupath),
      "RGBA, GRAYA",
      PLUGIN,
      outer_glow_params,
      []
    )
    gimp.install_procedure(
      "python_layer_fx_outer_glow",
      outer_glow_description,
      outer_glow_help,
      authorname,
      copyrightname,
      date,
      "%s_Outer Glow..." % (layersmenupath),
      "RGBA, GRAYA",
      PLUGIN,
      outer_glow_params,
      []
    )
    inner_glow_description = "Creates an inner glow effect around a layer."
    inner_glow_help = "Creates an inner glow effect around a layer."
    inner_glow_params = (
      (PDB_INT32,    "run_mode", "Run mode"),
      (PDB_IMAGE,    "image",    "Input image"),
      (PDB_DRAWABLE, "drawable", "Input drawable"),
      (PDB_COLOR,    "color",    "The glow's color. You can also use a gradient name here."),
      (PDB_FLOAT,    "opacity",  "The glow's opacity (0 <= opacity <= 100)"),
      (PDB_INT32,    "contour",  "A contour used to modify the glow's intensity curve (0 <= contour <= 11)"),
      (PDB_FLOAT,    "noise",    "The amount of noise applied to the glow (0 <= noise <= 100)"),
      (PDB_INT32,    "mode",     "The glow layer's combination mode %s" % (modevals)),
      (PDB_INT32,    "source",   "Source (0 = Center, 1 = Edge)"),
      (PDB_FLOAT,    "choke",    "Choke (0 <= choke <= 100)"),
      (PDB_INT32,    "size",     "The size of the glow's blur (0 <= size <= 250)"),
      (PDB_INT32,    "merge",    "Merge with layer (TRUE or FALSE)")
    )
    gimp.install_procedure(
      "python_layerfx_inner_glow",
      inner_glow_description,
      inner_glow_help,
      authorname,
      copyrightname,
      date,
      "%s_Inner Glow..." % (imgmenupath),
      "RGB*, GRAY*",
      PLUGIN,
      inner_glow_params,
      []
    )
    gimp.install_procedure(
      "python_layer_fx_inner_glow",
      inner_glow_description,
      inner_glow_help,
      authorname,
      copyrightname,
      date,
      "%s_Inner Glow..." % (layersmenupath),
      "RGB*, GRAY*",
      PLUGIN,
      inner_glow_params,
      []
    )
    bevel_emboss_description = "Creates beveling and embossing effects over a layer."
    bevel_emboss_help = "Creates beveling and embossing effects over a layer."
    bevel_emboss_params = (
      (PDB_INT32,    "run_mode",          "Run mode"),
      (PDB_IMAGE,    "image",             "Input image"),
      (PDB_DRAWABLE, "drawable",          "Input drawable"),
      (PDB_INT32,    "style",             "Beveling Style (0 = Outer Bevel, 1 = Inner Bevel, 2 = Emboss, 3 = Pillow Emboss)"),
      (PDB_INT32,    "depth",             "Depth (1 <= depth <= 65)"),
      (PDB_INT32,    "direction",         "Direction (0 = Up, 1 = Down)"),
      (PDB_INT32,    "size",              "The size of the bevel (0 <= size <= 250)"),
      (PDB_INT32,    "soften",            "Soften (0 <= soften <= 16)"),
      (PDB_FLOAT,    "angle",             "Angle of the light source (-180 <= angle <= 180)"),
      (PDB_FLOAT,    "altitude",          "Altitude of the light source (0 <= altitude <= 90)"),
      (PDB_INT32,    "gloss_contour",     "A contour used to modify the gloss's intensity curve (0 <= gloss_contour <= 11)"),
      (PDB_COLOR,    "highlight_color",   "The highlight color"),
      (PDB_INT32,    "highlight_mode",    "The highlight layer's combination mode %s" % (modevals)),
      (PDB_FLOAT,    "highlight_opacity", "The highlight's opacity (0 <= highlight_opacity <= 100)"),
      (PDB_COLOR,    "shadow_color",      "The shadow color"),
      (PDB_INT32,    "shadow_mode",       "The shadow layer's combination mode %s" % (modevals)),
      (PDB_FLOAT,    "shadow_opacity",    "The shadow's opacity (0 <= shadow_opacity <= 100)"),
      (PDB_INT32,    "surface_contour",   "A contour used to modify the surface shape (0 <= gloss_contour <= 11)"),
      (PDB_INT32,    "use_texture",       "Apply a texture to the surface (TRUE or FALSE)"),
      (PDB_STRING,   "pattern",           "The texture pattern"),
      (PDB_FLOAT,    "scale",             "The texture scale (1 <= scale <= 1000)"),
      (PDB_FLOAT,    "tex_depth",         "The texture depth (-1000 <= tex_depth <= 1000)"),
      (PDB_INT32,    "invert",            "Invert (TRUE or FALSE)"),
      (PDB_INT32,    "merge",             "Merge with layer (TRUE or FALSE)")
    )
    gimp.install_procedure(
      "python_layerfx_bevel_emboss",
      bevel_emboss_description,
      bevel_emboss_help,
      authorname,
      copyrightname,
      date,
      "%s_Bevel and Emboss..." % (imgmenupath),
      "RGBA, GRAYA",
      PLUGIN,
      bevel_emboss_params,
      []
    )
    gimp.install_procedure(
      "python_layer_fx_bevel_emboss",
      bevel_emboss_description,
      bevel_emboss_help,
      authorname,
      copyrightname,
      date,
      "%s_Bevel and Emboss..." % (layersmenupath),
      "RGBA, GRAYA",
      PLUGIN,
      bevel_emboss_params,
      []
    )
    satin_description = "Creates a satin effect over a layer."
    satin_help = "Creates a satin effect over a layer."
    satin_params = (
      (PDB_INT32,    "run_mode",        "Run mode"),
      (PDB_IMAGE,    "image",           "Input image"),
      (PDB_DRAWABLE, "drawable",        "Input drawable"),
      (PDB_COLOR,    "color",           "The shadow color"),
      (PDB_FLOAT,    "opacity",         "The shadow opacity (0 <= opacity <= 100)"),
      (PDB_INT32,    "mode",            "The shadow layer's combination mode %s" % (modevals)),
      (PDB_FLOAT,    "offset_angle",    "The angle the shadow is cast in (-180 <= offset_angle <= 180)"),
      (PDB_FLOAT,    "offset_distance", "The offset distance (0 <= offset_distance <= 30000)"),
      (PDB_INT32,    "size",            "The size of the satin's blur (0 <= size <= 250)"),
      (PDB_INT32,    "contour",         "A contour used to modify the shadow's intensity curve (0 <= contour <= 11)"),
      (PDB_INT32,    "invert",          "Invert (TRUE or FALSE)"),
      (PDB_INT32,    "merge",           "Merge with layer (TRUE or FALSE)")
    )
    gimp.install_procedure(
      "python_layerfx_satin",
      satin_description,
      satin_help,
      authorname,
      copyrightname,
      date,
      "%s_Satin..." % (imgmenupath),
      "RGBA, GRAYA",
      PLUGIN,
      satin_params,
      []
    )
    gimp.install_procedure(
      "python_layer_fx_satin",
      satin_description,
      satin_help,
      authorname,
      copyrightname,
      date,
      "%s_Satin..." % (layersmenupath),
      "RGBA, GRAYA",
      PLUGIN,
      satin_params,
      []
    )
    stroke_description = "Creates a stroke around a layer."
    stroke_help = "Creates a stroke around a layer."
    stroke_params = (
      (PDB_INT32,    "run_mode", "Run mode"),
      (PDB_IMAGE,    "image",    "Input image"),
      (PDB_DRAWABLE, "drawable", "Input drawable"),
      (PDB_COLOR,    "color",    "The stroke color. This can also be a list or tuple to use gradients or patterns. For a gradient stroke, use a list or tuple of the form (gradient_name, gradient_type, repeat, reverse, center_x, center_y, angle, width). For a patterned stroke, use the form (pattern_name, scale, interpolation_type)."),
      (PDB_FLOAT,    "opacity",  "The stroke opacity (0 <= opacity <= 100)"),
      (PDB_INT32,    "mode",     "The stroke layer's combination mode %s" % (modevals)),
      (PDB_INT32,    "size",     "The stroke's width (1 <= size <= 250)"),
      (PDB_FLOAT,    "position", "The stroke's position; 0 = inside the layer borders, 100 = outside the layer border (0 <= position <= 100)"),
      (PDB_INT32,    "merge",    "Merge with layer (TRUE or FALSE)")
    )
    gimp.install_procedure(
      "python_layerfx_stroke",
      stroke_description,
      stroke_help,
      authorname,
      copyrightname,
      date,
      "%sS_troke..." % (imgmenupath),
      "RGBA, GRAYA",
      PLUGIN,
      stroke_params,
      []
    )
    gimp.install_procedure(
      "python_layer_fx_stroke",
      stroke_description,
      stroke_help,
      authorname,
      copyrightname,
      date,
      "%sS_troke..." % (layersmenupath),
      "RGBA, GRAYA",
      PLUGIN,
      stroke_params,
      []
    )
    color_overlay_description = "Overlays a color over a layer."
    color_overlay_help = "Overlays a color over a layer."
    color_overlay_params = (
      (PDB_INT32,    "run_mode", "Run mode"),
      (PDB_IMAGE,    "image",    "Input image"),
      (PDB_DRAWABLE, "drawable", "Input drawable"),
      (PDB_COLOR,    "color",    "The color to overlay"),
      (PDB_FLOAT,    "opacity",  "The overlay opacity (0 <= opacity <= 100)"),
      (PDB_INT32,    "mode",     "The overlay layer's combination mode %s" % (modevals)),
      (PDB_INT32,    "merge",    "Merge with layer (TRUE or FALSE)")
    )
    gimp.install_procedure(
      "python_layerfx_color_overlay",
      color_overlay_description,
      color_overlay_help,
      authorname,
      copyrightname,
      date,
      "%s_Color Overlay..." % (imgmenupath),
      "RGB*, GRAY*",
      PLUGIN,
      color_overlay_params,
      []
    )
    gimp.install_procedure(
      "python_layer_fx_color_overlay",
      color_overlay_description,
      color_overlay_help,
      authorname,
      copyrightname,
      date,
      "%s_Color Overlay..." % (layersmenupath),
      "RGB*, GRAY*",
      PLUGIN,
      color_overlay_params,
      []
    )
    gradient_overlay_description = "Overlays a gradient over a layer."
    gradient_overlay_help = "Overlays a gradient over a layer."
    gradient_overlay_params = (
      (PDB_INT32,    "run_mode",      "Run mode"),
      (PDB_IMAGE,    "image",         "Input image"),
      (PDB_DRAWABLE, "drawable",      "Input drawable"),
      (PDB_STRING,   "gradient",      "The gradient to overlay"),
      (PDB_INT32,    "gradient_type", "The type of gradient { GRADIENT-LINEAR (0), GRADIENT-BILINEAR (1), GRADIENT-RADIAL (2), GRADIENT-SQUARE (3), GRADIENT-CONICAL-SYMMETRIC (4), GRADIENT-CONICAL-ASYMMETRIC (5), GRADIENT-SHAPEBURST-ANGULAR (6), GRADIENT-SHAPEBURST-SPHERICAL (7), GRADIENT-SHAPEBURST-DIMPLED (8), GRADIENT-SPIRAL-CLOCKWISE (9), GRADIENT-SPIRAL-ANTICLOCKWISE (10) }"),
      (PDB_INT32,    "repeat",        "Repeat mode { REPEAT-NONE (0), REPEAT-SAWTOOTH (1), REPEAT-TRIANGULAR (2) }"),
      (PDB_INT32,    "reverse",       "Use the reverse gradient (TRUE or FALSE)"),
      (PDB_FLOAT,    "opacity",       "The overlay opacity (0 <= opacity <= 100)"),
      (PDB_INT32,    "mode",          "The overlay layer's combination mode %s" % (modevals)),
      (PDB_FLOAT,    "center_x",      "X coordinate of center"),
      (PDB_FLOAT,    "center_y",      "Y coordinate of center"),
      (PDB_FLOAT,    "angle",         "Gradient angle (-180 <= angle <= 180)"),
      (PDB_FLOAT,    "width",         "Gradient width (0 <= width <= 262144"),
      (PDB_INT32,    "merge",         "Merge with layer (TRUE or FALSE)")
    )
    gimp.install_procedure(
      "python_layerfx_gradient_overlay",
      gradient_overlay_description,
      gradient_overlay_help,
      authorname,
      copyrightname,
      date,
      "%s_Gradient Overlay..." % (imgmenupath),
      "RGB*, GRAY*",
      PLUGIN,
      gradient_overlay_params,
      []
    )
    gimp.install_procedure(
      "python_layer_fx_gradient_overlay",
      gradient_overlay_description,
      gradient_overlay_help,
      authorname,
      copyrightname,
      date,
      "%s_Gradient Overlay..." % (layersmenupath),
      "RGB*, GRAY*",
      PLUGIN,
      gradient_overlay_params,
      []
    )
    pattern_overlay_description = "Overlays a pattern over a layer."
    pattern_overlay_help = "Overlays a pattern over a layer."
    pattern_overlay_params = (
      (PDB_INT32,    "run_mode",      "Run mode"),
      (PDB_IMAGE,    "image",         "Input image"),
      (PDB_DRAWABLE, "drawable",      "Input drawable"),
      (PDB_STRING,   "pattern",       "The pattern to overlay"),
      (PDB_FLOAT,    "opacity",       "The overlay opacity (0 <= opacity <= 100)"),
      (PDB_INT32,    "mode",          "The overlay layer's combination mode %s" % (modevals)),
      (PDB_FLOAT,    "scale",         "Amount to scale the pattern (1 <= scale <= 1000)"),
      (PDB_INT32,    "interpolation", "Type of interpolation { INTERPOLATION-NONE (0), INTERPOLATION-LINEAR (1), INTERPOLATION-CUBIC (2), INTERPOLATION-LANCZOS (3) }"),
      (PDB_INT32,    "merge",         "Merge with layer (TRUE or FALSE)")
    )
    gimp.install_procedure(
      "python_layerfx_pattern_overlay",
      pattern_overlay_description,
      pattern_overlay_help,
      authorname,
      copyrightname,
      date,
      "%s_Pattern Overlay..." % (imgmenupath),
      "RGB*, GRAY*",
      PLUGIN,
      pattern_overlay_params,
      []
    )
    gimp.install_procedure(
      "python_layer_fx_pattern_overlay",
      pattern_overlay_description,
      pattern_overlay_help,
      authorname,
      copyrightname,
      date,
      "%s_Pattern Overlay..." % (layersmenupath),
      "RGB*, GRAY*",
      PLUGIN,
      pattern_overlay_params,
      []
    )
    reapply_effects_description = "Reapply all effects previously applied to a layer."
    reapply_effects_help = "Reapply all effects previously applied to a layer. NOTE: Does not work for effects that were applied with the \"Merge with layer\" option active."
    reapply_effects_params = (
      (PDB_INT32,    "run_mode", "Run mode"),
      (PDB_IMAGE,    "image",    "Input image"),
      (PDB_DRAWABLE, "drawable", "Input drawable")
    )
    gimp.install_procedure(
      "python_layerfx_reapply_effects",
      reapply_effects_description,
      reapply_effects_help,
      authorname,
      copyrightname,
      date,
      "%s_Reapply Effects" % (imgmenupath),
      "RGB*, GRAY*",
      PLUGIN,
      reapply_effects_params,
      []
    )
    gimp.install_procedure(
      "python_layer_fx_reapply_effects",
      reapply_effects_description,
      reapply_effects_help,
      authorname,
      copyrightname,
      date,
      "%s_Reapply Effects" % (layersmenupath),
      "RGB*, GRAY*",
      PLUGIN,
      reapply_effects_params,
      []
    )

  def python_layerfx_drop_shadow(
    self,
    runmode,
    img,
    drawable,
    color = gimpcolor.RGB(0, 0, 0, 255),
    opacity = 75.0,
    contour = 0,
    noise = 0.0,
    mode = MULTIPLY_MODE,
    spread = 0.0,
    size = 5,
    offsetangle = 120.0,
    offsetdist = 5.0,
    knockout = 0,
    merge = 0
  ):
    layerfx_drop_shadow(runmode, img, drawable, color, opacity, contour, noise, mode, spread, size, offsetangle, offsetdist, knockout, merge)

  def python_layer_fx_drop_shadow(
    self,
    runmode,
    img,
    drawable,
    color = gimpcolor.RGB(0, 0, 0, 255),
    opacity = 75.0,
    contour = 0,
    noise = 0.0,
    mode = MULTIPLY_MODE,
    spread = 0.0,
    size = 5,
    offsetangle = 120.0,
    offsetdist = 5.0,
    knockout = 0,
    merge = 0
  ):
    layerfx_drop_shadow(runmode, img, drawable, color, opacity, contour, noise, mode, spread, size, offsetangle, offsetdist, knockout, merge)

  def python_layerfx_inner_shadow(
    self,
    runmode,
    img,
    drawable,
    color = gimpcolor.RGB(0, 0, 0, 255),
    opacity = 75.0,
    contour = 0,
    noise = 0.0,
    mode = MULTIPLY_MODE,
    source = 1,
    choke = 0.0,
    size = 5,
    offsetangle = 120.0,
    offsetdist = 5.0,
    merge = 0
  ):
    layerfx_inner_shadow(runmode, img, drawable, color, opacity, contour, noise, mode, source, choke, size, offsetangle, offsetdist, merge)

  def python_layer_fx_inner_shadow(
    self,
    runmode,
    img,
    drawable,
    color = gimpcolor.RGB(0, 0, 0, 255),
    opacity = 75.0,
    contour = 0,
    noise = 0.0,
    mode = MULTIPLY_MODE,
    source = 1,
    choke = 0.0,
    size = 5,
    offsetangle = 120.0,
    offsetdist = 5.0,
    merge = 0
  ):
    layerfx_inner_shadow(runmode, img, drawable, color, opacity, contour, noise, mode, source, choke, size, offsetangle, offsetdist, merge)

  def python_layerfx_outer_glow(
    self,
    runmode,
    img,
    drawable,
    color = gimpcolor.RGB(255, 255, 190, 255),
    opacity = 75.0,
    contour = 0,
    noise = 0.0,
    mode = SCREEN_MODE,
    spread = 0.0,
    size = 5,
    knockout = 0,
    merge = 0
  ):
    layerfx_outer_glow(runmode, img, drawable, color, opacity, contour, noise, mode, spread, size, knockout, merge)

  def python_layer_fx_outer_glow(
    self,
    runmode,
    img,
    drawable,
    color = gimpcolor.RGB(255, 255, 190, 255),
    opacity = 75.0,
    contour = 0,
    noise = 0.0,
    mode = SCREEN_MODE,
    spread = 0.0,
    size = 5,
    knockout = 0,
    merge = 0
  ):
    layerfx_outer_glow(runmode, img, drawable, color, opacity, contour, noise, mode, spread, size, knockout, merge)

  def python_layerfx_inner_glow(
    self,
    runmode,
    img,
    drawable,
    color = gimpcolor.RGB(255, 255, 190, 255),
    opacity = 75.0,
    contour = 0,
    noise = 0.0,
    mode = SCREEN_MODE,
    source = 1,
    choke = 0.0,
    size = 5,
    merge = 0
  ):
    layerfx_inner_glow(runmode, img, drawable, color, opacity, contour, noise, mode, source, choke, size, merge)

  def python_layer_fx_inner_glow(
    self,
    runmode,
    img,
    drawable,
    color = gimpcolor.RGB(255, 255, 190, 255),
    opacity = 75.0,
    contour = 0,
    noise = 0.0,
    mode = SCREEN_MODE,
    source = 1,
    choke = 0.0,
    size = 5,
    merge = 0
  ):
    layerfx_inner_glow(runmode, img, drawable, color, opacity, contour, noise, mode, source, choke, size, merge)

  def python_layerfx_bevel_emboss(
    self,
    runmode,
    img,
    drawable,
    style = 0,
    depth = 3,
    direction = 0,
    size = 5,
    soften = 0,
    angle = 120.0,
    altitude = 30.0,
    glosscontour = 0,
    highlightcolor = gimpcolor.RGB(255, 255, 255, 255),
    highlightmode = SCREEN_MODE,
    highlightopacity = 75.0,
    shadowcolor = gimpcolor.RGB(0, 0, 0, 255),
    shadowmode = MULTIPLY_MODE,
    shadowopacity = 75.0,
    surfacecontour = 0,
    use_texture = 0,
    pattern = "",
    scale = 100.0,
    tex_depth = 100.0,
    invert = 0,
    merge = 0
  ):
    layerfx_bevel_emboss(runmode, img, drawable, style, depth, direction, size, soften, angle, altitude, glosscontour, highlightcolor, highlightmode, highlightopacity, shadowcolor, shadowmode, shadowopacity, surfacecontour, use_texture, pattern, scale, tex_depth, invert, merge)

  def python_layer_fx_bevel_emboss(
    self,
    runmode,
    img,
    drawable,
    style = 0,
    depth = 3,
    direction = 0,
    size = 5,
    soften = 0,
    angle = 120.0,
    altitude = 30.0,
    glosscontour = 0,
    highlightcolor = gimpcolor.RGB(255, 255, 255, 255),
    highlightmode = SCREEN_MODE,
    highlightopacity = 75.0,
    shadowcolor = gimpcolor.RGB(0, 0, 0, 255),
    shadowmode = MULTIPLY_MODE,
    shadowopacity = 75.0,
    surfacecontour = 0,
    use_texture = 0,
    pattern = "",
    scale = 100.0,
    tex_depth = 100.0,
    invert = 0,
    merge = 0
  ):
    layerfx_bevel_emboss(runmode, img, drawable, style, depth, direction, size, soften, angle, altitude, glosscontour, highlightcolor, highlightmode, highlightopacity, shadowcolor, shadowmode, shadowopacity, surfacecontour, use_texture, pattern, scale, tex_depth, invert, merge)

  def python_layerfx_satin(
    self,
    runmode,
    img,
    drawable,
    color = gimpcolor.RGB(0, 0, 0, 255),
    opacity = 75.0,
    mode = MULTIPLY_MODE,
    offsetangle = 19.0,
    offsetdist = 11.0,
    size = 14,
    contour = 5,
    invert = 1,
    merge = 0
  ):
    layerfx_satin(runmode, img, drawable, color, opacity, mode, offsetangle, offsetdist, size, contour, invert, merge)

  def python_layer_fx_satin(
    self,
    runmode,
    img,
    drawable,
    color = gimpcolor.RGB(0, 0, 0, 255),
    opacity = 75.0,
    mode = MULTIPLY_MODE,
    offsetangle = 19.0,
    offsetdist = 11.0,
    size = 14,
    contour = 5,
    invert = 1,
    merge = 0
  ):
    layerfx_satin(runmode, img, drawable, color, opacity, mode, offsetangle, offsetdist, size, contour, invert, merge)

  def python_layerfx_stroke(
    self,
    runmode,
    img,
    drawable,
    color = gimpcolor.RGB(255, 0, 0, 255),
    opacity = 100.0,
    mode = NORMAL_MODE,
    size = 3,
    position = 50.0,
    merge = 0
  ):
    layerfx_stroke(runmode, img, drawable, color, opacity, mode, size, position, merge)

  def python_layer_fx_stroke(
    self,
    runmode,
    img,
    drawable,
    color = gimpcolor.RGB(255, 0, 0, 255),
    opacity = 100.0,
    mode = NORMAL_MODE,
    size = 3,
    position = 50.0,
    merge = 0
  ):
    layerfx_stroke(runmode, img, drawable, color, opacity, mode, size, position, merge)

  def python_layerfx_color_overlay(
    self,
    runmode,
    img,
    drawable,
    color = gimpcolor.RGB(255, 255, 255, 255),
    opacity = 100.0,
    mode = NORMAL_MODE,
    merge = 0
  ):
    layerfx_color_overlay(runmode, img, drawable, color, opacity, mode, merge)

  def python_layer_fx_color_overlay(
    self,
    runmode,
    img,
    drawable,
    color = gimpcolor.RGB(255, 255, 255, 255),
    opacity = 100.0,
    mode = NORMAL_MODE,
    merge = 0
  ):
    layerfx_color_overlay(runmode, img, drawable, color, opacity, mode, merge)

  def python_layerfx_gradient_overlay(
    self,
    runmode,
    img,
    drawable,
    gradient = "FG to BG (RGB)",
    gradienttype = GRADIENT_LINEAR,
    repeat = REPEAT_NONE,
    reverse = 0,
    opacity = 100.0,
    mode = NORMAL_MODE,
    centerx = 0.0,
    centery = 0.0,
    angle = 90.0,
    width = 10.0,
    merge = 0
  ):
    layerfx_gradient_overlay(runmode, img, drawable, gradient, gradienttype, repeat, reverse, opacity, mode, centerx, centery, angle, width, merge)

  def python_layer_fx_gradient_overlay(
    self,
    runmode,
    img,
    drawable,
    gradient = "FG to BG (RGB)",
    gradienttype = GRADIENT_LINEAR,
    repeat = REPEAT_NONE,
    reverse = 0,
    opacity = 100.0,
    mode = NORMAL_MODE,
    centerx = 0.0,
    centery = 0.0,
    angle = 90.0,
    width = 10.0,
    merge = 0
  ):
    layerfx_gradient_overlay(runmode, img, drawable, gradient, gradienttype, repeat, reverse, opacity, mode, centerx, centery, angle, width, merge)

  def python_layerfx_pattern_overlay(
    self,
    runmode,
    img,
    drawable,
    pattern = "",
    opacity = 100.0,
    mode = NORMAL_MODE,
    scale = 100.0,
    interpolation_type = INTERPOLATION_NONE,
    merge = 0
  ):
    layerfx_pattern_overlay(runmode, img, drawable, pattern, opacity, mode, scale, interpolation_type, merge)

  def python_layer_fx_pattern_overlay(
    self,
    runmode,
    img,
    drawable,
    pattern = "",
    opacity = 100.0,
    mode = NORMAL_MODE,
    scale = 100.0,
    interpolation_type = INTERPOLATION_NONE,
    merge = 0
  ):
    layerfx_pattern_overlay(runmode, img, drawable, pattern, opacity, mode, scale, interpolation_type, merge)

  def python_layerfx_reapply_effects(
    self,
    runmode,
    img,
    drawable
  ):
    layerfx_reapply_effects(runmode, img, drawable)

  def python_layer_fx_reapply_effects(
    self,
    runmode,
    img,
    drawable
  ):
    layerfx_reapply_effects(runmode, img, drawable)

if __name__ == "__main__":
  layerfxplugin().start()
