#!/bin/bash

xrandr --output DP-2 --auto
xrandr --output LVDS-1 --off
xrandr --output DP-3 --auto --primary --left-of DP-2
