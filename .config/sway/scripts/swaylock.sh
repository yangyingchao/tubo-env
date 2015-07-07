#!/bin/bash
#


swaylock --screenshots --clock --indicator \
         --indicator-radius 100 \
         --indicator-thickness 12 \
         --ring-color 2E3440 \
         --key-hl-color ECEFF4 \
         --line-color 88C0D0 \
         --inside-color 00000088 \
         --separator-color 00000000 \
         --datestr %Y-%m-%d \
         --text-color ECEFF4 \
         --text-caps-lock-color ECEFF4 \
         --show-failed-attempts \
         --fade-in 0.1 \
         --effect-scale 0.5 \
         --effect-blur 15x2 \
         --effect-scale 2 \
         --effect-vignette 0.5:0.5
