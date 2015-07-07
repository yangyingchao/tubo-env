#!/bin/bash

# Set the static decrement value.  Keep in mind that this will
# be done twice.
read -r MaxVal < "/sys/class/backlight/intel_backlight/max_brightness"
DecVal=$((MaxVal/100))

# Set the Minimum we will accept.
MinVal=0

# Get the current brightness value.
#CurrVal=$(cat /sys/class/backlight/intel_backlight/brightness);
read -r CurrVal < "/sys/class/backlight/intel_backlight/brightness"

# Set the new value minus the decrement value.
NewVal=$(($CurrVal - $DecVal));
echo $NewVal

# Set it to the threshold of the min value.
ThresholdVal=$(($NewVal>$MinVal?$NewVal:$MinVal))
echo $ThresholdVal

# Set the new value directly.
echo -n $ThresholdVal | sudo tee /sys/class/backlight/intel_backlight/brightness
