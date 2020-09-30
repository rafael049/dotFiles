#!/bin/bash


EDP=`xrandr | grep -e "^eDP.*[ ]connected" | awk '{print $1}'`
HDMI=`xrandr | grep -e "^HDMI.*[ ]connected" | awk '{print $1}'`

echo $HDMI


if [ -n "$HDMI" ];then
    `xrandr --output "$HDMI" --auto`
    `xrandr --output "$EDP" --off`
fi

