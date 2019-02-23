#!/usr/bin/env bash


function get_batval () {
		cat /sys/class/power_supply/BAT0/capacity
}

function get_battery_display {
		# bat=$(cat /sys/class/power_supply/BAT0/capacity)
		bat="$1"
		case $bat in
				100)
						echo "[■■■■■]"
						;;
				[8-9]*)
						echo "[ ■■■■]"
						;;
				[6-7]*)
						echo "[  ■■■]"
						;;
				[4-5]*)
						echo "[   ■■]"
						;;
				[2-3]*)
						echo "[    ■]"
						;;
				[0-1]*)
						echo "[  !  ]"
						;;
		esac
}

while true
do
		batval=$(get_batval)
		xsetroot -name "$(get_battery_display $batval) $batval% | $( date +"%F %R:%S" )"
		sleep 1
done& disown
