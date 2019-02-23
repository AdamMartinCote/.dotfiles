
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
