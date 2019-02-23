log='/home/adam/autostart.log'

echo "[$(data)] setting resolution; output = " >> log
xrandr --output eDP1 --mode 1600x900 --output DP2 --mode 1920x1080 --right-of eDP1 >> log
