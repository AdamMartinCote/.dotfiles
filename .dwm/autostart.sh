#!/usr/bin/env bash

function log () {
	logfile='/home/adam/.startup.log'
	echo "[$(date)] $1" >> $logfile
}

log "autostart script commencing"

# set keyboard rate
xset r rate 200 40

log "rate set"

# start compositor
#ret=$(compton 2>&1 &)
#if [ "$ret" != "" ]; then
#	log "$ret"
#fi
compton&

log "comp set"

# Top bar
./top_bar.sh&

log "top bar set"

# set media keys with ~/.xbindkey
xbindkeys

log "xbindkeys set"

# Screen Resolution
hi_res='1920x1080'
med_res='1600x900'
ext_monitor=$(xrandr | grep DP2 | cut -d ' ' -f2)
if [ "$ext_monitor" == "connected" ]; then
		log "setting 2-screens resolution"
		ret=$(xrandr --output eDP1 --mode $med_res --output DP2 --mode 1920x1080 --right-of eDP1 2>&1)
else
		log "setting 1-screen resolution"
		ret=$(xrandr --output eDP1 --mode $hi_res 2>&1)
fi

if [ "$ret" != "" ]; then
	log "xrandr: $ret"
fi

# Wallpaper
feh --bg-fill '/home/adam/Pictures/temple-reflexion.jpg'		

log "autostart done"

