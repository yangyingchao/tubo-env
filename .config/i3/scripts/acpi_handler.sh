#!/bin/bash
#
source ${HOME}/.config/i3/scripts/common.sh

set $*

group=${1%%/*}
action=${1#*/}
device=$2
id=$3
value=$4

function get_number_of_monitors ()
{
    local number=0
    for DEVICE in /sys/class/drm/card*-*; do
        [ -e "${DEVICE}/status" ] && grep -q "^connected$" "${DEVICE}/status" || continue
        number=$((number+1))
    done
    echo $number
    return $number
}


do_log_unhandled() {
    echo "ACPI event not handled: $@"
}


case "$group" in
    battery)
    ;;
	button)
		case "$action" in
			power)
				/etc/acpi/actions/powerbtn.sh
				;;
            lid)
                case $device in
                    LID)
                        case $id in
                            close)
                                # actions when LID is closed: switch to external
                                # display, or suspend if there is no external
                                # display,

                                num=`get_number_of_monitors`

                                if [ $num -eq 1 ]; then
                                    # only one monitor, suspend...
                                    systemctl suspend
                                elif [ $num -eq 2 ]; then
                                    # external monitor is connected, turn off internal monitor
                                    # DISPLAY=:0
                                    su - arthas -c "/home/arthas/.config/i3/scripts/setup-display.sh External"
                                fi
                                ;;
                            *)
                                ;;
                        esac
                        ;;
                    *)
                        logger "ACPI: device ${device} not handled."
                        ;;
                esac
                ;;

			# if your laptop doesnt turn on/off the display via hardware
			# switch and instead just generates an acpi event, you can force
			# X to turn off the display via dpms.  note you will have to run
			# 'xhost +local:0' so root can access the X DISPLAY.
			#lid)
			#	xset dpms force off
			#	;;

			*)	do_log_unhandled $* ;;
		esac
		;;

	ac_adapter)
		case "$value" in
			# Add code here to handle when the system is unplugged
			# (maybe change cpu scaling to powersave mode).  For
			# multicore systems, make sure you set powersave mode
			# for each core!
			#*0)
			#	cpufreq-set -g powersave
			#	;;

			# Add code here to handle when the system is plugged in
			# (maybe change cpu scaling to performance mode).  For
			# multicore systems, make sure you set performance mode
			# for each core!
			#*1)
			#	cpufreq-set -g performance
			#	;;

			*)	do_log_unhandled $* ;;
		esac
		;;
	*)	do_log_unhandled $* ;;
esac
