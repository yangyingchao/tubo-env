#!/bin/bash
#

for container in `docker ps | grep -v  -i 'container id' | awk '{print $1}'`; do
    address=`docker exec ${container} 'ifconfig' | grep broadcast | awk '{print $2}'`
    if [ -n "${address}" ]; then
        echo "${container}    ${address}"
    fi
done
