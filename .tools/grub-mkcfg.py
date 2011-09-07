#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import sys

content = "default 0\ntimeout 2\n\n"

def parse_root(info):
    entry = info[0]
    if not entry.startswith("/dev/sd"):
        print "entry is: %s, not supported now."%entry
        sys.exit(1)

    rt_info = {}

    rt_info["path"]      = entry
    rt_info["type"]      = info[2]

    return rt_info

def parse_boot(info):
    entry = info[0]
    if not entry.startswith("/dev/sd"):
        print "entry is: %s, not supported now."%entry
        sys.exit(1)

    bt_info = {}

    bt_info["path"]      = entry
    bt_info["disk"]      = ord(entry[7]) - ord('a')
    bt_info["partition"] = ord(entry[8]) - ord('1')

    return bt_info


def get_disk_info():
    ret = {}
    try:
        fd = open("/etc/fstab", "r")
        disks = fd.readlines()
        fd.close()
    except:
        print sys.exc_info()
        return None
    else:
        for item in disks:
            item = item.replace("\t", " ").strip()
            if len(item) == 0:
                continue
            info = item.split()
            if info[1] == "/": # ROOT Entry
                ret["root_disk"] = parse_root(info)
            elif info[1] == "/boot": # BOOT Entry
                ret["boot_disk"] = parse_boot(info)
            else:
                continue
    return ret


def get_kernel_list():
    """
    List installed kernels.
    """
    kernels = []

    for item in os.listdir("/boot"):
        if item.startswith("vmlinuz"):
            kernels.append(item)
    return kernels



if __name__ == '__main__':

    disk_info = get_disk_info()

    if disk_info is None:
        print "Failed to get boot disk!"
        sys.exit(1)

    kernel_list = get_kernel_list()

    if not kernel_list:
        print "Failed to list kernels"
        sys.exit(1)

    for kernel in kernel_list:
        version = kernel.split("-")[1]
        content += "title Getoo Linux %s\n"%version
        content += "root (hd%d,%d)\n"%\
            (disk_info.get("boot_disk").get("disk"),
             disk_info.get("boot_disk").get("partition"))
        content += "kernel /%s root=%s ro quiet rootfstype=%s"%\
            (kernel, disk_info.get("root_disk").get("path"),
             disk_info.get("root_disk").get("type"))
        content += " \n\n"

    fd = open("/boot/grub/grub.conf", "w")
    fd.write(content)
    fd.close()
    print "Finished, add boot entries"
    for item in kernel_list:
        print "\t%s"%item


