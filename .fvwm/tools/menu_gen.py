#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import sys
from copy import deepcopy


#################  Customized Variables #################
FVWM_HOME = os.path.join(os.getenv("HOME"), ".fvwm")

menu_template = "Menu_Template"
menu_template = os.path.join(FVWM_HOME, "tools", menu_template)
menu_template_head = menu_template + "_Head"
menu_template_tail = menu_template + "_Tail"

fvwm_icon_home = os.path.join(FVWM_HOME, "icons/apps")
fvwm_menu_output = os.path.join(FVWM_HOME, "Menu")

DESKTOP_SEARCH_PATH = ["/usr/share/applications"]
ICON_PATHS          = ["/usr/share/pixmaps", "/usr/share/icons/hicolor",
                       "/home/yyc/.icons/default"]


#             +-----------+------------+-------------+-----------+
# fvwm_menu = | Category1 | Category2  | Category 3  | ........  |
#             +----+------+------------+-------------+-----------+
#                  |
#                  v                 +--> Name = ...
#             +-----------+          |
#             |  Entry 1  |--------> +--> Exec = ...
#             +-----------+          |
#             |  Entry 2  |          +--> Icon = ...
#             +-----------+
#             |  Entry 3  |
#             +-----------+
#             |    .      |
#             +-----------+
fvwm_menu     = {}
img_data      = {}
CATEGORY_LIST = ["Office", "Graphics", "System", "Engineering" "Utility",
                 "Network", "Development", "AudioVideo"]
keywords      = ["Name", "Exec", "Icon", "Categories"]

verbose = True

def log_print(msg):
    global verbose
    if verbose:
        print msg
    else:
        pass

def gen_img_data():
    for path in ICON_PATHS:
        if os.path.islink(path):
            path = os.path.realpath(path)
        log_print("\t Processing icon directory: %s"%path)
        os.path.walk(path, process_img, None)

def process_img(arg, dirname, filenames):
    """
    Process each image in subdir, store in img_data.
    """
    for fn  in filenames:
        path = os.path.join(dirname, fn)
        if "16x16" in path or "22x22" in path:
            continue
        if os.path.isdir(path):
            continue
        else:
            key = os.path.basename(path).split(".")[0].lower()
            img_data[key] = path


def process_desktop_entries(arg, dirname, filenames):
    """
    Parse each file in this subdir, store information into fvwm_menu;
    """
    for filename in filenames:
        path = os.path.join(dirname, filename)
        log_print ("Processing %s"%path)
        if os.path.isdir(path):
            continue
        else:
            tmp = parse_single(path)
            if tmp.get("Show") :
                add_to_menubase(deepcopy(tmp))

def parse_single(path):
    """
    Parse single desktop entry, return the pared entry.
    Arguments:
    - `path`:
    """
    content = open(path).readlines()
    tmp_dic = {}
    for item in content:
        for key in keywords:
            if item.startswith(key+"="):
                val = item.split("=")[-1].strip("\n")
                if key == "Categories": # Catergories
                    pos = val.find(";")
                    if  pos != -1:
                        vlist = val.split(";")
                        val = "Other"
                        for v in vlist:
                            if v in CATEGORY_LIST:
                                val = v
                                break
                elif key == "Exec":
                    pos = val.rfind("%")
                    if pos != -1:
                        val = val[:pos]
                    pos = val.rfind(" -")
                    if pos != -1:
                        val = val[:pos]
                tmp_dic[key] = val
                break
    if tmp_dic.get("Icon") is None or "@" in tmp_dic.get("Icon"):
        tmp_dic["Show"] = False
    else:
        tmp_dic["Show"] = True

    return deepcopy(tmp_dic)

def add_to_menubase(dic):
    """
    Add parsed menu into global database.
    """
    global fvwm_menu
    if dic.get("Categories") is None:
        dic["Categories"] = "Other"

    if not fvwm_menu.has_key(dic["Categories"]):
        fvwm_menu[dic["Categories"]] = []
    fvwm_menu[dic["Categories"]].append(dic)

def sort_name(entry1, entry2):
    return (entry1["Name"] <= entry2["Name"])

def find_icon(name, menu_type=1):
    """
    """
    if os.access(name, os.F_OK): # Input is the absloute path of file.
        return name
    if not menu_type: # Categories start with applications-.
        if name == "network":
            name = "internet"
        elif name == "audiovideo":
            name = "multimedia"
        name = "applications-%s.png"%name

    pos = name.rfind(".")
    if pos != -1:
        name = name[:pos]
    icon_path = img_data.get(name.lower())
    if icon_path is None:
        icon_path = ""
    return icon_path


if __name__ == '__main__':

    if len(sys.argv) > 1:
        verbose = False

    print "Generating image database ..."
    gen_img_data()
    print "Finished to generate image database\n"

    print "Searching and analyzing desktop entries..."
    for item in DESKTOP_SEARCH_PATH:
        os.path.walk(item, process_desktop_entries, item)
    print "Finished analyze desktop entries.\n"

    print "Writing new menu items for fvwm..."
    content = open(menu_template_head).read() # Head of template
    ### Generate categories.
    for key in fvwm_menu.keys():
        icon_path = find_icon(key.lower(), 0)
        icon_out = ""

        if len(icon_path):
            icon_out = os.path.join(fvwm_icon_home, os.path.basename(icon_path));
            os.system("convert -resize 24x24 %s %s"%(icon_path, icon_out))

        content += '+ "%%%s%%%s" Popup Menu%s\n'%(icon_out, key, key)

    content += open(menu_template_tail).read() # Tail fo template


    ### Generate Desktop entry.

    for key in fvwm_menu.keys():
        content += "DestroyMenu Menu%s\nAddToMenu Menu%s\n"%(key, key)

        tmp_list = fvwm_menu[key]
        val_list = sorted(tmp_list, cmp=sort_name)

        log_print ("Adding category: %s"%key)
        for val in val_list:
            name = val.get("Name")
            exec_file = val.get("Exec")
            icon = val.get("Icon")
            if name is None or exec_file is None or icon is None:
                continue

            log_print ("\t+---- Adding entry: %s"%name)
            icon_path = find_icon(icon, 1)
            icon = os.path.basename(icon)
            pos = icon.rfind(".")
            if pos != -1:
                icon = icon[:pos]
            icon_out = ""

            if len(icon_path):
                icon_out = os.path.join(fvwm_icon_home, "%s.png"%icon).lower();
                os.system("convert -resize 24x24 %s %s"%(icon_path, icon_out))

            content += '+ "%%%s%%%s" Exec exec %s\n'%(icon_out, name,
                                                      exec_file)
        log_print ("\n")


        content += "\n\n"

    fp = open(fvwm_menu_output, "w")
    fp.write(content)
    fp.flush()
    fp.close()

    print "All work done!"
