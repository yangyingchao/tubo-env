#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
from copy import deepcopy

menu_template = "Menu_Template"
desktop_search_path = ["/usr/share/applications"]
categories_icon_path = "/usr/share/icons/gnome/24x24/categories"
icon_paths = ["/usr/share/pixmaps", "/usr/share/icons/hicolor",
              "/usr/share/icons/oxygen", "/home/yyc/.icons/Mac4Lin_Icons"]
menu_output = "Menu"
fvwm_menu = {}
category_list = ["Office", "Graphics", "System", "Engineering" "Utility",
                 "Network", "Development"]
keywords = ["Name", "Exec", "Icon", "Categories"]

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

img_data = {}

def gen_img_data():
    for path in icon_paths:
        os.path.walk(path, process_img, None)

def process_img(arg, dirname, filenames):
    """
    Process each image in subdir, store in img_data.
    """
    for fn  in filenames:
        path = os.path.join(dirname, fn)
        if os.path.isdir(path):
            os.path.walk(path, process_img, None)
        else:
            key = os.path.basename(path).split(".")[0].lower()
            img_data[key] = path

def process_subdir(arg, dirname, filenames):
    """
    Parse each file in this subdir, store information into fvwm_menu;
    """
    path = os.path.join(arg, dirname)
    for filename in filenames:
        path_tmp = os.path.join(path, filename)

        if os.path.isdir(path_tmp):
            os.path.walk(path_tmp, process_subdir, path_tmp)
        else:
            tmp = parse_single(path_tmp)
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
                pos = val.find(";")
                if  pos != -1:
                    vlist = val.split(";")
                    val = "Other"
                    for v in vlist:
                        if v in category_list:
                            val = v
                            break
                tmp_dic[key] = val
                break
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

def find_icon(name, menu_type=1):
    """
    """
    print "Input: %s"%name
    if os.access(name, os.F_OK): # Input is the absloute path of file.
        print "Output: %s"%name
        return name
    if menu_type:
        pos = name.rfind(".")
        if pos != -1:
            name = name[:pos]
        icon_path = img_data.get(name.lower())
        if icon_path is None:
            icon_path = ""
    else:
        if name == "network":
            name = "internet"
        icon_path = os.path.join(categories_icon_path,
                                 "applications-%s.png"%name)
        if not os.access(icon_path, os.F_OK):
            icon_path = ""
    return icon_path


if __name__ == '__main__':
    fvwm_home = os.path.join(os.getenv("HOME"), ".fvwm")
    icon_home = os.path.join(os.getenv("HOME"), ".fvwm", "icons/24x24")
    menu_template = os.path.join(os.getenv("HOME"), ".fvwm", menu_template)
    menu_template_head = menu_template + "_Head"
    menu_template_tail = menu_template + "_Tail"
    menu_output = os.path.join(os.getenv("HOME"), ".fvwm", menu_output)

    gen_img_data()

    for item in desktop_search_path:
        os.path.walk(item, process_subdir, item)

    content = open(menu_template_head).read() # Head of template

    ### Generate categories.

    for key in fvwm_menu.keys():
        icon_path = find_icon(key.lower(), 0)
        icon_out = ""

        if len(icon_path):
            icon_out = os.path.join(icon_home, os.path.basename(icon_path));
            os.system("convert -resize 24x24 %s %s"%(icon_path, icon_out))

        content += '+ "%%%s%%%s" Popup Menu%s\n'%(icon_out, key, key)

    content += open(menu_template_tail).read() # Tail fo template


    ### Generate Desktop entry.

    for key in fvwm_menu.keys():
        content += "DestroyMenu Menu%s\nAddToMenu Menu%s\n"%(key, key)

        val_list = fvwm_menu[key]
        for val in val_list:
            name = val.get("Name")
            exec_file = val.get("Exec")
            icon = val.get("Icon")
            if name is None or exec_file is None or icon is None:
                continue

            print "Adding entry: %s\n"%name
            icon_path = find_icon(icon, 1)
            print "Using icon: %s"%icon_path
            icon = os.path.basename(icon)
            pos = icon.rfind(".")
            if pos != -1:
                icon = icon[:pos]
            icon_out = ""

            if len(icon_path):
                icon_out = os.path.join(icon_home, "%s.png"%icon).lower();
                os.system("convert -resize 24x24 %s %s"%(icon_path, icon_out))

            content += '+ "%%%s%%%s" Exec exec %s\n'%(icon_out, name, exec_file)

        content += "\n\n"

    fp = open(menu_output, "w")
    fp.write(content)
    fp.flush()
    fp.close()
