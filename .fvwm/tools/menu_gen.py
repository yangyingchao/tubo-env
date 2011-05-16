#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
from copy import deepcopy


#################  Customized Variables #################
FVWM_HOME = os.path.join(os.getenv("HOME"), ".fvwm")

menu_template = "Menu_Template"
menu_template = os.path.join(FVWM_HOME, "tools", menu_template)
menu_template_head = menu_template + "_Head"
menu_template_tail = menu_template + "_Tail"

fvwm_icon_home = os.path.join(FVWM_HOME, "icons/apps")
fvwm_menu_output = os.path.join(FVWM_HOME, "Menu")

DESKTOP_SEARCH_PATH = ["/usr/share/applications", "/home/yyc/.local/share/applications"]
ICON_PATHS          = ["/usr/share/pixmaps", "/usr/share/icons/hicolor",
                       "/home/yyc/.icons/Mac4Lin_Icons"]

img_data      = {}
CATEGORY_LIST = ["Office", "Graphics", "System",
                 "Network", "Development", "AudioVideo", "Other"]
keywords      = ["Name", "Exec", "Icon", "Categories" ]

ENTRY_ROOT = 1
ENTRY_FILE = 2
ENTRY_DIR  = 3
ENTRY_INVALID = 4

root_entry = None

class Entry:
    """
    Virtual Entry.
    """

    def __init__(self, name, parent=None):
        self.name = name
        self.parent = parent
        self.type = ENTRY_ROOT
        self.children_list = []
        self.action = "Popup"
        self.direntries = {} # for quick search

    def Add(self, child, path="fake"):
        """
        Add entry to children_list.
        Arguments:
        - `entry`: Child to append.
        """
        self.children_list.append(child)
        self.direntries[path] = child
        child.parent = self

    def Dump(self):
        content = ""
        for obj in self.children_list:
            if obj.type == ENTRY_DIR:
                tmp = '+ "%%%s%%%s" Popup Menu%s\n'%(obj.icon, obj.name,
                                                   obj.name)
            else:
                continue
            content += tmp
        return content

    def mycmp(self, a, b):
        return a.name < b.name

    def MakeDetail(self):
        content = ""
        self.children_list = sorted(self.children_list, self.mycmp)
        for obj in self.children_list:
            if obj.type == ENTRY_DIR:
                content += obj.Dump()
        return content



class DirEntry(Entry):
    """
    Directory Entry, may be a virtual entry.
    """
    def __init__(self, path, parent=None, virtual=False):
        Entry.__init__(self, os.path.basename(path), parent)
        self.type = ENTRY_DIR
        if not virtual:
            self.icon = ""
            self.name = os.path.basename(path);
        else:
            name = self.name.lower()
            if name == "network":
                name = "internet"
            elif name == "audiovideo":
                name = "multimedia"
            name = "applications-%s"%name
            icon_path = img_data.get(name.lower())
            if icon_path is None:
                print "DirEntry: Can not find icon: %s"%(name)
                icon_out = ""
            else:
                icon_out = os.path.join(fvwm_icon_home,
                                        os.path.basename(icon_path));
                os.system("convert -resize 24x24 %s %s"%(icon_path, icon_out))
            self.icon = icon_out

    def Dump(self):
        content = ""
        content = "DestroyMenu Menu%s\nAddToMenu Menu%s\n"%\
                  (self.name, self.name)
        for obj in self.children_list:
            print "\tAdding: %s"%obj.path
            content += obj.Dump()
        return content

def find_parent(input, is_category=True):
    """
    Find parent object of this input.
    """
    if is_category:
        for obj in root_entry.children_list:
            if input.lower() in obj.name.lower():
                return obj
        for obj in root_entry.children_list:
            if "other" in obj.name.lower():
                return obj
    else:
        path = os.path.dirname(input)
        entry = root_entry.direntries.get(path)
        if entry is None:
            entry = root_entry
        return entry

class DesktopEntry:
    """
    Desktop Entry Classs.
    """
    def __init__(self, path, parent=None):
        """
        Init function of DesktopEntry Class.
        """
        self.path     = path
        self.name     = ""
        self.category = "Other"
        self.icon     = ""
        self.exec_file     = ""

        if (path is None) or (os.path.isdir(path)):
            return

        content = open(path).readlines()
        tmp_dic = {}
        for item in content:
            key = item.split("=")[0]
            val = item.split("=")[-1].strip("\n")

            if key == "Name":
                self.name = val

            elif key == "Icon":
                if os.access(val, os.F_OK):
                    icon_out = os.path.join(fvwm_icon_home,
                                    os.path.basename(val));
                    os.system("convert -resize 24x24 %s %s"%\
                              (val, icon_out))
                else:
                    val = val.lower()
                    pos = val.rfind(".")
                    if pos != -1:
                        val = val[:pos]
                    icon_path = img_data.get(val.lower())
                    if icon_path is None:
                        print "Desktop Entry: Can not find icon: %s"%val
                        icon_out = ""
                    else:
                        icon_out = os.path.join(fvwm_icon_home,
                                        os.path.basename(icon_path));
                        os.system("convert -resize 24x24 %s %s"%\
                                  (icon_path, icon_out))
                self.icon = icon_out
            elif key == "Categories":
                pos = val.find(";")
                if  pos != -1:
                    vlist = val.split(";")
                    for v in vlist:
                        if v in CATEGORY_LIST:
                            val = v
                            break
                    self.category = val

            elif key == "Exec":
                pos = val.rfind("%")
                if pos != -1:
                    val = val[:pos]
                pos = val.rfind(" -")
                if pos != -1:
                    val = val[:pos]
                self.exec_file = val

            else:
                pass

        if self.category:
            self.parent = find_parent(self.category)
        else:
            self.parent = find_parent(path, False)

        if not self.exec_file or len(self.icon) == 0:
            print "INVALID: ", self.path
            self.type = ENTRY_INVALID
        else:
            self.type = ENTRY_FILE
            self.parent.children_list.append(self)

    def Dump(self):
        """
        Change this object into strin form.
        """
        string = '+ "%%%s%%%s" Exec exec %s\n'%(self.icon, self.name,
                                                self.exec_file)
        return string


def gen_img_data():
    for path in ICON_PATHS:
        if os.path.islink(path):
            path = os.path.realpath(path)
        print "\t Processing icon directory: %s"%path
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
    Parse each file in this subdir.
    """
    parent = arg
    for filename in filenames:
        path = os.path.join(dirname, filename)
        if os.path.isdir(path):
            continue
        else:
            entry = DesktopEntry(path, parent)


if __name__ == '__main__':


    print "Generating image database ..."
    gen_img_data()
    print "Finished to generate image database\n"

    print "Setting up virtual menu hirechy ..."
    root_entry = Entry("ROOT", None)
    for name in CATEGORY_LIST:
        obj = DirEntry(name, parent=root_entry, virtual=True)
        root_entry.Add(obj)
    print "Done."

    print "Searching and analyzing desktop entries..."
    for item in DESKTOP_SEARCH_PATH:
        if os.path.islink(item):
            item = os.path.realpath(item)
        os.path.walk(item, process_desktop_entries, root_entry)
    print "Finished analyze desktop entries.\n"

    print "Writing new menu items for fvwm..."
    content = open(menu_template_head).read() # Head of template
    content += root_entry.Dump()
    content += open(menu_template_tail).read() # Tail fo template
    content += "\n\n"
    content += root_entry.MakeDetail()

    fp = open(fvwm_menu_output, "w")
    fp.write(content)
    fp.flush()
    fp.close()

    print "All work done!"
