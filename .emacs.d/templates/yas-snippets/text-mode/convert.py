#!/usr/bin/env python
import os

def lst2str (lst):
    string = ""
    for item in lst:
        string += item
    return string



def func(args, dirname, fnames):

    if dirname.find(".svn") != -1:
        return

    for fn in fnames:
        path = os.path.join(dirname, fn)
        if os.path.isdir(path):
            continue

        print "Processing: ", path
        flag = False
        content = open(path).readlines()
        if not content:
            print "Empty file: ", path
            continue

        if (content[1].find("# -*- mode: snippet -*-") == -1):
            for item in content[2:]:
                if item.find("# --") != -1:
                    content.pop(1)
                    flag = True
                    break
        if not flag:
            continue
        else:
            print "Modifying ", path

        fp = open(path, "w")

        if fp is None:
            print("Failed to open file: %s\n"%path)
            continue

        fp.write(lst2str(content))

        fp.flush()

        fp.close()



if __name__ == '__main__':

    os.path.walk(".", func, None)

