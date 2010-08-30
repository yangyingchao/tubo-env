#!/usr/bin/env python
import os

if __name__ == '__main__':
    for file in os.listdir("."):
        if file.endswith("x"):
            os.rename(file, file[:-1])
            print "Finished: %s"%file
