#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Utility to parse and process apple property list.

import traceback
import os
import sys
import math
import struct

BPLIST_MAGIC = b'bplist'
BPLIST_MAGIC_SIZE = 6

BPLIST_VERSION = b"00"
BPLIST_VERSION_SIZE = 2

BPLIST_NULL = 0x00
BPLIST_FALSE = 0x08
BPLIST_TRUE = 0x09
BPLIST_FILL = 0x0F
BPLIST_UINT = 0x10
BPLIST_REAL = 0x20
BPLIST_DATE = 0x30
BPLIST_DATA = 0x40
BPLIST_STRING = 0x50
BPLIST_UNICODE = 0x60
BPLIST_UNK_0x70 = 0x70
BPLIST_UID = 0x80
BPLIST_ARRAY = 0xA0
BPLIST_SET = 0xC0
BPLIST_DICT = 0xD0
BPLIST_MASK = 0xF0

BP_DESC = {
    BPLIST_MAGIC: "BPLIST_MAGIC",
    BPLIST_MAGIC_SIZE: "BPLIST_MAGIC_SIZE",
    BPLIST_VERSION: "BPLIST_VERSION",
    BPLIST_VERSION_SIZE: "BPLIST_VERSION_SIZE",
    BPLIST_NULL: "BPLIST_NULL",
    BPLIST_FALSE: "BPLIST_FALSE",
    BPLIST_TRUE: "BPLIST_TRUE",
    BPLIST_FILL: "BPLIST_FILL",
    BPLIST_UINT: "BPLIST_UINT",
    BPLIST_REAL: "BPLIST_REAL",
    BPLIST_DATE: "BPLIST_DATE",
    BPLIST_DATA: "BPLIST_DATA",
    BPLIST_STRING: "BPLIST_STRING",
    BPLIST_UNICODE: "BPLIST_UNICODE",
    BPLIST_UNK_0x70: "BPLIST_UNK_0x70",
    BPLIST_UID: "BPLIST_UID",
    BPLIST_ARRAY: "BPLIST_ARRAY",
    BPLIST_SET: "BPLIST_SET",
    BPLIST_DICT: "BPLIST_DICT",
    BPLIST_MASK: "BPLIST_MASK"
}


def die(fmt, *args):
    """
    Utility to show debug logs.
    """
    try:
        msg = fmt % args
    except:
        msg = "Failed to format string.." + \
            '%s' % fmt + ', args: %s' % args
    finally:
        raise Exception('FATAL ERROR: %s' % (msg))


def PDEBUG(fmt, *args, **kwargs):
    """
    Utility to show debug logs.
    """
    stack = traceback.extract_stack(None, 2)[0]
    try:
        msg = fmt % args
    except:
        msg = "Failed to format string.."
    finally:
        print("DEBUG - (%s:%d -- %s): %s" %
              (stack[0], stack[1], stack[2], msg))


def UINT_TO_HOST(c, p, s):
    return struct.unpack_from('>1' + {
        1: 'B',
        2: 'H',
        4: 'I',
        8: 'Q'
    }.get(s), c, p)[0]


def GET_WIDTH_FMT(val):
    """
    Return minimal width large enough to hold value, together with format used to pack it.
    Arguments:
    - `val`: An integer.
    """
    for w in [1, 2, 4, 8]:
        if val < (1 << w * 8):
            break

    f = '>' + {1: 'B', 2: 'H', 4: 'I', 8: 'Q'}.get(w)

    return (w, f)


stack = []


def myTrace(func):
    def _myTrace(*args, **kwargs):
        if len(stack) != 0:
            print('TRACE:  %s' % (' ==> '.join(stack)))

        stack.append(func.__name__)
        print('\tENTER: {0}, \n\targs: {1}\n\tkwargs: {2}'.format(
            func.__name__, args, kwargs))
        ret = func(*args, **kwargs)
        print('LEAVE: %s' % func)

        if (ret is None):
            die('should not be none: current stack: %s' % ' ==> '.join(stack))

        stack.pop()
        return ret

    return _myTrace


class BPEntry(object):
    """ An entry in BPList.
    """

    def __init__(self, typ):
        self.typ = typ
        self.idx = -1

    def Print(self, level=1):
        """
        """
        return 'Type: %s, Val: %s' % (BP_DESC.get(
            self.typ), self.__str_priv__(level))
        pass

    def __str_priv__(self, level=1):
        """
        """
        return ''

    def DumpTo(self, fd, ref_fmt):
        """
        """
        raise Exception("Not implemented: %s" % BP_DESC.get(self.typ))

    def WriteMarkerAndSize(self, size):
        """
        """
        if (not isinstance(size, int)):
            die('Expect INT type, but got: %s (%s)' %
                (type(size).__name__, size))

        marker = self.typ | (size if size < 15 else 0xf)
        content = struct.pack('>B', marker)
        if size >= 15:
            content += self.WriteInt(size)

        return content

    def WriteInt(self, val):
        """Write val as int.
        """

        (w, f) = GET_WIDTH_FMT(val)
        sz = BPLIST_UINT | int(math.log2(w))
        content = struct.pack('B', sz)

        val &= ((1 << (w * 8)) - 1)
        content += struct.pack(f, val)

        return content


class BPEntryWrap(BPEntry):
    """Simple wraper of data, not intent to be used directly.
    """

    def __init__(self, type, data):
        """
        """

        super(BPEntryWrap, self).__init__(type)
        self.data = data

    def __str_priv__(self, level=1):
        """
        """
        return ' ' * level + '%s' % self.data

    def DumpTo(self, fd, ref_fmt):
        """
        """
        size = len(self.data)

        PDEBUG('SIZE: %u, type: %s', size, type(self.data).__name__)

        content = self.WriteMarkerAndSize(size)
        content += struct.pack('%us' % size, self.data)

        PDEBUG('content: %s\ndata: %s', content, self.data)

        return fd.write(content)


class BPEntryData(BPEntryWrap):
    """Reprents data entry, including DATA, STRING.
    """

    def __init__(self, data):
        """
        """

        super(BPEntryData, self).__init__(BPLIST_DATA, data)


class BPEntryString(BPEntryWrap):
    """Reprents data entry, including DATA, STRING.
    """

    def __init__(self, data):
        """
        """

        super(BPEntryString, self).__init__(BPLIST_STRING, data)


class BPEntryDict(BPEntry):
    """
    """

    def __init__(self):
        """
        """
        self.keys = []  # we want order of keys to be maintained.
        self.dicts = {}

        super(BPEntryDict, self).__init__(BPLIST_DICT)

    def AddPair(self, key, val):
        PDEBUG('KEY: %s', key)

        tmp = self.dicts.get(key, None)
        if (tmp is not None):
            die('Duplicated key (%s), val:\n\t[OLD]: %s\n\t[NEW]: %s' %
                (key, tmp, val))

        self.keys.append(key)
        self.dicts[key] = val

    def __str_priv__(self, level=1):
        """
        """
        ret = ''

        for key in self.keys:
            ret += '\n' + '  ' * level + ' %s -- %s' % (
                key.Print(), self.dicts[key].Print(level + 1))
        return ret

    def DumpTo(self, fd, ref_fmt):
        """Dump entry and return serialized result.
        """

        size = len(self.keys)
        content = self.WriteMarkerAndSize(size)

        for key in self.keys:
            key_idx = key.idx
            content += struct.pack(ref_fmt, key_idx)

        for key in self.keys:
            val_idx = self.dicts[key].idx
            content += struct.pack(ref_fmt, val_idx)

        return fd.write(content)


class BPList(object):
    """Binary Property List.
    """

    TRAILER_FMT = struct.Struct('>8B3Q')  # used to parse tail from bplist

    _trailer_fmt_ = '''Trailer:
    offset_size         : {0},
    ref_size            : {1},
    num_objects         : {2},
    root_object_index   : {3}
    offset_table_offset : {4}'''

    def __init__(self):
        super(BPList, self).__init__()
        self.level = 0
        self.BP_PARSERS = {
            BPLIST_DICT: self.ParseDict,
            BPLIST_STRING: self.ParseString,
            BPLIST_DATA: self.ParseData,
        }
        self.entries = []
        pass

    def Load(self, path):
        """Parse from file.
        """
        with open(path, mode='rb') as fd:
            self.content = fd.read()

        self.end = len(self.content)
        if self.end == 0:
            die('Empty file.')

        tmp = self.TRAILER_FMT.unpack_from(self.content,
                                           -self.TRAILER_FMT.size)

        self.offset_size = tmp[6]
        self.ref_size = tmp[7]
        self.num_objects = tmp[8]
        self.root_object_index = tmp[9]
        self.offset_table_offset = tmp[10]

        if self.offset_size not in [1, 2, 4, 8]:
            die('Wrong offset size: %u' % (self.offset_size))

        PDEBUG(
            '%s',
            self._trailer_fmt_.format(self.offset_size, self.ref_size,
                                      self.num_objects, self.root_object_index,
                                      self.offset_table_offset))

        # Start parsing from root_index.
        self.entries = []
        for i in range(self.num_objects):
            self.entries.append(None)

        self.root = self.ParseAtIndex(self.root_object_index)

        print('\nRoot Data:\n%s' % (self.root.Print()))

    def ParseBinNode(self, pos):
        """
        Generic entry to parse a binary node.
        """

        obj = struct.unpack_from('>1B', self.content, pos)[0]
        pos += 1

        type = obj & BPLIST_MASK
        size = obj & BPLIST_FILL

        PDEBUG('type: 0x%X (%s), size: %u' %
               (type, BP_DESC.get(type, "Unknown"), size))

        if size == BPLIST_FILL:
            if type in [
                    BPLIST_DATA, BPLIST_STRING, BPLIST_UNICODE, BPLIST_ARRAY,
                    BPLIST_SET, BPLIST_DICT
            ]:
                next_obj = UINT_TO_HOST(self.content, pos, 1)
                pos += 1

                next_size = next_obj & BPLIST_FILL
                next_type = next_obj & BPLIST_MASK

                PDEBUG('next type: %s, next_size: %u',
                       BP_DESC.get(next_type, "Unknown"), next_size)

                if (next_type != BPLIST_UINT):
                    die('oops...')

                next_size = 1 << next_size

                if (pos + next_size >= self.end):
                    die('outside valid range')

                size = UINT_TO_HOST(self.content, pos, next_size)  # XXX:
                PDEBUG('pos: %u, next_size: %u, new size: %u', pos, next_size,
                       size)
                pos += next_size

                pass
            else:
                raise Exception("Not implemented.")

        parser = self.BP_PARSERS.get(type, None)
        if parser is None:
            die("Can't find parser for type: %s", BP_DESC.get(type, "Unkown"))

        return parser(pos, size)

    def ParseDict(self, pos, size):
        """
        """
        # For now, haven't figure out how to orgnaize dict entries, simply
        # print them.

        ret = BPEntryDict()

        for i in range(size):
            str_i = pos + i * self.ref_size
            str_j = pos + (i + size) * self.ref_size

            PDEBUG('str_i: %u, str_j: %u', str_i, str_j)

            key_idx = UINT_TO_HOST(self.content, str_i, self.ref_size)
            val_idx = UINT_TO_HOST(self.content, str_j, self.ref_size)

            PDEBUG('i1: %u, i2: %u', key_idx, val_idx)

            key = self.ParseAtIndex(key_idx)
            val = self.ParseAtIndex(val_idx)

            ret.AddPair(key, val)

        PDEBUG('DICT: %s', ret)
        return ret

    def ParseString(self, pos, size):
        """Parse string.
        """
        val = self.content[pos:pos + size]
        return BPEntryString(val)

    def ParseAtIndex(self, index):
        p = UINT_TO_HOST(self.content,
                         self.offset_table_offset + index * self.offset_size,
                         self.offset_size)

        if p >= self.end:
            die('EOF reached.')

        if index >= self.num_objects:
            die(
                'node index (%u) must be smaller than the number of objects(%u)',
                index, self.num_objects)

        index_pos = self.offset_table_offset + index * self.offset_size
        PDEBUG('Index: %u, Pos: %u', index, index_pos)

        pos = UINT_TO_HOST(self.content, index_pos, self.offset_size)

        PDEBUG('pos: %u', pos)
        self.level += 1
        ret = self.ParseBinNode(pos)
        ret.idx = index
        self.level -= 1

        self.entries[index] = ret

        return ret

    def ParseData(self, pos, size):
        """
        """
        return BPEntryData(self.content[pos:pos + size])

    # Writter
    def Dump(self, path):
        """Dump content to file.

        Arguments:

        - `path`:
        """
        with open(path, mode='wb') as fd:

            # Magic header.
            fd.write(BPLIST_MAGIC)
            fd.write(BPLIST_VERSION)

            # calculate ref-size
            num_objects = len(self.entries)
            (ref_size, ref_fmt) = GET_WIDTH_FMT(num_objects)

            offsets = []
            offset = 8  # plus leading magic & version
            for i in range(self.num_objects):
                offsets.append(offset)
                entry = self.entries[i]
                PDEBUG('i: %d, entry: %s', i, entry)
                offset += entry.DumpTo(fd, ref_fmt)
            pass

            PDEBUG('offsets: %s, final: %d', offsets, offset)

            # Now appending offset table...
            (offset_size, offset_fmt) = GET_WIDTH_FMT(offset)

            PDEBUG('offset_size: %d', offset_size)

            content = b''
            for o in offsets:
                content += struct.pack(offset_fmt, o)
            content += self.TRAILER_FMT.pack(
                # 6 reserved ones..
                0, 0, 0, 0, 0, 0,
                offset_size, ref_size, num_objects, 0, offset
            )

            fd.write(content)


        pass


if __name__ == '__main__':
    bpl = BPList()
    PDEBUG('%s', sys.argv)
    ifile = 'title.webarchive' if len(sys.argv) == 1 else sys.argv[1]
    ofile = os.path.join(os.path.dirname(ifile),
                         'out_' + os.path.basename(ifile))

    bpl.Load(ifile)
    bpl.Dump(ofile)
