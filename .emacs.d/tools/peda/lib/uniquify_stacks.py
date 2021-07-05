#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# https://github.com/flandr/uniq-stacks.git

import re
import os
import sys
import tempfile
import subprocess

sys.path.insert(0, os.path.dirname(__file__))
from utils import *
import argparse

gdb_imported = False
try:
    import gdb
    gdb_imported = True
except ImportError as e:
    pass

# regex
r_match_thread = re.compile(r'Thread\s+(\d+)\s+\(Thread\s+(.*?)\s+.*?"(.*?)"\):')
r_match_address = re.compile(r'#\d+\s+(.*?)\s+')
r_match_suspicious_thread = re.compile('(signal.*handler.*called)|( raise)')
r_match_zero_frame = re.compile('#0\s+0x')
r_match_abbr_stack = re.compile('\s+\(.*?\)\s*.*')


class StackTrace(object):
    '''
    A stack trace from a thread
    '''

    class Frame(object):
        '''
        A frame from a stack trace
        '''

        @staticmethod
        def maybe_get_sal(gdb_frame):
            '''
            Get source and line info, if available; otherwise (None, None)
            '''
            sal = gdb_frame.find_sal()
            if not sal:
                return (None, None)
            if not sal.is_valid():
                return (None, None)
            if not sal.symtab:
                return (None, None)
            return (sal.symtab.filename, sal.line)

        def __init__(self, gdb_frame, position, show_source, current_pc=None):
            self.position = position
            self.pc = gdb_frame.pc()
            self.name = gdb_frame.name()
            self.active = current_pc and self.pc == current_pc

            if self.name is None:
                self.name = "[unknown {:#x}]".format(self.pc)
            if gdb_frame.type() == gdb.INLINE_FRAME:
                self.name += " [inlined]"
            if show_source:
                (self.source_file, self.line) = self.maybe_get_sal(gdb_frame)
            else:
                self.source_file = None
                self.line = None

        def __eq__(self, other):
            ret = self.position == other.position and \
                    self.name == other.name
            return ret and self.pc == other.pc

        def __ne__(self, other):
            return not self.__eq__(other)

        def __hash__(self):
            base = hash((self.position, self.name))
            return hash((base, self.pc))

        def __str__(self):
            if self.source_file:
                return "{} #{:3d} {:#x} {}:{} {}".format(
                    "=>" if self.active else "  ",
                    self.position, self.pc,
                        self.source_file, self.line, self.name)
            else:
                return "{} #{:3d} {:#x} {}".format(
                    "=>" if self.active else "  ",
                    self.position, self.pc,
                        self.name)

    @staticmethod
    def accumulate_backtrace(frame, frame_limit, show_source, current_pc):
        '''
        Performs the backtrace over the selected thread, assembling a list
        of Frame objects
        '''

        i = 0
        frames = []
        has_more = False

        if frame_limit > 0:
            while frame and i < frame_limit:
                frames.append(StackTrace.Frame(frame, i, show_source, current_pc))

                frame = frame.older()
                i += 1

            if frame and frame.older():
                has_more = True
        else:
            # dirty work, we have to first seek to the oldest frame, then
            # traverse them one by one...

            next_frame = frame.older()
            n = 0

            while next_frame:
                frame = next_frame
                next_frame = frame.older()
                n += 1

            frame_limit = -frame_limit

            while frame and i < frame_limit:
                frames.insert(0, StackTrace.Frame(frame, n-i, show_source, current_pc))

                frame = frame.newer()
                i += 1

            if frame and frame.newer():
                has_more = True
            pass

        return (has_more, frames)

    def __init__(self, gdb_thread, frame_limit, show_source, current_pc = None):
        if not gdb_thread.is_valid():
            raise RuntimeError("Invalid thread object")

        self.gdb_thread_id = gdb_thread.num
        self.name = gdb_thread.name
        self.current_pc = current_pc

        (self.pid, self.lwpid, self.tid) = gdb_thread.ptid

        # Preserve previous selected thread (may be None)
        orig_thread = gdb.selected_thread()

        try:
            # Switch to current thread
            gdb_thread.switch()

            # Get the backtrace in its entirety
            (self.has_more, self.frames) = self.accumulate_backtrace(gdb.newest_frame(),
                                                                     frame_limit, show_source,
                                                                     current_pc)
        finally:
            if orig_thread:
                orig_thread.switch()

    def __eq__(self, other):
        return self.frames == other.frames

    def __ne__(self, other):
        return not self.__eq__(other)

    def __hash__(self):
        # I know you're trying to protect ourselves from ourselves,
        # Python, but I really do want a hash of this list
        return hash(str([hash(f) for f in self.frames]))

    def __str__(self):
        framestr = [str(f) for f in self.frames]
        return "\n".join(framestr)


def uniquify_stacks_gdb(limit):
    """
    Get unique stacks, used under gdb...
    """
    traces = []
    for thread in gdb.inferiors()[0].threads():
        traces.append(StackTrace(thread, limit, False))

    uniq = {}
    for stack in traces:
        uniq.setdefault(stack,[]).append(stack.gdb_thread_id)

    sorter = lambda d: sorted(d.items(), key=lambda item: item[1][0],
                              reverse=False)

    gdb.write("\n== Printing {} unique stack(s) from {} thread(s)\n\n".format(
        blue('%d'%len(uniq)), green('%s'%len(traces))))

    suspicious_threads = []

    for k, v in sorter(uniq):
        stack = str(k)
        suspicious = k.has_more or r_match_suspicious_thread.search(stack)

        if suspicious:
            suspicious_threads.extend(v)
            gdb.write("Stack common for {} thread(s); Thread name: {}, Thread id(s): {} {}\n".format(
                red('%s'%(len(v))), k.name, red('%s'%sorted(v)),
                red('        <=============== suspicious')))
        else:
            gdb.write("Stack common for {} thread(s); Thread name: {}, Thread id(s): {}\n".format(
                len(v), k.name, sorted(v)))

        gdb.write(stack)
        gdb.write("\n\n")


    gdb.write('\nThreads Info: total: %s, unique: %s, suspicious: %s.\n' % (
        blue('%d'%len(uniq)), green('%s'%len(traces)),
        'none' if not suspicious_threads
        else '(%s) -- %s'%( yellow('%d'%len(suspicious_threads)),
                            red('%s'%sorted(suspicious_threads)))))

    gdb.flush()
    pass


def uniquify_stacks_text(out, abbr=False):
    """Print unique stacks in out.

    If abbr is True, don't print arguments or file names.
    """

    if out is None:
        error_msg('input is None')
        return

    if len(out) == 0:
        error_msg('input is empty')
        return

    if r_match_thread.search(out) is None:
        msg('Single Thread process:\n' + out)
        return

    # find all threads...
    threads = []
    unique_threads = {}
    suspicious_threads = []

    beg = 0
    pos = 0
    start_pos = 0

    first_match = True
    last_tid = None
    last_full_tid = None
    last_name = None
    last_header = ''
    while True:
        m = r_match_thread.search(out, pos)
        if m is None:
            if not first_match:
                stack = out[start_pos:-1]
                if abbr:
                    stack = r_match_abbr_stack.sub(' ()', stack)
                threads.append((last_tid, last_header, stack, last_full_tid, last_name))
            break

        beg = m.start(0)
        pos = m.end(0)

        if not first_match:
            stack = out[start_pos:beg - 1]
            if abbr:
                stack = r_match_abbr_stack.sub(' ()', stack)
            threads.append((last_tid, last_header, stack, last_full_tid, last_name))

        first_match = False
        last_tid = m.group(1)
        last_full_tid = m.group(2)
        last_name = m.group(3)
        last_header = out[beg: pos]
        start_pos = pos

        pos += 1

    for (tid, header, stack, full_tid, name) in threads[::-1]:  # walk through in reversed order
        pos = 0
        addrs = []

        if r_match_suspicious_thread.search(stack):
            suspicious_threads.append("%s (%s)"% (tid, name))
            header = red(header)
        elif not r_match_zero_frame.search(stack):
            suspicious_threads.append("%s (%s)"% (tid, name))
            stack = '\n... %s ...\n' % red('some frames may be missing') + stack
            header = red(header)

        while True:
            s = r_match_address.search(stack, pos)
            if not s:
                break

            entry = s.group(1)
            addrs.append(entry)
            pos = s.end(0) + 1

        code = hash('%s' % addrs)
        if code not in unique_threads.keys():
            unique_threads[code] = (tid, header, stack,
                                    [],  # tid
                                    []  # full_tid
                                    )
        else:
            unique_threads[code][-1].append(full_tid)
            unique_threads[code][-2].append(tid)

    summary = 'Threads Info: total: %s, unique: %s, suspicious: %s.\n' % (
        green('%d' % len(threads)),
        blue('%d' % len(unique_threads.keys())),
        'none' if not suspicious_threads
        else '(%s) -- %s'%( yellow('%d'%len(suspicious_threads)),
                            red('%s'%(', '.join(suspicious_threads)))))

    msg(summary)

    # sort threads by tid...
    unique_list = list(unique_threads.values())
    unique_list.sort(key=lambda x: int(x[0]))
    for (tid, header, stack, others, others_full) in unique_list:
        print('%s, total: %d, similar threads: %s\n%s\n' % (
            header,
            len(others)+1,
            '(%s), aka (%s)' % (blue(', '.join(others) if others else 'NIL'),
                                                 blue(', '.join(others_full) if others_full else 'NIL'))
            if len(others) > 0 else 'none',
            stack.strip()))
        pass

    msg(summary)

    return


if __name__ == '__main__' and not gdb_imported:
    parser = argparse.ArgumentParser()
    parser.add_argument('-p', '--process',
                        help='attach to specified process',
                        type=int)
    parser.add_argument('input',
                        help='path of input file (generated by gstack)',
                        nargs="?")

    args = parser.parse_args()
    # print('args: %s' % (args))

    if args.process and args.input:
        print('Conflict options: process & input_file, showing usage:\n')
        parser.print_help()
        sys.exit(1)

    if args.process is None and args.input is None:
        print('Missing operand, showing usage:\n')
        parser.print_help()
        sys.exit(1)

    if args.input:
        if os.path.exists(args.input):
            uniquify_stacks_text(open(args.input).read())
            sys.exit(0)
        else:
            print('File %s does not exist.' % (args.input))
            sys.exit(1)

    # TODO: uniquify_stacks_gdb() is much faster!!!
    if args.process:
        fp = tempfile.NamedTemporaryFile()
        gdb_commands = [
            'set width 0',
            'set height 0',
            'set pagination no',
            'attach {}'.format(args.process),
            'source {}'.format(sys.argv[0]),
            'py uniquify_stacks_gdb(1024)'
        ]

        fp.write(str.encode(('\n'.join(gdb_commands))))
        fp.seek(0)
        kk = fp.read().decode()
        p = subprocess.Popen(['gdb', '-q', '--batch', '-nx', '-x',  fp.name],
                             stdout=subprocess.PIPE)
        output, errors = p.communicate()
        print('gdb retunrs: %d' % (p.returncode))
        print(output.decode())
        sys.exit(0)
