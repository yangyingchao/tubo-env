#       PEDA - Python Exploit Development Assistance for GDB
#
#       Copyright (C) 2012 Long Le Dinh <longld at vnsecurity.net>
#
#       License: see LICENSE file for details
#
import sys

import re
import os
import shlex
import string
import time
import traceback
import codecs
import subprocess


# point to absolute path of peda.py
PEDAFILE = os.path.abspath(os.path.expanduser(__file__))
if os.path.islink(PEDAFILE):
    PEDAFILE = os.readlink(PEDAFILE)
sys.path.insert(0, os.path.dirname(PEDAFILE) + "/lib/")

from utils import *

if sys.version_info[0] < 3:
    print('%s'%(red(
        'PEDA may not work properly, it requires python-3 or above, but current python is {} ...'.format(
            sys.version))))


import config
from uniquify_stacks import uniquify_stacks_gdb, StackTrace

REGISTERS = {
    8 : ["al", "ah", "bl", "bh", "cl", "ch", "dl", "dh"],
    16: ["ax", "bx", "cx", "dx"],
    32: ["eax", "ebx", "ecx", "edx", "esi", "edi", "ebp", "esp", "eip"],
    64: ["rax", "rbx", "rcx", "rdx", "rsi", "rdi", "rbp", "rsp", "rip",
         "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"]
}

RIGHT_ARROW = " \u2192 "
CROSS = "\u2718 "
TICK = "\u2713 "
BP_GLYPH = "\u25cf"
PEDA_ALIAS = []
PRIVATE_PRINTS = {}

PRIVATE_PRINT_FILE = os.path.join(os.path.dirname(PEDAFILE), 'private-print.txt')

###########################################################################
class PEDA(object):
    """
    Class for actual functions of PEDA commands
    """
    def __init__(self):
        self.SAVED_COMMANDS = {} # saved GDB user's commands


    ####################################
    #   GDB Interaction / Misc Utils   #
    ####################################
    def execute(self, gdb_command):
        """
        Wrapper for gdb.execute, catch the exception so it will not stop python script

        Args:
            - gdb_command (String)

        Returns:
            - True if execution succeed (Bool)
        """
        try:
            gdb.execute(gdb_command)
            return True
        except Exception as e:
            if config.Option.get("debug") == "on":
                msg('Exception (%s): %s' % (gdb_command, e), "red")
                traceback.print_exc()
            return False

    def execute_redirect(self, gdb_command, silent=False):
        """
        Execute a gdb command and capture its output

        Args:
            - gdb_command (String)
            - silent: discard command's output, redirect to /dev/null (Bool)

        Returns:
            - output of command (String)
        """
        result = None
        #init redirection
        if silent:
            logfd = open(os.path.devnull, "r+")
        else:
            logfd = tmpfile()
        logname = logfd.name

        debug_msg('COMMAND: %s' % (gdb_command))

        gdb.execute('set logging off') # prevent nested call
        gdb.execute('set pagination off') # disable paging
        gdb.execute('set logging file %s' % logname)
        gdb.execute('set logging overwrite on')
        gdb.execute('set logging redirect on')
        gdb.execute('set logging on')

        try:
            gdb.execute(gdb_command)
            gdb.flush()
            gdb.execute('set logging off')
            if not silent:
                logfd.flush()
                result = logfd.read()
            logfd.close()
        except Exception as e:
            gdb.execute('set logging off') #to be sure
            msg('Exception (%s): %s' % (gdb_command, e), "red")
            if config.Option.get("debug") == "on":
                traceback.print_exc()
            logfd.close()
        if config.Option.get("verbose") == "on":
            msg(result)
        return result

    def parse_and_eval(self, exp):
        """
        Work around implementation for gdb.parse_and_eval with enhancements

        Args:
            - exp: expression to evaluate (String)

        Returns:
            - value of expression
        """
        regs = sum(REGISTERS.values(), [])
        for r in regs:
            if "$"+r not in exp and "e"+r not in exp and "r"+r not in exp:
                exp = exp.replace(r, "$%s" % r)

        p = re.compile(r"(.*)\[(.*)\]") # DWORD PTR [esi+eax*1]
        matches = p.search(exp)
        if not matches:
            p = re.compile(r"(.*).s:(0x.*)") # DWORD PTR ds:0xdeadbeef
            matches = p.search(exp)

        if matches:
            mod = "w"
            if "BYTE" in matches.group(1):
                mod = "b"
            elif "QWORD" in matches.group(1):
                mod = "g"
            elif "DWORD" in matches.group(1):
                mod = "w"
            elif "WORD" in matches.group(1):
                mod = "h"

            out = self.execute_redirect("x/%sx %s" % (mod, matches.group(2)))
            if not out:
                return None
            else:
                return out.split(":\t")[-1].strip()

        else:
            out = self.execute_redirect("print %s" % exp)
        if not out:
            return None
        else:
            out = gdb.history(0).__str__()
            out = out.encode('ascii', 'ignore')
            out = decode_string_escape(out)
            return out.strip()

    def string_to_argv(self, str):
        """
        Convert a string to argv list, pre-processing register and variable values

        Args:
            - str: input string (String)

        Returns:
            - argv list (List)
        """
        try:
            str = str.encode('ascii', 'ignore')
        except:
            pass
        args = list(map(lambda x: decode_string_escape(x), shlex.split(str.decode())))
        # need more processing here
        for idx, a in enumerate(args):
            a = a.strip(",")
            if a.startswith("$"): # try to get register/variable value
                v = self.parse_and_eval(a)
                if v != None and v != "void":
                    if v.startswith("0x"): # int
                        args[idx] = v.split()[0] # workaround for 0xdeadbeef <symbol+x>
                    else: # string, complex data
                        args[idx] = v
            elif a.startswith("+"): # relative value to prev arg
                adder = to_int(self.parse_and_eval(a[1:]))
                if adder is not None:
                    args[idx] = "%s" % to_hex(to_int(args[idx-1]) + adder)
            elif is_math_exp(a):
                try:
                    v = eval("%s" % a)
                    # XXX hack to avoid builtin functions/types
                    if not isinstance(v, str):
                        continue
                    args[idx] = "%s" % (to_hex(v) if to_int(v) != None else v)
                except:
                    pass
        if config.Option.get("verbose") == "on":
            msg(args)
        return args


    ################################
    #   GDB User-Defined Helpers   #
    ################################
    def save_user_command(self, cmd):
        """
        Save user-defined command and deactivate it

        Args:
            - cmd: user-defined command (String)

        Returns:
            - True if success to save (Bool)
        """
        commands = self.execute_redirect("show user %s" % cmd)
        if not commands:
            return False

        commands = "\n".join(commands.splitlines()[1:])
        commands = "define %s\n" % cmd + commands + "end\n"
        self.SAVED_COMMANDS[cmd] = commands
        tmp = tmpfile()
        tmp.write("define %s\nend\n" % cmd)
        tmp.flush()
        result = self.execute("source %s" % tmp.name)
        tmp.close()
        return result

    def define_user_command(self, cmd, code):
        """
        Define a user-defined command, overwrite the old content

        Args:
            - cmd: user-defined command (String)
            - code: gdb script code to append (String)

        Returns:
            - True if success to define (Bool)
        """
        commands = "define %s\n" % cmd + code + "\nend\n"
        tmp = tmpfile(is_binary_file=False)
        tmp.write(commands)
        tmp.flush()
        result = self.execute("source %s" % tmp.name)
        tmp.close()
        return result

    def append_user_command(self, cmd, code):
        """
        Append code to a user-defined command, define new command if not exist

        Args:
            - cmd: user-defined command (String)
            - code: gdb script code to append (String)

        Returns:
            - True if success to append (Bool)
        """

        commands = self.execute_redirect("show user %s" % cmd)
        if not commands:
            return self.define_user_command(cmd, code)
        # else
        commands = "\n".join(commands.splitlines()[1:])
        if code in commands:
            return True

        commands = "define %s\n" % cmd + commands + code + "\nend\n"
        tmp = tmpfile()
        tmp.write(commands)
        tmp.flush()
        result = self.execute("source %s" % tmp.name)
        tmp.close()
        return result

    def restore_user_command(self, cmd):
        """
        Restore saved user-defined command

        Args:
            - cmd: user-defined command (String)

        Returns:
            - True if success to restore (Bool)
        """
        if cmd == "all":
            commands = "\n".join(self.SAVED_COMMANDS.values())
            self.SAVED_COMMANDS = {}
        else:
            if cmd not in self.SAVED_COMMANDS:
                return False
            else:
                commands = self.SAVED_COMMANDS[cmd]
                self.SAVED_COMMANDS.pop(cmd)
        tmp = tmpfile()
        tmp.write(commands)
        tmp.flush()
        result = self.execute("source %s" % tmp.name)
        tmp.close()

        return result

    def run_gdbscript_code(self, code):
        """
        Run basic gdbscript code as it is typed in interactively

        Args:
            - code: gdbscript code, lines are splitted by "\n" or ";" (String)

        Returns:
            - True if success to run (Bool)
        """
        tmp = tmpfile()
        tmp.write(code.replace(";", "\n"))
        tmp.flush()
        result = self.execute("source %s" % tmp.name)
        tmp.close()
        return result

    #########################
    #   Debugging Helpers   #
    #########################
    @memoized
    def is_target_remote(self):
        """
        Check if current target is remote

        Returns:
            - True if target is remote (Bool)
        """
        out = self.execute_redirect("info program")
        if out and "serial line" in out: # remote target
            return True

        return False

    @memoized
    def getfile(self):
        """
        Get exec file of debugged program

        Returns:
            - full path to executable file (String)
        """
        result = None
        out = self.execute_redirect('info files')
        if out and '"' in out:
            p = re.compile(r".*exec file:\s*`(.*)'")
            m = p.search(out)
            if m:
                result = m.group(1)
            else: # stripped file, get symbol file
                p = re.compile(r"Symbols from \"([^\"]*)")
                m = p.search(out)
                if m:
                    result = m.group(1)

        return result

    def get_status(self):
        """
        Get execution status of debugged program

        Returns:
            - current status of program (String)
                STOPPED - not being run
                BREAKPOINT - breakpoint hit
                SIGXXX - stopped by signal XXX
                UNKNOWN - unknown, not implemented
        """
        status = "UNKNOWN"
        out = self.execute_redirect("info program")
        for line in out.splitlines():
            if line.startswith("It stopped"):
                if "signal" in line: # stopped by signal
                    status = line.split("signal")[1].split(",")[0].strip()
                    break
                if "breakpoint" in line: # breakpoint hit
                    status = "BREAKPOINT"
                    break
            if "not being run" in line:
                status = "STOPPED"
                break
        return status

    @memoized
    def getpid(self):
        """
        Get PID of the debugged process

        Returns:
            - pid (Int)
        """

        status = self.get_status()
        if not status or status == "STOPPED":
            return None
        pid = gdb.selected_inferior().pid
        return int(pid) if pid else None

    def getos(self):
        """
        Get running OS info

        Returns:
            - os version (String)
        """
        # TODO: get remote os by calling uname()
        return os.uname()[0]

    @memoized
    def getarch(self):
        """
        Get architecture of debugged program

        Returns:
            - tuple of architecture info (arch (String), bits (Int))
        """
        arch = "unknown"
        bits = 32
        out = self.execute_redirect('maintenance info sections ?').splitlines()
        for line in out:
            if "file type" in line:
                arch = line.split()[-1][:-1]
                break
        if "64" in arch:
            bits = 64
        return (arch, bits)

    def intsize(self):
        """
        Get dword size of debugged program

        Returns:
            - size (Int)
                + intsize = 4/8 for 32/64-bits arch
        """

        (arch, bits) = self.getarch()
        return bits // 8

    def getregs(self, reglist=None):
        """
        Get value of some or all registers

        Returns:
            - dictionary of {regname(String) : value(Int)}
        """
        if reglist:
            reglist = reglist.replace(",", " ")
        else:
            reglist = ""
        regs = self.execute_redirect("info registers %s" % reglist)
        if not regs:
            return None

        result = {}
        if regs:
            for r in regs.splitlines():
                r = r.split()
                if len(r) > 1 and to_int(r[1]) is not None:
                    result[r[0]] = to_int(r[1])

        return result

    def getreg(self, register):
        """
        Get value of a specific register

        Args:
            - register: register name (String)

        Returns:
            - register value (Int)
        """
        r = register.lower()
        regs = self.execute_redirect("info registers %s" % r)
        if regs:
            regs = regs.splitlines()
            if len(regs) > 1:
                return None
            else:
                result = to_int(regs[0].split()[1])
                return result

        return None

    def set_breakpoint(self, location, temp=0, hard=0):
        """
        Wrapper for GDB break command
            - location: target function or address (String ot Int)

        Returns:
            - True if can set breakpoint
        """
        cmd = "break"
        if hard:
            cmd = "h" + cmd
        if temp:
            cmd = "t" + cmd

        if to_int(location) is not None:
            return peda.execute("%s *0x%x" % (cmd, to_int(location)))
        else:
            return peda.execute("%s %s" % (cmd, location))

    def get_breakpoint(self, num):
        """
        Get info of a specific breakpoint
        TODO: support catchpoint, watchpoint

        Args:
            - num: breakpoint number

        Returns:
            - tuple (Num(Int), Type(String), Disp(Bool), Enb(Bool), Address(Int), What(String), commands(String))
        """
        out = self.execute_redirect("info breakpoints %d" % num)
        if not out or "No breakpoint" in out:
            return None

        lines = out.splitlines()[1:]
        # breakpoint regex
        p = re.compile(r"^(\d*)\s*(.*breakpoint)\s*(keep|del)\s*(y|n)\s*(0x[^ ]*)\s*(.*)")
        m = p.match(lines[0])
        if not m:
            # catchpoint/watchpoint regex
            p = re.compile(r"^(\d*)\s*(.*point)\s*(keep|del)\s*(y|n)\s*(.*)")
            m = p.match(lines[0])
            if not m:
                return None
            else:
                (num, type, disp, enb, what) = m.groups()
                addr = ''
        else:
            (num, type, disp, enb, addr, what) = m.groups()

        disp = True if disp == "keep" else False
        enb = True if enb == "y" else False
        addr = to_int(addr)
        m = re.match(r"in.*at(.*:\d*)", what)
        if m:
            what = m.group(1)
        else:
            if addr: # breakpoint
                what = ""

        commands = ""
        if len(lines) > 1:
            for line in lines[1:]:
                if "already hit" in line: continue
                commands += line + "\n"

        return (num, type, disp, enb, addr, what, commands.rstrip())

    def get_breakpoints(self):
        """
        Get list of current breakpoints

        Returns:
            - list of tuple (Num(Int), Type(String), Disp(Bool), Nnb(Bool), Address(Int), commands(String))
        """
        result = []
        out = self.execute_redirect("info breakpoints")
        if not out:
            return []

        bplist = []
        for line in out.splitlines():
            m = re.match(r"^(\d*).*", line)
            if m and to_int(m.group(1)):
                bplist += [to_int(m.group(1))]

        for num in bplist:
            r = self.get_breakpoint(num)
            if r:
                result += [r]
        return result

    def save_breakpoints(self, filename):
        """
        Save current breakpoints to file as a script

        Args:
            - filename: target file (String)

        Returns:
            - True if success to save (Bool)
        """
        # use built-in command for gdb 7.2+
        result = self.execute_redirect("save breakpoints %s" % filename)
        if result == '':
            return True

        bplist = self.get_breakpoints()
        if not bplist:
            return False

        try:
            fd = open(filename, "w")
            for (num, type, disp, enb, addr, what, commands) in bplist:
                m = re.match("(.*)point", type)
                if m:
                    cmd = m.group(1).split()[-1]
                else:
                    cmd = "break"
                if "hw" in type and cmd == "break":
                    cmd = "h" + cmd
                if "read" in type:
                    cmd = "r" + cmd
                if "acc" in type:
                    cmd = "a" + cmd

                if not disp:
                    cmd = "t" + cmd
                if what:
                    location = what
                else:
                    location = "*0x%x" % addr
                text = "%s %s" % (cmd, location)
                if commands:
                    if "stop only" not in commands:
                        text += "\ncommands\n%s\nend" % commands
                    else:
                        text += commands.split("stop only", 1)[1]
                fd.write(text + "\n")
            fd.close()
            return True
        except:
            return False

    def get_config_filename(self, name):
        filename = peda.getfile()
        if not filename:
            filename = peda.getpid()
            if not filename:
                filename = 'unknown'

        filename = os.path.basename("%s" % filename)
        tmpl_name = config.Option.get(name)
        if tmpl_name:
            return tmpl_name.replace("#FILENAME#", filename)
        else:
            return "peda-%s-%s" % (name, filename)

    def save_session(self, filename=None):
        """
        Save current working gdb session to file as a script

        Args:
            - filename: target file (String)

        Returns:
            - True if success to save (Bool)
        """
        session = ""
        if not filename:
            filename = self.get_config_filename("session")

        # exec-wrapper
        out = self.execute_redirect("show exec-wrapper")
        wrapper = out.split('"')[1]
        if wrapper:
            session += "set exec-wrapper %s\n" % wrapper

        try:
            # save breakpoints
            self.save_breakpoints(filename)
            fd = open(filename, "a+")
            fd.write("\n" + session)
            fd.close()
            return True
        except:
            return False

    def restore_session(self, filename=None):
        """
        Restore previous saved working gdb session from file

        Args:
            - filename: source file (String)

        Returns:
            - True if success to restore (Bool)
        """
        if not filename:
            filename = self.get_config_filename("session")

        # temporarily save and clear breakpoints
        tmp = tmpfile()
        self.save_breakpoints(tmp.name)
        self.execute("delete")
        result = self.execute("source %s" % filename)
        if not result:
            self.execute("source %s" % tmp.name)
        tmp.close()
        return result

    def disassemble(self, *arg):
        """
        Wrapper for disassemble command
            - arg: args for disassemble command

        Returns:
            - text code (String)
        """
        code = ""
        modif = ""
        arg = list(arg)
        if len(arg) > 1:
            if "/" in arg[0]:
                modif = arg[0]
                arg = arg[1:]
        if len(arg) == 1 and to_int(arg[0]) != None:
            arg += [to_hex(to_int(arg[0]) + 32)]

        self.execute("set disassembly-flavor intel")
        out = self.execute_redirect("disassemble %s %s" % (modif, ",".join(arg)))
        if not out:
            return None
        else:
            code = out

        return code

    @memoized
    def prev_inst(self, address, count=1):
        """
        Get previous instructions at an address

        Args:
            - address: address to get previous instruction (Int)
            - count: number of instructions to read (Int)

        Returns:
            - list of tuple (address(Int), code(String))
        """
        result = []
        backward = 64+16*count
        for i in range(backward):
            if self.getpid() and not self.is_address(address-backward+i):
                continue

            code = self.execute_redirect("disassemble %s, %s" % (to_hex(address-backward+i), to_hex(address+1)))
            if code and ("%x" % address) in code:
                lines = code.strip().splitlines()[1:-1]
                if len(lines) > count and "(bad)" not in " ".join(lines):
                    for line in lines[-count-1:-1]:
                        (addr, code) = line.split(":", 1)
                        addr = re.search("(0x[^ ]*)", addr).group(1)
                        result += [(to_int(addr), code)]
                    return result
        return None

    @memoized
    def current_inst(self, address):
        """
        Parse instruction at an address

        Args:
            - address: address to get next instruction (Int)

        Returns:
            - tuple of (address(Int), code(String))
        """
        out = self.execute_redirect("x/i 0x%x" % address)
        if not out:
            return None

        (addr, code) = out.split(":", 1)
        addr = re.search("(0x[^ ]*)", addr).group(1)
        addr = to_int(addr)
        code = code.strip()

        return (addr, code)

    @memoized
    def next_inst(self, address, count=1):
        """
        Get next instructions at an address

        Args:
            - address: address to get next instruction (Int)
            - count: number of instructions to read (Int)

        Returns:
            - - list of tuple (address(Int), code(String))
        """
        result = []
        code = self.execute_redirect("x/%di 0x%x" % (count+1, address))
        if not code:
            return None

        lines = code.strip().splitlines()
        for i in range(1, count+1):
            (addr, code) = lines[i].split(":", 1)
            addr = re.search("(0x[^ ]*)", addr).group(1)
            result += [(to_int(addr), code)]
        return result

    @memoized
    def disassemble_around(self, address, count=8):
        """
        Disassemble instructions nearby current PC or an address

        Args:
            - address: start address to disassemble around (Int)
            - count: number of instructions to disassemble

        Returns:
            - text code (String)
        """
        count = min(count, 256)
        pc = address
        if pc is None:
            return None

        # check if address is reachable
        if not self.execute_redirect("x/x 0x%x" % pc):
            return None

        prev_code = self.prev_inst(pc, count//2-1)
        if prev_code:
            start = prev_code[0][0]
        else:
            start = pc
        if start == pc:
            count = count//2

        code = self.execute_redirect("x/%di 0x%x" % (count, start))
        if "0x%x" % pc not in code:
            code = self.execute_redirect("x/%di 0x%x" % (count//2, pc))

        return code.rstrip()

    @memoized
    def xrefs(self, search="", filename=None):
        """
        Search for all call references or data access to a function/variable

        Args:
            - search: function or variable to search for (String)
            - filename: binary/library to search (String)

        Returns:
            - list of tuple (address(Int), asm instruction(String))
        """
        result = []
        if not filename:
            filename = self.getfile()

        if not filename:
            return None
        vmap = self.get_vmmap(filename)
        elfbase = vmap[0][0] if vmap else 0

        if to_int(search) is not None:
            search = "%x" % to_int(search)

        search_data = 1
        if search == "":
            search_data = 0

        out = execute_external_command("%s -M intel -z --prefix-address -d '%s' | grep '%s'" % (config.OBJDUMP, filename, search))

        for line in out.splitlines():
            if not line: continue
            addr = to_int("0x" + line.split()[0].strip())
            if not addr: continue

            # update with runtime values
            if addr < elfbase:
                addr += elfbase
            out = self.execute_redirect("x/i 0x%x" % addr)
            if out:
                line = out
                p = re.compile(r"\s*(0x[^ ]*).*?:\s*([^ ]*)\s*(.*)")
            else:
                p = re.compile(r"(.*?)\s*<.*?>\s*([^ ]*)\s*(.*)")

            m = p.search(line)
            if m:
                (address, opcode, opers) = m.groups()
                if "call" in opcode and search in opers:
                    result += [(addr, line.strip())]
                if search_data:
                     if "mov" in opcode and search in opers:
                         result += [(addr, line.strip())]

        return result

    def _get_function_args_32(self, code, argc=None):
        """
        Guess the number of arguments passed to a function - i386
        """
        if not argc:
            argc = 0
            p = re.compile(r".*mov.*\[esp(.*)\],")
            matches = p.findall(code)
            if matches:
                l = len(matches)
                for v in matches:
                    if v.startswith("+"):
                        offset = to_int(v[1:])
                        if offset is not None and (offset//4) > l:
                            continue
                    argc += 1
            else: # try with push style
                argc = code.count("push")

        argc = min(argc, 6)
        if argc == 0:
            return []

        args = []
        sp = self.getreg("sp")
        mem = self.dumpmem(sp, sp+4*argc)
        for i in range(argc):
            args += [struct.unpack("<L", mem[i*4:(i+1)*4])[0]]

        return args

    def _get_function_args_64(self, code, argc=None):
        """
        Guess the number of arguments passed to a function - x86_64
        """

        # just retrieve max 6 args
        arg_order = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
        p = re.compile(r":\s*([^ ]*)\s*(.*),")
        matches = p.findall(code)
        regs = [r for (_, r) in matches]
        p = re.compile((r"di|si|dx|cx|r8|r9"))
        m = p.findall(" ".join(regs))
        m = list(set(m)) # uniqify
        argc = 0
        if "si" in m and "di" not in m: # dirty fix
            argc += 1
        argc += m.count("di")
        if argc > 0:
            argc += m.count("si")
        if argc > 1:
            argc += m.count("dx")
        if argc > 2:
            argc += m.count("cx")
        if argc > 3:
            argc += m.count("r8")
        if argc > 4:
            argc += m.count("r9")

        if argc == 0:
            return []

        args = []
        regs = self.getregs()
        for i in range(argc):
            args += [regs[arg_order[i]]]

        return args

    def get_function_args(self, argc=None):
        """
        Get the guessed arguments passed to a function when stopped at a call instruction

        Args:
            - argc: force to get specific number of arguments (Int)

        Returns:
            - list of arguments (List)
        """

        args = []
        regs = self.getregs()
        if regs is None:
            return []

        (arch, bits) = self.getarch()
        pc = self.getreg("pc")
        prev_insts = self.prev_inst(pc, 12)

        code = ""
        if not prev_insts:
            return []

        for (addr, inst) in prev_insts[::-1]:
            if "call" in inst.strip().split()[0]:
                break
            code = "0x%x:%s\n" % (addr, inst) + code

        if "i386" in arch:
            args = self._get_function_args_32(code, argc)
        if "64" in arch:
            args = self._get_function_args_64(code, argc)

        return args

    @memoized
    def backtrace_depth(self, sp=None):
        """
        Get number of frames in backtrace

        Args:
            - sp: stack pointer address, for caching (Int)

        Returns:
            - depth: number of frames (Int)
        """
        backtrace = self.execute_redirect("backtrace")
        return backtrace.count("#")


    def get_eflags(self):
        """
        Get flags value from EFLAGS register

        Returns:
            - dictionary of named flags
        """

        # Eflags bit masks, source vdb
        EFLAGS_CF = 1 << 0
        EFLAGS_PF = 1 << 2
        EFLAGS_AF = 1 << 4
        EFLAGS_ZF = 1 << 6
        EFLAGS_SF = 1 << 7
        EFLAGS_TF = 1 << 8
        EFLAGS_IF = 1 << 9
        EFLAGS_DF = 1 << 10
        EFLAGS_OF = 1 << 11

        flags = {"CF":0, "PF":0, "AF":0, "ZF":0, "SF":0, "TF":0, "IF":0, "DF":0, "OF":0}
        eflags = self.getreg("eflags")
        if not eflags:
            return None
        flags["CF"] = bool(eflags & EFLAGS_CF)
        flags["PF"] = bool(eflags & EFLAGS_PF)
        flags["AF"] = bool(eflags & EFLAGS_AF)
        flags["ZF"] = bool(eflags & EFLAGS_ZF)
        flags["SF"] = bool(eflags & EFLAGS_SF)
        flags["TF"] = bool(eflags & EFLAGS_TF)
        flags["IF"] = bool(eflags & EFLAGS_IF)
        flags["DF"] = bool(eflags & EFLAGS_DF)
        flags["OF"] = bool(eflags & EFLAGS_OF)

        return flags

    def set_eflags(self, flagname, value):
        """
        Set/clear/toggle value of a flag register

        Returns:
            - True if success (Bool)
        """

        # Eflags bit masks, source vdb
        EFLAGS_CF = 1 << 0
        EFLAGS_PF = 1 << 2
        EFLAGS_AF = 1 << 4
        EFLAGS_ZF = 1 << 6
        EFLAGS_SF = 1 << 7
        EFLAGS_TF = 1 << 8
        EFLAGS_IF = 1 << 9
        EFLAGS_DF = 1 << 10
        EFLAGS_OF = 1 << 11

        flags = {"carry": "CF", "parity": "PF", "adjust": "AF", "zero": "ZF", "sign": "SF",
                    "trap": "TF", "interrupt": "IF", "direction": "DF", "overflow": "OF"}

        flagname = flagname.lower()

        if flagname not in flags:
            return False

        eflags = self.get_eflags()
        if not eflags:
            return False

        # If value doesn't match the current, or we want to toggle, toggle
        if value is None or eflags[flags[flagname]] != value:
            reg_eflags = self.getreg("eflags")
            reg_eflags ^= eval("EFLAGS_%s" % flags[flagname])
            result = self.execute("set $eflags = 0x%x" % reg_eflags)
            return result

        return True

    def eval_target(self, inst):
        """
        Evaluate target address of an instruction, used for jumpto decision

        Args:
            - inst: ASM instruction text (String)

        Returns:
            - target address (Int)
        """

        target = None
        inst = inst.strip()
        opcode = inst.split(":\t")[-1].split()[0]
        # this regex includes x86_64 RIP relateive address reference
        p = re.compile(r".*?:\s*[^ ]*\s*(.* PTR ).*(0x[^ ]*)")
        m = p.search(inst)
        if not m:
            p = re.compile(r".*?:\s.*\s(0x[^ ]*|\w+)")
            m = p.search(inst)
            if m:
                target = m.group(1)
                target = self.parse_and_eval(target)
            else:
                target = None
        else:
            if "]" in m.group(2): # e.g DWORD PTR [ebx+0xc]
                p = re.compile(r".*?:\s*[^ ]*\s*(.* PTR ).*\[(.*)\]")
                m = p.search(inst)
            target = self.parse_and_eval("%s[%s]" % (m.group(1), m.group(2).strip()))

        return to_int(target)

    def testjump(self, inst=None):
        """
        Test if jump instruction is taken or not

        Returns:
            - (status, address of target jumped instruction)
        """

        flags = self.get_eflags()
        if not flags:
            return None

        if not inst:
            pc = self.getreg("pc")
            inst = self.execute_redirect("x/i 0x%x" % pc)
            if not inst:
                return None

        opcode = inst.split(":\t")[-1].split()[0]
        next_addr = self.eval_target(inst)
        if next_addr is None:
            next_addr = 0

        if opcode == "jmp":
            return next_addr
        if opcode == "je" and flags["ZF"]:
            return next_addr
        if opcode == "jne" and not flags["ZF"]:
            return next_addr
        if opcode == "jg" and not flags["ZF"] and (flags["SF"] == flags["OF"]):
            return next_addr
        if opcode == "jge" and (flags["SF"] == flags["OF"]):
            return next_addr
        if opcode == "ja" and not flags["CF"] and not flags["ZF"]:
            return next_addr
        if opcode == "jae" and not flags["CF"]:
            return next_addr
        if opcode == "jl" and (flags["SF"] != flags["OF"]):
            return next_addr
        if opcode == "jle" and (flags["ZF"] or (flags["SF"] != flags["OF"])):
            return next_addr
        if opcode == "jb" and flags["CF"]:
            return next_addr
        if opcode == "jbe" and (flags["CF"] or flags["ZF"]):
            return next_addr
        if opcode == "jo" and flags["OF"]:
            return next_addr
        if opcode == "jno" and not flags["OF"]:
            return next_addr
        if opcode == "jz" and flags["ZF"]:
            return next_addr
        if opcode == "jnz" and flags["OF"]:
            return next_addr

        return None


    #########################
    #   Memory Operations   #
    #########################
    @memoized
    def get_vmmap(self, name=None):
        """
        Get virtual memory mapping address ranges of debugged process

        Args:
            - name: name/address of binary/library to get mapping range (String)
                + name = "binary" means debugged program
                + name = "all" means all virtual maps

        Returns:
            - list of virtual mapping ranges (start(Int), end(Int), permission(String), mapname(String))

        """
        def _get_offline_maps():
            name = self.getfile()
            if not name:
                return None
            headers = self.elfheader()
            binmap = []
            hlist = [x for x in headers.items() if x[1][2] == 'code']
            hlist = sorted(hlist, key=lambda x:x[1][0])
            binmap += [(hlist[0][1][0], hlist[-1][1][1], "rx-p", name)]

            hlist = [x for x in headers.items() if x[1][2] == 'rodata']
            hlist = sorted(hlist, key=lambda x:x[1][0])
            binmap += [(hlist[0][1][0], hlist[-1][1][1], "r--p", name)]

            hlist = [x for x in headers.items() if x[1][2] == 'data']
            hlist = sorted(hlist, key=lambda x:x[1][0])
            binmap += [(hlist[0][1][0], hlist[-1][1][1], "rw-p", name)]

            return binmap

        def _get_allmaps_osx(pid, remote=False):
            maps = []
            #_DATA                 00007fff77975000-00007fff77976000 [    4K] rw-/rw- SM=COW  /usr/lib/system/libremovefile.dylib
            pattern = re.compile(r"([^\n]*)\s*  ([0-9a-f][^-\s]*)-([^\s]*) \[.*\]\s([^/]*).*  (.*)")

            if remote: # remote target, not yet supported
                return maps
            else: # local target
                try:  out = execute_external_command("/usr/bin/vmmap -w %s" % self.getpid())
                except: error_msg("could not read vmmap of process")

            matches = pattern.findall(out)
            if matches:
                for (name, start, end, perm, mapname) in matches:
                    if name.startswith("Stack"):
                        mapname = "[stack]"
                    start = to_int("0x%s" % start)
                    end = to_int("0x%s" % end)
                    if mapname == "":
                        mapname = name.strip()
                    maps += [(start, end, perm, mapname)]
            return maps


        def _get_allmaps_freebsd(pid, remote=False):
            maps = []
            mpath = "/proc/%s/map" % pid
            # 0x8048000 0x8049000 1 0 0xc36afdd0 r-x 1 0 0x1000 COW NC vnode /path/to/file NCH -1
            pattern = re.compile(r"0x([0-9a-f]*) 0x([0-9a-f]*)(?: [^ ]*){3} ([rwx-]*)(?: [^ ]*){6} ([^ ]*)")

            if remote: # remote target, not yet supported
                return maps
            else: # local target
                try:  out = open(mpath).read()
                except: error_msg("could not open %s; is procfs mounted?" % mpath)

            matches = pattern.findall(out)
            if matches:
                for (start, end, perm, mapname) in matches:
                    if start[:2] in ["bf", "7f", "ff"] and "rw" in perm:
                        mapname = "[stack]"
                    start = to_int("0x%s" % start)
                    end = to_int("0x%s" % end)
                    if mapname == "-":
                        if start == maps[-1][1] and maps[-1][-1][0] == "/":
                            mapname = maps[-1][-1]
                        else:
                            mapname = "mapped"
                    maps += [(start, end, perm, mapname)]
            return maps

        def _get_allmaps_linux(pid, remote=False):
            maps = []
            mpath = "/proc/%s/maps" % pid
            #00400000-0040b000 r-xp 00000000 08:02 538840  /path/to/file
            pattern = re.compile(r"([0-9a-f]*)-([0-9a-f]*) ([rwxps-]*)(?: [^ ]*){3} *(.*)")

            if remote: # remote target
                tmp = tmpfile()
                self.execute("remote get %s %s" % (mpath, tmp.name))
                tmp.seek(0)
                out = tmp.read()
                tmp.close()
            else: # local target
                out = open(mpath).read()

            matches = pattern.findall(out)
            if matches:
                for (start, end, perm, mapname) in matches:
                    start = to_int("0x%s" % start)
                    end = to_int("0x%s" % end)
                    if mapname == "":
                        mapname = "mapped"
                    maps += [(start, end, perm, mapname)]
            return maps

        result = []
        pid = self.getpid()
        if not pid: # not running, try to use elfheader()
            try:
                return _get_offline_maps()
            except:
                return []

        # retrieve all maps
        os   = self.getos()
        rmt  = self.is_target_remote()
        maps = []
        try:
            if   os == "FreeBSD": maps = _get_allmaps_freebsd(pid, rmt)
            elif os == "Linux"  : maps = _get_allmaps_linux(pid, rmt)
            elif os == "Darwin" : maps = _get_allmaps_osx(pid, rmt)
        except Exception as e:
            if config.Option.get("debug") == "on":
                msg("Exception: %s" %e)
                traceback.print_exc()

        # select maps matched specific name
        if name == "binary":
            name = self.getfile()
        if name is None or name == "all":
            name = ""

        if to_int(name) is None:
            for (start, end, perm, mapname) in maps:
                if name in mapname:
                    result += [(start, end, perm, mapname)]
        else:
            addr = to_int(name)
            for (start, end, perm, mapname) in maps:
                if start <= addr and addr < end:
                    result += [(start, end, perm, mapname)]

        return result

    @memoized
    def get_vmrange(self, address, maps=None):
        """
        Get virtual memory mapping range of an address

        Args:
            - address: target address (Int)
            - maps: only find in provided maps (List)

        Returns:
            - tuple of virtual memory info (start, end, perm, mapname)
        """
        if address is None:
            return None
        if maps is None:
            maps = self.get_vmmap()
        if maps:
            for (start, end, perm, mapname) in maps:
                if start <= address and end > address:
                    return (start, end, perm, mapname)
        # failed to get the vmmap
        else:
            try:
                gdb.selected_inferior().read_memory(address, 1)
                start = address & 0xfffffffffffff000
                end = start + 0x1000
                return (start, end, 'rwx', 'unknown')
            except:
                return None


    @memoized
    def is_executable(self, address, maps=None):
        """
        Check if an address is executable

        Args:
            - address: target address (Int)
            - maps: only check in provided maps (List)

        Returns:
            - True if address belongs to an executable address range (Bool)
        """
        vmrange = self.get_vmrange(address, maps)
        if vmrange and "x" in vmrange[2]:
            return True
        else:
            return False

    @memoized
    def is_writable(self, address, maps=None):
        """
        Check if an address is writable

        Args:
            - address: target address (Int)
            - maps: only check in provided maps (List)

        Returns:
            - True if address belongs to a writable address range (Bool)
        """
        vmrange = self.get_vmrange(address, maps)
        if vmrange and "w" in vmrange[2]:
            return True
        else:
            return False

    @memoized
    def is_address(self, value, maps=None):
        """
        Check if a value is a valid address (belongs to a memory region)

        Args:
            - value (Int)
            - maps: only check in provided maps (List)

        Returns:
            - True if value belongs to an address range (Bool)
        """
        vmrange = self.get_vmrange(value, maps)
        return vmrange is not None

    @memoized
    def get_disasm(self, address, count=1):
        """
        Get the ASM code of instruction at address

        Args:
            - address: address to read instruction (Int)
            - count: number of code lines (Int)

        Returns:
            - asm code (String)
        """
        code = self.execute_redirect("x/%di 0x%x" % (count, address))
        if code:
            return code.rstrip()
        else:
            return ""

    def dumpmem(self, start, end):
        """
        Dump process memory from start to end

        Args:
            - start: start address (Int)
            - end: end address (Int)

        Returns:
            - memory content (raw bytes)
        """
        mem = None
        logfd = tmpfile(is_binary_file=True)
        logname = logfd.name
        out = self.execute_redirect("dump memory %s 0x%x 0x%x" % (logname, start, end))
        if out is None:
            return None
        else:
            logfd.flush()
            mem = logfd.read()
            logfd.close()

        return mem

    def readmem(self, address, size):
        """
        Read content of memory at an address

        Args:
            - address: start address to read (Int)
            - size: bytes to read (Int)

        Returns:
            - memory content (raw bytes)
        """
        # try fast dumpmem if it works
        mem = self.dumpmem(address, address+size)
        if mem is not None:
            return mem

        # failed to dump, use slow x/gx way
        mem = ""
        out = self.execute_redirect("x/%dbx 0x%x" % (size, address))
        if out:
            for line in out.splitlines():
                bytes = line.split(":\t")[-1].split()
                mem += "".join([chr(int(c, 0)) for c in bytes])

        return mem

    def read_int(self, address, intsize=None):
        """
        Read an interger value from memory

        Args:
            - address: address to read (Int)
            - intsize: force read size (Int)

        Returns:
            - mem value (Int)
        """
        if not intsize:
            intsize = self.intsize()
        value = self.readmem(address, intsize)
        if value:
            value = to_int("0x" + codecs.encode(value[::-1], 'hex'))
            return value
        else:
            return None


    def read_long(self, address):
        """
        Read a long long value from memory

        Args:
            - address: address to read (Int)

        Returns:
            - mem value (Long Long)
        """
        return self.read_int(address, 8)

    def writemem(self, address, buf):
        """
        Write buf to memory start at an address

        Args:
            - address: start address to write (Int)
            - buf: data to write (raw bytes)

        Returns:
            - number of written bytes (Int)
        """
        out = None
        if not buf:
            return 0

        if self.getpid():
            # try fast restore mem
            tmp = tmpfile(is_binary_file=True)
            tmp.write(buf)
            tmp.flush()
            out = self.execute_redirect("restore %s binary 0x%x" % (tmp.name, address))
            tmp.close()
        if not out: # try the slow way
            for i in range(len(buf)):
                if not self.execute("set {char}0x%x = 0x%x" % (address+i, ord(buf[i]))):
                    return i
            return i+1
        elif "error" in out: # failed to write the whole buf, find written byte
            for i in range(0, len(buf), 1):
                if not self.is_address(address+i):
                    return i
        else:
            return len(buf)

    def write_int(self, address, value, intsize=None):
        """
        Write an interger value to memory

        Args:
            - address: address to read (Int)
            - value: int to write to (Int)
            - intsize: force write size (Int)

        Returns:
            - Bool
        """
        if not intsize:
            intsize = self.intsize()
        buf = hex2str(value, intsize).ljust(intsize, "\x00")[:intsize]
        saved = self.readmem(address, intsize)
        if not saved:
            return False

        ret = self.writemem(address, buf)
        if ret != intsize:
            self.writemem(address, saved)
            return False
        return True

    def write_long(self, address, value):
        """
        Write a long long value to memory

        Args:
            - address: address to read (Int)
            - value: value to write to

        Returns:
            - Bool
        """
        return self.write_int(address, value, 8)

    def cmpmem(self, start, end, buf):
        """
        Compare contents of a memory region with a buffer

        Args:
            - start: start address (Int)
            - end: end address (Int)
            - buf: raw bytes

        Returns:
            - dictionary of array of diffed bytes in hex (Dictionary)
            {123: [("A", "B"), ("C", "C"))]}
        """
        line_len = 32
        if end < start:
            (start, end) = (end, start)

        mem = self.dumpmem(start, end)
        if mem is None:
            return None

        length = min(len(mem), len(buf))
        result = {}
        lineno = 0
        for i in range(length//line_len):
            diff = 0
            bytes_ = []
            for j in range(line_len):
                offset = i*line_len+j
                bytes_ += [(mem[offset:offset + 1], buf[offset:offset + 1])]
                if mem[offset] != buf[offset]:
                    diff = 1
            if diff == 1:
                result[start+lineno] = bytes_
            lineno += line_len

        bytes_ = []
        diff = 0
        for i in range(length % line_len):
            offset = lineno+i
            bytes_ += [(mem[offset:offset + 1], buf[offset:offset + 1])]
            if mem[offset] != buf[offset]:
                diff = 1
        if diff == 1:
            result[start+lineno] = bytes_

        return result

    def xormem(self, start, end, key):
        """
        XOR a memory region with a key

        Args:
            - start: start address (Int)
            - end: end address (Int)
            - key: XOR key (String)

        Returns:
            - xored memory content (raw bytes)
        """
        mem = self.dumpmem(start, end)
        if mem is None:
            return None

        if to_int(key) != None:
            key = hex2str(to_int(key), self.intsize())
        mem = list(bytes_iterator(mem))
        for index, char in enumerate(mem):
            key_idx = index % len(key)
            mem[index] = chr(ord(char) ^ ord(key[key_idx]))

        buf = b"".join([to_binary_string(x) for x in mem])
        bytes = self.writemem(start, buf)
        return buf

    def searchmem(self, start, end, search, mem=None):
        """
        Search for all instances of a pattern in memory from start to end

        Args:
            - start: start address (Int)
            - end: end address (Int)
            - search: string or python regex pattern (String)
            - mem: cached mem to not re-read for repeated searches (raw bytes)

        Returns:
            - list of found result: (address(Int), hex encoded value(String))

        """

        result = []
        if end < start:
            (start, end) = (end, start)

        if mem is None:
            mem = self.dumpmem(start, end)

        if not mem:
            return result

        if isinstance(search, str) and search.startswith("0x"):
            # hex number
            search = search[2:]
            if len(search) %2 != 0:
                search = "0" + search
            search = codecs.decode(search, 'hex')[::-1]
            search = re.escape(search)

        # Convert search to bytes if is not already
        if not isinstance(search, bytes):
            search = search.encode('utf-8')

        try:
            p = re.compile(search)
        except:
            search = re.escape(search)
            p = re.compile(search)

        found = list(p.finditer(mem))
        for m in found:
            index = 1
            if m.start() == m.end() and m.lastindex:
                index = m.lastindex+1
            for i in range(0,index):
                if m.start(i) != m.end(i):
                    result += [(start + m.start(i), codecs.encode(mem[m.start(i):m.end(i)], 'hex'))]

        return result

    def searchmem_by_range(self, mapname, search):
        """
        Search for all instances of a pattern in virtual memory ranges

        Args:
            - search: string or python regex pattern (String)
            - mapname: name of virtual memory range (String)

        Returns:
            - list of found result: (address(Int), hex encoded value(String))
        """

        result = []
        ranges = self.get_vmmap(mapname)
        if ranges:
            for (start, end, perm, name) in ranges:
                if "r" in perm:
                    result += self.searchmem(start, end, search)

        return result

    @memoized
    def search_reference(self, search, mapname=None):
        """
        Search for all references to a value in memory ranges

        Args:
            - search: string or python regex pattern (String)
            - mapname: name of target virtual memory range (String)

        Returns:
            - list of found result: (address(int), hex encoded value(String))
        """

        maps = self.get_vmmap()
        ranges = self.get_vmmap(mapname)
        result = []
        search_result = []
        for (start, end, perm, name) in maps:
            if "r" in perm:
                search_result += self.searchmem(start, end, search)

        for (start, end, perm, name) in ranges:
            for (a, v) in search_result:
                result += self.searchmem(start, end, to_address(a))

        return result

    @memoized
    def search_address(self, searchfor="stack", belongto="binary"):
        """
        Search for all valid addresses in memory ranges

        Args:
            - searchfor: memory region to search for addresses (String)
            - belongto: memory region that target addresses belong to (String)

        Returns:
            - list of found result: (address(Int), value(Int))
        """

        result = []
        maps = self.get_vmmap()
        if maps is None:
            return result

        searchfor_ranges = self.get_vmmap(searchfor)
        belongto_ranges = self.get_vmmap(belongto)
        step = self.intsize()
        for (start, end, _, _) in searchfor_ranges[::-1]: # dirty trick, to search in rw-p mem first
            mem = self.dumpmem(start, end)
            if not mem:
                continue
            for i in range(0, len(mem), step):
                search = "0x" + codecs.encode(mem[i:i+step][::-1], 'hex').decode('utf-8')
                addr = to_int(search)
                if self.is_address(addr, belongto_ranges):
                    result += [(start+i, addr)]

        return result

    @memoized
    def search_pointer(self, searchfor="stack", belongto="binary"):
        """
        Search for all valid pointers in memory ranges

        Args:
            - searchfor: memory region to search for pointers (String)
            - belongto: memory region that pointed addresses belong to (String)

        Returns:
            - list of found result: (address(Int), value(Int))
        """

        search_result = []
        result = []
        maps = self.get_vmmap()
        searchfor_ranges = self.get_vmmap(searchfor)
        belongto_ranges = self.get_vmmap(belongto)
        step = self.intsize()
        for (start, end, _, _) in searchfor_ranges[::-1]:
            mem = self.dumpmem(start, end)
            if not mem:
                continue
            for i in range(0, len(mem), step):
                search = "0x" + codecs.encode(mem[i:i+step][::-1], 'hex').decode('utf-8')
                addr = to_int(search)
                if self.is_address(addr):
                    (v, t, vn) = self.examine_mem_value(addr)
                    if t != 'value':
                        if self.is_address(to_int(vn), belongto_ranges):
                            if (to_int(v), v) not in search_result:
                                search_result += [(to_int(v), v)]

            for (a, v) in search_result:
                result += self.searchmem(start, end, to_address(a), mem)

        return result

    @memoized
    def examine_mem_value(self, value):
        """
        Examine a value in memory for its type and reference

        Args:
            - value: value to examine (Int)

        Returns:
            - tuple of (value(Int), type(String), next_value(Int))
        """
        def examine_data(value, bits=32):
            out = self.execute_redirect("x/%sx 0x%x" % ("g" if bits == 64 else "w", value))
            if out:
                v = out.split(":\t")[-1].strip()
                if is_printable(int2hexstr(to_int(v), bits//8)):
                    out = self.execute_redirect("x/s 0x%x" % value)
            return out

        result = (None, None, None)
        if value is None:
            return result

        maps = self.get_vmmap()
        binmap = self.get_vmmap("binary")

        (arch, bits) = self.getarch()
        if not self.is_address(value): # a value
            result = (to_hex(value), "value", "")
            return result
        else:
            (_, _, _, mapname) = self.get_vmrange(value)

        # check for writable first so rwxp mem will be treated as data
        if self.is_writable(value): # writable data address
            out = examine_data(value, bits)
            if out:
                result = (to_hex(value), "data", out.split(":", 1)[1].strip())

        elif self.is_executable(value): # code/rodata address
            if self.is_address(value, binmap):
                headers = self.elfheader()
            else:
                headers = self.elfheader_solib(mapname)

            if headers:
                headers = sorted(headers.items(), key=lambda x: x[1][1])
                for (k, (start, end, type)) in headers:
                    if value >= start and value < end:
                        if type == "code":
                            out = self.get_disasm(value)
                            p = re.compile(r".*?0x[^ ]*?\s(.*)")
                            m = p.search(out)
                            result = (to_hex(value), "code", m.group(1))
                        else: # rodata address
                            out = examine_data(value, bits)
                            result = (to_hex(value), "rodata", out.split(":", 1)[1].strip())
                        break

                if result[0] is None: # not fall to any header section
                    out = examine_data(value, bits)
                    result = (to_hex(value), "rodata", out.split(":", 1)[1].strip())

            else: # not belong to any lib: [heap], [vdso], [vsyscall], etc
                out = self.get_disasm(value)
                if "(bad)" in out:
                    out = examine_data(value, bits)
                    result = (to_hex(value), "rodata", out.split(":", 1)[1].strip())
                else:
                    p = re.compile(r".*?0x[^ ]*?\s(.*)")
                    m = p.search(out)
                    result = (to_hex(value), "code", m.group(1))

        else: # readonly data address
            out = examine_data(value, bits)
            if out:
                result = (to_hex(value), "rodata", out.split(":", 1)[1].strip())
            else:
                result = (to_hex(value), "rodata", "MemError")

        return result

    @memoized
    def examine_mem_reference(self, value, depth=5):
        """
        Deeply examine a value in memory for its references

        Args:
            - value: value to examine (Int)

        Returns:
            - list of tuple of (value(Int), type(String), next_value(Int))
        """
        result = []
        if depth <= 0:
            depth = 0xffffffff

        (v, t, vn) = self.examine_mem_value(value)
        while vn is not None:
            if len(result) > depth:
                _v, _t, _vn = result[-1]
                result[-1] = (_v, _t, "--> ...")
                break

            result += [(v, t, vn)]
            if v == vn or to_int(v) == to_int(vn): # point to self
                break
            if to_int(vn) is None:
                break
            if to_int(vn) in [to_int(v) for (v, _, _) in result]: # point back to previous value
                break
            (v, t, vn) = self.examine_mem_value(to_int(vn))

        return result

    @memoized
    def format_search_result(self, result, display=256):
        """
        Format the result from various memory search commands

        Args:
            - result: result of search commands (List)
            - display: number of items to display

        Returns:
            - text: formatted text (String)
        """

        text = ""
        if not result:
            text = "Not found"
        else:
            maxlen = 0
            maps = self.get_vmmap()
            shortmaps = []
            for (start, end, perm, name) in maps:
                shortname = os.path.basename(name)
                if shortname.startswith("lib"):
                    shortname = shortname.split("-")[0]
                shortmaps += [(start, end, perm, shortname)]

            count = len(result)
            if display != 0:
                count = min(count, display)
            text += "Found %d results, display max %d items:\n" % (len(result), count)
            for (addr, v) in result[:count]:
                vmrange = self.get_vmrange(addr, shortmaps)
                maxlen = max(maxlen, len(vmrange[3]))

            for (addr, v) in result[:count]:
                vmrange = self.get_vmrange(addr, shortmaps)
                chain = self.examine_mem_reference(addr)
                text += "%s : %s" % (vmrange[3].rjust(maxlen), format_reference_chain(chain) + "\n")

        return text


    ##########################
    #     Exploit Helpers    #
    ##########################
    @memoized
    def elfentry(self):
        """
        Get entry point address of debugged ELF file

        Returns:
            - entry address (Int)
        """
        out = self.execute_redirect("info files")
        p = re.compile(r"Entry point: ([^\s]*)")
        if out:
            m = p.search(out)
            if m:
                return to_int(m.group(1))
        return None

    @memoized
    def elfheader(self, name=None):
        """
        Get headers information of debugged ELF file

        Args:
            - name: specific header name (String)

        Returns:
            - dictionary of headers {name(String): (start(Int), end(Int), type(String))}
        """
        elfinfo = {}
        elfbase = 0
        if self.getpid():
            binmap = self.get_vmmap("binary")
            elfbase = binmap[0][0] if binmap else 0

        out = self.execute_redirect("maintenance info sections")
        if not out:
            return {}

        p = re.compile(r"\s*(0x[^-]*)->(0x[^ ]*) at (0x[^:]*):\s*([^ ]*)\s*(.*)")
        matches = p.findall(out)

        for (start, end, offset, hname, attr) in matches:
            start, end, offset = to_int(start), to_int(end), to_int(offset)
            # skip unuseful header
            if start < offset:
                continue
            # if PIE binary, update with runtime address
            if start < elfbase:
                start += elfbase
                end += elfbase

            if "CODE" in attr:
                htype = "code"
            elif "READONLY" in attr:
                htype = "rodata"
            else:
                htype = "data"

            elfinfo[hname.strip()] = (start, end, htype)

        result = {}
        if name is None:
            result = elfinfo
        else:
            if name in elfinfo:
                result[name] = elfinfo[name]
            else:
                for (k, v) in elfinfo.items():
                    if name in k:
                        result[k] = v
        return result

    @memoized
    def elfsymbols(self, pattern=None):
        """
        Get all non-debugging symbol information of debugged ELF file

        Returns:
            - dictionary of (address(Int), symname(String))
        """
        headers = self.elfheader()
        if ".plt" not in headers: # static binary
            return {}

        binmap = self.get_vmmap("binary")
        elfbase = binmap[0][0] if binmap else 0

        # get the .dynstr header
        headers = self.elfheader()
        if ".dynstr" not in headers:
            return {}
        (start, end, _) = headers[".dynstr"]
        mem = self.dumpmem(start, end)
        if not mem and self.getfile():
            fd = open(self.getfile())
            fd.seek(start, 0)
            mem = fd.read(end-start)
            fd.close()

        # Convert names into strings
        dynstrings = [name.decode('utf-8') for name in mem.split(b"\x00")]

        if pattern:
            dynstrings = [s for s in dynstrings if re.search(pattern, s)]

        # get symname@plt info
        symbols = {}
        for symname in dynstrings:
            if not symname: continue
            symname += "@plt"
            out = self.execute_redirect("info functions %s" % symname)
            if not out: continue
            m = re.findall(r".*(0x[^ ]*)\s*%s" % re.escape(symname), out)
            for addr in m:
                addr = to_int(addr)
                if self.is_address(addr, binmap):
                    if symname not in symbols:
                        symbols[symname] = addr
                        break

        # if PIE binary, update with runtime address
        for (k, v) in symbols.items():
            if v < elfbase:
                symbols[k] = v + elfbase

        return symbols

    @memoized
    def elfsymbol(self, symname=None):
        """
        Get non-debugging symbol information of debugged ELF file

        Args:
            - name: target function name (String), special cases:
                + "data": data transfer functions
                + "exec": exec helper functions

        Returns:
            - if exact name is not provided: dictionary of tuple (symname, plt_entry)
            - if exact name is provided: dictionary of tuple (symname, plt_entry, got_entry, reloc_entry)
        """
        datafuncs = ["printf", "puts", "gets", "cpy"]
        execfuncs = ["system", "exec", "mprotect", "mmap", "syscall"]
        result = {}
        if not symname or symname in ["data", "exec"]:
            symbols = self.elfsymbols()
        else:
            symbols = self.elfsymbols(symname)

        if not symname:
            result = symbols
        else:
            sname = symname.replace("@plt", "") + "@plt"
            if sname in symbols:
                plt_addr = symbols[sname]
                result[sname] = plt_addr # plt entry
                out = self.get_disasm(plt_addr, 2)
                for line in out.splitlines():
                    if "jmp" in line:
                        addr = to_int("0x" + line.strip().rsplit("0x")[-1].split()[0])
                        result[sname.replace("@plt","@got")] = addr # got entry
                    if "push" in line:
                        addr = to_int("0x" + line.strip().rsplit("0x")[-1])
                        result[sname.replace("@plt","@reloc")] = addr # reloc offset
            else:
                keywords = [symname]
                if symname == "data":
                    keywords = datafuncs
                if symname == "exec":
                    keywords = execfuncs
                for (k, v) in symbols.items():
                    for f in keywords:
                        if f in k:
                            result[k] = v

        return result

    @memoized
    def main_entry(self):
        """
        Get address of main function of stripped ELF file

        Returns:
            - main function address (Int)
        """
        refs = self.xrefs("__libc_start_main@plt")
        if refs:
            inst = self.prev_inst(refs[0][0])
            if inst:
                addr = re.search(".*(0x.*)", inst[0][1])
                if addr:
                    return to_int(addr.group(1))
        return None

    @memoized
    def readelf_header(self, filename, name=None):
        """
        Get headers information of an ELF file using 'readelf'

        Args:
            - filename: ELF file (String)
            - name: specific header name (String)

        Returns:
            - dictionary of headers (name(String), value(Int)) (Dict)
        """
        elfinfo = {}
        vmap = self.get_vmmap(filename)
        elfbase = vmap[0][0] if vmap else 0
        out = execute_external_command("%s -W -S %s" % (config.READELF, filename))
        if not out:
            return {}
        p = re.compile(r".*\[.*\] (\.[^ ]*) [^0-9]* ([^ ]*) [^ ]* ([^ ]*)(.*)")
        matches = p.findall(out)
        if not matches:
            return result

        for (hname, start, size, attr) in matches:
            start, end = to_int("0x"+start), to_int("0x"+start) + to_int("0x"+size)
            # if PIE binary or DSO, update with runtime address
            if start < elfbase:
                start += elfbase
            if end < elfbase:
                end += elfbase

            if "X" in attr:
                htype = "code"
            elif "W" in attr:
                htype = "data"
            else:
                htype = "rodata"
            elfinfo[hname.strip()] = (start, end, htype)

        result = {}
        if name is None:
            result = elfinfo
        else:
            if name in elfinfo:
                result[name] = elfinfo[name]
            else:
                for (k, v) in elfinfo.items():
                    if name in k:
                        result[k] = v
        return result

    @memoized
    def elfheader_solib(self, solib=None, name=None):
        """
        Get headers information of Shared Object Libraries linked to target

        Args:
            - solib: shared library name (String)
            - name: specific header name (String)

        Returns:
            - dictionary of headers {name(String): start(Int), end(Int), type(String))
        """
        # hardcoded ELF header type
        header_type = {"code": [".text", ".fini", ".init", ".plt", "__libc_freeres_fn"],
            "data": [".dynamic", ".data", ".ctors", ".dtors", ".jrc", ".got", ".got.plt",
                    ".bss", ".tdata", ".tbss", ".data.rel.ro", ".fini_array",
                    "__libc_subfreeres", "__libc_thread_subfreeres"]
        }

        @memoized
        def _elfheader_solib_all():
            out = self.execute_redirect("info files")
            if not out:
                return None

            p = re.compile(r"[^\n]*\s*(0x[^ ]*) - (0x[^ ]*) is (\.[^ ]*) in (.*)")
            soheaders = p.findall(out)

            result = []
            for (start, end, hname, libname) in soheaders:
                start, end = to_int(start), to_int(end)
                result += [(start, end, hname, os.path.realpath(libname))] # tricky, return the realpath version of libraries
            return result

        elfinfo = {}

        headers = _elfheader_solib_all()
        if not headers:
            return {}

        if solib is None:
            return headers

        vmap = self.get_vmmap(solib)
        elfbase = vmap[0][0] if vmap else 0

        for (start, end, hname, libname) in headers:
            if solib in libname:
                # if PIE binary or DSO, update with runtime address
                if start < elfbase:
                    start += elfbase
                if end < elfbase:
                    end += elfbase
                # determine the type
                htype = "rodata"
                if hname in header_type["code"]:
                    htype = "code"
                elif hname in header_type["data"]:
                    htype = "data"
                elfinfo[hname.strip()] = (start, end, htype)

        result = {}
        if name is None:
            result = elfinfo
        else:
            if name in elfinfo:
                result[name] = elfinfo[name]
            else:
                for (k, v) in elfinfo.items():
                    if name in k:
                        result[k] = v
        return result

    def checksec(self, filename=None):
        """
        Check for various security options of binary (ref: http://www.trapkit.de/tools/checksec.sh)

        Args:
            - file: path name of file to check (String)

        Returns:
            - dictionary of (setting(String), status(Int)) (Dict)
        """
        result = {}
        result["RELRO"] = 0
        result["CANARY"] = 0
        result["NX"] = 1
        result["PIE"] = 0
        result["FORTIFY"] = 0

        if filename is None:
            filename = self.getfile()

        if not filename:
            return None

        out =  execute_external_command("%s -W -a \"%s\" 2>&1" % (config.READELF, filename))
        if "Error:" in out:
            return None

        for line in out.splitlines():
            if "GNU_RELRO" in line:
                result["RELRO"] |= 2
            if "BIND_NOW" in line:
                result["RELRO"] |= 1
            if "__stack_chk_fail" in line:
                result["CANARY"] = 1
            if "GNU_STACK" in line and "RWE" in line:
                result["NX"] = 0
            if "Type:" in line and "DYN (" in line:
                result["PIE"] = 4 # Dynamic Shared Object
            if "(DEBUG)" in line and result["PIE"] == 4:
                result["PIE"] = 1
            if "_chk@" in line:
                result["FORTIFY"] = 1

        if result["RELRO"] == 1:
            result["RELRO"] = 0 # ? | BIND_NOW + NO GNU_RELRO = NO PROTECTION
        # result["RELRO"] == 2 # Partial | NO BIND_NOW + GNU_RELRO
        # result["RELRO"] == 3 # Full | BIND_NOW + GNU_RELRO
        return result

    def _verify_rop_gadget(self, start, end, depth=5):
        """
        Verify ROP gadget code from start to end with max number of instructions

        Args:
            - start: start address (Int)
            - end: end addres (Int)
            - depth: number of instructions (Int)

        Returns:
            - list of valid gadgets (address(Int), asmcode(String))
        """

        result = []
        valid = 0
        out = self.execute_redirect("disassemble 0x%x, 0x%x" % (start, end+1))
        if not out:
            return []

        code = out.splitlines()[1:-1]
        for line in code:
            if "bad" in line:
                return []
            (addr, code) = line.strip().split(":", 1)
            addr = to_int(addr.split()[0])
            result += [(addr, " ".join(code.strip().split()))]
            if "ret" in code:
                return result
            if len(result) > depth:
                break

        return []

    @memoized
    def search_asm(self, start, end, asmcode, rop=0):
        """
        Search for ASM instructions in memory

        Args:
            - start: start address (Int)
            - end: end address (Int)
            - asmcode: assembly instruction (String)
                + multiple instructions are separated by ";"
                + wildcard ? supported, will be replaced by registers or multi-bytes

        Returns:
            - list of (address(Int), hexbyte(String))
        """
        wildcard = asmcode.count('?')
        magic_bytes = ["0x00", "0xff", "0xdead", "0xdeadbeef", "0xdeadbeefdeadbeef"]

        ops = [x for x in asmcode.split(';') if x]
        def buildcode(code=b"", pos=0, depth=0):
            if depth == wildcard and pos == len(ops):
                yield code
                return

            c = ops[pos].count('?')
            if c > 2: return
            elif c == 0:
                asm = self.assemble(ops[pos])
                if asm:
                    for code in buildcode(code + asm, pos+1, depth):
                        yield code
            else:
                save = ops[pos]
                for regs in REGISTERS.values():
                    for reg in regs:
                        ops[pos] = save.replace("?", reg, 1)
                        for asmcode_reg in buildcode(code, pos, depth+1):
                            yield asmcode_reg
                for byte in magic_bytes:
                    ops[pos] = save.replace("?", byte, 1)
                    for asmcode_mem in buildcode(code, pos, depth+1):
                        yield asmcode_mem
                ops[pos] = save

        searches = []

        def decode_hex_escape(str_):
            """Decode string as hex and escape for regex"""
            return re.escape(codecs.decode(str_, 'hex'))

        for machine_code in buildcode():
            search = re.escape(machine_code)
            search = search.replace(decode_hex_escape(b"dead"), b"..")\
                .replace(decode_hex_escape(b"beef"), b"..")\
                .replace(decode_hex_escape(b"00"), b".")\
                .replace(decode_hex_escape(b"ff"), b".")

            if rop and 'ret' not in asmcode:
                search += b".{0,24}\\xc3"
            searches.append(search)

        if not searches:
            warning_msg("invalid asmcode: '%s'" % asmcode)
            return []

        search = b"(?=(" + b"|".join(searches) + b"))"
        candidates = self.searchmem(start, end, search)

        if rop:
            result = {}
            for (a, v) in candidates:
                gadget = self._verify_rop_gadget(a, a+len(v)//2 - 1)
                # gadget format: [(address, asmcode), (address, asmcode), ...]
                if gadget != []:
                    blen = gadget[-1][0] - gadget[0][0] + 1
                    bytes = v[:2*blen]
                    asmcode_rs = "; ".join([c for _, c in gadget])
                    if re.search(re.escape(asmcode).replace(r"\ ",".*").replace(r"\?",".*"), asmcode_rs)\
                        and a not in result:
                        result[a] = (bytes, asmcode_rs)
            result = list(result.items())
        else:
            result = []
            for (a, v) in candidates:
                asmcode = self.execute_redirect("disassemble 0x%x, 0x%x" % (a, a+(len(v)//2)))
                if asmcode:
                    asmcode = "\n".join(asmcode.splitlines()[1:-1])
                    matches = re.findall(".*:([^\n]*)", asmcode)
                    result += [(a, (v, ";".join(matches).strip()))]

        return result

    def common_rop_gadget(self, mapname=None):
        """
        Get common rop gadgets in binary: ret, popret, pop2ret, pop3ret, add [mem] reg, add reg [mem]

        Returns:
            - dictionary of (gadget(String), address(Int))
        """

        def _valid_register_opcode(bytes_):
            if not bytes_:
                return False

            for c in bytes_iterator(bytes_):
                if ord(c) not in list(range(0x58, 0x60)):
                    return False
            return True

        result = {}
        if mapname is None:
            mapname = "binary"
        maps = self.get_vmmap(mapname)
        if maps is None:
            return result

        for (start, end, _, _) in maps:
            if not self.is_executable(start, maps): continue

            mem = self.dumpmem(start, end)
            found = self.searchmem(start, end, b"....\xc3", mem)
            for (a, v) in found:
                v = codecs.decode(v, 'hex')
                if "ret" not in result:
                    result["ret"] = a+4
                if "leaveret" not in result:
                    if v[-2] == "\xc9":
                        result["leaveret"] = a+3
                if "popret" not in result:
                    if _valid_register_opcode(v[-2:-1]):
                        result["popret"] = a+3
                if "pop2ret" not in result:
                    if _valid_register_opcode(v[-3:-1]):
                        result["pop2ret"] = a+2
                if "pop3ret" not in result:
                    if _valid_register_opcode(v[-4:-1]):
                        result["pop3ret"] = a+1
                if "pop4ret" not in result:
                    if _valid_register_opcode(v[-5:-1]):
                        result["pop4ret"] = a

            # search for add esp, byte 0xNN
            found = self.searchmem(start, end, b"\x83\xc4([^\xc3]){0,24}\xc3", mem)
            # search for add esp, 0xNNNN
            found += self.searchmem(start, end, b"\x81\xc4([^\xc3]){0,24}\xc3", mem)
            for (a, v) in found:
                if v.startswith(b"81"):
                    offset = to_int("0x" + codecs.encode(codecs.decode(v, 'hex')[2:5][::-1], 'hex').decode('utf-8'))
                elif v.startswith(b"83"):
                    offset = to_int("0x" + v[4:6].decode('utf-8'))
                gg = self._verify_rop_gadget(a, a+len(v)//2-1)
                for (_, c) in gg:
                    if "pop" in c:
                        offset += 4
                gadget = "addesp_%d" % offset
                if gadget not in result:
                    result[gadget] = a

        return result

    def search_jmpcall(self, start, end, regname=None):
        """
        Search memory for jmp/call reg instructions

        Args:
            - start: start address (Int)
            - end: end address (Int)
            - reg: register name (String)

        Returns:
            - list of (address(Int), instruction(String))
        """

        result = []
        REG = {0: "eax", 1: "ecx", 2: "edx", 3: "ebx", 4: "esp", 5: "ebp", 6: "esi", 7:"edi"}
        P2REG = {0: "[eax]", 1: "[ecx]", 2: "[edx]", 3: "[ebx]", 6: "[esi]", 7:"[edi]"}
        OPCODE = {0xe: "jmp", 0xd: "call"}
        P2OPCODE = {0x1: "call", 0x2: "jmp"}
        JMPREG = [b"\xff" + bytes_chr(i) for i in range(0xe0, 0xe8)]
        JMPREG += [b"\xff" + bytes_chr(i) for i in range(0x20, 0x28)]
        CALLREG = [b"\xff" + bytes_chr(i) for i in range(0xd0, 0xd8)]
        CALLREG += [b"\xff" + bytes_chr(i) for i in range(0x10, 0x18)]
        JMPCALL = JMPREG + CALLREG

        if regname is None:
            regname = ""
        regname = regname.lower()
        pattern = re.compile(b'|'.join(JMPCALL).replace(b' ', b'\ '))
        mem = self.dumpmem(start, end)
        found = pattern.finditer(mem)
        (arch, bits) = self.getarch()
        for m in list(found):
            inst = ""
            addr = start + m.start()
            opcode = codecs.encode(m.group()[1:2], 'hex')
            type = int(opcode[0:1], 16)
            reg = int(opcode[1:2], 16)
            if type in OPCODE:
                inst = OPCODE[type] + " " + REG[reg]

            if type in P2OPCODE and reg in P2REG:
                inst = P2OPCODE[type] + " " + P2REG[reg]

            if inst != "" and regname[-2:] in inst.split()[-1]:
                if bits == 64:
                    inst = inst.replace("e", "r")
                result += [(addr, inst)]

        return result

    def search_substr(self, start, end, search, mem=None):
        """
        Search for substrings of a given string/number in memory

        Args:
            - start: start address (Int)
            - end: end address (Int)
            - search: string to search for (String)
            - mem: cached memory (raw bytes)

        Returns:
            - list of tuple (substr(String), address(Int))
        """
        def substr(s1, s2):
            "Search for a string in another string"
            s1 = to_binary_string(s1)
            s2 = to_binary_string(s2)
            i = 1
            found = 0
            while i <= len(s1):
                if s2.find(s1[:i]) != -1:
                    found = 1
                    i += 1
                    if s1[:i-1][-1:] == b"\x00":
                        break
                else:
                    break
            if found == 1:
                return i-1
            else:
                return -1

        result = []
        if end < start:
            start, end = end, start

        if mem is None:
            mem = self.dumpmem(start, end)

        if search[:2] == "0x": # hex number
            search = search[2:]
            if len(search) %2 != 0:
                search = "0" + search
            search = codecs.decode(search, 'hex')[::-1]
        search = to_binary_string(decode_string_escape(search))
        while search:
            l = len(search)
            i = substr(search, mem)
            if i != -1:
                sub = search[:i]
                addr = start + mem.find(sub)
                if not check_badchars(addr):
                    result.append((sub, addr))
            else:
                result.append((search, -1))
                return result
            search = search[i:]
        return result

    def print_internal(self, *arg):
        """
        Internal usage only.
        Print args, and return string.
        """
        args = list(arg)
        debug_msg('args: %s' % args)

        fmt = ''
        exp = ''
        text=''

        if args:
            if args[0].startswith('/'):
                fmt=args[0]
                args.pop(0)

        if args:
            exp = " ".join(args)
            pfx= ''

            while True:
                mod_found = False
                for mod in ['*', '&']:
                    if exp.startswith(mod):
                        pfx += mod + ' '
                        exp = exp[1:].strip()
                        mod_found = True

                if not mod_found:
                    break

            exp2 = PRIVATE_PRINTS.get(exp, None)
            if exp2 is not None:
                exp = pfx + exp2
                text += 'EXP: %s\n\n'%(exp)
            else:
                exp = pfx + exp

        out = self.execute_redirect('print %s %s' %(fmt, exp))
        if out is None:
            out = red('exception occurred...')

        text += out
        return text

    r_match_address = re.compile('\$\d+\s+=\s*.*?(0x.*?)\s')
    def evaluate_expr(self, expr):
        """
        Evaluate expr into result.
        """
        r = peda.print_internal(expr)
        m = self.r_match_address.search(r)

        debug_msg("expr: %s, r: %s, m: %s"%(expr, r, m))

        if m:
            return to_int(m.group(1))
        else:
            return None

    def expr_to_address(self, var):
        """
        Convert var to address.
        Returns tuple: (value, converted)
        """
        if isinstance(var, int):
            return (var, False)

        if isinstance(var, str):
            if var.startswith('0x'):
                return (to_int(var), False)

            # has to convert var into address...
            r = self.evaluate_expr(var)
            if r:
                return (r, True)

        return (None, False)


###########################################################################
class PEDACmd(object):
    """
    Class for PEDA commands that interact with GDB
    """
    commands = []
    def __init__(self):
        # list of all available commands
        self.commands = [c for c in dir(self) if callable(getattr(self, c)) and not c.startswith("_")]

    ##################
    #   Misc Utils   #
    ##################
    def _missing_argument(self):
        """
        Raise exception for missing argument, for internal use
        """
        text = "missing argument"
        error_msg(text)
        raise Exception(text)

    def _is_running(self, warn_if_not_running=True):
        """
        Check if program is running, for internal use
        """
        pid = peda.getpid()
        if pid is None:
            if warn_if_not_running:
                warning_msg("not running")
            return None
            #raise Exception(text)
        else:
            return pid

    def reload(self, *arg):
        """
        Reload PEDA sources, keep current options untouch
        Usage:
            MYNAME [name]
        """
        (modname,) = normalize_argv(arg, 1)
        # save current PEDA options
        saved_opt = config.Option
        peda_path = os.path.dirname(PEDAFILE) + "/lib/"
        if not modname:
            modname = "PEDA" # just for notification
            ret = peda.execute("source %s" % PEDAFILE)
        else:
            if not modname.endswith(".py"):
                modname = modname + ".py"
            filepath = "%s/%s" % (peda_path, modname)
            if os.path.exists(filepath):
                ret = peda.execute("source %s" % filepath)
                peda.execute("source %s" % PEDAFILE)
            else:
                ret = False

        config.Option = saved_opt
        if ret:
            msg("%s reloaded!" % modname, "blue")
        else:
            msg("Failed to reload %s source from: %s" % (modname, peda_path))
        return

    def _get_helptext(self, *arg):
        """
        Get the help text, for internal use by help command and other aliases
        """

        (cmd,) = normalize_argv(arg, 1)
        helptext = ""
        if cmd is None:
            helptext = red("PEDA", "bold") + blue(" - Python Exploit Development Assistance for GDB", "bold") + "\n"
            helptext += "For latest update, check peda project page: %s\n" % green("https://github.com/longld/peda/")
            helptext += "List of \"peda\" subcommands, type the subcommand to invoke it:\n"
            i = 0
            for cmd in self.commands:
                if cmd.startswith("_"): continue # skip internal use commands
                func = getattr(self, cmd)
                helptext += "%s -- %s\n" % (cmd, green(trim(func.__doc__.strip("\n").splitlines()[0])))
            helptext += "\nType \"help\" followed by subcommand for full documentation."
        else:
            if cmd in self.commands:
                func = getattr(self, cmd)
                lines = trim(func.__doc__).splitlines()
                helptext += green(lines[0]) + "\n"
                for line in lines[1:]:
                    if "Usage:" in line:
                        helptext += blue(line) + "\n"
                    else:
                        helptext += line + "\n"
            else:
                for c in self.commands:
                    if not c.startswith("_") and cmd in c:
                        func = getattr(self, c)
                        helptext += "%s -- %s\n" % (c, green(trim(func.__doc__.strip("\n").splitlines()[0])))

        return helptext

    def help(self, *arg):
        """
        Print the usage manual for PEDA commands
        Usage:
            MYNAME
            MYNAME command
        """
        (target,) = normalize_argv(arg, 1)
        debug_msg('T: %s'%(target))
        prefix = ""
        suffix = ""
        helptext = ""
        if target is None:
            prefix = red("PEDA", "bold") + blue(" - Python Exploit Development Assistance for GDB", "bold") + "\n"
            prefix += "For latest update, check peda project page: %s\n" % green("https://github.com/longld/peda/")
            prefix += "List of \"peda\" subcommands & alias, type the subcommand to invoke it:\n\n"
            suffix = "\nType \"help\" followed by subcommand for full documentation.\n"

            for (cmd, doc) in PEDA_ALIAS:
                helptext += "%s -- %s\n" % (cmd, doc.splitlines()[0])

        else:
            matches = 0
            for (cmd, doc) in PEDA_ALIAS:
                if target in cmd:
                    matches += 1
                    helptext += "%s -- %s\n" % (cmd, doc.splitlines()[0])

            if matches == 0:
                prefix = 'No match found for target command: %s\n' % blue(target, "bold")
            else:
                prefix = "Total %d matche(s) found, as follows:\n\n"%matches
                suffix = "\nType \"help\" followed by subcommand for full documentation.\n"

        msg(prefix + helptext + suffix)

        return

    help.options = commands

    def pyhelp(self, *arg):
        """
        Wrapper for python built-in help
        Usage:
            MYNAME (enter interactive help)
            MYNAME help_request
        """
        (request,) = normalize_argv(arg, 1)
        if request is None:
            help()
            return

        peda_methods = ["%s" % c for c in dir(PEDA) if callable(getattr(PEDA, c)) and \
                                not c.startswith("_")]

        if request in peda_methods:
            request = "peda.%s" % request
        try:
            if request.lower().startswith("peda"):
                request = eval(request)
                help(request)
                return

            if "." in request:
                module, _, function = request.rpartition('.')
                if module:
                    module = module.split(".")[0]
                    __import__(module)
                    mod = sys.modules[module]
                    if function:
                        request = getattr(mod, function)
                    else:
                        request = mod
            else:
                mod = sys.modules['__main__']
                request = getattr(mod, request)

            # wrapper for python built-in help
            help(request)
        except: # fallback to built-in help
            try:
                help(request)
            except Exception as e:
                if config.Option.get("debug") == "on":
                    msg('Exception (%s): %s' % ('pyhelp', e), "red")
                    traceback.print_exc()
                msg("no Python documentation found for '%s'" % request)

        return
    pyhelp.options = ["%s" % c for c in dir(PEDA) if callable(getattr(PEDA, c)) and \
                        not c.startswith("_")]

    # show [option | args | env]
    def show(self, *arg):
        """
        Show various PEDA options and other settings
        Usage:
            MYNAME option [optname]
            MYNAME (show all options)
            MYNAME args
            MYNAME env [envname]
        """
        # show options
        def _show_option(name=None):
            if name is None:
                name = ""
            filename = peda.getfile()
            if filename:
               filename = os.path.basename(filename)
            else:
                filename = None
            for (k, v) in sorted(config.Option.show(name).items()):
                if filename and isinstance(v, str) and "#FILENAME#" in v:
                    v = v.replace("#FILENAME#", filename)
                msg("%s = %s" % (k, repr(v)))
            return

        # show args
        def _show_arg():
            arg = peda.execute_redirect("show args")
            arg = arg.split("started is ")[1][1:-3]
            arg = (peda.string_to_argv(arg))
            if not arg:
                msg("No argument")
            for (i, a) in enumerate(arg):
                text = "arg[%d]: %s" % ((i+1), a if is_printable(a) else to_hexstr(a))
                msg(text)
            return

        # show envs
        def _show_env(name=None):
            if name is None:
                name = ""
            env = peda.execute_redirect("show env")
            for line in env.splitlines():
                (k, v) = line.split("=", 1)
                if k.startswith(name):
                    msg("%s = %s" % (k, v if is_printable(v) else to_hexstr(v)))
            return

        (opt, name) = normalize_argv(arg, 2)

        if opt is None or opt.startswith("opt"):
            _show_option(name)
        elif opt.startswith("arg"):
            _show_arg()
        elif opt.startswith("env"):
            _show_env(name)
        else:
            msg("Unknown show option: %s" % opt)
        return
    show.options = ["option", "arg", "env"]

    # set [option | arg | env]
    def set(self, *arg):
        """
        Set various PEDA options and other settings
        Usage:
            MYNAME option name value
            MYNAME arg string
            MYNAME env name value
                support input non-printable chars, e.g MYNAME env EGG "\\x90"*1000
        """
        # set options
        def _set_option(name, value):
            if name in config.Option.options:
                config.Option.set(name, value)
                msg("%s = %s" % (name, repr(value)))
            else:
                msg("Unknown option: %s" % name)
            return

        # set args
        def _set_arg(*arg):
            cmd = "set args"
            for a in arg:
                try:
                    a = eval('%s' % a)
                except:
                    pass
                cmd += " '%s'" % a
            peda.execute(cmd)
            return

        # set env
        def _set_env(name, value):
            env = peda.execute_redirect("show env")
            cmd = "set env %s " % name
            try:
                value = eval('%s' % value)
            except:
                pass
            cmd += '%s' % value
            peda.execute(cmd)

            return

        (opt, name, value) = normalize_argv(arg, 3)
        if opt is None:
            self._missing_argument()

        if opt.startswith("opt"):
            if value is None:
                self._missing_argument()
            _set_option(name, value)
        elif opt.startswith("arg"):
            _set_arg(*arg[1:])
        elif opt.startswith("env"):
            _set_env(name, value)
        else:
            msg("Unknown set option: %s" % known_args.opt)
        return
    set.options = ["option", "arg", "env"]

    def hexprint(self, *arg):
        """
        Display hexified of data in memory
        Usage:
            MYNAME address (display 16 bytes from address)
            MYNAME address count
            MYNAME address /count (display "count" lines, 16-bytes each)
        """
        (address, count) = normalize_argv(arg, 2)
        if address is None:
            self._missing_argument()

        if count is None:
            count = 16

        if not to_int(count) and count.startswith("/"):
            count = to_int(count[1:])
            count = count * 16 if count else None

        bytes_ = peda.dumpmem(address, address+count)
        if bytes_ is None:
            warning_msg("cannot retrieve memory content")
        else:
            hexstr = to_hexstr(bytes_)
            linelen = 16 # display 16-bytes per line
            i = 0
            text = ""
            while hexstr:
                text += '%s : "%s"\n' % (blue(to_address(address+i*linelen)), hexstr[:linelen*4])
                hexstr = hexstr[linelen*4:]
                i += 1
            pager(text)

        return


    def hexdump(self, *arg):
        """
        Display hex/ascii dump of data in memory
        Usage:
            MYNAME address (dump 16 bytes from address)
            MYNAME address count
            MYNAME address /count (dump "count" lines, 16-bytes each)
        """
        def ascii_char(ch):
            if ord(ch) >= 0x20 and ord(ch) < 0x7e:
                return chr(ord(ch))  # Ensure we return a str
            else:
                return "."

        (address, count) = normalize_argv(arg, 2)
        (address,_) = peda.expr_to_address(address)
        if address is None:
            self._missing_argument()

        if count is None:
            count = 16

        if not to_int(count) and count.startswith("/"):
            count = to_int(count[1:])
            count = count * 16 if count else None


        bytes_ = peda.dumpmem(address, address+count)
        if bytes_ is None:
            warning_msg("cannot retrieve memory content")
        else:
            linelen = 16 # display 16-bytes per line
            i = 0
            text = ""
            while bytes_:
                buf = bytes_[:linelen]
                hexbytes = " ".join(["%02x" % ord(c) for c in bytes_iterator(buf)])
                asciibytes = "".join([ascii_char(c) for c in bytes_iterator(buf)])
                text += '%s : %s  %s\n' % (blue(to_address(address+i*linelen)), hexbytes.ljust(linelen*3), asciibytes)
                bytes_ = bytes_[linelen:]
                i += 1
            pager(text)

        return

    def aslr(self, *arg):
        """
        Show/set ASLR setting of GDB
        Usage:
            MYNAME [on|off]
        """
        (option,) = normalize_argv(arg, 1)
        if option is None:
            out = peda.execute_redirect("show disable-randomization")
            if not out:
                warning_msg("ASLR setting is unknown or not available")
                return

            if "is off" in out:
                msg("ASLR is %s" % green("ON"))
            if "is on" in out:
                msg("ASLR is %s" % red("OFF"))
        else:
            option = option.strip().lower()
            if option in ["on", "off"]:
                peda.execute("set disable-randomization %s" % ("off" if option == "on" else "on"))

        return


    def distance(self, *arg):
        """
        Calculate distance between two addresses
        Usage:
            MYNAME address (calculate from current $SP to address)
            MYNAME address1 address2
        """
        (start, end) = normalize_argv(arg, 2)

        (start_addr, start_converted) = peda.expr_to_address(start)
        if start_addr is None:
            self._missing_argument()

        (end_addr, end_converted) = peda.expr_to_address(end)
        if end_addr is None and not self._is_running():
            self._missing_argument()

        sp = None
        if end_addr is None:
            sp = peda.getreg("sp")

            end = start
            end_addr = start_addr
            end_converted = start_converted

            start = "SP"
            start_addr = sp
            start_converted = True

        dist = end_addr - start_addr
        text = "From 0x%x%s to 0x%x%s: " % (
            start_addr, " (%s)"%start if start_converted else "",
            end_addr,   " (%s)"%end   if end_converted else "")

        text += "%d bytes, %d dwords%s" % (dist, dist//4, " (+%d bytes)" % (dist%4) if (dist%4 != 0) else "")
        msg(text)

        return

    def session(self, *arg):
        """
        Save/restore a working gdb session to file as a script
        Usage:
            MYNAME save [filename]
            MYNAME restore [filename]
        """
        options = ["save", "restore", "autosave"]
        (option, filename) = normalize_argv(arg, 2)
        if option not in options:
            self._missing_argument()

        if not filename:
            filename = peda.get_config_filename("session")

        if option == "save":
            if peda.save_session(filename):
                msg("Saved GDB session to file %s" % filename)
            else:
                msg("Failed to save GDB session")

        if option == "restore":
            if peda.restore_session(filename):
                msg("Restored GDB session from file %s" % filename)
            else:
                msg("Failed to restore GDB session")

        if option == "autosave":
            if config.Option.get("autosave") == "on":
                peda.save_session(filename)

        return
    session.options = ["save", "restore"]

    #################################
    #   Debugging Helper Commands   #
    #################################
    def procinfo(self, *arg):
        """
        Display various info from /proc/pid/
        Usage:
            MYNAME [pid]
        """
        options = ["exe", "fd", "pid", "ppid", "uid", "gid"]

        if peda.getos() != "Linux":
            warning_msg("this command is only available on Linux")

        (pid,) = normalize_argv(arg, 1)

        if not pid:
            pid = peda.getpid()

        if not pid:
            return

        info = {}
        try:
            info["exe"] = os.path.realpath("/proc/%d/exe" % pid)
        except:
            warning_msg("cannot access /proc/%d/" % pid)
            return

        # fd list
        info["fd"] = {}
        fdlist = os.listdir("/proc/%d/fd" % pid)
        for fd in fdlist:
            rpath = os.readlink("/proc/%d/fd/%s" % (pid, fd))
            sock = re.search(r"socket:\[(.*)\]", rpath)
            if sock:
                spath = execute_external_command("netstat -aen | grep %s" % sock.group(1))
                if spath:
                    rpath = spath.strip()
            info["fd"][to_int(fd)] = rpath

        # uid/gid, pid, ppid
        info["pid"] = pid
        status = open("/proc/%d/status" % pid).read()
        ppid = re.search(r"PPid:\s*([^\s]*)", status).group(1)
        info["ppid"] = to_int(ppid) if ppid else -1
        uid = re.search(r"Uid:\s*([^\n]*)", status).group(1)
        info["uid"] = [to_int(id) for id in uid.split()]
        gid = re.search(r"Gid:\s*([^\n]*)", status).group(1)
        info["gid"] = [to_int(id) for id in gid.split()]

        for opt in options:
            if opt == "fd":
                for (fd, path) in info[opt].items():
                    msg("fd[%d] -> %s" % (fd, path))
            else:
                msg("%s = %s" % (opt, info[opt]))
        return

    # getfile()
    def getfile(self):
        """
        Get exec filename of current debugged process
        Usage:
            MYNAME
        """
        filename = peda.getfile()
        if filename == None:
            msg("No file specified")
        else:
            msg(filename)
        return

    # getpid()
    def getpid(self):
        """
        Get PID of current debugged process
        Usage:
            MYNAME
        """
        pid = self._is_running()
        msg(pid)
        return

    # disassemble()
    def pdisass(self, *arg):
        """
        Format output of gdb disassemble command with colors
        Usage:
            MYNAME "args for gdb disassemble command"
            MYNAME address /NN: equivalent to "x/NNi address"
        """
        (address, fmt_count) = normalize_argv(arg, 2)
        if isinstance(fmt_count, str) and fmt_count.startswith("/"):
            count = to_int(fmt_count[1:])
            if not count or to_int(address) is None:
                self._missing_argument()
            else:
                code = peda.get_disasm(address, count)
        else:
            code = peda.disassemble(*arg)

        msg(format_disasm_code(code))

        return

    # disassemble_around
    def nearpc(self, *arg):
        """
        Disassemble instructions nearby current PC or given address
        Usage:
            MYNAME [count]
            MYNAME address [count]
                count is maximum 256
        """
        (address, count) = normalize_argv(arg, 2)
        address = to_int(address)

        count = to_int(count)
        if address is not None and address < 0x40000:
            count = address
            address = None

        if address is None:
            address = peda.getreg("pc")

        if count is None:
            code = peda.disassemble_around(address)
        else:
            code = peda.disassemble_around(address, count)

        if code:
            msg(format_disasm_code(code, address))
        else:
            error_msg("invalid $pc address or instruction count")
        return

    # get_function_args()
    def dumpargs(self, *arg):
        """
        Display arguments passed to a function when stopped at a call instruction
        Usage:
            MYNAME [count]
                count: force to display "count args" instead of guessing
        """

        (count,) = normalize_argv(arg, 1)
        if not self._is_running():
            return

        args = peda.get_function_args(count)
        if args:
            msg("Guessed arguments:")
            for (i, a) in enumerate(args):
                chain = peda.examine_mem_reference(a)
                msg("arg[%d]: %s" % (i, format_reference_chain(chain)))
        else:
            msg("No argument")

        return

    def xuntil(self, *arg):
        """
        Continue execution until an address or function
        Usage:
            MYNAME address | function
        """
        (address,) = normalize_argv(arg, 1)
        if to_int(address) is None:
            peda.execute("tbreak %s" % address)
        else:
            peda.execute("tbreak *0x%x" % address)
        pc = peda.getreg("pc")
        if pc is None:
            peda.execute("run")
        else:
            peda.execute("continue")
        return

    def goto(self, *arg):
        """
        Continue execution at an address
        Usage:
            MYNAME address
        """
        (address,) = normalize_argv(arg, 1)
        if to_int(address) is None:
            self._missing_argument()

        peda.execute("set $pc = 0x%x" % address)
        peda.execute("stop")
        return

    def skipi(self, *arg):
        """
        Skip execution of next count instructions
        Usage:
            MYNAME [count]
        """
        (count,) = normalize_argv(arg, 1)
        if to_int(count) is None:
            count = 1

        if not self._is_running():
            return

        next_code = peda.next_inst(peda.getreg("pc"), count)
        if not next_code:
            warning_msg("failed to get next instructions")
            return
        last_addr = next_code[-1][0]
        peda.execute("set $pc = 0x%x" % last_addr)
        peda.execute("stop")
        return

    def start(self, *arg):
        """
        Start debugged program and stop at most convenient entry
        Usage:
            MYNAME
        """
        entries = ["main"]
        main_addr = peda.main_entry()
        if main_addr:
            entries += ["*0x%x" % main_addr]
        entries += ["__libc_start_main@plt"]
        entries += ["_start"]
        entries += ["_init"]

        started = 0
        for e in entries:
            out = peda.execute_redirect("tbreak %s" % e)
            if out and "breakpoint" in out:
                peda.execute("run %s" % ' '.join(arg))
                started = 1
                break

        if not started: # try ELF entry point or just "run" as the last resort
            elf_entry = peda.elfentry()
            if elf_entry:
                out = peda.execute_redirect("tbreak *%s" % elf_entry)

            peda.execute("run")

        return


    @msg.bufferize
    def context_register(self, *arg):
        """
        Display register information of current execution context
        Usage:
            MYNAME
        """
        # if not self._is_running():
        #     return

        pc = peda.getreg("pc")
        # display register info
        msg("[%s]" % "registers".center(92, "-"), "blue")
        self.xinfo("register")

        return

    @msg.bufferize
    def context_code(self, *arg):
        """
        Display nearby disassembly at $PC of current execution context
        Usage:
            MYNAME [linecount]
        """
        (count,) = normalize_argv(arg, 1)

        if count is None:
            count = 8

        pc = peda.getreg("pc")
        if peda.is_address(pc):
            inst = peda.get_disasm(pc)
        else:
            inst = None

        text = blue("[%s]" % "code".center(92, "-"))
        msg(text)
        if inst: # valid $PC
            text = ""
            opcode = inst.split(":\t")[-1].split()[0]
            # stopped at function call
            if "call" in opcode:
                text += peda.disassemble_around(pc, count)
                msg(format_disasm_code(text, pc))
                self.dumpargs()
            # stopped at jump
            elif "j" in opcode:
                jumpto = peda.testjump(inst)
                if jumpto: # JUMP is taken
                    code = peda.disassemble_around(pc, count)
                    code = code.splitlines()
                    pc_idx = 999
                    for (idx, line) in enumerate(code):
                        if ("0x%x" % pc) in line.split(":")[0]:
                            pc_idx = idx
                        if idx <= pc_idx:
                            text += line + "\n"
                        else:
                            text += " | %s\n" % line.strip()
                    text = format_disasm_code(text, pc) + "\n"
                    text += " |->"
                    code = peda.get_disasm(jumpto, count//2)
                    if not code:
                        code = "   Cannot evaluate jump destination\n"

                    code = code.splitlines()
                    text += red(code[0]) + "\n"
                    for line in code[1:]:
                        text += "       %s\n" % line.strip()
                    text += red("JUMP is taken".rjust(79))
                else: # JUMP is NOT taken
                    text += format_disasm_code(peda.disassemble_around(pc, count), pc)
                    text += "\n" + green("JUMP is NOT taken".rjust(79))

                msg(text.rstrip())
            # stopped at other instructions
            else:
                text += peda.disassemble_around(pc, count)
                msg(format_disasm_code(text, pc))
        else: # invalid $PC
            msg("Invalid $PC address: 0x%x" % pc, "red")

        return

    @msg.bufferize
    def context_stack(self, *arg):
        """
        Display stack of current execution context
        Usage:
            MYNAME [linecount]
        """
        (count,) = normalize_argv(arg, 1)

        text = blue("[%s]" % "stack".center(92, "-"))
        msg(text)
        sp = peda.getreg("sp")
        if peda.is_address(sp):
            self.telescope(sp, count)
        else:
            msg("Invalid $SP address: 0x%x" % sp, "red")

        return


    @msg.bufferize
    def context_source(self, *arg):
        """
        Display nearby disassembly at $PC of current execution context
        Usage:
            MYNAME [linecount]
        """
        (count,) = normalize_argv(arg, 1)

        if count is None:
            count = 8

        pc = peda.getreg("pc")

        try:
            symtabline = gdb.find_pc_line(pc)
            symtab = symtabline.symtab
            line_num = symtabline.line - 1     # we subtract one because line number returned by gdb start at 1
            if not symtab.is_valid():
                print('Invalid...')
                return

            fpath = symtab.fullname()
            with open(fpath, "r") as f:
                lines = [l.rstrip() for l in f.readlines()]

        except Exception:
            print('Exception in context_source')
            return

        # file_base_name = os.path.basename(symtab.filename)
        # bp_locations = [b.location for b in gdb.breakpoints() if file_base_name in b.location]

        fn = symtab.filename
        # if len(fn) > 20:
        #     fn = "{}[...]{}".format(fn, os.path.splitext(fn)[1])
        title = "source:{}+{}".format(fn, line_num + 1)

        text = blue("[%s]" % title.center(92, "-"))
        msg(text)

        # show_extra_info = self.get_setting("show_source_code_variable_values")

        for i in range(line_num - count + 1, line_num + count):
            if i < 0:
                continue

            bp_prefix = " "
            if i < line_num:
                msg("   {:4d}\t {:s}".format(i + 1, lines[i],))

            if i == line_num:
                prefix = "{}{}{:4d}\t ".format(bp_prefix, '=>', i + 1)
                msg(green("{}{:s}".format(prefix, lines[i])))

            if i > line_num:
                try:
                    msg("{}  {:4d}\t {:s}".format(bp_prefix, i + 1, lines[i],))
                except IndexError:
                    break
        return


    def context(self, *arg):
        """
        Display various information of current execution context
        Usage:
           1. To show some type(s) of context(s):
              MYNAME [reg,code,stack,source] [code/stack length]

           2. Turn on/off contexts when program stop/step.
              MYNAME [on] [reg,code,stack,source]
              MYNAME [off]
        """

        (opt, count) = normalize_argv(arg, 2)

        if opt == 'on' or opt == 'ON':
            cmd = 'peda context'

            # in shit case, count is actually context to be displayed.
            if count is None:
                opt = "register,code,source,stack"
            elif isinstance(count, str):
                opt = count
                cmd += count
            else:
                opt = "register,code,source,stack"
            cmd += '\n'

            peda.append_user_command("hook-stop", cmd)
            print('context turned on')
            pass

        if opt == 'off' or opt == 'OFF':
            peda.save_user_command('hook-stop')
            print('context turned off')
            return

        if to_int(count) is None:
            count = 8
        if opt is None:
            opt = config.Option.get("context")
        if opt == "all":
            opt = "register,code,stack"

        opt = opt.replace(" ", "").split(",")

        if not opt:
            return

        clearscr = config.Option.get("clearscr")
        if clearscr == "on":
            clearscreen()

        status = peda.get_status()
        # display registers
        if "reg" in opt or "register" in opt:
            self.context_register()

        # display assembly code
        if "code" in opt:
            self.context_code(count)

        # display source code
        if "source" in opt or 'src'  in opt:
            self.context_source(count)

        # display stack content, forced in case SIGSEGV
        if "stack" in opt or "SIGSEGV" in status:
            self.context_stack(count)

        msg("[%s]" % ("-"*92), "blue")
        msg("Legend: %s, %s, %s, value" % (red("code"), blue("data"), green("rodata")))

        # display stopped reason
        if "SIG" in status:
            msg("Stopped reason: %s" % red(status))

        return

    #################################
    #   Memory Operation Commands   #
    #################################
    # get_vmmap()
    def vmmap(self, *arg):
        """
        Get virtual mapping address ranges of section(s) in debugged process
        Usage:
            MYNAME [mapname] (e.g binary, all, libc, stack)
            MYNAME address (find mapname contains this address)
            MYNAME (equiv to cat /proc/pid/maps)
        """

        (mapname,) = normalize_argv(arg, 1)
        if not self._is_running():
            maps = peda.get_vmmap()
        elif to_int(mapname) is None:
            maps = peda.get_vmmap(mapname)
        else:
            addr = to_int(mapname)
            maps = []
            allmaps = peda.get_vmmap()
            if allmaps is not None:
                for (start, end, perm, name) in allmaps:
                    if addr >= start and addr < end:
                        maps += [(start, end, perm, name)]

        if maps is not None and len(maps) > 0:
            l = 10 if peda.intsize() == 4 else 18
            msg("%s %s %s\t%s" % ("Start".ljust(l, " "), "End".ljust(l, " "), "Perm", "Name"), "blue", "bold")
            for (start, end, perm, name) in maps:
                color = "red" if "rwx" in perm else None
                msg("%s %s %s\t%s" % (to_address(start).ljust(l, " "), to_address(end).ljust(l, " "), perm, name), color)
        else:
            warning_msg("not found or cannot access procfs")
        return


    # examine_mem_reference()
    def telescope(self, *arg):
        """
        Display memory content at an address with smart dereferences
        Usage:
            MYNAME [linecount] (analyze at current $SP)
            MYNAME address [linecount]
        """

        (address, count) = normalize_argv(arg, 2)

        if self._is_running(False):
            sp = peda.getreg("sp")
        else:
            sp = None

        if count is None:
            count = 8
            if address is None:
                address = sp
            elif address < 0x1000:
                count = address
                address = sp

        if not address:
            return

        step = peda.intsize()
        if not peda.is_address(address): # cannot determine address
            msg("Invalid $SP address: 0x%x" % address, "red")
            return
            for i in range(count):
                if not peda.execute("x/%sx 0x%x" % ("g" if step == 8 else "w", address + i*step)):
                    break
            return

        result = []
        for i in range(count):
            value = address + i*step
            if peda.is_address(value):
                result += [peda.examine_mem_reference(value)]
            else:
                result += [None]
        idx = 0
        text = ""
        for chain in result:
            text += "%04d| " % (idx)
            text += format_reference_chain(chain)
            text += "\n"
            idx += step

        pager(text)

        return

    def eflags(self, *arg):
        """
        Display/set/clear/toggle value of eflags register
        Usage:
            MYNAME
            MYNAME [set|clear|toggle] flagname
        """
        FLAGS = ["CF", "PF", "AF", "ZF", "SF", "TF", "IF", "DF", "OF"]
        FLAGS_TEXT = ["Carry", "Parity", "Adjust", "Zero", "Sign", "Trap",
                        "Interrupt", "Direction", "Overflow"]

        (option, flagname) = normalize_argv(arg, 2)
        if not self._is_running(False):
            return

        elif option and not flagname:
            self._missing_argument()

        elif option is None: # display eflags
            flags = peda.get_eflags()
            text = ""
            for (i, f) in enumerate(FLAGS):
                if flags[f]:
                    text += "%s " % red(FLAGS_TEXT[i].upper(), "bold")
                else:
                    text += "%s " % green(FLAGS_TEXT[i].lower())

            eflags = peda.getreg("eflags")
            msg("%s: 0x%x (%s)" % (green("EFLAGS"), eflags, text.strip()))

        elif option == "set":
            peda.set_eflags(flagname, True)

        elif option == "clear":
            peda.set_eflags(flagname, False)

        elif option == "toggle":
            peda.set_eflags(flagname, None)

        return
    eflags.options = ["set", "clear", "toggle"]

    def xinfo(self, *arg):
        """
        Display detail information of address/registers
        Usage:
            MYNAME address
            MYNAME register [reg1 reg2]
        """

        (address, regname) = normalize_argv(arg, 2)
        if address is None:
            self._missing_argument()

        text = ""

        def get_reg_text(r, v):
            text = green("%s" % r.upper().ljust(3)) + ": "
            chain = peda.examine_mem_reference(v)
            text += format_reference_chain(chain)
            text += "\n"
            return text

        (arch, bits) = peda.getarch()
        if str(address).startswith("r"):
            # Register
            regs = peda.getregs(" ".join(arg[1:]))
            if regname is None:
                for r in REGISTERS[bits]:
                    if r in regs:
                        text += get_reg_text(r, regs[r])
            else:
                for (r, v) in sorted(regs.items()):
                    text += get_reg_text(r, v)
            if text:
                msg(text.strip())
            if regname is None or "eflags" in regname:
                self.eflags()
            return

        elif to_int(address) is None:
            warning_msg("not a register nor an address")
        else:
            # Address
            chain = peda.examine_mem_reference(address, depth=0)
            text += format_reference_chain(chain) + "\n"
            vmrange = peda.get_vmrange(address)
            if vmrange:
                (start, end, perm, name) = vmrange
                text += "Virtual memory mapping:\n"
                text += green("Start : %s\n" % to_address(start))
                text += green("End   : %s\n" % to_address(end))
                text += yellow("Offset: 0x%x\n" % (address-start))
                text += red("Perm  : %s\n" % perm)
                text += blue("Name  : %s" % name)
        msg(text)

        return
    xinfo.options = ["register"]

    def strings(self, *arg):
        """
        Display printable strings in memory
        Usage:
            MYNAME start end [minlen]
            MYNAME mapname [minlen]
            MYNAME (display all printable strings in binary - slow)
        """
        (start, end, minlen) = normalize_argv(arg, 3)

        mapname = None
        if start is None:
            mapname = "binary"
        elif to_int(start) is None or (end < start):
            (mapname, minlen) = normalize_argv(arg, 2)

        if minlen is None:
            minlen = 1

        if mapname:
            maps = peda.get_vmmap(mapname)
        else:
            maps = [(start, end, None, None)]

        if not maps:
            warning_msg("failed to get memory map for %s" % mapname)
            return

        text = ""
        regex_pattern = "[%s]{%d,}" % (re.escape(string.printable), minlen)
        p = re.compile(regex_pattern.encode('utf-8'))
        for (start, end, _, _) in maps:
            mem = peda.dumpmem(start, end)
            if not mem: continue
            found = p.finditer(mem)
            if not found: continue

            for m in found:
                text += "0x%x: %s\n" % (start+m.start(), string_repr(mem[m.start():m.end()].strip(), show_quotes=False))

        pager(text)
        return


    ###############################
    #   Exploit Helper Commands   #
    ###############################
    # elfheader()
    def elfheader(self, *arg):
        """
        Get headers information from debugged ELF file
        Usage:
            MYNAME [header_name]
        """

        (name,) = normalize_argv(arg, 1)
        result = peda.elfheader(name)
        if len(result) == 0:
            warning_msg("%s not found, did you specify the FILE to debug?" % (name if name else "headers"))
        elif len(result) == 1:
            (k, (start, end, type)) = list(result.items())[0]
            msg("%s: 0x%x - 0x%x (%s)" % (k, start, end, type))
        else:
            for (k, (start, end, type)) in sorted(result.items(), key=lambda x: x[1]):
                msg("%s = 0x%x" % (k, start))
        return

    # readelf_header(), elfheader_solib()
    def readelf(self, *arg):
        """
        Get headers information from an ELF file
        Usage:
            MYNAME mapname [header_name]
            MYNAME filename [header_name]
        """

        (filename, hname) = normalize_argv(arg, 2)
        result = {}
        maps = peda.get_vmmap()
        if filename is None: # fallback to elfheader()
            result = peda.elfheader()
        else:
            result = peda.elfheader_solib(filename, hname)

        if not result:
            result = peda.readelf_header(filename, hname)
        if len(result) == 0:
            warning_msg("%s or %s not found" % (filename, hname))
        elif len(result) == 1:
            (k, (start, end, type)) = list(result.items())[0]
            msg("%s: 0x%x - 0x%x (%s)" % (k, start, end, type))
        else:
            for (k, (start, end, type)) in sorted(result.items(), key=lambda x: x[1]):
                msg("%s = 0x%x" % (k, start))
        return


    def crashdump(self, *arg):
        """
        Display crashdump info and save to file
        Usage:
            MYNAME [reason_text]
        """
        (reason,) = normalize_argv(arg, 1)
        if not reason:
            reason = "Interactive dump"

        logname = peda.get_config_filename("crashlog")
        logfd = open(logname, "a")
        config.Option.set("_teefd", logfd)
        msg("[%s]" % "START OF CRASH DUMP".center(92, "-"))
        msg("Timestamp: %s" % time.ctime())
        msg("Reason: %s" % red(reason))

        # exploitability
        pc = peda.getreg("pc")
        if not peda.is_address(pc):
            exp = red("EXPLOITABLE")
        else:
            exp = "Unknown"
        msg("Exploitability: %s" % exp)

        # registers, code, stack
        self.context_register()
        self.context_code(16)
        self.context_stack()

        # backtrace
        msg("[%s]" % "backtrace (innermost 10 frames)".center(92, "-"), "blue")
        msg(peda.execute_redirect("backtrace 10"))

        msg("[%s]\n" % "END OF CRASH DUMP".center(92, "-"))
        config.Option.set("_teefd", "")
        logfd.close()

        return

    def utils(self, *arg):
        """
        Miscelaneous utilities from utils module
        Usage:
            MYNAME command arg
        """
        (command, carg) = normalize_argv(arg, 2)
        cmds = ["int2hexstr", "list2hexstr", "str2intlist"]
        if not command or command not in cmds or not carg:
            self._missing_argument()

        func = globals()[command]
        if command == "int2hexstr":
            if to_int(carg) is None:
                msg("Not a number")
                return
            result = func(to_int(carg))
            result = to_hexstr(result)

        if command == "list2hexstr":
            if to_int(carg) is not None:
                msg("Not a list")
                return
            result = func(eval("%s" % carg))
            result = to_hexstr(result)

        if command == "str2intlist":
            res = func(carg)
            result = "["
            for v in res:
                result += "%s, " % to_hex(v)
            result = result.rstrip(", ") + "]"

        msg(result)
        return
    utils.options = ["int2hexstr", "list2hexstr", "str2intlist"]


    ### my extentions..
    def follow_fork(self, *args):
        """
        Change follow-fork-mode.
        Usage:
            MYNAME parent/child
        """
        (opt,) = normalize_argv(args, 1)

        if opt == 'parent' or opt == 'child':
            peda.execute("set follow-fork-mode " + opt)
            peda.execute("show follow-fork-mode ")
        else:
            print('Usage: %s'%pedacmd._get_helptext())
        pass


    def __print_threads(self, threads, active=-1):
        current_thread = gdb.selected_thread ()

        # Parse tid from gbd outputs, since tid of python thread is not set
        text = peda.execute_redirect('info thr')
        tids = {}
        p = re.compile(r'Thread\s(0x[0-9a-f]+)\s+\(LWP\s+([0-9]+)\)')
        for line in text.split('\n'):
            m = p.search(line)
            if m:
                tids[to_int(m.group(2))] = m.group(1)

        msg('  Id   Target Id                                    Frame')
        for thread in sorted(threads, key=lambda x: x.num):
            thread.switch()
            (_, lwpid, tid) = thread.ptid
            num = thread.num
            name = thread.name if thread.name else "unavailable"
            padding = '    '
            if tid == 0:
                tid = tids.get(lwpid, 0)

            pad_num = 25 - len(name)
            if pad_num > 0:
                padding = ' ' * pad_num


            func = gdb.newest_frame().name()
            critical = True if func in ['raise', 'abort'] else False

            text = '{} {:<2}  Thread {} LWP ({} - {:#x})  "{}"{}{} ()'.format(
                '*' if num == active else ' ',
                num, tid, lwpid, lwpid, name,
                padding, func)

            if critical:
                text = red(text)

            msg(text)

        current_thread.switch()
        return


    def pthread(self, *arg):
        """
        Print thread-information.
        Usage:
            MYNAME --- show current thread info
            MYNAME num --- switch to specified thread
            MYNAME unique [limit] --- Show unique thread backtraces.
                If limit is specified, show at most limit frames.
            MYNAME find expr --- find thread in which expr evaluates to true.
                EG: thread find (value == 1), or simply:
                    thread find value == 1
            MYNAME find name --- Find threads that match a regular expression.
            MYNAME apply ---  Find threads that match a regular expression.
        """
        args=list(arg)
        debug_msg('thread args: {}'.format(args))

        if len(args) == 0:
            return peda.execute('thread')

        opt = args[0]
        args.pop(0)

        if to_int(opt) is not None:
            if len(args) > 0:
                error_msg('wrong argument, type "help thread" for usage.')
                return
            return peda.execute('thread {}'.format(opt))

        if opt in gen_strings('unique'):
            limit = 1024

            if len(args) > 0:
                if args[0] == 'full':
                    limit = sys.maxsize
                elif to_int(args[0]) is not None:
                    limit = to_int(args[0])

            return uniquify_stacks_gdb(limit)

        if opt in gen_strings('info'):
            return self.__print_threads(gdb.inferiors()[0].threads(), gdb.selected_thread ().num)


        # following actions require args to be not None
        if len(args) == 0:
            error_msg("FIND requires expr...")
            return

        expr = ' '.join(args)
        if opt in gen_strings('apply'):
            return peda.execute('thread apply {}'.format(expr))

        if opt in gen_strings('find'):
            if len(args) == 1:
                # find name, forward to gdb directly
                return peda.execute('thread find {}'.format(args[0]))

            ## show thread number where expr evaluates to true.
            valid_threads = []

            # Save and restore current thread...
            current_thread = gdb.selected_thread ()

            for thread in gdb.inferiors()[0].threads():
                thread.switch()

                # evaluate expr, save to valid_threads if expr yields true.
                if peda.evaluate_expr(expr):
                    valid_threads.append(thread)

            current_thread.switch()

            if valid_threads:
                msg('Total {} thread(s) match(s) condition: {}\n'.format(
                    len(valid_threads), expr))
                self.__print_threads(valid_threads, current_thread.num)
            else:
                warning_msg('no match found.')
            return

        error_msg('wrong argument, type "phelp thread" for usage.')
        return


    def pprint(self, *arg):
        """
        Enhancement of GDB's print command.
        It combines GDB's print result with result of private functions.
        Usage:
            MYNAME expression
        """
        result = peda.print_internal(*arg)
        if result is not None:
            msg(result)

        return

    def load_stl_views(self, *arg):
        """
        Load stl viewes.
        Usage:
            MYNAME [path]
        """
        file =  os.path.join(os.path.dirname(PEDAFILE), "stl-views-1.0.3.gdb")

        peda.execute('source ' + file)

        msg('file %s loaded' % file)
        return


    def load_stl_printer(self, *arg):
        """
        Load stl printer (shipped with gcc) from given path.
        Usage:
            MYNAME [path]
        """
        (path,) = normalize_argv(arg, 1)

        if path is None:
            # TODO: parse path from gdb executable..
            path = None

            if path is None:
                error_msg("Failed to parse gdb executable path.")
                return

        if not os.path.exists(path):
            error_msg("path %s does not exist.")
            return

        sys.path.insert(0, path)

        try:
            from libstdcxx.v6.printers import register_libstdcxx_printers
            register_libstdcxx_printers (None)
        except Exception as e:
            output = str(e)
            if 'pretty-printer already registered: libstdc++-v6' in output:
                pass
            else:
                error_msg("failed to load register stl printers from path: %s --%s" %
                          (path, output))
        return


    def addr2line(self, *arg):
        """
        Simple wrapper of info addr, to show line information of given line
        Usage:
            MYNAME addr
            MYNAME *addr
        """
        args = list(arg)
        if len(args) > 2:
            error_msg('wrong argument, type "phelp addr2line" for usage.')
            return

        addr=''.join(args)
        if not addr.startswith('*'):
            addr = '*' + addr

        peda.execute('info line ' + addr)
        return


    def update(self):
        """
        Update this extension, by fetching changes from upstream.
        Usage:
           MYNAME
        """
        directory = os.path.dirname(PEDAFILE)

        print('Running git pull in directory: %s ...'%(green(directory)))
        p = subprocess.Popen(["bash", "-c", "cd %s && git pull" % directory],
                             stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        output, errors = p.communicate()
        if p.returncode != 0:
            print('Update failed: %s'%(errors.decode()))
        else:
            print('PEDA updated...')
        pass

    def mp_mode(self, *arg):
        """
        Turn on Multi-Process mode, easier to debug multiple processes.
        TODO: add argument to turn it on/off...
        Usage:
           MYNAME [on/off]
        """
        peda.execute("set detach-on-fork off")
        peda.execute("set schedule-multiple on")
        peda.execute("set follow-fork-mode parent")
        pass

    def raise_continue(self):
        """Continue from raise.
        """
        peda.execute('handle SIGSTOP nostop noprint nopass')
        peda.execute('handle set scheduler-locking on')
        peda.execute('finish')
        pass

    def offsets(self, *arg):
        """
        Show offsets of member fields.
        Usage:
            MYNAME STYPE
        """
        (name,) = normalize_argv(arg, 1)
        stype = gdb.lookup_type('struct %s' % name)

        out = '%s {\n' % name
        for field in stype.fields():
            out += '    [0x%x] %s\n' % (field.bitpos//8, field.name)
        out += '}'
        msg(out)


    def framecount(self, *arg):
        """
        Count total frames.
        Usage:
           MYNAME
        """
        f = gdb.newest_frame()
        count = 0
        while f:
          count += 1
          f = f.older()
        print(count, "frames")


    def bt(self, *arg):
        """
        Print backtraces. If you want to call original backtrace command
        shipped with gdb, use 'backtrace' command, not bt.

        Usage:
            MYNAME [limit] [full]

        If limit is specified (default is 1024), print at most LIMIT
        frames from TOP. Limit can also be negative, which means print frames
        from BOTTOM. If limit is 0, limit will be set to sys.maxsize.

        If full is not specified, file/lines are ignored.
        """
        args = list(arg)

        abbr = True
        limit = 1024

        for var in gen_strings('full'):
            if var in args:
                abbr = False
                args.remove(var)

        if len(args) > 1:
            error_msg('wrong argument, type "phelp bt" for usage.')
            return

        if len(args) == 1:
            val = to_int(args[0])
            if val is None:
                error_msg('wrong argument, type "phelp bt" for usage.')
                return

            limit = val
            if limit == 0:
                limit = sys.maxsize

        if abbr:
            try:
                st = StackTrace(gdb.selected_thread(), limit, False,  gdb.selected_frame().pc())
                print('%s'%(st))
            except Exception as e:
                print('%s'%(e))
        else:
            gdb.execute('where {}'.format(limit))
        pass


    def chr(self, *arg):
        """Print chr(x)
        Usage: chr NUM
        """
        (num,) = normalize_argv(arg, 1)

        if num is None or not isinstance(num, int):
            error_msg("wrong argument")
            return

        msg(chr(num))
        pass


    def ord(self, *arg):
        """Print ord(x)
        Usage: chr CHAR
        """
        (num,) = normalize_argv(arg, 1)

        if num is None or not isinstance(num, str) or len(num) > 1:
            error_msg("wrong argument")
            return

        msg(ord(num[0]))
        pass


###########################################################################
class pedaGDBCommand(gdb.Command):
    """
    Wrapper of gdb.Command for master "peda" command
    """
    def __init__(self, cmdname="peda"):
        self.cmdname = cmdname
        self.__doc__ = pedacmd._get_helptext()
        super(pedaGDBCommand, self).__init__(self.cmdname, gdb.COMMAND_DATA)

    def invoke(self, arg_string, from_tty):
        # do not repeat command
        self.dont_repeat()
        arg = peda.string_to_argv(arg_string)
        if len(arg) < 1:
            pedacmd.help()
        else:
            cmd = arg[0]
            if cmd in pedacmd.commands:
                func = getattr(pedacmd, cmd)
                try:
                    # reset memoized cache
                    reset_cache(sys.modules['__main__'])
                    func(*arg[1:])
                except Exception as e:
                    if config.Option.get("debug") == "on":
                        msg("Exception: %s" %e)
                        traceback.print_exc()
                    peda.restore_user_command("all")
                    pedacmd.help(cmd)
            else:
                msg("Undefined command: %s. Try \"peda help\"" % cmd)
        return

    def complete(self, text, word):
        completion = []
        if text != "":
            cmd = text.split()[0]
            if cmd in pedacmd.commands:
                func = getattr(pedacmd, cmd)
                for opt in func.options:
                    if word in opt:
                        completion += [opt]
            else:
                for cmd in pedacmd.commands:
                    if cmd.startswith(text.strip()):
                        completion += [cmd]
        else:
            for cmd in pedacmd.commands:
                if word in cmd and cmd not in completion:
                    completion += [cmd]
        return completion


###########################################################################
class Alias(gdb.Command):
    """
    Generic alias, create short command names
    This doc should be changed dynamically
    """
    def __init__(self, alias, command, shorttext=1):
        (cmd, opt) = (command + " ").split(" ", 1)
        if cmd == "peda" or cmd == "pead":
            cmd = opt.split(" ")[0]
        if not shorttext:
            self.__doc__ = pedacmd._get_helptext(cmd)
        else:
            self.__doc__ = green("Alias for '%s'" % command)
        self._command = command
        self._alias = alias
        global PEDA_ALIAS

        if alias != 'pead':
            PEDA_ALIAS.append((alias, self.__doc__))
        super(Alias, self).__init__(alias, gdb.COMMAND_NONE)

    def invoke(self, args, from_tty):
        self.dont_repeat()
        gdb.execute("%s %s" %(self._command, args))

    def complete(self, text, word):
        completion = []
        cmd = self._command.split("peda ")[1]
        for opt in getattr(pedacmd, cmd).options: # list of command's options
            if text in opt and opt not in completion:
                completion += [opt]
        if completion != []:
            return completion
        if cmd in ["set", "show"] and text.split()[0] in ["option"]:
            opname = [x for x in config.OPTIONS.keys() if x.startswith(word.strip())]
            if opname != []:
                completion = opname
            else:
                completion = list(config.OPTIONS.keys())
        return completion


###########################################################################
## INITIALIZATION ##
# global instances of PEDA() and PEDACmd()
peda = PEDA()
pedacmd = PEDACmd()
pedacmd.help.__func__.options = pedacmd.commands # XXX HACK

# register "peda" command in gdb
pedaGDBCommand()
Alias("pead", "peda") # just for auto correction

# create aliases for subcommands
for cmd in pedacmd.commands:
    func = getattr(pedacmd, cmd)
    func.__func__.__doc__ = func.__doc__.replace("MYNAME", cmd)
    if cmd not in ["help", "show", "set"]:
        Alias(cmd, "peda %s" % cmd, 0)

# common used shell commands aliases
shellcmds = ["man", "ls", "ps", "grep", "cat", "more", "less", "pkill",
             "clear", "vi", "nano", "who", "rg"]
for cmd in shellcmds:
        Alias(cmd, "shell %s" % cmd)

# custom command aliases, add any alias you want
Alias("phelp", "peda help")
Alias("pset", "peda set")
Alias("pshow", "peda show")

Alias("stack", "peda telescope $sp")
Alias("viewmem", "peda telescope")
Alias("reg", "peda xinfo register")

Alias("ctx", "peda context")

Alias("lockon", "set scheduler-locking on")
Alias("lockoff", "set scheduler-locking off")

Alias("noraise", "handle SIGSTOP nostop noprint nopass")
Alias("doraise", "handle SIGSTOP stop print pass")

Alias("ffp", "follow_fork parent")
Alias("ffc", "follow_fork child")

Alias("p", "pprint")
Alias("hd", "hexdump")
Alias("rc", "raise_continue")
Alias("disas", "pdisass")
Alias("thr", "peda pthread")
Alias("t", "peda pthread")

# misc gdb settings
peda.execute("set confirm off")
peda.execute("set verbose off")
peda.execute("set output-radix 0x10")
peda.execute("set prompt (peda)$ \001") # custom prompt
peda.execute("set height 0") # disable paging
peda.execute('set pagination off') # disable paging
peda.execute("set history expansion on")
peda.execute("set history save on") # enable history saving
peda.execute("set disassembly-flavor intel")
peda.execute("set backtrace past-main on")
peda.execute("set step-mode on")

peda.execute("set print pretty on")
peda.execute('set print elements 0')

peda.execute("set breakpoint pending on")

peda.execute("handle SIGALRM print nopass") # ignore SIGALRM
peda.execute("handle SIGSEGV stop print nopass") # catch SIGSEGV
peda.execute("handle SIGUSR2 nostop print pass") # ignore SIGUSR2
peda.execute("handle SIGUSR1 nostop")
peda.execute("handle SIGPIPE nostop")

peda.execute("set auto-load safe-path /")

# Frequently used breakpoints.
for function in ['__asan::ReportGenericError', 'exit', '_exit']:
    peda.execute('b ' + function)

if sys.platform.startswith('darwin'):
    peda.execute("set set startup-with-shell off")


# load private prints when exists.
if os.path.exists(PRIVATE_PRINT_FILE):
    for line in open(PRIVATE_PRINT_FILE, "r").readlines():
        if '----' not in line:
            continue
        try:
            (k,v) = line.strip().split(' ---- ')
        except Exception as e:
            pass
        else:
            PRIVATE_PRINTS[k.strip()] = v.strip()
        pass
    pass

# load private settings, if exists
PRIVATE_INIT_FILE = os.path.join(os.getenv('HOME'), '.gdbinit_priv')
if os.path.exists(PRIVATE_INIT_FILE):
    peda.execute('source ' + PRIVATE_INIT_FILE)
    pass
