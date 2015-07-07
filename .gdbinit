handle SIGUSR1 nostop
handle SIGPIPE nostop

set pagination off
set breakpoint pending on

define bta
  thread apply all bt
end

set logging off
set logging overwrite on
set print pretty
set print elements 0

define lockon
  set scheduler-locking on
end

define lockoff
  set scheduler-locking off
end


## Frequently used breakpoints.
b __asan::ReportGenericError
b exit

set startup-with-shell off

set auto-solib-add off
set auto-load safe-path /

python
import os
import sys
if os.path.exists('/opt/rh/devtoolset-8/root/usr/share/gdb/python/'):
    sys.path.insert(0, '/opt/rh/devtoolset-8/root/usr/share/gdb/python/')
    from libstdcxx.v6.printers import register_libstdcxx_printers
    register_libstdcxx_printers (None)
end

define noraise
  handle SIGSTOP nostop
end

# To use peda, clone peda from github manually:
# git clone https://github.com/longld/peda.git ~/Work/peda
define load_peda
  shell if test -f ~/Work/peda/peda.py; then echo source ~/Work/peda/peda.py; fi > /tmp/gdb_peda
  source /tmp/gdb_peda
end
