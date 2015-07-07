#!/bin/bash
#
# Author: Yang,Ying-chao <yangyingchao@g-data.com>, 2017-03-15
#
__assert_sigpipe_ok() {
    # When extracting a tar file like this:
    #
    #     bzip2 -dc foo.tar.bz2 | tar xof -
    #
    # For some tar files (see bug #309001), tar will
    # close its stdin pipe when the decompressor still has
    # remaining data to be written to its stdout pipe. This
    # causes the decompressor to be killed by SIGPIPE. In
    # this case, we want to ignore pipe writers killed by
    # SIGPIPE, and trust the exit status of tar. We refer
    # to the bash manual section "3.7.5 Exit Status"
    # which says, "When a command terminates on a fatal
    # signal whose number is N, Bash uses the value 128+N
    # as the exit status."

    local x pipestatus=${PIPESTATUS[*]}
    for x in $pipestatus ; do
        # Allow SIGPIPE through (128 + 13)
        if [[ $x -ne 0 && $x -ne ${PORTAGE_SIGPIPE_STATUS:-141} ]]
        then
            echo "$@"
            return 1
        fi
    done

    # Require normal success for the last process (tar).
    if [[ $x -ne 0 ]]; then
        echo "$@"
        return 1
    fi
}

unpack() {
    local x
    local y y_insensitive
    local suffix suffix_insensitive
    local myfail
    local eapi=${EAPI:-0}
    [ -z "$*" ] && echo "Nothing passed to the 'unpack' command" && return 1

    for x in "$@"; do
        echo ">>> Unpacking ${x} to ${PWD}"
        suffix=${x##*.}
        suffix_insensitive=$(LC_ALL=C tr "[:upper:]" "[:lower:]" <<< "${suffix}")
        y=${x%.*}
        y=${y##*.}
        y_insensitive=$(LC_ALL=C tr "[:upper:]" "[:lower:]" <<< "${y}")

        if [[ ! -s ${x} ]]; then
            echo "unpack: ${x} does not exist"
            return 1
        fi

        __unpack_tar() {
            $* -c -- $x | tar xovf -
            __assert_sigpipe_ok "$myfail" || return 1
        }

        myfail="unpack: failure unpacking ${x}"
        case "${suffix_insensitive}" in
            tar)
                if ! tar xof "$x"; then
                    echo "$myfail"
                    return 1
                fi
                ;;
            tgz)
                if ! tar xozf "$x"; then
                    echo "$myfail"
                    return 1
                fi
                ;;
            tbz|tbz2)
                __unpack_tar bzip2 -d | return 1
                __assert_sigpipe_ok "$myfail" || return 1
                ;;
            zip|jar)
                # unzip will interactively prompt under some error conditions,
                # as reported in bug #336285
                if ! unzip -qo "${x}"; then
                    echo "$myfail"
                    return 1
                fi < <(set +x ; while true ; do echo n || break ; done)
                ;;
            gz|z)
                __unpack_tar gzip -d || return 1
                ;;
            bz2|bz)
                __unpack_tar bzip2 -d || return 1
                ;;
            7z)
                local my_output
                my_output="$(7z x -y "${x}")"
                if [ $? -ne 0 ]; then
                    echo "${my_output}" >&2
                    die "$myfail"
                fi
                ;;
            rar)
                if ! unrar x -idq -o+ "${x}"; then
                    echo "$myfail"
                    return 1
                fi
                ;;
            lha|lzh)
                if ! lha xfq "${x}"; then
                    echo "$myfail"
                    return 1
                fi
                ;;
            a)
                if ! ar x "${x}"; then
                    echo "$myfail"
                    return 1
                fi
                ;;
            deb)
                # Unpacking .deb archives can not always be done with
                # `ar`.  For instance on AIX this doesn't work out.
                # If `ar` is not the GNU binutils version and we have
                # `deb2targz` installed, prefer it over `ar` for that
                # reason.  We just make sure on AIX `deb2targz` is
                # installed.
                if [[ $(ar --version 2>/dev/null) != "GNU ar"* ]] && \
                       type -P deb2targz > /dev/null; then
                    y=${x##*/}
                    local created_symlink=0
                    if [ ! "$x" -ef "$y" ] ; then
                        # deb2targz always extracts into the same directory as
                        # the source file, so create a symlink in the current
                        # working directory if necessary.
                        if ! ln -sf "$x" "$y"; then
                            echo "$myfail"
                            return 1
                        fi
                        created_symlink=1
                    fi
                    if ! deb2targz "$y"; then
                        echo "$myfail"
                        return 1
                    fi
                    if [ $created_symlink = 1 ] ; then
                        # Clean up the symlink so the ebuild
                        # doesn't inadvertently install it.
                        rm -f "$y"
                    fi
                    if ! mv -f "${y%.deb}".tar.gz data.tar.gz; then
                        if ! mv -f "${y%.deb}".tar.xz data.tar.xz; then
                            echo "$myfail"
                            return 1
                        fi
                    fi
                else
                    if ! ar x "$x"; then
                        echo "$myfail"
                        return 1
                    fi
                fi
                ;;
            lzma)
                __unpack_tar lzma -d || return 1
                ;;
            xz)
                __unpack_tar xz  -d || return 1
                ;;
            txz)
                __unpack_tar xz -d || return 1
                ;;
            *)
                echo "unpack ${x}: file format not recognized. Ignoring."
                ;;
        esac
    done
}

unpack $*
