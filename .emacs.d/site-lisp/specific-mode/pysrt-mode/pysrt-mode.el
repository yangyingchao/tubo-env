;;; pysrt-mode.el --- Major mode for editing Python programs

;; INSTALLATION:
; Put this file into the emacs load-path, and add following linese into your
; emacs init file:
;
; (autoload 'pysrt-mode "pysrt-mode" "Pysrt-script editing mode." t)
; (add-to-list 'auto-mode-alist '("\\.srt$" . pysrt-mode))

(defconst pysrt-version "1.0.0"
  "`pysrt-mode' version number.")

;;; Code:

(require 'comint)
(require 'custom)
(require 'cl)
(require 'compile)
(require 'ansi-color)


;; user definable variables
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
(defgroup srt nil
  "Major mode for editing text files in Srt format."
  :prefix "pysrt-"
  :group 'wp)

(defcustom pysrt-tab-always-indent t
  "*Non-nil means TAB in Srt mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used."
  :type 'boolean
  :group 'srt)

(defcustom pysrt-pysrt-command "srt"
  "*Shell command used to start Srt interpreter."
  :type 'string
  :group 'srt)

(make-obsolete-variable 'pysrt-jpysrt-command 'pysrt-jython-command)
(defcustom pysrt-jython-command "jython"
  "*Shell command used to start the Jython interpreter."
  :type 'string
  :group 'srt
  :tag "Jython Command")

(defcustom pysrt-enable-math nil
  "Syntax highlighting for inline LaTeX expressions.
This will not take effect until Emacs is restarted."
  :group 'srt
  :type 'boolean)

(defcustom pysrt-default-interpreter 'csrt
  "*Which Srt interpreter is used by default.
The value for this variable can be either `csrt' or `jython'.

When the value is `csrt', the variables `py-pysrt-command' and
`py-pysrt-command-args' are consulted to determine the interpreter
and arguments to use.

When the value is `jython', the variables `py-jython-command' and
`py-jython-command-args' are consulted to determine the interpreter
and arguments to use.

Note that this variable is consulted only the first time that a Srt
mode buffer is visited during an Emacs session.  After that, use
\\[py-toggle-shells] to change the interpreter shell."
  :type '(choice (const :tag "Srt (a.k.a. CSrt)" csrt)
                 (const :tag "Jython" jython))
  :group 'srt)

(defcustom pysrt-pysrt-command-args '("-i")
  "*List of string arguments to be used when starting a Srt shell."
  :type '(repeat string)
  :group 'srt)

(make-obsolete-variable 'pysrt-jpysrt-command-args 'pysrt-jython-command-args)
(defcustom pysrt-jython-command-args '("-i")
  "*List of string arguments to be used when starting a Jython shell."
  :type '(repeat string)
  :group 'srt
  :tag "Jython Command Args")

(defcustom pysrt-indent-offset 4
  "*Amount of offset per level of indentation.
`\\[py-guess-indent-offset]' can usually guess a good value when
you're editing someone else's Srt code."
  :type 'integer
  :group 'srt)

(defcustom pysrt-continuation-offset 4
  "*Additional amount of offset to give for some continuation lines.
Continuation lines are those that immediately follow a backslash
terminated line.  Only those continuation lines for a block opening
statement are given this extra offset."
  :type 'integer
  :group 'srt)

(defcustom pysrt-smart-indentation t
  "*Should `pysrt-mode' try to automagically set some indentation variables?
When this variable is non-nil, two things happen when a buffer is set
to `pysrt-mode':

    1. `py-indent-offset' is guessed from existing code in the buffer.
       Only guessed values between 2 and 8 are considered.  If a valid
       guess can't be made (perhaps because you are visiting a new
       file), then the value in `py-indent-offset' is used.

    2. `indent-tabs-mode' is turned off if `py-indent-offset' does not
       equal `tab-width' (`indent-tabs-mode' is never turned on by
       Srt mode).  This means that for newly written code, tabs are
       only inserted in indentation if one tab is one indentation
       level, otherwise only spaces are used.

Note that both these settings occur *after* `pysrt-mode-hook' is run,
so if you want to defeat the automagic configuration, you must also
set `py-smart-indentation' to nil in your `pysrt-mode-hook'."
  :type 'boolean
  :group 'srt)

(defcustom pysrt-align-multiline-strings-p t
  "*Flag describing how multi-line triple quoted strings are aligned.
When this flag is non-nil, continuation lines are lined up under the
preceding line's indentation.  When this flag is nil, continuation
lines are aligned to column zero."
  :type '(choice (const :tag "Align under preceding line" t)
                 (const :tag "Align to column zero" nil))
  :group 'srt)

(defcustom pysrt-block-comment-prefix "//"
  "*String used by \\[comment-region] to comment out a block of code.
This should follow the convention for non-indenting comment lines so
that the indentation commands won't get confused (i.e., the string
should be of the form `#x...' where `x' is not a blank or a tab, and
`...' is arbitrary).  However, this string should not end in whitespace."
  :type 'string
  :group 'srt)

(defcustom pysrt-honor-comment-indentation t
  "*Controls how comment lines influence subsequent indentation.

When nil, all comment lines are skipped for indentation purposes, and
if possible, a faster algorithm is used (i.e. X/Emacs 19 and beyond).

When t, lines that begin with a single `#' are a hint to subsequent
line indentation.  If the previous line is such a comment line (as
opposed to one that starts with `py-block-comment-prefix'), then its
indentation is used as a hint for this line's indentation.  Lines that
begin with `py-block-comment-prefix' are ignored for indentation
purposes.

When not nil or t, comment lines that begin with a single `#' are used
as indentation hints, unless the comment character is in column zero."
  :type '(choice
          (const :tag "Skip all comment lines (fast)" nil)
          (const :tag "Single # `sets' indentation for next line" t)
          (const :tag "Single # `sets' indentation except at column zero"
                 other)
          )
  :group 'srt)

(defcustom pysrt-temp-directory
  (let ((ok '(lambda (x)
               (and x
                    (setq x (expand-file-name x)) ; always true
                    (file-directory-p x)
                    (file-writable-p x)
                    x))))
    (or (funcall ok (getenv "TMPDIR"))
        (funcall ok "/usr/tmp")
        (funcall ok "/tmp")
        (funcall ok "/var/tmp")
        (funcall ok  ".")
        (error
         "Couldn't find a usable temp directory -- set `py-temp-directory'")))
  "*Directory used for temporary files created by a *Srt* process.
By default, the first directory from this list that exists and that you
can write into: the value (if any) of the environment variable TMPDIR,
/usr/tmp, /tmp, /var/tmp, or the current directory."
  :type 'string
  :group 'srt)

(defcustom pysrt-beep-if-tab-change t
  "*Ring the bell if `tab-width' is changed.
If a comment of the form

  \t# vi:set tabsize=<number>:

is found before the first code line when the file is entered, and the
current value of (the general Emacs variable) `tab-width' does not
equal <number>, `tab-width' is set to <number>, a message saying so is
displayed in the echo area, and if `py-beep-if-tab-change' is non-nil
the Emacs bell is also rung as a warning."
  :type 'boolean
  :group 'srt)

(defcustom pysrt-jump-on-exception t
  "*Jump to innermost exception frame in *Srt Output* buffer.
When this variable is non-nil and an exception occurs when running
Srt code synchronously in a subprocess, jump immediately to the
source code of the innermost traceback frame."
  :type 'boolean
  :group 'srt)

(defcustom pysrt-ask-about-save t
  "If not nil, ask about which buffers to save before executing some code.
Otherwise, all modified buffers are saved without asking."
  :type 'boolean
  :group 'srt)

(defcustom pysrt-backspace-function 'backward-delete-char-untabify
  "*Function called by `py-electric-backspace' when deleting backwards."
  :type 'function
  :group 'srt)

(defcustom pysrt-delete-function 'delete-char
  "*Function called by `py-electric-delete' when deleting forwards."
  :type 'function
  :group 'srt)

(defcustom pysrt-imenu-show-method-args-p nil
  "*Controls echoing of arguments of functions & methods in the Imenu buffer.
When non-nil, arguments are printed."
  :type 'boolean
  :group 'srt)
(make-variable-buffer-local 'pysrt-indent-offset)

(defcustom pysrt-pdbtrack-do-tracking-p t
  "*Controls whether the pdbtrack feature is enabled or not.
When non-nil, pdbtrack is enabled in all comint-based buffers,
e.g. shell buffers and the *Srt* buffer.  When using pdb to debug a
Srt program, pdbtrack notices the pdb prompt and displays the
source file and line that the program is stopped at, much the same way
as gud-mode does for debugging C programs with gdb."
  :type 'boolean
  :group 'srt)
(make-variable-buffer-local 'pysrt-pdbtrack-do-tracking-p)

(defcustom pysrt-pdbtrack-minor-mode-string " PDB"
  "*String to use in the minor mode list when pdbtrack is enabled."
  :type 'string
  :group 'srt)

(defcustom pysrt-import-check-point-max
  20000
  "Maximum number of characters to search for a Java-ish import statement.
When `pysrt-mode' tries to calculate the shell to use (either a
CSrt or a Jython shell), it looks at the so-called `shebang' line
-- i.e. #! line.  If that's not available, it looks at some of the
file heading imports to see if they look Java-like."
  :type 'integer
  :group 'srt
  )

(make-obsolete-variable 'pysrt-jpysrt-packages 'pysrt-jython-packages)
(defcustom pysrt-jython-packages
  '("java" "javax" "org" "com")
  "Imported packages that imply `jython-mode'."
  :type '(repeat string)
  :group 'srt)

;; Not customizable
(defvar pysrt-master-file nil
  "If non-nil, execute the named file instead of the buffer's file.
The intent is to allow you to set this variable in the file's local
variable section, e.g.:

    # Local Variables:
    # pysrt-master-file: \"master.py\"
    # End:

so that typing \\[py-execute-buffer] in that buffer executes the named
master file instead of the buffer's file.  If the file name has a
relative path, the value of variable `default-directory' for the
buffer is prepended to come up with a file name.")
(make-variable-buffer-local 'pysrt-master-file)

(defcustom pysrt-pychecker-command "pychecker"
  "*Shell command used to run Pychecker."
  :type 'string
  :group 'srt
  :tag "Pychecker Command")

(defcustom pysrt-pychecker-command-args '("--stdlib")
  "*List of string arguments to be passed to pychecker."
  :type '(repeat string)
  :group 'srt
  :tag "Pychecker Command Args")

(defvar pysrt-shell-alist
  '(("jython" . 'jython)
    ("srt" . 'csrt))
  "*Alist of interpreters and srt shells. Used by `py-choose-shell'
to select the appropriate srt interpreter mode for a file.")

(defcustom fname "Nil"
  "*Current file name."
  :type 'string
  :group 'srt
  :tag "File Name")

(defcustom pysrt-shell-input-prompt-1-regexp "^>>> "
  "*A regular expression to match the input prompt of the shell."
  :type 'string
  :group 'srt)

(defcustom pysrt-shell-input-prompt-2-regexp "^[.][.][.] "
  "*A regular expression to match the input prompt of the shell after the
  first line of input."
  :type 'string
  :group 'srt)

(defcustom pysrt-shell-switch-buffers-on-execute t
  "*Controls switching to the Srt buffer where commands are
  executed.  When non-nil the buffer switches to the Srt buffer, if
  not no switching occurs."
  :type 'boolean
  :group 'srt)




;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; NO USER DEFINABLE VARIABLES BEYOND THIS POINT

(defvar pysrt-line-number-offset 0
  "When an exception occurs as a result of pysrt-execute-region, a
subsequent pysrt-up-exception needs the line number where the region
started, in order to jump to the correct file line.  This variable is
set in pysrt-execute-region and used in pysrt-jump-to-exception.")

(defconst pysrt-emacs-features
  (let (features)
	features)
  "A list of features extant in the Emacs you are using.
There are many flavors of Emacs out there, with different levels of
support for features needed by `pysrt-mode'.")

;; Face for None, True, False, self, and Ellipsis
(defvar pysrt-pseudo-keyword-face 'pysrt-pseudo-keyword-face
  "Face for pseudo keywords in Srt mode, like self, True, False, Ellipsis.")
(make-face 'pysrt-pseudo-keyword-face)

;; PEP 318 decorators
(defvar pysrt-decorators-face 'pysrt-decorators-face
  "Face method decorators.")
(make-face 'pysrt-decorators-face)

(defcustom pysrt-func-types
  '( "MAIN" "ENDMAIN" "SUB" "ENDSUB")
  "Link types for syntax highlighting of URIs."
  :group 'srt
  :type 'list)

(defcustom pysrt-loop-types
  '( "WHILE" "ENDWHILE" "FOR" "ENDFOR" "DOUNTIL" "IF"
     "ENDIF" "ELSE" "ELSEIF" )
  "Link types for syntax highlighting of URIs."
  :group 'srt
  :type 'list)

(defcustom pysrt-command-types
  '("PDU_HDR" "ISID" "CDB" "REP" "NAME" "CALL" "END"   "EXEC" "GOTO" "GOSUB"
  "HALT"     "INCLUDE" "RUN" "ONEVENT" "RELEASE_EVENT" "DROP_PDU"  "CAP" "INQ"
  "LOAD" "MSL"   "MSL10" "MSN" "MSN10"  "PACKET" "PRE" "ALW" "RD" "RD"
  "RD10" "RD10" "RD12"  "RD12" "RD16" "RD16" "REPORTLUNS" "REWIND" "SEEK"
  "SEND_CDB" "SEND" "TUR" "WR"  "WR" "WR10" "WR10"  "NOP_OUT_COMMAND" "WR12"
  "WR12"  "WR16" "WR16"   "ABORT_TASK"  "ABORT_TASK_NI" "ABORT_TASK_SET"
  "ABORT_TASK_SET_NI"   "CLEAR_TASK_SET"  "CLEAR_TASK_SET_NI"
  "LOGICAL_UNIT_RESET"  "LOGICAL_UNIT_RESET_NI"  "TARGET_COLD_RESET"
  "TARGET_COLD_RESET_NI"   "TARGET_WARM_RESET"  "TARGET_WARM_RESET_NI"
  "CONNECT" "REASON" "DISCONNECT" "DISCOVERY"   "ISNS_DISCOVERY"
  "SET_ISNS_ADDRESS" "LOGIN" "LOGOUT"  "USE_CONNECTION"   "SET_PORT" "SET_IP"
  "ADD_NEGOTIATION_ITEM" "CLEAR_NEGOTIATION_ITEMS"  "ALTCODE" "ALTERR" "AUSE"
  "ETDEC" "ET_RAND_MAX" "PDATE" "EFINE_EVENT" "APPEND"  "CMP" "CMP" "CMP"
  "COPY" "COPY" "FIND" "FINDTEXT" "STRLEN" "FREE" "GET" "GETW"     "GETDW"
  "GET64" "GETS" "MOD" "NEW" "PAT" "PATI" "PATD" "PATR" "PATT" "PATZ" "PATV"
  "PUT" "PUTW" "PUTDW" "PUT64" "PUTS" "RFILE" "WFILE" "SAVE" "SAVE_O"
  "SETSIZE" "SWAP" "ASCII2INT" "INT2ASCII" "CLS" "CURDIR" "DATE" "LOG" "LOG_O"
  "LOG_C" "LOGOFF" "PRN" "PRNX" "PRNB" "PRNB" "PRN_PERFORMANCE" "PRN_LUNS"
    "PRN_TARGETS" "REPORT" "REPORT" "REPORT" "REPORT" "REPORT" "REPORT" "REPORT"
    "REPORT_PERFORMANCE" "SCRIPT" "TIME" "VER" "ZERO" "WAIT" "WAIT"
    "INIT_PERFORMANCE" "REGISTER_EVENT" "BREAK" "NAME" "DESCRIPTION" "AUTHOR"
    "VERSION" "RESULT" "PRINT" "PRINTE" "STRCMP" "TEXT_COMMAND" "TASK_MGT_COMMAND"
    "DATA_OUT_COMMAND" "DEL_NEGOTIATION_ITEM" "CRC32C" "DROP_CONNECTION"
    "GET_NEGOTIATION_VALUE" )
  "Link types for syntax highlighting of URIs."
  :group 'srt
  :type 'list)

(defcustom pysrt-pdufield-types
  '( "OPCODE" "FLAGS" "I_FLAG" "F_FLAG" "R_FLAG" "S_FLAG" "W_FLAG" "VERSION_MAX"
     "VERSION_MIN" "VERSION_ACTIVE" "DATA_SEGMENT_LENGTH" "EXP_DATA_TRANS_LENGTH"
     "BUFFER_OFFSET" "INITIATOR_TASK_TAG" "TARGET_TRANS_TAG" "CID" "CMD_SN"
     "EXP_CMD_SN" "MAX_CMD_SN" "STAT_SN" "EXP_STAT_SN" "STATUS" "STATUS_CLASS"
     "STATUS_DETAIL" "LUN" "HEADER_DIGEST" "RESPONSE" "DATA_SN" "EXP_DATA_SN"
     "BIDI_RESIDUAL_COUNT" "RESIDUAL_COUNT" "R2T_SN" "REASON_CODE" "TIME_2_WAIT"
     "TIME_2_RETAIN" "TSIH" "RECALC_DIGEST" "PASS" "NULL" "TRUE" "FALSE"
     "CMD_IN_PROGRESS" "DATA_DIGEST" "HEADER_DIGEST" "RTT" "REF_CMD_SN"
     "DESIRED_DATA_TRANS_LEN" "A_FLAG" "INITIAL_CMD_SN" )
  "Link types for syntax highlighting of URIs."
  :group 'srt
  :type 'list)

(defcustom pysrt-evt-types
  '("TCP_CONNECT" "TCP_DISCONNECT" "SCSI_COMMAND" "SCSI_RESPONSE"
    "TASK_MANAGEMENT_REQUEST" "TASK_MANAGEMENT_RESPONSE" "DATA_OUT" "DATA_IN"
    "R2T" "ASYNCHRONOUS_MESSAGE" "TEXT_REQUEST" "TEXT_RESPONSE" "LOGIN_REQUEST"
    "LOGIN_RESPONSE" "LOGOUT_REQUEST" "LOGOUT_RESPONSE" "SNACK" "REJECT" "NOP_OUT"
    "NOP_IN")
  "Link types for syntax highlighting of URIs."
  :group 'srt
  :type 'list)

;; Face for builtins
(require 'font-lock)

(defvar pysrt-italic-face 'pysrt-italic-face
  "Face name to use for italic text.")

(defvar pysrt-bold-face 'pysrt-bold-face
  "Face name to use for bold text.")

(defvar pysrt-header-face 'pysrt-header-face
  "Face name to use as a base for headers.")

(defvar pysrt-header-face-1 'pysrt-header-face-1
  "Face name to use for level-1 headers.")

(defvar pysrt-header-face-2 'pysrt-header-face-2
  "Face name to use for level-2 headers.")

(defvar pysrt-header-face-3 'pysrt-header-face-3
  "Face name to use for level-3 headers.")

(defvar pysrt-header-face-4 'pysrt-header-face-4
  "Face name to use for level-4 headers.")

(defvar pysrt-header-face-5 'pysrt-header-face-5
  "Face name to use for level-5 headers.")

(defvar pysrt-header-face-6 'pysrt-header-face-6
  "Face name to use for level-6 headers.")

(defvar pysrt-inline-code-face 'pysrt-inline-code-face
  "Face name to use for inline code.")

(defvar pysrt-list-face 'pysrt-list-face
  "Face name to use for list markers.")

(defvar pysrt-blockquote-face 'pysrt-blockquote-face
  "Face name to use for blockquote.")

(defvar pysrt-pre-face 'pysrt-pre-face
  "Face name to use for preformatted text.")

(defvar pysrt-loop-face 'pysrt-loop-face
  "Face name to use for loop.")

(defvar pysrt-command-face 'pysrt-command-face
 "Face name to use for command.")

(defvar pysrt-pdufield-face 'pysrt-pdufield-face
 "Face name to use for command.")

(defvar pysrt-evt-face 'pysrt-evt-face
 "Face name to use for command.")

(defvar pysrt-func-face 'pysrt-func-face
 "Face name to use for command.")

(defvar pysrt-reference-face 'pysrt-reference-face
  "Face name to use for reference.")

(defvar pysrt-url-face 'pysrt-url-face
  "Face name to use for URLs.")

(defvar pysrt-link-title-face 'pysrt-link-title-face
  "Face name to use for reference link titles.")

(defvar pysrt-comment-face 'pysrt-comment-face
  "Face name to use for HTML comments.")

(defvar pysrt-math-face 'pysrt-math-face
  "Face name to use for LaTeX expressions.")


(defgroup pysrt-faces nil
  "Faces used in Srt Mode"
  :group 'srt
  :group 'faces)

(defface pysrt-italic-face
  '((t :inherit font-lock-variable-name-face :italic t))
  "Face for italic text."
  :group 'pysrt-faces)

(defface pysrt-bold-face
  '((t :inherit font-lock-variable-name-face :bold t))
  "Face for bold text."
  :group 'pysrt-faces)

(defface pysrt-header-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Base face for headers."
  :group 'pysrt-faces)

(defface pysrt-header-face-1
  '((t :inherit pysrt-header-face))
  "Face for level-1 headers."
  :group 'pysrt-faces)

(defface pysrt-header-face-2
  '((t :inherit pysrt-header-face))
  "Face for level-2 headers."
  :group 'pysrt-faces)

(defface pysrt-header-face-3
  '((t :inherit pysrt-header-face))
  "Face for level-3 headers."
  :group 'pysrt-faces)

(defface pysrt-header-face-4
  '((t :inherit pysrt-header-face))
  "Face for level-4 headers."
  :group 'pysrt-faces)

(defface pysrt-header-face-5
  '((t :inherit pysrt-header-face))
  "Face for level-5 headers."
  :group 'pysrt-faces)

(defface pysrt-header-face-6
  '((t :inherit pysrt-header-face))
  "Face for level-6 headers."
  :group 'pysrt-faces)

(defface pysrt-inline-code-face
  '((t :inherit font-lock-constant-face))
  "Face for inline code."
  :group 'pysrt-faces)

(defface pysrt-list-face
  '((t :inherit font-lock-builtin-face))
  "Face for list item markers."
  :group 'pysrt-faces)

(defface pysrt-blockquote-face
  '((t :inherit font-lock-doc-face))
  "Face for blockquote sections."
  :group 'pysrt-faces)

(defface pysrt-pre-face
  '((t :inherit font-lock-constant-face))
  "Face for preformatted text."
  :group 'pysrt-faces)

(defface pysrt-loop-face
  '((t :inherit font-lock-keyword-face))
  "Face for links."
  :group 'pysrt-faces)

(defface pysrt-command-face
  '((t :inherit font-lock-builtin-face))
  "Face for command item markers."
  :group 'pysrt-faces)

(defface pysrt-pdufield-face
  '((t :inherit font-lock-doc-face))
  "Face for command item markers."
  :group 'pysrt-faces)

(defface pysrt-evt-face
  '((t :inherit font-lock-constant-face))
  "Face for command item markers."
  :group 'pysrt-faces)

(defface pysrt-func-face
  '((t :inherit font-lock-comment-face))
  "Face for command item markers."
  :group 'pysrt-faces)

(defface pysrt-reference-face
  '((t :inherit font-lock-type-face))
  "Face for link references."
  :group 'pysrt-faces)

(defface pysrt-url-face
  '((t :inherit font-lock-string-face))
  "Face for URLs."
  :group 'pysrt-faces)

(defface pysrt-link-title-face
  '((t :inherit font-lock-comment-face))
  "Face for reference link titles."
  :group 'pysrt-faces)

(defface pysrt-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for HTML comments."
  :group 'pysrt-faces)

(defface pysrt-math-face
  '((t :inherit font-lock-string-face))
  "Face for LaTeX expressions."
  :group 'pysrt-faces)

(defface pysrt-string-face
  '((t :inherit font-lock-string-face))
  "Face for LaTeX expressions."
  :group 'pysrt-faces)

(defconst pysrt-regex-link-inline
  "\\(!?\\[[^]]*?\\]\\)\\(([^\\)]*)\\)"
  "Regular expression for a [text](file) or an image link ![text](file).")

(defconst pysrt-regex-link-reference
  "\\(!?\\[[^]]+?\\]\\)[ ]?\\(\\[[^]]*?\\]\\)"
  "Regular expression for a reference link [text][id].")

(defconst pysrt-regex-reference-definition
  "^ \\{0,3\\}\\(\\[.+?\\]\\):\\s *\\(.*?\\)\\s *\\( \"[^\"]*\"$\\|$\\)"
  "Regular expression for a link definition [id]: ...")

(defconst pysrt-regex-header-1-atx
  "^\\(# \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 1 atx-style (hash mark) headers.")

(defconst pysrt-regex-header-2-atx
  "^\\(## \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 2 atx-style (hash mark) headers.")

(defconst pysrt-regex-header-3-atx
  "^\\(### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 3 atx-style (hash mark) headers.")

(defconst pysrt-regex-header-4-atx
  "^\\(#### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 4 atx-style (hash mark) headers.")

(defconst pysrt-regex-header-5-atx
  "^\\(##### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 5 atx-style (hash mark) headers.")

(defconst pysrt-regex-header-6-atx
  "^\\(###### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 6 atx-style (hash mark) headers.")

(defconst pysrt-regex-header-1-setext
  "^\\(.*\\)\n\\(===+\\)$"
  "Regular expression for level 1 setext-style (underline) headers.")

(defconst pysrt-regex-header-2-setext
  "^\\(.*\\)\n\\(---+\\)$"
  "Regular expression for level 2 setext-style (underline) headers.")

(defconst pysrt-regex-hr
  "^\\(\\*[ ]?\\*[ ]?\\*[ ]?[\\* ]*\\|-[ ]?-[ ]?-[--- ]*\\)$"
  "Regular expression for matching Srt horizontal rules.")

(defconst pysrt-regex-code
  "\\(^\\|[^\\]\\)\\(\\(`\\{1,2\\}\\)\\([^ \\]\\|[^ ].*?[^ \\]\\)\\3\\)"
  "Regular expression for matching inline code fragments.")

(defconst pysrt-regex-pre
  "^\\(    \\|\t\\).*$"
  "Regular expression for matching preformatted text sections.")

(defconst pysrt-regex-list
  "^[ \t]*\\([0-9]+\\.\\|[\\*\\+-]\\) "
  "Regular expression for matching list markers.")

(defconst pysrt-regex-loop
  (concat
   "\\(" (mapconcat 'identity pysrt-loop-types "\\|")
 "\\)\\>[ \n\t(]")
  "Regular expression for matching inline URIs.")

(defconst pysrt-regex-command
  (concat
   "\\(" (mapconcat 'identity pysrt-command-types "\\|")
 "\\)\\>[ \n\t(]")
  "Regular expression for matching inline URIs.")

(defconst pysrt-regex-pdufield
  (concat
   "\\(" (mapconcat 'identity pysrt-pdufield-types "\\|")
 "\\)\\>[ \n\t(]")
  "Regular expression for matching inline URIs.")

(defconst pysrt-regex-evt
  (concat
   "\\(" (mapconcat 'identity pysrt-evt-types "\\|")
 "\\)\\>[ ,\n\t(]")
  "Regular expression for matching inline URIs.")

(defconst pysrt-regex-func
  (concat
   "^\\(" (mapconcat 'identity pysrt-func-types "\\|")
 "\\)\\>[ \n\t(]")
  "Regular expression for matching inline URIs.")

(defconst pysrt-regex-latex-expression
  "\\(^\\|[^\\]\\)\\(\\$\\($\\([^\\$]\\|\\\\.\\)*\\$\\|\\([^\\$]\\|\\\\.\\)*\\)\\$\\)"
  "Regular expression for itex $..$ or $$..$$ math mode expressions.")

(defconst pysrt-regex-latex-display
    "^\\\\\\[\\(.\\|\n\\)*?\\\\\\]$"
  "Regular expression for itex \[..\] display mode expressions.")

(defconst pysrt-regex-list-indent
  "^\\(\\s *\\)\\([0-9]+\\.\\|[\\*\\+-]\\)\\(\\s +\\)"
  "Regular expression for matching indentation of list items.")

(defvar pysrt-mode-font-lock-keywords-basic
  (list
   (cons pysrt-regex-func 'pysrt-func-face)
   (cons pysrt-regex-loop 'pysrt-loop-face)
   (cons pysrt-regex-command 'pysrt-command-face)
   (cons pysrt-regex-pdufield 'pysrt-pdufield-face)
   (cons pysrt-regex-evt 'pysrt-evt-face)
   )
  "Syntax highlighting for Srt files.")

(defconst pysrt-mode-font-lock-keywords-latex
  (list
    (cons pysrt-regex-latex-expression '(2 pysrt-math-face))
   ;; Display mode equations with brackets: \[ \]
   (cons pysrt-regex-latex-display 'pysrt-math-face)
   ;; Equation reference (eq:foo)
   (cons "(eq:\\w+)" 'pysrt-reference-face)
   ;; Equation reference \eqref{foo}
   (cons "\\\\eqref{\\w+}" 'pysrt-reference-face))
  "Syntax highlighting for LaTeX fragments.")

(defvar pysrt-font-lock-keywords
  (append
   (if pysrt-enable-math
       pysrt-mode-font-lock-keywords-latex)
   pysrt-mode-font-lock-keywords-basic)
  "Default highlighting expressions for Srt mode.")

(put 'pysrt-mode 'font-lock-defaults '(pysrt-font-lock-keywords))

;; have to bind pysrt-file-queue before installing the kill-emacs-hook
(defvar pysrt-file-queue nil
  "Queue of Srt temp files awaiting execution.
Currently-active file is at the head of the list.")

(defvar pysrt-pdbtrack-is-tracking-p nil)

(defvar pysrt-pychecker-history nil)



;; Constants

(defconst pysrt-stringlit-re
  (concat
   ;; These fail if backslash-quote ends the string (not worth
   ;; fixing?).  They precede the short versions so that the first two
   ;; quotes don't look like an empty short string.
   ;;
   ;; (maybe raw), long single quoted triple quoted strings (SQTQ),
   ;; with potential embedded single quotes
   "[rR]?'''[^']*\\(\\('[^']\\|''[^']\\)[^']*\\)*'''"
   "\\|"
   ;; (maybe raw), long double quoted triple quoted strings (DQTQ),
   ;; with potential embedded double quotes
   "[rR]?\"\"\"[^\"]*\\(\\(\"[^\"]\\|\"\"[^\"]\\)[^\"]*\\)*\"\"\""
   "\\|"
   "[rR]?'\\([^'\n\\]\\|\\\\.\\)*'"     ; single-quoted
   "\\|"                                ; or
   "[rR]?\"\\([^\"\n\\]\\|\\\\.\\)*\""  ; double-quoted
   )
  "Regular expression matching a Srt string literal.")

(defconst pysrt-continued-re
  ;; This is tricky because a trailing backslash does not mean
  ;; continuation if it's in a comment
  (concat
   "\\(" "[^#'\"\n\\]" "\\|" pysrt-stringlit-re "\\)*"
   "\\\\$")
  "Regular expression matching Srt backslash continuation lines.")

(defconst pysrt-blank-or-comment-re "[ \t]*\\($\\|//\\)"
  "Regular expression matching a blank or comment line.")

(defconst pysrt-outdent-re
  (concat "\\(" (mapconcat 'identity
                           '("else:"
                             "except\\(\\s +.*\\)?:"
                             "finally:"
                             "elif\\s +.*:")
                           "\\|")
          "\\)")
  "Regular expression matching statements to be dedented one level.")

(defconst pysrt-block-closing-keywords-re
  "\\(return\\|raise\\|break\\|continue\\|pass\\)"
  "Regular expression matching keywords which typically close a block.")

(defconst pysrt-no-outdent-re
  (concat
   "\\("
   (mapconcat 'identity
              (list "try:"
                    "except\\(\\s +.*\\)?:"
                    "while\\s +.*:"
                    "for\\s +.*:"
                    "if\\s +.*:"
                    "elif\\s +.*:"
                    (concat pysrt-block-closing-keywords-re "[ \t\n]")
                    )
              "\\|")
   "\\)")
  "Regular expression matching lines not to dedent after.")

(defvar pysrt-traceback-line-re
  "[ \t]+File \"\\([^\"]+\\)\", line \\([0-9]+\\)"
  "Regular expression that describes tracebacks.")

;; pdbtrack constants
(defconst pysrt-pdbtrack-stack-entry-regexp
										;  "^> \\([^(]+\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_]+\\)()"
  "^> \\(.*\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_]+\\)()"
  "Regular expression pdbtrack uses to find a stack trace entry.")

(defconst pysrt-pdbtrack-input-prompt "\n[(<]*[Pp]db[>)]+ "
  "Regular expression pdbtrack uses to recognize a pdb prompt.")

(defconst pysrt-pdbtrack-track-range 10000
  "Max number of characters from end of buffer to search for stack entry.")



;; Major mode boilerplate

;; define a mode-specific abbrev table for those who use such things
(defvar pysrt-mode-abbrev-table nil
  "Abbrev table in use in `pysrt-mode' buffers.")
(define-abbrev-table 'pysrt-mode-abbrev-table nil)

(defvar pysrt-mode-hook nil
  "*Hook called by `pysrt-mode'.")

(make-obsolete-variable 'jpysrt-mode-hook 'jython-mode-hook)
(defvar jython-mode-hook nil
  "*Hook called by `jython-mode'. `jython-mode' also calls
`pysrt-mode-hook'.")

(defvar pysrt-shell-hook nil
  "*Hook called by `py-shell'.")

;; In previous version of pysrt-mode.el, the hook was incorrectly
;; called pysrt-mode-hook, and was not defvar'd.  Deprecate its use.
(and (fboundp 'make-obsolete-variable)
     (make-obsolete-variable 'pysrt-mode-hook 'pysrt-mode-hook))

(defvar pysrt-mode-map ()
  "Keymap used in `pysrt-mode' buffers.")
(if pysrt-mode-map
    nil
  (setq pysrt-mode-map (make-sparse-keymap))
  ;; electric keys
  (define-key pysrt-mode-map ":" 'pysrt-electric-colon)
  ;; indentation level modifiers
  (define-key pysrt-mode-map "\C-c\C-l"  'pysrt-shift-region-left)
  (define-key pysrt-mode-map "\C-c\C-r"  'pysrt-shift-region-right)
  (define-key pysrt-mode-map "\C-c<"     'pysrt-shift-region-left)
  (define-key pysrt-mode-map "\C-c>"     'pysrt-shift-region-right)
  ;; subprocess commands
  (define-key pysrt-mode-map "\C-c\C-c"  'pysrt-execute-script)
  (define-key pysrt-mode-map "\C-c!"     'pysrt-shell)
  (define-key pysrt-mode-map "\C-c\C-t"  'pysrt-toggle-shells)
  ;; Caution!  Enter here at your own risk.  We are trying to support
  ;; several behaviors and it gets disgusting. :-( This logic ripped
  ;; largely from CC Mode.
  ;;
  ;; In XEmacs 19, Emacs 19, and Emacs 20, we use this to bind
  ;; backwards deletion behavior to DEL, which both Delete and
  ;; Backspace get translated to.  There's no way to separate this
  ;; behavior in a clean way, so deal with it!  Besides, it's been
  ;; this way since the dawn of time.
  (if (not (boundp 'delete-key-deletes-forward))
      (define-key pysrt-mode-map "\177" 'pysrt-electric-backspace)
    ;; However, XEmacs 20 actually achieved enlightenment.  It is
    ;; possible to sanely define both backward and forward deletion
    ;; behavior under X separately (TTYs are forever beyond hope, but
    ;; who cares?  XEmacs 20 does the right thing with these too).
    (define-key pysrt-mode-map [delete]    'pysrt-electric-delete)
    (define-key pysrt-mode-map [backspace] 'pysrt-electric-backspace))
  ;; Separate M-BS from C-M-h.  The former should remain
  ;; backward-kill-word.
  (define-key pysrt-mode-map [(control meta h)] 'pysrt-mark-def-or-class)
  (define-key pysrt-mode-map "\C-c\C-k"  'pysrt-mark-block)
  ;; Miscellaneous
  (define-key pysrt-mode-map "\C-c:"     'pysrt-guess-indent-offset)
  (define-key pysrt-mode-map "\C-c\t"    'pysrt-indent-region)
  (define-key pysrt-mode-map "\C-c\C-d"  'pysrt-pdbtrack-toggle-stack-tracking)
  (define-key pysrt-mode-map "\C-c\C-f"  'pysrt-sort-imports)
  (define-key pysrt-mode-map "\C-c\C-n"  'pysrt-next-statement)
  (define-key pysrt-mode-map "\C-c\C-p"  'pysrt-previous-statement)
  (define-key pysrt-mode-map "\C-c\C-u"  'pysrt-goto-block-up)
  (define-key pysrt-mode-map "\C-c#"     'pysrt-comment-region)
  (define-key pysrt-mode-map "\C-c?"     'pysrt-describe-mode)
  (define-key pysrt-mode-map "\C-c\C-h"  'pysrt-help-at-point)
  (define-key pysrt-mode-map "\e\C-a"    'pysrt-beginning-of-def-or-class)
  (define-key pysrt-mode-map "\e\C-e"    'pysrt-end-of-def-or-class)
  (define-key pysrt-mode-map "\C-c-"     'pysrt-up-exception)
  (define-key pysrt-mode-map "\C-c="     'pysrt-down-exception)
  ;; stuff that is `standard' but doesn't interface well with
  ;; pysrt-mode, which forces us to rebind to special commands
  (define-key pysrt-mode-map "\C-xnd"    'pysrt-narrow-to-defun)
  ;; information
  (define-key pysrt-mode-map "\C-c\C-b" 'pysrt-submit-bug-report)
  (define-key pysrt-mode-map "\C-c\C-v" 'pysrt-version)
  (define-key pysrt-mode-map "\C-c\C-w" 'pysrt-pychecker-run)
  ;; shadow global bindings for newline-and-indent w/ the pysrt- version.
  ;; BAW - this is extremely bad form, but I'm not going to change it
  ;; for now.
  (mapc #'(lambda (key)
            (define-key pysrt-mode-map key 'pysrt-newline-and-indent))
        (where-is-internal 'newline-and-indent))
  ;; Force RET to be pysrt-newline-and-indent even if it didn't get
  ;; mapped by the above code.  motivation: Emacs' default binding for
  ;; RET is `newline' and C-j is `newline-and-indent'.  Most Srteers
  ;; expect RET to do a `py-newline-and-indent' and any Emacsers who
  ;; dislike this are probably knowledgeable enough to do a rebind.
  ;; However, we do *not* change C-j since many Emacsers have already
  ;; swapped RET and C-j and they don't want C-j bound to `newline' to
  ;; change.
  (define-key pysrt-mode-map "\C-m" 'pysrt-newline-and-indent)
  )

(defvar pysrt-mode-output-map nil
  "Keymap used in *Srt Output* buffers.")
(if pysrt-mode-output-map
    nil
  (setq pysrt-mode-output-map (make-sparse-keymap))
  (define-key pysrt-mode-output-map [button2]  'pysrt-mouseto-exception)
  (define-key pysrt-mode-output-map "\C-c\C-c" 'pysrt-goto-exception)
  ;; TBD: Disable all self-inserting keys.  This is bogus, we should
  ;; really implement this as *Srt Output* buffer being read-only
  (mapc #' (lambda (key)
             (define-key pysrt-mode-output-map key
               #'(lambda () (interactive) (beep))))
           (where-is-internal 'self-insert-command))
  )

(defvar pysrt-shell-map nil
  "Keymap used in *Srt* shell buffers.")
(if pysrt-shell-map
    nil
  (setq pysrt-shell-map (copy-keymap comint-mode-map))
  (define-key pysrt-shell-map [tab]   'tab-to-tab-stop)
  (define-key pysrt-shell-map "\C-c-" 'pysrt-up-exception)
  (define-key pysrt-shell-map "\C-c=" 'pysrt-down-exception)
  )

(defvar pysrt-mode-syntax-table nil
  "Syntax table used in `pysrt-mode' buffers.")
(when (not pysrt-mode-syntax-table)
  (setq pysrt-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\( "()" pysrt-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" pysrt-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" pysrt-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" pysrt-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" pysrt-mode-syntax-table)
  (modify-syntax-entry ?\} "){" pysrt-mode-syntax-table)
  ;; Add operator symbols misassigned in the std table
  (modify-syntax-entry ?\$ "."  pysrt-mode-syntax-table)
  (modify-syntax-entry ?\% "."  pysrt-mode-syntax-table)
  (modify-syntax-entry ?\& "."  pysrt-mode-syntax-table)
  (modify-syntax-entry ?\* "."  pysrt-mode-syntax-table)
  (modify-syntax-entry ?\+ "."  pysrt-mode-syntax-table)
  (modify-syntax-entry ?\- "."  pysrt-mode-syntax-table)
  (modify-syntax-entry ?\/ "."  pysrt-mode-syntax-table)
  (modify-syntax-entry ?\< "."  pysrt-mode-syntax-table)
  (modify-syntax-entry ?\= "."  pysrt-mode-syntax-table)
  (modify-syntax-entry ?\> "."  pysrt-mode-syntax-table)
  (modify-syntax-entry ?\| "."  pysrt-mode-syntax-table)
  ;; For historical reasons, underscore is word class instead of
  ;; symbol class.  GNU conventions say it should be symbol class, but
  ;; there's a natural conflict between what major mode authors want
  ;; and what users expect from `forward-word' and `backward-word'.
  ;; Guido and I have hashed this out and have decided to keep
  ;; underscore in word class.  If you're tempted to change it, try
  ;; binding M-f and M-b to pysrt-forward-into-nomenclature and
  ;; pysrt-backward-into-nomenclature instead.  This doesn't help in all
  ;; situations where you'd want the different behavior
  ;; (e.g. backward-kill-word).
  (modify-syntax-entry ?\_ "w"  pysrt-mode-syntax-table)
  ;; Both single quote and double quote are string delimiters
  (modify-syntax-entry ?\' "\"" pysrt-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" pysrt-mode-syntax-table)
  ;; backquote is open and close paren
  (modify-syntax-entry ?\` "$"  pysrt-mode-syntax-table)
  ;; comment delimiters
  (modify-syntax-entry ?\# "<"  pysrt-mode-syntax-table)
  (modify-syntax-entry ?\n ">"  pysrt-mode-syntax-table)
  )

;; An auxiliary syntax table which places underscore and dot in the
;; symbol class for simplicity
(defvar pysrt-dotted-expression-syntax-table nil
  "Syntax table used to identify Srt dotted expressions.")
(when (not pysrt-dotted-expression-syntax-table)
  (setq pysrt-dotted-expression-syntax-table
        (copy-syntax-table pysrt-mode-syntax-table))
  (modify-syntax-entry ?_ "_" pysrt-dotted-expression-syntax-table)
  (modify-syntax-entry ?. "_" pysrt-dotted-expression-syntax-table))



;; Utilities
(defmacro pysrt-safe (&rest body)
  "Safely execute BODY, return nil if an error occurred."
  `(condition-case nil
       (progn ,@ body)
     (error nil)))

(defsubst pysrt-keep-region-active ()
  "Keep the region active in XEmacs."
  ;; Ignore byte-compiler warnings you might see.  Also note that
  ;; FSF's Emacs 19 does it differently; its policy doesn't require us
  ;; to take explicit action.
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))

(defsubst pysrt-point (position)
  "Returns the value of point at certain commonly referenced POSITIONs.
POSITION can be one of the following symbols:

  bol  -- beginning of line
  eol  -- end of line
  bod  -- beginning of def or class
  eod  -- end of def or class
  bob  -- beginning of buffer
  eob  -- end of buffer
  boi  -- back to indentation
  bos  -- beginning of statement

This function does not modify point or mark."
  (let ((here (point)))
    (cond
     ((eq position 'bol) (beginning-of-line))
     ((eq position 'eol) (end-of-line))
     ((eq position 'bod) (pysrt-beginning-of-def-or-class 'either))
     ((eq position 'eod) (pysrt-end-of-def-or-class 'either))
     ;; Kind of funny, I know, but useful for pysrt-up-exception.
     ((eq position 'bob) (goto-char (point-min)))
     ((eq position 'eob) (goto-char (point-max)))
     ((eq position 'boi) (back-to-indentation))
     ((eq position 'bos) (pysrt-goto-initial-line))
     (t (error "Unknown buffer position requested: %s" position))
     )
    (prog1
        (point)
      (goto-char here))))

(defsubst pysrt-highlight-line (from to file line)
  (cond
   ((fboundp 'make-extent))
   (t
    ;; Emacs -- Please port this!
    )
   ))

(defun pysrt-in-literal (&optional lim)
  "Return non-nil if point is in a Srt literal (a comment or string).
Optional argument LIM indicates the beginning of the containing form,
i.e. the limit on how far back to scan."
  ;; This is the version used for non-XEmacs, which has a nicer
  ;; interface.
  ;;
  ;; WARNING: Watch out for infinite recursion.
  (let* ((lim (or lim (pysrt-point 'bod)))
         (state (parse-partial-sexp lim (point))))
    (cond
     ((nth 3 state) 'string)
     ((nth 4 state) 'comment)
     (t nil))))


;; XEmacs has a built-in function that should make this much quicker.
;; In this case, lim is ignored
(defun pysrt-fast-in-literal (&optional lim)
  "Fast version of `py-in-literal', used only by XEmacs.
Optional LIM is ignored."
  ;; don't have to worry about context == 'block-comment
  )


;; Menu definitions, only relevent if you have the easymenu.el package
;; (standard in the latest Emacs 19 and XEmacs 19 distributions).
(defvar pysrt-menu nil
  "Menu for Srt Mode.
This menu will get created automatically if you have the `easymenu'
package.  Note that the latest X/Emacs releases contain this package.")

(and (pysrt-safe (require 'easymenu) t)
     (easy-menu-define
	   pysrt-menu pysrt-mode-map "Srt Mode menu"
	   '("Srt"
		 ["Comment Out Region"   pysrt-comment-region  (mark)]
		 ["Uncomment Region"     (pysrt-comment-region (point) (mark) '(4)) (mark)]
		 "-"
		 ["Mark current block"   pysrt-mark-block t]
		 ["Mark current def"     pysrt-mark-def-or-class t]
		 ["Mark current class"   (pysrt-mark-def-or-class t) t]
		 "-"
		 ["Shift region left"    pysrt-shift-region-left (mark)]
		 ["Shift region right"   pysrt-shift-region-right (mark)]
		 "-"
		 ["Import/reload file"   pysrt-execute-import-or-reload t]
		 ["Execute region"       pysrt-execute-region (mark)]
		 ["Execute def or class" pysrt-execute-def-or-class (mark)]
		 ["Start interpreter..." pysrt-shell t]
		 "-"
		 ["Go to start of block" pysrt-goto-block-up t]
		 ["Go to start of class" (pysrt-beginning-of-def-or-class t) t]
		 ["Move to end of class" (pysrt-end-of-def-or-class t) t]
		 ["Move to start of def" pysrt-beginning-of-def-or-class t]
		 ["Move to end of def"   pysrt-end-of-def-or-class t]
		 "-"
		 ["Describe mode"        pysrt-describe-mode t]
		 )))



;; Imenu definitions
(defvar pysrt-imenu-class-regexp
  (concat                               ; <<classes>>
   "\\("                                ;
   "^[ \t]*"                            ; newline and maybe whitespace
   "\\(class[ \t]+[a-zA-Z0-9_]+\\)"     ; class name
                                        ; possibly multiple superclasses
   "\\([ \t]*\\((\\([a-zA-Z0-9_,. \t\n]\\)*)\\)?\\)"
   "[ \t]*:"                            ; and the final :
   "\\)"                                ; >>classes<<
   )
  "Regexp for Srt classes for use with the Imenu package."
  )

(defvar pysrt-imenu-method-regexp
  (concat                               ; <<methods and functions>>
   "\\("                                ;
   "^[ \t]*"                            ; new line and maybe whitespace
   "\\(def[ \t]+"                       ; function definitions start with def
   "\\([a-zA-Z0-9_]+\\)"                ;   name is here
                                        ;   function arguments...
   ;;   "[ \t]*(\\([-+/a-zA-Z0-9_=,\* \t\n.()\"'#]*\\))"
   "[ \t]*(\\([^:#]*\\))"
   "\\)"                                ; end of def
   "[ \t]*:"                            ; and then the :
   "\\)"                                ; >>methods and functions<<
   )
  "Regexp for Srt methods/functions for use with the Imenu package."
  )

(defvar pysrt-imenu-method-no-arg-parens '(2 8)
  "Indices into groups of the Srt regexp for use with Imenu.

Using these values will result in smaller Imenu lists, as arguments to
functions are not listed.

See the variable `py-imenu-show-method-args-p' for more
information.")

(defvar pysrt-imenu-method-arg-parens '(2 7)
  "Indices into groups of the Srt regexp for use with imenu.
Using these values will result in large Imenu lists, as arguments to
functions are listed.

See the variable `py-imenu-show-method-args-p' for more
information.")

;; Note that in this format, this variable can still be used with the
;; imenu--generic-function. Otherwise, there is no real reason to have
;; it.
(defvar pysrt-imenu-generic-expression
  (cons
   (concat
    pysrt-imenu-class-regexp
    "\\|"                               ; or...
    pysrt-imenu-method-regexp
    )
   pysrt-imenu-method-no-arg-parens)
  "Generic Srt expression which may be used directly with Imenu.
Used by setting the variable `imenu-generic-expression' to this value.
Also, see the function \\[py-imenu-create-index] for a better
alternative for finding the index.")

;; These next two variables are used when searching for the Srt
;; class/definitions. Just saving some time in accessing the
;; generic-pysrt-expression, really.
(defvar pysrt-imenu-generic-regexp nil)
(defvar pysrt-imenu-generic-parens nil)


(defun pysrt-imenu-create-index-function ()
  "Srt interface function for the Imenu package.
Finds all Srt classes and functions/methods. Calls function
\\[py-imenu-create-index-engine].  See that function for the details
of how this works."
  (setq pysrt-imenu-generic-regexp (car pysrt-imenu-generic-expression)
        pysrt-imenu-generic-parens (if pysrt-imenu-show-method-args-p
									 pysrt-imenu-method-arg-parens
								   pysrt-imenu-method-no-arg-parens))
  (goto-char (point-min))
  ;; Warning: When the buffer has no classes or functions, this will
  ;; return nil, which seems proper according to the Imenu API, but
  ;; causes an error in the XEmacs port of Imenu.  Sigh.
  (pysrt-imenu-create-index-engine nil))

(defun pysrt-imenu-create-index-engine (&optional start-indent)
  "Function for finding Imenu definitions in Srt.

Finds all definitions (classes, methods, or functions) in a Srt
file for the Imenu package.

Returns a possibly nested alist of the form

        (INDEX-NAME . INDEX-POSITION)

The second element of the alist may be an alist, producing a nested
list as in

        (INDEX-NAME . INDEX-ALIST)

This function should not be called directly, as it calls itself
recursively and requires some setup.  Rather this is the engine for
the function \\[py-imenu-create-index-function].

It works recursively by looking for all definitions at the current
indention level.  When it finds one, it adds it to the alist.  If it
finds a definition at a greater indentation level, it removes the
previous definition from the alist. In its place it adds all
definitions found at the next indentation level.  When it finds a
definition that is less indented then the current level, it returns
the alist it has created thus far.

The optional argument START-INDENT indicates the starting indentation
at which to continue looking for Srt classes, methods, or
functions.  If this is not supplied, the function uses the indentation
of the first definition found."
  (let (index-alist
        sub-method-alist
        looking-p
        def-name prev-name
        cur-indent def-pos
        (class-paren (first  pysrt-imenu-generic-parens))
        (def-paren   (second pysrt-imenu-generic-parens)))
    (setq looking-p
          (re-search-forward pysrt-imenu-generic-regexp (point-max) t))
    (while looking-p
      (save-excursion
        ;; used to set def-name to this value but generic-extract-name
        ;; is new to imenu-1.14. this way it still works with
        ;; imenu-1.11
        ;;(imenu--generic-extract-name pysrt-imenu-generic-parens))
        (let ((cur-paren (if (match-beginning class-paren)
                             class-paren def-paren)))
          (setq def-name
                (buffer-substring-no-properties (match-beginning cur-paren)
                                                (match-end cur-paren))))
        (save-match-data
          (pysrt-beginning-of-def-or-class 'either))
        (beginning-of-line)
        (setq cur-indent (current-indentation)))
      ;; HACK: want to go to the next correct definition location.  We
      ;; explicitly list them here but it would be better to have them
      ;; in a list.
      (setq def-pos
            (or (match-beginning class-paren)
                (match-beginning def-paren)))
      ;; if we don't have a starting indent level, take this one
      (or start-indent
          (setq start-indent cur-indent))
      ;; if we don't have class name yet, take this one
      (or prev-name
          (setq prev-name def-name))
      ;; what level is the next definition on?  must be same, deeper
      ;; or shallower indentation
      (cond
       ;; Skip code in comments and strings
       ((pysrt-in-literal))
       ;; at the same indent level, add it to the list...
       ((= start-indent cur-indent)
        (push (cons def-name def-pos) index-alist))
       ;; deeper indented expression, recurse
       ((< start-indent cur-indent)
        ;; the point is currently on the expression we're supposed to
        ;; start on, so go back to the last expression. The recursive
        ;; call will find this place again and add it to the correct
        ;; list
        (re-search-backward pysrt-imenu-generic-regexp (point-min) 'move)
        (setq sub-method-alist (pysrt-imenu-create-index-engine cur-indent))
        (if sub-method-alist
            ;; we put the last element on the index-alist on the start
            ;; of the submethod alist so the user can still get to it.
            (let ((save-elmt (pop index-alist)))
              (push (cons prev-name
                          (cons save-elmt sub-method-alist))
                    index-alist))))
       ;; found less indented expression, we're done.
       (t
        (setq looking-p nil)
        (re-search-backward pysrt-imenu-generic-regexp (point-min) t)))
      ;; end-cond
      (setq prev-name def-name)
      (and looking-p
           (setq looking-p
                 (re-search-forward pysrt-imenu-generic-regexp
                                    (point-max) 'move))))
    (nreverse index-alist)))



(defun pysrt-choose-shell-by-shebang ()
  "Choose CSrt or Jython mode by looking at #! on the first line.
Returns the appropriate mode function.
Used by `py-choose-shell', and similar to but distinct from
`set-auto-mode', though it uses `auto-mode-interpreter-regexp' (if available)."
  ;; look for an interpreter specified in the first line
  ;; similar to set-auto-mode (files.el)
  (let* ((re (if (boundp 'auto-mode-interpreter-regexp)
                 auto-mode-interpreter-regexp
               ;; stolen from Emacs 21.2
               "#![ \t]?\\([^ \t\n]*/bin/env[ \t]\\)?\\([^ \t\n]+\\)"))
         (interpreter (save-excursion
                        (goto-char (point-min))
                        (if (looking-at re)
                            (match-string 2)
                          "")))
         elt)
    ;; Map interpreter name to a mode.
    (setq elt (assoc (file-name-nondirectory interpreter)
                     pysrt-shell-alist))
    (and elt (caddr elt))))



(defun pysrt-choose-shell-by-import ()
  "Choose CSrt or Jython mode based imports.
If a file imports any packages in `py-jython-packages', within
`py-import-check-point-max' characters from the start of the file,
return `jython', otherwise return nil."
  (let (mode)
    (save-excursion
      (goto-char (point-min))
      (while (and (not mode)
                  (search-forward-regexp
                   "^\\(\\(from\\)\\|\\(import\\)\\) \\([^ \t\n.]+\\)"
                   pysrt-import-check-point-max t))
        (setq mode (and (member (match-string 4) pysrt-jython-packages)
                        'jython
                        ))))
    mode))


(defun pysrt-choose-shell ()
  "Choose CSrt or Jython mode. Returns the appropriate mode function.
This does the following:
 - look for an interpreter with `py-choose-shell-by-shebang'
 - examine imports using `py-choose-shell-by-import'
 - default to the variable `py-default-interpreter'"
  (interactive)
  (or (pysrt-choose-shell-by-shebang)
      (pysrt-choose-shell-by-import)
      pysrt-default-interpreter
										;      'csrt ;; don't use to pysrt-default-interpreter, because default
										;               ;; is only way to choose CSrt
      ))


;;;###autoload
(defun pysrt-mode ()
  "Major mode for editing Srt files.
To submit a problem report, enter `\\[py-submit-bug-report]' from a
`pysrt-mode' buffer.  Do `\\[py-describe-mode]' for detailed
documentation.  To see what version of `pysrt-mode' you are running,
enter `\\[py-version]'.

This mode knows about Srt indentation, tokens, comments and
continuation lines.  Paragraphs are separated by blank lines only.

COMMANDS
\\{py-mode-map}
VARIABLES

py-indent-offset\t\tindentation increment
py-block-comment-prefix\t\tcomment string used by `comment-region'
py-pysrt-command\t\tshell command to invoke Srt interpreter
py-temp-directory\t\tdirectory used for temp files (if needed)
py-beep-if-tab-change\t\tring the bell if `tab-width' is changed"
  (interactive)
  ;; set up local variables
  (kill-all-local-variables)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-indent-function)
  (make-local-variable 'indent-region-function)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'add-log-current-defun-function)
  (make-local-variable 'fill-paragraph-function)
  ;;
  (set-syntax-table pysrt-mode-syntax-table)
  (setq major-mode              'pysrt-mode
        mode-name               "Srt"
        local-abbrev-table      pysrt-mode-abbrev-table
        font-lock-defaults      '(pysrt-font-lock-keywords)
        paragraph-separate      "^[ \t]*$"
        paragraph-start         "^[ \t]*$"
        require-final-newline   t
        comment-start           "// "
        comment-end             ""
        comment-start-skip      "// *"
        comment-column          40
        comment-indent-function 'pysrt-comment-indent-function
        indent-region-function  'pysrt-indent-region
        indent-line-function    'pysrt-indent-line
        ;; tell add-log.el how to find the current function/method/variable
        add-log-current-defun-function 'pysrt-current-defun

        fill-paragraph-function 'pysrt-fill-paragraph
        )
  (use-local-map pysrt-mode-map)
  ;; add the menu
  (if pysrt-menu
      (easy-menu-add pysrt-menu))
  ;; Emacs 19 requires this
  (if (boundp 'comment-multi-line)
      (setq comment-multi-line nil))
  ;; Install Imenu if available
  ;; (when (pysrt-safe (require 'imenu))
  ;;   (setq imenu-create-index-function #'pysrt-imenu-create-index-function)
  ;;   (setq imenu-generic-expression pysrt-imenu-generic-expression)
  ;;   (if (fboundp 'imenu-add-to-menubar)
  ;;       (imenu-add-to-menubar (format "%s-%s" "IM" mode-name)))
  ;;   )
  ;; Run the mode hook.  Note that pysrt-mode-hook is deprecated.
  (if pysrt-mode-hook
      (run-hooks 'pysrt-mode-hook)
    (run-hooks 'pysrt-mode-hook))
  ;; Now do the automagical guessing
  (if pysrt-smart-indentation
	  (let ((offset pysrt-indent-offset))
		;; It's okay if this fails to guess a good value
		(if (and (pysrt-safe (pysrt-guess-indent-offset))
				 (<= pysrt-indent-offset 8)
				 (>= pysrt-indent-offset 2))
			(setq offset pysrt-indent-offset))
		(setq pysrt-indent-offset offset)
		;; Only turn indent-tabs-mode off if tab-width !=
		;; pysrt-indent-offset.  Never turn it on, because the user must
		;; have explicitly turned it off.
										;(if (/= tab-width pysrt-indent-offset)
		(setq indent-tabs-mode nil);)
		)))


(make-obsolete 'jpysrt-mode 'jython-mode)
(defun jython-mode ()
  "Major mode for editing Jython/Jython files.
This is a simple wrapper around `pysrt-mode'.
It runs `jython-mode-hook' then calls `pysrt-mode.'
It is added to `interpreter-mode-alist' and `py-choose-shell'.
"
  (interactive)
  (pysrt-mode)
  (pysrt-toggle-shells 'jython)
  (when jython-mode-hook
	(run-hooks 'jython-mode-hook)))


;; It's handy to add recognition of Srt files to the
;; interpreter-mode-alist and to auto-mode-alist.  With the former, we
;; can specify different `derived-modes' based on the #! line, but
;; with the latter, we can't.  So we just won't add them if they're
;; already added.
;;;###autoload
(let ((modes '(("jython" . jython-mode)
               ("srt" . pysrt-mode))))
  (while modes
    (when (not (assoc (car modes) interpreter-mode-alist))
      (push (car modes) interpreter-mode-alist))
    (setq modes (cdr modes))))
;;;###autoload
(when (not (or (rassq 'pysrt-mode auto-mode-alist)
               (rassq 'jython-mode auto-mode-alist)))
  (push '("\\.py$" . pysrt-mode) auto-mode-alist))



;; electric characters
(defun pysrt-outdent-p ()
  "Returns non-nil if the current line should dedent one level."
  (save-excursion
    (and (progn (back-to-indentation)
                (looking-at pysrt-outdent-re))
         ;; short circuit infloop on illegal construct
         (not (bobp))
         (progn (forward-line -1)
                (pysrt-goto-initial-line)
                (back-to-indentation)
                (while (or (looking-at pysrt-blank-or-comment-re)
                           (bobp))
                  (backward-to-indentation 1))
                (not (looking-at pysrt-no-outdent-re)))
         )))

(defun pysrt-electric-colon (arg)
  "Insert a colon.
In certain cases the line is dedented appropriately.  If a numeric
argument ARG is provided, that many colons are inserted
non-electrically.  Electric behavior is inhibited inside a string or
comment."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  ;; are we in a string or comment?
  (if (save-excursion
        (let ((pps (parse-partial-sexp (save-excursion
                                         (pysrt-beginning-of-def-or-class)
                                         (point))
                                       (point))))
          (not (or (nth 3 pps) (nth 4 pps)))))
      (save-excursion
        (let ((here (point))
              (outdent 0)
              (indent (pysrt-compute-indentation t)))
          (if (and (not arg)
                   (pysrt-outdent-p)
                   (= indent (save-excursion
                               (pysrt-next-statement -1)
                               (pysrt-compute-indentation t)))
                   )
              (setq outdent pysrt-indent-offset))
          ;; Don't indent, only dedent.  This assumes that any lines
          ;; that are already dedented relative to
          ;; pysrt-compute-indentation were put there on purpose.  It's
          ;; highly annoying to have `:' indent for you.  Use TAB, C-c
          ;; C-l or C-c C-r to adjust.  TBD: Is there a better way to
          ;; determine this???
          (if (< (current-indentation) indent) nil
            (goto-char here)
            (beginning-of-line)
            (delete-horizontal-space)
            (indent-to (- indent outdent))
            )))))


;; Srt subprocess utilities and filters
(defun pysrt-execute-file (proc filename)
  "Send to Srt interpreter process PROC \"execfile('FILENAME')\".
Make that process's buffer visible and force display.  Also make
comint believe the user typed this string so that
`kill-output-from-shell' does The Right Thing."
  (let ((curbuf (current-buffer))
        (procbuf (process-buffer proc))
										;       (comint-scroll-to-bottom-on-output t)
        (msg (format "## working on region in file %s...\n" filename))
        ;; add some comment, so that we can filter it out of history
        (cmd (format "execfile(r'%s') # PYSRT-MODE\n" filename)))
    (unwind-protect
        (save-excursion
          (set-buffer procbuf)
          (goto-char (point-max))
          (move-marker (process-mark proc) (point))
          (funcall (process-filter proc) proc msg))
      (set-buffer curbuf))
    (process-send-string proc cmd)))

(defun pysrt-comint-output-filter-function (string)
  "Watch output for Srt prompt and exec next file waiting in queue.
This function is appropriate for `comint-output-filter-functions'."
  ;;remove ansi terminal escape sequences from string, not sure why they are
  ;;still around...
  (setq string (ansi-color-filter-apply string))
  (when (and (string-match pysrt-shell-input-prompt-1-regexp string)
			 pysrt-file-queue)
    (if pysrt-shell-switch-buffers-on-execute
		(pop-to-buffer (current-buffer)))
    (pysrt-safe (delete-file (car pysrt-file-queue)))
    (setq pysrt-file-queue (cdr pysrt-file-queue))
    (if pysrt-file-queue
        (let ((pyproc (get-buffer-process (current-buffer))))
          (pysrt-execute-file pyproc (car pysrt-file-queue))))
    ))

(defun pysrt-pdbtrack-overlay-arrow (activation)
  "Activate or de arrow at beginning-of-line in current buffer."
  ;; This was derived/simplified from edebug-overlay-arrow
  (cond (activation
         (setq overlay-arrow-position (make-marker))
         (setq overlay-arrow-string "=>")
         (set-marker overlay-arrow-position (pysrt-point 'bol) (current-buffer))
         (setq pysrt-pdbtrack-is-tracking-p t))
        (overlay-arrow-position
         (setq overlay-arrow-position nil)
         (setq pysrt-pdbtrack-is-tracking-p nil))
        ))

(defun pysrt-pdbtrack-track-stack-file (text)
  "Show the file indicated by the pdb stack entry line, in a separate window.

Activity is disabled if the buffer-local variable
`py-pdbtrack-do-tracking-p' is nil.

We depend on the pdb input prompt matching `py-pdbtrack-input-prompt'
at the beginning of the line.

If the traceback target file path is invalid, we look for the most
recently visited pysrt-mode buffer which either has the name of the
current function \(or class) or which defines the function \(or
class).  This is to provide for remote scripts, eg, Zope's 'Script
(Srt)' - put a _copy_ of the script in a buffer named for the
script, and set to pysrt-mode, and pdbtrack will find it.)"
  ;; Instead of trying to piece things together from partial text
  ;; (which can be almost useless depending on Emacs version), we
  ;; monitor to the point where we have the next pdb prompt, and then
  ;; check all text from comint-last-input-end to process-mark.
  ;;
  ;; Also, we're very conservative about clearing the overlay arrow,
  ;; to minimize residue.  This means, for instance, that executing
  ;; other pdb commands wipe out the highlight.  You can always do a
  ;; 'where' (aka 'w') command to reveal the overlay arrow.
  (let* ((origbuf (current-buffer))
         (currproc (get-buffer-process origbuf)))

    (if (not (and currproc pysrt-pdbtrack-do-tracking-p))
        (pysrt-pdbtrack-overlay-arrow nil)

      (let* ((procmark (process-mark currproc))
             (block (buffer-substring (max comint-last-input-end
                                           (- procmark
                                              pysrt-pdbtrack-track-range))
                                      procmark))
             target target_fname target_lineno target_buffer)

        (if (not (string-match (concat pysrt-pdbtrack-input-prompt "$") block))
            (pysrt-pdbtrack-overlay-arrow nil)

          (setq target (pysrt-pdbtrack-get-source-buffer block))

          (if (stringp target)
              (message "pdbtrack: %s" target)

            (setq target_lineno (car target))
            (setq target_buffer (cadr target))
            (setq target_fname (buffer-file-name target_buffer))
            (switch-to-buffer-other-window target_buffer)
            (goto-line target_lineno)
            (message "pdbtrack: line %s, file %s" target_lineno target_fname)
            (pysrt-pdbtrack-overlay-arrow t)
            (pop-to-buffer origbuf t)

            )))))
  )

(defun pysrt-pdbtrack-get-source-buffer (block)
  "Return line number and buffer of code indicated by block's traceback text.

We look first to visit the file indicated in the trace.

Failing that, we look for the most recently visited pysrt-mode buffer
with the same name or having the named function.

If we're unable find the source code we return a string describing the
problem as best as we can determine."

  (if (not (string-match pysrt-pdbtrack-stack-entry-regexp block))

      "Traceback cue not found"

    (let* ((filename (match-string 1 block))
           (lineno (string-to-number (match-string 2 block)))
           (funcname (match-string 3 block))
           funcbuffer)

      (cond ((file-exists-p filename)
             (list lineno (find-file-noselect filename)))

            ((setq funcbuffer (pysrt-pdbtrack-grub-for-buffer funcname lineno))
             (if (string-match "/Script (Srt)$" filename)
                 ;; Add in number of lines for leading '##' comments:
                 (setq lineno
                       (+ lineno
                          (save-excursion
                            (set-buffer funcbuffer)
                            (count-lines
                             (point-min)
                             (max (point-min)
                                  (string-match "^\\([^#]\\|#[^#]\\|#$\\)"
                                                (buffer-substring (point-min)
                                                                  (point-max)))
                                  ))))))
             (list lineno funcbuffer))

            ((= (elt filename 0) ?\<)
             (format "(Non-file source: '%s')" filename))

            (t (format "Not found: %s(), %s" funcname filename)))
      )
    )
  )

(defun pysrt-pdbtrack-grub-for-buffer (funcname lineno)
  "Find most recent buffer itself named or having function funcname.

We walk the buffer-list history for pysrt-mode buffers that are
named for funcname or define a function funcname."
  (let ((buffers (buffer-list))
        buf
        got)
    (while (and buffers (not got))
      (setq buf (car buffers)
            buffers (cdr buffers))
      (if (and (save-excursion (set-buffer buf)
                               (string= major-mode "pysrt-mode"))
               (or (string-match funcname (buffer-name buf))
                   (string-match (concat "^\\s-*\\(def\\|class\\)\\s-+"
                                         funcname "\\s-*(")
                                 (save-excursion
                                   (set-buffer buf)
                                   (buffer-substring (point-min)
                                                     (point-max))))))
          (setq got buf)))
    got))

(defun pysrt-postprocess-output-buffer (buf)
  "Highlight exceptions found in BUF.
If an exception occurred return t, otherwise return nil.  BUF must exist."
  (let (line file bol err-p)
    (save-excursion
      (set-buffer buf)
      (goto-char (point-min))
      (while (re-search-forward pysrt-traceback-line-re nil t)
        (setq file (match-string 1)
              line (string-to-number (match-string 2))
              bol (pysrt-point 'bol))
        (pysrt-highlight-line bol (pysrt-point 'eol) file line)))
    (when (and pysrt-jump-on-exception line)
      (beep)
      (pysrt-jump-to-exception file line)
      (setq err-p t))
    err-p))



;;; Subprocess commands

;; only used when (memq 'broken-temp-names pysrt-emacs-features)
(defvar pysrt-serial-number 0)
(defvar pysrt-exception-buffer nil)
(defvar pysrt-output-buffer "*Srt Output*")
(make-variable-buffer-local 'pysrt-output-buffer)

;; for toggling between CSrt and Jython
(defvar pysrt-which-shell nil)
(defvar pysrt-which-args  pysrt-pysrt-command-args)
(defvar pysrt-which-bufname "Srt")
(make-variable-buffer-local 'pysrt-which-shell)
(make-variable-buffer-local 'pysrt-which-args)
(make-variable-buffer-local 'pysrt-which-bufname)

(defun pysrt-toggle-shells (arg)
  "Toggles between the CSrt and Jython shells.

With positive argument ARG (interactively \\[universal-argument]),
uses the CSrt shell, with negative ARG uses the Jython shell, and
with a zero argument, toggles the shell.

Programmatically, ARG can also be one of the symbols `csrt' or
`jython', equivalent to positive arg and negative arg respectively."
  (interactive "P")
  ;; default is to toggle
  (if (null arg)
      (setq arg 0))
  ;; preprocess arg
  (cond
   ((equal arg 0)
    ;; toggle
    (if (string-equal pysrt-which-bufname "Srt")
        (setq arg -1)
      (setq arg 1)))
   ((equal arg 'csrt) (setq arg 1))
   ((equal arg 'jython) (setq arg -1)))
  (let (msg)
    (cond
     ((< 0 arg)
      ;; set to CSrt
      (setq pysrt-which-shell pysrt-pysrt-command
            pysrt-which-args pysrt-pysrt-command-args
            pysrt-which-bufname "Srt"
            msg "CSrt")
      (if (string-equal pysrt-which-bufname "Jython")
          (setq mode-name "Srt")))
     ((> 0 arg)
      (setq pysrt-which-shell pysrt-jython-command
            pysrt-which-args pysrt-jython-command-args
            pysrt-which-bufname "Jython"
            msg "Jython")
      (if (string-equal pysrt-which-bufname "Srt")
          (setq mode-name "Jython")))
     )
    (message "Using the %s shell" msg)
    (setq pysrt-output-buffer (format "*%s Output*" pysrt-which-bufname))))

;;;###autoload
(defun pysrt-shell (&optional argprompt)
  "Start an interactive Srt interpreter in another window.
This is like Shell mode, except that Srt is running in the window
instead of a shell.  See the `Interactive Shell' and `Shell Mode'
sections of the Emacs manual for details, especially for the key
bindings active in the `*Srt*' buffer.

With optional \\[universal-argument], the user is prompted for the
flags to pass to the Srt interpreter.  This has no effect when this
command is used to switch to an existing process, only when a new
process is started.  If you use this, you will probably want to ensure
that the current arguments are retained (they will be included in the
prompt).  This argument is ignored when this function is called
programmatically, or when running in Emacs 19.34 or older.

Note: You can toggle between using the CSrt interpreter and the
Jython interpreter by hitting \\[py-toggle-shells].  This toggles
buffer local variables which control whether all your subshell
interactions happen to the `*Jython*' or `*Srt*' buffers (the
latter is the name used for the CSrt buffer).

Warning: Don't use an interactive Srt if you change sys.ps1 or
sys.ps2 from their default values, or if you're running code that
prints `>>> ' or `... ' at the start of a line.  `pysrt-mode' can't
distinguish your output from Srt's output, and assumes that `>>> '
at the start of a line is a prompt from Srt.  Similarly, the Emacs
Shell mode code assumes that both `>>> ' and `... ' at the start of a
line are Srt prompts.  Bad things can happen if you fool either
mode.

Warning:  If you do any editing *in* the process buffer *while* the
buffer is accepting output from Srt, do NOT attempt to `undo' the
changes.  Some of the output (nowhere near the parts you changed!) may
be lost if you do.  This appears to be an Emacs bug, an unfortunate
interaction between undo and process filters; the same problem exists in
non-Srt process buffers using the default (Emacs-supplied) process
filter."
  (interactive "P")
  ;; Set the default shell if not already set
  (when (null pysrt-which-shell)
    (pysrt-toggle-shells pysrt-default-interpreter))
  (let ((args pysrt-which-args))
    (when (and argprompt
               (interactive-p)
               (fboundp 'split-string))
      ;; TBD: Perhaps force "-i" in the final list?
      (setq args (split-string
                  (read-string (concat pysrt-which-bufname
                                       " arguments: ")
                               (concat
                                (mapconcat 'identity pysrt-which-args " ") " ")
                               ))))
    (if (not (equal (buffer-name) "*Srt*"))
        (switch-to-buffer-other-window
         (apply 'make-comint pysrt-which-bufname pysrt-which-shell nil args))
      (apply 'make-comint pysrt-which-bufname pysrt-which-shell nil args))
    (make-local-variable 'comint-prompt-regexp)
    (setq comint-prompt-regexp (concat pysrt-shell-input-prompt-1-regexp "\\|"
                                       pysrt-shell-input-prompt-2-regexp "\\|"
                                       "^([Pp]db) "))
    (add-hook 'comint-output-filter-functions
              'pysrt-comint-output-filter-function)
    ;; pdbtrack
    (add-hook 'comint-output-filter-functions 'pysrt-pdbtrack-track-stack-file)
    (setq pysrt-pdbtrack-do-tracking-p t)
    (set-syntax-table pysrt-mode-syntax-table)
    (use-local-map pysrt-shell-map)
    (run-hooks 'pysrt-shell-hook)
    ))

(defun pysrt-clear-queue ()
  "Clear the queue of temporary files waiting to execute."
  (interactive)
  (let ((n (length pysrt-file-queue)))
    (mapc 'delete-file pysrt-file-queue)
    (setq pysrt-file-queue nil)
    (message "%d pending files de-queued." n)))


(defun pysrt-execute-script()
  (interactive)
  (setq fname (buffer-file-name(current-buffer)))

  (let* ((buf (generate-new-buffer-name pysrt-output-buffer))
         (arg (if (string-equal pysrt-which-bufname "Python")
                  "-u" "")))
    (start-process pysrt-which-bufname buf "python" "d:/gnu/bin/ISCT.py" fname)

    (pop-to-buffer buf)
    )
  (shell-command-to-string fname))



;; Code execution commands



(defun pysrt-jump-to-exception (file line)
  "Jump to the Srt code in FILE at LINE."
  (let ((buffer (cond ((string-equal file "<stdin>")
                       (if (consp pysrt-exception-buffer)
                           (cdr pysrt-exception-buffer)
                         pysrt-exception-buffer))
                      ((and (consp pysrt-exception-buffer)
                            (string-equal file (car pysrt-exception-buffer)))
                       (cdr pysrt-exception-buffer))
                      ((pysrt-safe (find-file-noselect file)))
                      ;; could not figure out what file the exception
                      ;; is pointing to, so prompt for it
                      (t (find-file (read-file-name "Exception file: "
                                                    nil
                                                    file t))))))
    ;; Fiddle about with line number
    (setq line (+ pysrt-line-number-offset line))

    (pop-to-buffer buffer)
    ;; Force Srt mode
    (if (not (eq major-mode 'pysrt-mode))
        (pysrt-mode))
    (goto-line line)
    (message "Jumping to exception in file %s on line %d" file line)))

(defun pysrt-mouseto-exception (event)
  "Jump to the code which caused the Srt exception at EVENT.
EVENT is usually a mouse click."
  (interactive "e")
  (cond
   ((fboundp 'event-point))
   ;; Emacs -- Please port this!
   ))

(defun pysrt-goto-exception ()
  "Go to the line indicated by the traceback."
  (interactive)
  (let (file line)
    (save-excursion
      (beginning-of-line)
      (if (looking-at pysrt-traceback-line-re)
          (setq file (match-string 1)
                line (string-to-number (match-string 2)))))
    (if (not file)
        (error "Not on a traceback line"))
    (pysrt-jump-to-exception file line)))

(defun pysrt-find-next-exception (start buffer searchdir errwhere)
  "Find the next Srt exception and jump to the code that caused it.
START is the buffer position in BUFFER from which to begin searching
for an exception.  SEARCHDIR is a function, either
`re-search-backward' or `re-search-forward' indicating the direction
to search.  ERRWHERE is used in an error message if the limit (top or
bottom) of the trackback stack is encountered."
  (let (file line)
    (save-excursion
      (set-buffer buffer)
      (goto-char (pysrt-point start))
      (if (funcall searchdir pysrt-traceback-line-re nil t)
          (setq file (match-string 1)
                line (string-to-number (match-string 2)))))
    (if (and file line)
        (pysrt-jump-to-exception file line)
      (error "%s of traceback" errwhere))))

(defun pysrt-down-exception (&optional bottom)
  "Go to the next line down in the traceback.
With \\[univeral-argument] (programmatically, optional argument
BOTTOM), jump to the bottom (innermost) exception in the exception
stack."
  (interactive "P")
  (let* ((proc (get-process "Srt"))
         (buffer (if proc "*Srt*" pysrt-output-buffer)))
    (if bottom
        (pysrt-find-next-exception 'eob buffer 're-search-backward "Bottom")
      (pysrt-find-next-exception 'eol buffer 're-search-forward "Bottom"))))

(defun pysrt-up-exception (&optional top)
  "Go to the previous line up in the traceback.
With \\[universal-argument] (programmatically, optional argument TOP)
jump to the top (outermost) exception in the exception stack."
  (interactive "P")
  (let* ((proc (get-process "Srt"))
         (buffer (if proc "*Srt*" pysrt-output-buffer)))
    (if top
        (pysrt-find-next-exception 'bob buffer 're-search-forward "Top")
      (pysrt-find-next-exception 'bol buffer 're-search-backward "Top"))))


;; Electric deletion
(defun pysrt-electric-backspace (arg)
  "Delete preceding character or levels of indentation.
Deletion is performed by calling the function in `py-backspace-function'
with a single argument (the number of characters to delete).

If point is at the leftmost column, delete the preceding newline.

Otherwise, if point is at the leftmost non-whitespace character of a
line that is neither a continuation line nor a non-indenting comment
line, or if point is at the end of a blank line, this command reduces
the indentation to match that of the line that opened the current
block of code.  The line that opened the block is displayed in the
echo area to help you keep track of where you are.  With
\\[universal-argument] dedents that many blocks (but not past column
zero).

Otherwise the preceding character is deleted, converting a tab to
spaces if needed so that only a single column position is deleted.
\\[universal-argument] specifies how many characters to delete;
default is 1.

When used programmatically, argument ARG specifies the number of
blocks to dedent, or the number of characters to delete, as indicated
above."
  (interactive "*p")
  (if (or (/= (current-indentation) (current-column))
          (bolp)
          (pysrt-continuation-line-p)
										;         (not pysrt-honor-comment-indentation)
										;         (looking-at "#[^ \t\n]")      ; non-indenting #
          )
      (funcall pysrt-backspace-function arg)
    ;; else indent the same as the colon line that opened the block
    ;; force non-blank so pysrt-goto-block-up doesn't ignore it
    (insert-char ?* 1)
    (backward-char)
    (let ((base-indent 0)               ; indentation of base line
          (base-text "")                ; and text of base line
          (base-found-p nil))
      (save-excursion
        (while (< 0 arg)
          (condition-case nil           ; in case no enclosing block
              (progn
                (pysrt-goto-block-up 'no-mark)
                (setq base-indent (current-indentation)
                      base-text   (pysrt-suck-up-leading-text)
                      base-found-p t))
            (error nil))
          (setq arg (1- arg))))
      (delete-char 1)                   ; toss the dummy character
      (delete-horizontal-space)
      (indent-to base-indent)
      (if base-found-p
          (message "Closes block: %s" base-text)))))


(defun pysrt-electric-delete (arg)
  "Delete preceding or following character or levels of whitespace.

The behavior of this function depends on the variable
`delete-key-deletes-forward'.  If this variable is nil (or does not
exist, as in older Emacsen and non-XEmacs versions), then this
function behaves identically to \\[c-electric-backspace].

If `delete-key-deletes-forward' is non-nil and is supported in your
Emacs, then deletion occurs in the forward direction, by calling the
function in `py-delete-function'.

\\[universal-argument] (programmatically, argument ARG) specifies the
number of characters to delete (default is 1)."
  (interactive "*p")
  (if (or (and (fboundp 'delete-forward-p) ;XEmacs 21
               (delete-forward-p))
          (and (boundp 'delete-key-deletes-forward) ;XEmacs 20
               delete-key-deletes-forward))
      (funcall pysrt-delete-function arg)
    (pysrt-electric-backspace arg)))

;; required for pending-del and delsel modes
(put 'pysrt-electric-colon 'delete-selection t) ;delsel
(put 'pysrt-electric-colon 'pending-delete   t) ;pending-del
(put 'pysrt-electric-backspace 'delete-selection 'supersede) ;delsel
(put 'pysrt-electric-backspace 'pending-delete   'supersede) ;pending-del
(put 'pysrt-electric-delete    'delete-selection 'supersede) ;delsel
(put 'pysrt-electric-delete    'pending-delete   'supersede) ;pending-del



(defun pysrt-indent-line (&optional arg)
  "Fix the indentation of the current line according to Srt rules.
With \\[universal-argument] (programmatically, the optional argument
ARG non-nil), ignore dedenting rules for block closing statements
(e.g. return, raise, break, continue, pass)

This function is normally bound to `indent-line-function' so
\\[indent-for-tab-command] will call it."
  (interactive "P")
  (let* ((ci (current-indentation))
         (move-to-indentation-p (<= (current-column) ci))
         (need (pysrt-compute-indentation (not arg)))
         (cc (current-column)))
    ;; dedent out a level if previous command was the same unless we're in
    ;; column 1
    (if (and (equal last-command this-command)
             (/= cc 0))
        (progn
          (beginning-of-line)
          (delete-horizontal-space)
          (indent-to (* (/ (- cc 1) pysrt-indent-offset) pysrt-indent-offset)))
      (progn
        ;; see if we need to dedent
        (if (pysrt-outdent-p)
            (setq need (- need pysrt-indent-offset)))
        (if (or pysrt-tab-always-indent
                move-to-indentation-p)
            (progn (if (/= ci need)
                       (save-excursion
                       (beginning-of-line)
                       (delete-horizontal-space)
                       (indent-to need)))
                   (if move-to-indentation-p (back-to-indentation)))
            (insert-tab))))))

(defun pysrt-newline-and-indent ()
  "Strives to act like the Emacs `newline-and-indent'.
This is just `strives to' because correct indentation can't be computed
from scratch for Srt code.  In general, deletes the whitespace before
point, inserts a newline, and takes an educated guess as to how you want
the new line indented."
  (interactive)
  (let ((ci (current-indentation)))
    (if (< ci (current-column))         ; if point beyond indentation
        (newline-and-indent)
      ;; else try to act like newline-and-indent "normally" acts
      (beginning-of-line)
      (insert-char ?\n 1)
      (move-to-column ci))))

(defun pysrt-compute-indentation (honor-block-close-p)
  "Compute Srt indentation.
When HONOR-BLOCK-CLOSE-P is non-nil, statements such as `return',
`raise', `break', `continue', and `pass' force one level of
dedenting."
  (save-excursion
    (beginning-of-line)
    (let* ((bod (pysrt-point 'bod))
           (pps (parse-partial-sexp bod (point)))
           (boipps (parse-partial-sexp bod (pysrt-point 'boi)))
           placeholder)
      (cond
       ;; are we inside a multi-line string or comment?
       ((or (and (nth 3 pps) (nth 3 boipps))
            (and (nth 4 pps) (nth 4 boipps)))
        (save-excursion
          (if (not pysrt-align-multiline-strings-p) 0
            ;; skip back over blank & non-indenting comment lines
            ;; note: will skip a blank or non-indenting comment line
            ;; that happens to be a continuation line too
            (re-search-backward "^[ \t]*\\([^ \t\n#]\\|#[ \t\n]\\)" nil 'move)
            (back-to-indentation)
            (current-column))))
       ;; are we on a continuation line?
       ((pysrt-continuation-line-p)
        (let ((startpos (point))
              (open-bracket-pos (pysrt-nesting-level))
              endpos searching found state cind cline)
          (if open-bracket-pos
              (progn
                (setq endpos (pysrt-point 'bol))
                (pysrt-goto-initial-line)
                (setq cind (current-indentation))
                (setq cline cind)
                (dolist (bp
                         (nth 9 (save-excursion
                                  (parse-partial-sexp (point) endpos)))
                         cind)
                  (if (search-forward "\n" bp t) (setq cline cind))
                  (goto-char (1+ bp))
                  (skip-chars-forward " \t")
                  (setq cind (if (memq (following-char) '(?\n ?# ?\\))
                                 (+ cline pysrt-indent-offset)
                               (current-column)))))
            ;; else on backslash continuation line
            (forward-line -1)
            (if (pysrt-continuation-line-p) ; on at least 3rd line in block
                (current-indentation)   ; so just continue the pattern
              ;; else started on 2nd line in block, so indent more.
              ;; if base line is an assignment with a start on a RHS,
              ;; indent to 2 beyond the leftmost "="; else skip first
              ;; chunk of non-whitespace characters on base line, + 1 more
              ;; column
              (end-of-line)
              (setq endpos (point)
                    searching t)
              (back-to-indentation)
              (setq startpos (point))
              ;; look at all "=" from left to right, stopping at first
              ;; one not nested in a list or string
              (while searching
                (skip-chars-forward "^=" endpos)
                (if (= (point) endpos)
                    (setq searching nil)
                  (forward-char 1)
                  (setq state (parse-partial-sexp startpos (point)))
                  (if (and (zerop (car state)) ; not in a bracket
                           (null (nth 3 state))) ; & not in a string
                      (progn
                        (setq searching nil) ; done searching in any case
                        (setq found
                              (not (or
                                    (eq (following-char) ?=)
                                    (memq (char-after (- (point) 2))
                                          '(?< ?> ?!)))))))))
              (if (or (not found)       ; not an assignment
                      (looking-at "[ \t]*\\\\")) ; <=><spaces><backslash>
                  (progn
                    (goto-char startpos)
                    (skip-chars-forward "^ \t\n")))
              ;; if this is a continuation for a block opening
              ;; statement, add some extra offset.
              (+ (current-column) (if (pysrt-statement-opens-block-p)
                                      pysrt-continuation-offset 0)
                 1)
              ))))

       ;; not on a continuation line
       ((bobp) (current-indentation))

       ;; Dfn: "Indenting comment line".  A line containing only a
       ;; comment, but which is treated like a statement for
       ;; indentation calculation purposes.  Such lines are only
       ;; treated specially by the mode; they are not treated
       ;; specially by the Srt interpreter.

       ;; The rules for indenting comment lines are a line where:
       ;;   - the first non-whitespace character is `#', and
       ;;   - the character following the `#' is whitespace, and
       ;;   - the line is dedented with respect to (i.e. to the left
       ;;     of) the indentation of the preceding non-blank line.

       ;; The first non-blank line following an indenting comment
       ;; line is given the same amount of indentation as the
       ;; indenting comment line.

       ;; All other comment-only lines are ignored for indentation
       ;; purposes.

       ;; Are we looking at a comment-only line which is *not* an
       ;; indenting comment line?  If so, we assume that it's been
       ;; placed at the desired indentation, so leave it alone.
       ;; Indenting comment lines are aligned as statements down
       ;; below.
       ((and (looking-at "[ \t]*#[^ \t\n]")
             ;; NOTE: this test will not be performed in older Emacsen
             (fboundp 'forward-comment)
             (<= (current-indentation)
                 (save-excursion
                   (forward-comment (- (point-max)))
                   (current-indentation))))
        (current-indentation))

       ;; else indentation based on that of the statement that
       ;; precedes us; use the first line of that statement to
       ;; establish the base, in case the user forced a non-std
       ;; indentation for the continuation lines (if any)
       (t
        ;; skip back over blank & non-indenting comment lines note:
        ;; will skip a blank or non-indenting comment line that
        ;; happens to be a continuation line too.  use fast Emacs 19
        ;; function if it's there.
        (if (and (eq pysrt-honor-comment-indentation nil)
                 (fboundp 'forward-comment))
            (forward-comment (- (point-max)))
          (let ((prefix-re (concat pysrt-block-comment-prefix "[ \t]*"))
                done)
            (while (not done)
              (re-search-backward "^[ \t]*\\([^ \t\n#]\\|#\\)" nil 'move)
              (setq done (or (bobp)
                             (and (eq pysrt-honor-comment-indentation t)
                                  (save-excursion
                                    (back-to-indentation)
                                    (not (looking-at prefix-re))
                                    ))
                             (and (not (eq pysrt-honor-comment-indentation t))
                                  (save-excursion
                                    (back-to-indentation)
                                    (and (not (looking-at prefix-re))
                                         (or (looking-at "[^#]")
                                             (not (zerop (current-column)))
                                             ))
                                    ))
                             ))
              )))
        ;; if we landed inside a string, go to the beginning of that
        ;; string. this handles triple quoted, multi-line spanning
        ;; strings.
        (pysrt-goto-beginning-of-tqs (nth 3 (parse-partial-sexp bod (point))))
        ;; now skip backward over continued lines
        (setq placeholder (point))
        (pysrt-goto-initial-line)
        ;; we may *now* have landed in a TQS, so find the beginning of
        ;; this string.
        (pysrt-goto-beginning-of-tqs
         (save-excursion (nth 3 (parse-partial-sexp
                                 placeholder (point)))))
        (+ (current-indentation)
           (if (pysrt-statement-opens-block-p)
               pysrt-indent-offset
             (if (and honor-block-close-p (pysrt-statement-closes-block-p))
                 (- pysrt-indent-offset)
               0)))
        )))))

(defun pysrt-guess-indent-offset (&optional global)
  "Guess a good value for, and change, `py-indent-offset'.

By default, make a buffer-local copy of `py-indent-offset' with the
new value, so that other Srt buffers are not affected.  With
\\[universal-argument] (programmatically, optional argument GLOBAL),
change the global value of `py-indent-offset'.  This affects all
Srt buffers (that don't have their own buffer-local copy), both
those currently existing and those created later in the Emacs session.

Some people use a different value for `py-indent-offset' than you use.
There's no excuse for such foolishness, but sometimes you have to deal
with their ugly code anyway.  This function examines the file and sets
`py-indent-offset' to what it thinks it was when they created the
mess.

Specifically, it searches forward from the statement containing point,
looking for a line that opens a block of code.  `py-indent-offset' is
set to the difference in indentation between that line and the Srt
statement following it.  If the search doesn't succeed going forward,
it's tried again going backward."
  (interactive "P")                     ; raw prefix arg
  (let (new-value
        (start (point))
        (restart (point))
        (found nil)
        colon-indent)
    (pysrt-goto-initial-line)
    (while (not (or found (eobp)))
      (when (and (re-search-forward ":[ \t]*\\($\\|[#\\]\\)" nil 'move)
                 (not (pysrt-in-literal restart)))
        (setq restart (point))
        (pysrt-goto-initial-line)
        (if (pysrt-statement-opens-block-p)
            (setq found t)
          (goto-char restart))))
    (unless found
      (goto-char start)
      (pysrt-goto-initial-line)
      (while (not (or found (bobp)))
        (setq found (and
                     (re-search-backward ":[ \t]*\\($\\|[#\\]\\)" nil 'move)
                     (or (pysrt-goto-initial-line) t) ; always true -- side effect
                     (pysrt-statement-opens-block-p)))))
    (setq colon-indent (current-indentation)
          found (and found (zerop (pysrt-next-statement 1)))
          new-value (- (current-indentation) colon-indent))
    (goto-char start)
    (if (not found)
        (error "Sorry, couldn't guess a value for pysrt-indent-offset")
      (funcall (if global 'kill-local-variable 'make-local-variable)
               'pysrt-indent-offset)
      (setq pysrt-indent-offset new-value)
      (or noninteractive
          (message "%s value of pysrt-indent-offset set to %d"
                   (if global "Global" "Local")
                   pysrt-indent-offset)))
    ))

(defun pysrt-comment-indent-function ()
  "Srt version of `comment-indent-function'."
  ;; This is required when filladapt is turned off.  Without it, when
  ;; filladapt is not used, comments which start in column zero
  ;; cascade one character to the right
  (save-excursion
    (beginning-of-line)
    (let ((eol (pysrt-point 'eol)))
      (and comment-start-skip
           (re-search-forward comment-start-skip eol t)
           (setq eol (match-beginning 0)))
      (goto-char eol)
      (skip-chars-backward " \t")
      (max comment-column (+ (current-column) (if (bolp) 0 1)))
      )))

(defun pysrt-narrow-to-defun (&optional class)
  "Make text outside current defun invisible.
The defun visible is the one that contains point or follows point.
Optional CLASS is passed directly to `py-beginning-of-def-or-class'."
  (interactive "P")
  (save-excursion
    (widen)
    (pysrt-end-of-def-or-class class)
    (let ((end (point)))
      (pysrt-beginning-of-def-or-class class)
      (narrow-to-region (point) end))))


(defun pysrt-shift-region (start end count)
  "Indent lines from START to END by COUNT spaces."
  (save-excursion
    (goto-char end)
    (beginning-of-line)
    (setq end (point))
    (goto-char start)
    (beginning-of-line)
    (setq start (point))
    (indent-rigidly start end count)))

(defun pysrt-shift-region-left (start end &optional count)
  "Shift region of Srt code to the left.
The lines from the line containing the start of the current region up
to (but not including) the line containing the end of the region are
shifted to the left, by `py-indent-offset' columns.

If a prefix argument is given, the region is instead shifted by that
many columns.  With no active region, dedent only the current line.
You cannot dedent the region if any line is already at column zero."
  (interactive
   (let ((p (point))
         (m (mark))
         (arg current-prefix-arg))
     (if m
         (list (min p m) (max p m) arg)
       (list p (save-excursion (forward-line 1) (point)) arg))))
  ;; if any line is at column zero, don't shift the region
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (back-to-indentation)
      (if (and (zerop (current-column))
               (not (looking-at "\\s *$")))
          (error "Region is at left edge"))
      (forward-line 1)))
  (pysrt-shift-region start end (- (prefix-numeric-value
								  (or count pysrt-indent-offset))))
  (pysrt-keep-region-active))

(defun pysrt-shift-region-right (start end &optional count)
  "Shift region of Srt code to the right.
The lines from the line containing the start of the current region up
to (but not including) the line containing the end of the region are
shifted to the right, by `py-indent-offset' columns.

If a prefix argument is given, the region is instead shifted by that
many columns.  With no active region, indent only the current line."
  (interactive
   (let ((p (point))
         (m (mark))
         (arg current-prefix-arg))
     (if m
         (list (min p m) (max p m) arg)
       (list p (save-excursion (forward-line 1) (point)) arg))))
  (pysrt-shift-region start end (prefix-numeric-value
							   (or count pysrt-indent-offset)))
  (pysrt-keep-region-active))

(defun pysrt-indent-region (start end &optional indent-offset)
  "Reindent a region of Srt code.

The lines from the line containing the start of the current region up
to (but not including) the line containing the end of the region are
reindented.  If the first line of the region has a non-whitespace
character in the first column, the first line is left alone and the
rest of the region is reindented with respect to it.  Else the entire
region is reindented with respect to the (closest code or indenting
comment) statement immediately preceding the region.

This is useful when code blocks are moved or yanked, when enclosing
control structures are introduced or removed, or to reformat code
using a new value for the indentation offset.

If a numeric prefix argument is given, it will be used as the value of
the indentation offset.  Else the value of `py-indent-offset' will be
used.

Warning: The region must be consistently indented before this function
is called!  This function does not compute proper indentation from
scratch (that's impossible in Srt), it merely adjusts the existing
indentation to be correct in context.

Warning: This function really has no idea what to do with
non-indenting comment lines, and shifts them as if they were indenting
comment lines.  Fixing this appears to require telepathy.

Special cases: whitespace is deleted from blank lines; continuation
lines are shifted by the same amount their initial line was shifted,
in order to preserve their relative indentation with respect to their
initial line; and comment lines beginning in column 1 are ignored."
  (interactive "*r\nP")                 ; region; raw prefix arg
  (save-excursion
    (goto-char end)   (beginning-of-line) (setq end (point-marker))
    (goto-char start) (beginning-of-line)
    (let ((pysrt-indent-offset (prefix-numeric-value
							  (or indent-offset pysrt-indent-offset)))
          (indents '(-1))               ; stack of active indent levels
          (target-column 0)             ; column to which to indent
          (base-shifted-by 0)           ; amount last base line was shifted
          (indent-base (if (looking-at "[ \t\n]")
                           (pysrt-compute-indentation t)
                         0))
          ci)
      (while (< (point) end)
        (setq ci (current-indentation))
        ;; figure out appropriate target column
        (cond
         ((or (eq (following-char) ?#)  ; comment in column 1
              (looking-at "[ \t]*$"))   ; entirely blank
          (setq target-column 0))
         ((pysrt-continuation-line-p)      ; shift relative to base line
          (setq target-column (+ ci base-shifted-by)))
         (t                             ; new base line
          (if (> ci (car indents))      ; going deeper; push it
              (setq indents (cons ci indents))
            ;; else we should have seen this indent before
            (setq indents (memq ci indents)) ; pop deeper indents
            (if (null indents)
                (error "Bad indentation in region, at line %d"
                       (save-restriction
                         (widen)
                         (1+ (count-lines 1 (point)))))))
          (setq target-column (+ indent-base
                                 (* pysrt-indent-offset
                                    (- (length indents) 2))))
          (setq base-shifted-by (- target-column ci))))
        ;; shift as needed
        (if (/= ci target-column)
            (progn
              (delete-horizontal-space)
              (indent-to target-column)))
        (forward-line 1))))
  (set-marker end nil))

(defun pysrt-comment-region (beg end &optional arg)
  "Like `comment-region' but uses double hash (`#') comment starter."
  (interactive "r\nP")
  (let ((comment-start pysrt-block-comment-prefix))
    (comment-region beg end arg)))

(defun pysrt-join-words-wrapping (words separator line-prefix line-length)
  (let ((lines ())
        (current-line line-prefix))
    (while words
      (let* ((word (car words))
             (maybe-line (concat current-line word separator)))
        (if (> (length maybe-line) line-length)
            (setq lines (cons (substring current-line 0 -1) lines)
                  current-line (concat line-prefix word separator " "))
          (setq current-line (concat maybe-line " "))))
      (setq words (cdr words)))
    (setq lines (cons (substring
                       current-line 0 (- 0 (length separator) 1)) lines))
    (mapconcat 'identity (nreverse lines) "\n")))

(defun pysrt-sort-imports ()
  "Sort multiline imports.
Put point inside the parentheses of a multiline import and hit
\\[py-sort-imports] to sort the imports lexicographically"
  (interactive)
  (save-excursion
    (let ((open-paren (save-excursion (progn (up-list -1) (point))))
          (close-paren (save-excursion (progn (up-list 1) (point))))
          sorted-imports)
      (goto-char (1+ open-paren))
      (skip-chars-forward " \n\t")
      (setq sorted-imports
            (sort
             (delete-dups
              (split-string (buffer-substring
                             (point)
                             (save-excursion (goto-char (1- close-paren))
                                             (skip-chars-backward " \n\t")
                                             (point)))
                            ", *\\(\n *\\)?"))
             ;; XXX Should this sort case insensitively?
             'string-lessp))
      ;; Remove empty strings.
      (delete-region open-paren close-paren)
      (goto-char open-paren)
      (insert "(\n")
      (insert (pysrt-join-words-wrapping (remove "" sorted-imports) "," "    " 78))
      (insert ")")
      )))



;; Functions for moving point
(defun pysrt-previous-statement (count)
  "Go to the start of the COUNTth preceding Srt statement.
By default, goes to the previous statement.  If there is no such
statement, goes to the first statement.  Return count of statements
left to move.  `Statements' do not include blank, comment, or
continuation lines."
  (interactive "p")                     ; numeric prefix arg
  (if (< count 0) (pysrt-next-statement (- count))
    (pysrt-goto-initial-line)
    (let (start)
      (while (and
              (setq start (point))      ; always true -- side effect
              (> count 0)
              (zerop (forward-line -1))
              (pysrt-goto-statement-at-or-above))
        (setq count (1- count)))
      (if (> count 0) (goto-char start)))
    count))

(defun pysrt-next-statement (count)
  "Go to the start of next Srt statement.
If the statement at point is the i'th Srt statement, goes to the
start of statement i+COUNT.  If there is no such statement, goes to the
last statement.  Returns count of statements left to move.  `Statements'
do not include blank, comment, or continuation lines."
  (interactive "p")                     ; numeric prefix arg
  (if (< count 0) (pysrt-previous-statement (- count))
    (beginning-of-line)
    (let (start)
      (while (and
              (setq start (point))      ; always true -- side effect
              (> count 0)
              (pysrt-goto-statement-below))
        (setq count (1- count)))
      (if (> count 0) (goto-char start)))
    count))

(defun pysrt-goto-block-up (&optional nomark)
  "Move up to start of current block.
Go to the statement that starts the smallest enclosing block; roughly
speaking, this will be the closest preceding statement that ends with a
colon and is indented less than the statement you started on.  If
successful, also sets the mark to the starting point.

`\\[py-mark-block]' can be used afterward to mark the whole code
block, if desired.

If called from a program, the mark will not be set if optional argument
NOMARK is not nil."
  (interactive)
  (let ((start (point))
        (found nil)
        initial-indent)
    (pysrt-goto-initial-line)
    ;; if on blank or non-indenting comment line, use the preceding stmt
    (if (looking-at "[ \t]*\\($\\|#[^ \t\n]\\)")
        (progn
          (pysrt-goto-statement-at-or-above)
          (setq found (pysrt-statement-opens-block-p))))
    ;; search back for colon line indented less
    (setq initial-indent (current-indentation))
    (if (zerop initial-indent)
        ;; force fast exit
        (goto-char (point-min)))
    (while (not (or found (bobp)))
      (setq found
            (and
             (re-search-backward ":[ \t]*\\($\\|[#\\]\\)" nil 'move)
             (or (pysrt-goto-initial-line) t) ; always true -- side effect
             (< (current-indentation) initial-indent)
             (pysrt-statement-opens-block-p))))
    (if found
        (progn
          (or nomark (push-mark start))
          (back-to-indentation))
      (goto-char start)
      (error "Enclosing block not found"))))

(defun pysrt-beginning-of-def-or-class (&optional class count)
  "Move point to start of `def' or `class'.

Searches back for the closest preceding `def'.  If you supply a prefix
arg, looks for a `class' instead.  The docs below assume the `def'
case; just substitute `class' for `def' for the other case.
Programmatically, if CLASS is `either', then moves to either `class'
or `def'.

When second optional argument is given programmatically, move to the
COUNTth start of `def'.

If point is in a `def' statement already, and after the `d', simply
moves point to the start of the statement.

Otherwise (i.e. when point is not in a `def' statement, or at or
before the `d' of a `def' statement), searches for the closest
preceding `def' statement, and leaves point at its start.  If no such
statement can be found, leaves point at the start of the buffer.

Returns t iff a `def' statement is found by these rules.

Note that doing this command repeatedly will take you closer to the
start of the buffer each time.

To mark the current `def', see `\\[py-mark-def-or-class]'."
  (interactive "P")                     ; raw prefix arg
  (setq count (or count 1))
  (let ((at-or-before-p (<= (current-column) (current-indentation)))
        (start-of-line (goto-char (pysrt-point 'bol)))
        (start-of-stmt (goto-char (pysrt-point 'bos)))
        (start-re (cond ((eq class 'either) "^[ \t]*\\(class\\|def\\)\\>")
                        (class "^[ \t]*class\\>")
                        (t "^[ \t]*def\\>")))
        )
    ;; searching backward
    (if (and (< 0 count)
             (or (/= start-of-stmt start-of-line)
                 (not at-or-before-p)))
        (end-of-line))
    ;; search forward
    (if (and (> 0 count)
             (zerop (current-column))
             (looking-at start-re))
        (end-of-line))
    (if (re-search-backward start-re nil 'move count)
        (goto-char (match-beginning 0)))))

;; Backwards compatibility
(defalias 'beginning-of-pysrt-def-or-class 'pysrt-beginning-of-def-or-class)

(defun pysrt-end-of-def-or-class (&optional class count)
  "Move point beyond end of `def' or `class' body.

By default, looks for an appropriate `def'.  If you supply a prefix
arg, looks for a `class' instead.  The docs below assume the `def'
case; just substitute `class' for `def' for the other case.
Programmatically, if CLASS is `either', then moves to either `class'
or `def'.

When second optional argument is given programmatically, move to the
COUNTth end of `def'.

If point is in a `def' statement already, this is the `def' we use.

Else, if the `def' found by `\\[py-beginning-of-def-or-class]'
contains the statement you started on, that's the `def' we use.

Otherwise, we search forward for the closest following `def', and use that.

If a `def' can be found by these rules, point is moved to the start of
the line immediately following the `def' block, and the position of the
start of the `def' is returned.

Else point is moved to the end of the buffer, and nil is returned.

Note that doing this command repeatedly will take you closer to the
end of the buffer each time.

To mark the current `def', see `\\[py-mark-def-or-class]'."
  (interactive "P")                     ; raw prefix arg
  (if (and count (/= count 1))
      (pysrt-beginning-of-def-or-class (- 1 count)))
  (let ((start (progn (pysrt-goto-initial-line) (point)))
        (which (cond ((eq class 'either) "\\(class\\|def\\)")
                     (class "class")
                     (t "def")))
        (state 'not-found))
    ;; move point to start of appropriate def/class
    (if (looking-at (concat "[ \t]*" which "\\>")) ; already on one
        (setq state 'at-beginning)
      ;; else see if pysrt-beginning-of-def-or-class hits container
      (if (and (pysrt-beginning-of-def-or-class class)
               (progn (pysrt-goto-beyond-block)
                      (> (point) start)))
          (setq state 'at-end)
        ;; else search forward
        (goto-char start)
        (if (re-search-forward (concat "^[ \t]*" which "\\>") nil 'move)
            (progn (setq state 'at-beginning)
                   (beginning-of-line)))))
    (cond
     ((eq state 'at-beginning) (pysrt-goto-beyond-block) t)
     ((eq state 'at-end) t)
     ((eq state 'not-found) nil)
     (t (error "Internal error in `py-end-of-def-or-class'")))))

;; Backwards compabitility
(defalias 'end-of-pysrt-def-or-class 'pysrt-end-of-def-or-class)


;; Functions for marking regions
(defun pysrt-mark-block (&optional extend just-move)
  "Mark following block of lines.  With prefix arg, mark structure.
Easier to use than explain.  It sets the region to an `interesting'
block of succeeding lines.  If point is on a blank line, it goes down to
the next non-blank line.  That will be the start of the region.  The end
of the region depends on the kind of line at the start:

 - If a comment, the region will include all succeeding comment lines up
   to (but not including) the next non-comment line (if any).

 - Else if a prefix arg is given, and the line begins one of these
   structures:

     if elif else try except finally for while def class

   the region will be set to the body of the structure, including
   following blocks that `belong' to it, but excluding trailing blank
   and comment lines.  E.g., if on a `try' statement, the `try' block
   and all (if any) of the following `except' and `finally' blocks
   that belong to the `try' structure will be in the region.  Ditto
   for if/elif/else, for/else and while/else structures, and (a bit
   degenerate, since they're always one-block structures) def and
   class blocks.

 - Else if no prefix argument is given, and the line begins a Srt
   block (see list above), and the block is not a `one-liner' (i.e.,
   the statement ends with a colon, not with code), the region will
   include all succeeding lines up to (but not including) the next
   code statement (if any) that's indented no more than the starting
   line, except that trailing blank and comment lines are excluded.
   E.g., if the starting line begins a multi-statement `def'
   structure, the region will be set to the full function definition,
   but without any trailing `noise' lines.

 - Else the region will include all succeeding lines up to (but not
   including) the next blank line, or code or indenting-comment line
   indented strictly less than the starting line.  Trailing indenting
   comment lines are included in this case, but not trailing blank
   lines.

A msg identifying the location of the mark is displayed in the echo
area; or do `\\[exchange-point-and-mark]' to flip down to the end.

If called from a program, optional argument EXTEND plays the role of
the prefix arg, and if optional argument JUST-MOVE is not nil, just
moves to the end of the block (& does not set mark or display a msg)."
  (interactive "P")                     ; raw prefix arg
  (pysrt-goto-initial-line)
  ;; skip over blank lines
  (while (and
          (looking-at "[ \t]*$")        ; while blank line
          (not (eobp)))                 ; & somewhere to go
    (forward-line 1))
  (if (eobp)
      (error "Hit end of buffer without finding a non-blank stmt"))
  (let ((initial-pos (point))
        (initial-indent (current-indentation))
        last-pos                        ; position of last stmt in region
        (followers
         '((if elif else) (elif elif else) (else)
           (try except finally) (except except) (finally)
           (for else) (while else)
           (def) (class) ) )
        first-symbol next-symbol)

    (cond
     ;; if comment line, suck up the following comment lines
     ((looking-at "[ \t]*#")
      (re-search-forward "^[ \t]*[^ \t#]" nil 'move) ; look for non-comment
      (re-search-backward "^[ \t]*#")   ; and back to last comment in block
      (setq last-pos (point)))

     ;; else if line is a block line and EXTEND given, suck up
     ;; the whole structure
     ((and extend
           (setq first-symbol (pysrt-suck-up-first-keyword) )
           (assq first-symbol followers))
      (while (and
              (or (pysrt-goto-beyond-block) t) ; side effect
              (forward-line -1)         ; side effect
              (setq last-pos (point))   ; side effect
              (pysrt-goto-statement-below)
              (= (current-indentation) initial-indent)
              (setq next-symbol (pysrt-suck-up-first-keyword))
              (memq next-symbol (cdr (assq first-symbol followers))))
        (setq first-symbol next-symbol)))

     ;; else if line *opens* a block, search for next stmt indented <=
     ((pysrt-statement-opens-block-p)
      (while (and
              (setq last-pos (point))   ; always true -- side effect
              (pysrt-goto-statement-below)
              (> (current-indentation) initial-indent)
              )))

     ;; else plain code line; stop at next blank line, or stmt or
     ;; indenting comment line indented <
     (t
      (while (and
              (setq last-pos (point))   ; always true -- side effect
              (or (pysrt-goto-beyond-final-line) t)
              (not (looking-at "[ \t]*$")) ; stop at blank line
              (or
               (>= (current-indentation) initial-indent)
               (looking-at "[ \t]*#[^ \t\n]"))) ; ignore non-indenting #
        nil)))

    ;; skip to end of last stmt
    (goto-char last-pos)
    (pysrt-goto-beyond-final-line)

    ;; set mark & display
    (if just-move
        ()                              ; just return
      (push-mark (point) 'no-msg)
      (forward-line -1)
      (message "Mark set after: %s" (pysrt-suck-up-leading-text))
      (goto-char initial-pos))))

(defun pysrt-mark-def-or-class (&optional class)
  "Set region to body of def (or class, with prefix arg) enclosing point.
Pushes the current mark, then point, on the mark ring (all language
modes do this, but although it's handy it's never documented ...).

In most Emacs language modes, this function bears at least a
hallucinogenic resemblance to `\\[py-end-of-def-or-class]' and
`\\[py-beginning-of-def-or-class]'.

And in earlier versions of Srt mode, all 3 were tightly connected.
Turned out that was more confusing than useful: the `goto start' and
`goto end' commands are usually used to search through a file, and
people expect them to act a lot like `search backward' and `search
forward' string-search commands.  But because Srt `def' and `class'
can nest to arbitrary levels, finding the smallest def containing
point cannot be done via a simple backward search: the def containing
point may not be the closest preceding def, or even the closest
preceding def that's indented less.  The fancy algorithm required is
appropriate for the usual uses of this `mark' command, but not for the
`goto' variations.

So the def marked by this command may not be the one either of the
`goto' commands find: If point is on a blank or non-indenting comment
line, moves back to start of the closest preceding code statement or
indenting comment line.  If this is a `def' statement, that's the def
we use.  Else searches for the smallest enclosing `def' block and uses
that.  Else signals an error.

When an enclosing def is found: The mark is left immediately beyond
the last line of the def block.  Point is left at the start of the
def, except that: if the def is preceded by a number of comment lines
followed by (at most) one optional blank line, point is left at the
start of the comments; else if the def is preceded by a blank line,
point is left at its start.

The intent is to mark the containing def/class and its associated
documentation, to make moving and duplicating functions and classes
pleasant."
  (interactive "P")                     ; raw prefix arg
  (let ((start (point))
        (which (cond ((eq class 'either) "\\(class\\|def\\)")
                     (class "class")
                     (t "def"))))
    (push-mark start)
    (if (not (pysrt-go-up-tree-to-keyword which))
        (progn (goto-char start)
               (error "Enclosing %s not found"
                      (if (eq class 'either)
                          "def or class"
                        which)))
      ;; else enclosing def/class found
      (setq start (point))
      (pysrt-goto-beyond-block)
      (push-mark (point))
      (goto-char start)
      (if (zerop (forward-line -1))     ; if there is a preceding line
          (progn
            (if (looking-at "[ \t]*$")  ; it's blank
                (setq start (point))    ; so reset start point
              (goto-char start))        ; else try again
            (if (zerop (forward-line -1))
                (if (looking-at "[ \t]*#") ; a comment
                    ;; look back for non-comment line
                    ;; tricky: note that the regexp matches a blank
                    ;; line, cuz \n is in the 2nd character class
                    (and
                     (re-search-backward "^[ \t]*[^ \t#]" nil 'move)
                     (forward-line 1))
                  ;; no comment, so go back
                  (goto-char start)))))))
  (exchange-point-and-mark)
  (pysrt-keep-region-active))

;; ripped from cc-mode
(defun pysrt-forward-into-nomenclature (&optional arg)
  "Move forward to end of a nomenclature section or word.
With \\[universal-argument] (programmatically, optional argument ARG),
do it that many times.

A `nomenclature' is a fancy way of saying AWordWithMixedCaseNotUnderscores."
  (interactive "p")
  (let ((case-fold-search nil))
    (if (> arg 0)
        (re-search-forward
         "\\(\\W\\|[_]\\)*\\([A-Z]*[a-z0-9]*\\)"
         (point-max) t arg)
      (while (and (< arg 0)
                  (re-search-backward
                   "\\(\\W\\|[a-z0-9]\\)[A-Z]+\\|\\(\\W\\|[_]\\)\\w+"
                   (point-min) 0))
        (forward-char 1)
        (setq arg (1+ arg)))))
  (pysrt-keep-region-active))

(defun pysrt-backward-into-nomenclature (&optional arg)
  "Move backward to beginning of a nomenclature section or word.
With optional ARG, move that many times.  If ARG is negative, move
forward.

A `nomenclature' is a fancy way of saying AWordWithMixedCaseNotUnderscores."
  (interactive "p")
  (pysrt-forward-into-nomenclature (- arg))
  (pysrt-keep-region-active))



;; pdbtrack functions
(defun pysrt-pdbtrack-toggle-stack-tracking (arg)
  (interactive "P")
  (if (not (get-buffer-process (current-buffer)))
      (error "No process associated with buffer '%s'" (current-buffer)))
  ;; missing or 0 is toggle, >0 turn on, <0 turn off
  (if (or (not arg)
          (zerop (setq arg (prefix-numeric-value arg))))
      (setq pysrt-pdbtrack-do-tracking-p (not pysrt-pdbtrack-do-tracking-p))
    (setq pysrt-pdbtrack-do-tracking-p (> arg 0)))
  (message "%sabled Srt's pdbtrack"
           (if pysrt-pdbtrack-do-tracking-p "En" "Dis")))

(defun turn-on-pdbtrack ()
  (interactive)
  (pysrt-pdbtrack-toggle-stack-tracking 1))

(defun turn-off-pdbtrack ()
  (interactive)
  (pysrt-pdbtrack-toggle-stack-tracking 0))



;; Pychecker

;; hack for FSF Emacs
(unless (fboundp 'read-shell-command)
  (defalias 'read-shell-command 'read-string))

(defun pysrt-pychecker-run (command)
  "*Run pychecker (default on the file currently visited)."
  (interactive
   (let ((default
           (format "%s %s %s" pysrt-pychecker-command
                   (mapconcat 'identity pysrt-pychecker-command-args " ")
                   (buffer-file-name)))
         (last (when pysrt-pychecker-history
                 (let* ((lastcmd (car pysrt-pychecker-history))
                        (cmd (cdr (reverse (split-string lastcmd))))
                        (newcmd (reverse (cons (buffer-file-name) cmd))))
                   (mapconcat 'identity newcmd " ")))))

     (list
      (if (fboundp 'read-shell-command)
          (read-shell-command "Run pychecker like this: "
                              (if last
                                  last
                                default)
                              'pysrt-pychecker-history)
        (read-string "Run pychecker like this: "
                     (if last
                         last
                       default)
                     'pysrt-pychecker-history))
	  )))
  (save-some-buffers (not pysrt-ask-about-save) nil)
  (if (fboundp 'compilation-start)
      ;; Emacs.
      (compilation-start command)))



;; pydoc commands. The guts of this function is stolen from XEmacs's
;; symbol-near-point, but without the useless regexp-quote call on the
;; results, nor the interactive bit.  Also, we've added the temporary
;; syntax table setting, which Skip originally had broken out into a
;; separate function.  Note that Emacs doesn't have the original
;; function.
(defun pysrt-symbol-near-point ()
  "Return the first textual item to the nearest point."
  ;; alg stolen from etag.el
  (save-excursion
    (with-syntax-table pysrt-dotted-expression-syntax-table
      (if (or (bobp) (not (memq (char-syntax (char-before)) '(?w ?_))))
          (while (not (looking-at "\\sw\\|\\s_\\|\\'"))
            (forward-char 1)))
      (while (looking-at "\\sw\\|\\s_")
        (forward-char 1))
      (if (re-search-backward "\\sw\\|\\s_" nil t)
          (progn (forward-char 1)
                 (buffer-substring (point)
                                   (progn (forward-sexp -1)
                                          (while (looking-at "\\s'")
                                            (forward-char 1))
                                          (point))))
        nil))))



;; Documentation functions

;; dump the long form of the mode blurb; does the usual doc escapes,
;; plus lines of the form ^[vc]:name$ to suck variable & command docs
;; out of the right places, along with the keys they're on & current
;; values
(defun pysrt-dump-help-string (str)
  (with-output-to-temp-buffer "*Help*"
    (let ((locals (buffer-local-variables))
          funckind funcname func funcdoc
          (start 0) mstart end
          keys )
      (while (string-match "^%\\([vc]\\):\\(.+\\)\n" str start)
        (setq mstart (match-beginning 0)  end (match-end 0)
              funckind (substring str (match-beginning 1) (match-end 1))
              funcname (substring str (match-beginning 2) (match-end 2))
              func (intern funcname))
        (princ (substitute-command-keys (substring str start mstart)))
        (cond
         ((equal funckind "c")          ; command
          (setq funcdoc (documentation func)
                keys (concat
                      "Key(s): "
                      (mapconcat 'key-description
                                 (where-is-internal func pysrt-mode-map)
                                 ", "))))
         ((equal funckind "v")          ; variable
          (setq funcdoc (documentation-property func 'variable-documentation)
                keys (if (assq func locals)
                         (concat
                          "Local/Global values: "
                          (prin1-to-string (symbol-value func))
                          " / "
                          (prin1-to-string (default-value func)))
                       (concat
                        "Value: "
                        (prin1-to-string (symbol-value func))))))
         (t                             ; unexpected
          (error "Error in pysrt-dump-help-string, tag `%s'" funckind)))
        (princ (format "\n-> %s:\t%s\t%s\n\n"
                       (if (equal funckind "c") "Command" "Variable")
                       funcname keys))
        (princ funcdoc)
        (terpri)
        (setq start end))
      (princ (substitute-command-keys (substring str start))))
    (print-help-return-message)))

(defun pysrt-describe-mode ()
  "Dump long form of Pysrt-mode docs."
  (interactive)
  (pysrt-dump-help-string "Major mode for editing Srt files.
Knows about Srt indentation, tokens, comments and continuation lines.
Paragraphs are separated by blank lines only.

Major sections below begin with the string `@'; specific function and
variable docs begin with `->'.

@EXECUTING SRT CODE

\\[py-execute-import-or-reload]\timports or reloads the file in the Srt interpreter
\\[py-execute-buffer]\tsends the entire buffer to the Srt interpreter
\\[py-execute-region]\tsends the current region
\\[py-execute-def-or-class]\tsends the current function or class definition
\\[py-execute-string]\tsends an arbitrary string
\\[py-shell]\tstarts a Srt interpreter window; this will be used by
\tsubsequent Srt execution commands
%c:py-execute-import-or-reload
%c:py-execute-buffer
%c:py-execute-region
%c:py-execute-def-or-class
%c:py-execute-string
%c:py-shell

@VARIABLES

py-indent-offset\tindentation increment
py-block-comment-prefix\tcomment string used by comment-region

py-pysrt-command\tshell command to invoke Srt interpreter
py-temp-directory\tdirectory used for temp files (if needed)

py-beep-if-tab-change\tring the bell if tab-width is changed
%v:py-indent-offset
%v:py-block-comment-prefix
%v:py-pysrt-command
%v:py-temp-directory
%v:py-beep-if-tab-change

@KINDS OF LINES

Each physical line in the file is either a `continuation line' (the
preceding line ends with a backslash that's not part of a comment, or
the paren/bracket/brace nesting level at the start of the line is
non-zero, or both) or an `initial line' (everything else).

An initial line is in turn a `blank line' (contains nothing except
possibly blanks or tabs), a `comment line' (leftmost non-blank
character is `#'), or a `code line' (everything else).

Comment Lines

Although all comment lines are treated alike by Srt, Srt mode
recognizes two kinds that act differently with respect to indentation.

An `indenting comment line' is a comment line with a blank, tab or
nothing after the initial `#'.  The indentation commands (see below)
treat these exactly as if they were code lines: a line following an
indenting comment line will be indented like the comment line.  All
other comment lines (those with a non-whitespace character immediately
following the initial `#') are `non-indenting comment lines', and
their indentation is ignored by the indentation commands.

Indenting comment lines are by far the usual case, and should be used
whenever possible.  Non-indenting comment lines are useful in cases
like these:

\ta = b   # a very wordy single-line comment that ends up being
\t        #... continued onto another line

\tif a == b:
##\t\tprint 'panic!' # old code we've `commented out'
\t\treturn a

Since the `#...' and `##' comment lines have a non-whitespace
character following the initial `#', Srt mode ignores them when
computing the proper indentation for the next line.

Continuation Lines and Statements

The Pysrt-mode commands generally work on statements instead of on
individual lines, where a `statement' is a comment or blank line, or a
code line and all of its following continuation lines (if any)
considered as a single logical unit.  The commands in this mode
generally (when it makes sense) automatically move to the start of the
statement containing point, even if point happens to be in the middle
of some continuation line.


@INDENTATION

Primarily for entering new code:
\t\\[indent-for-tab-command]\t indent line appropriately
\t\\[py-newline-and-indent]\t insert newline, then indent
\t\\[py-electric-backspace]\t reduce indentation, or delete single character

Primarily for reindenting existing code:
\t\\[py-guess-indent-offset]\t guess pysrt-indent-offset from file content; change locally
\t\\[universal-argument] \\[py-guess-indent-offset]\t ditto, but change globally

\t\\[py-indent-region]\t reindent region to match its context
\t\\[py-shift-region-left]\t shift region left by pysrt-indent-offset
\t\\[py-shift-region-right]\t shift region right by pysrt-indent-offset

Unlike most programming languages, Srt uses indentation, and only
indentation, to specify block structure.  Hence the indentation supplied
automatically by Pysrt-mode is just an educated guess:  only you know
the block structure you intend, so only you can supply correct
indentation.

The \\[indent-for-tab-command] and \\[py-newline-and-indent] keys try to suggest plausible indentation, based on
the indentation of preceding statements.  E.g., assuming
py-indent-offset is 4, after you enter
\tif a > 0: \\[py-newline-and-indent]
the cursor will be moved to the position of the `_' (_ is not a
character in the file, it's just used here to indicate the location of
the cursor):
\tif a > 0:
\t    _
If you then enter `c = d' \\[py-newline-and-indent], the cursor will move
to
\tif a > 0:
\t    c = d
\t    _
Pysrt-mode cannot know whether that's what you intended, or whether
\tif a > 0:
\t    c = d
\t_
was your intent.  In general, Pysrt-mode either reproduces the
indentation of the (closest code or indenting-comment) preceding
statement, or adds an extra pysrt-indent-offset blanks if the preceding
statement has `:' as its last significant (non-whitespace and non-
comment) character.  If the suggested indentation is too much, use
\\[py-electric-backspace] to reduce it.

Continuation lines are given extra indentation.  If you don't like the
suggested indentation, change it to something you do like, and Pysrt-
mode will strive to indent later lines of the statement in the same way.

If a line is a continuation line by virtue of being in an unclosed
paren/bracket/brace structure (`list', for short), the suggested
indentation depends on whether the current line contains the first item
in the list.  If it does, it's indented pysrt-indent-offset columns beyond
the indentation of the line containing the open bracket.  If you don't
like that, change it by hand.  The remaining items in the list will mimic
whatever indentation you give to the first item.

If a line is a continuation line because the line preceding it ends with
a backslash, the third and following lines of the statement inherit their
indentation from the line preceding them.  The indentation of the second
line in the statement depends on the form of the first (base) line:  if
the base line is an assignment statement with anything more interesting
than the backslash following the leftmost assigning `=', the second line
is indented two columns beyond that `='.  Else it's indented to two
columns beyond the leftmost solid chunk of non-whitespace characters on
the base line.

Warning:  indent-region should not normally be used!  It calls \\[indent-for-tab-command]
repeatedly, and as explained above, \\[indent-for-tab-command] can't guess the block
structure you intend.
%c:indent-for-tab-command
%c:py-newline-and-indent
%c:py-electric-backspace


The next function may be handy when editing code you didn't write:
%c:py-guess-indent-offset


The remaining `indent' functions apply to a region of Srt code.  They
assume the block structure (equals indentation, in Srt) of the region
is correct, and alter the indentation in various ways while preserving
the block structure:
%c:py-indent-region
%c:py-shift-region-left
%c:py-shift-region-right

@MARKING & MANIPULATING REGIONS OF CODE

\\[py-mark-block]\t mark block of lines
\\[py-mark-def-or-class]\t mark smallest enclosing def
\\[universal-argument] \\[py-mark-def-or-class]\t mark smallest enclosing class
\\[comment-region]\t comment out region of code
\\[universal-argument] \\[comment-region]\t uncomment region of code
%c:py-mark-block
%c:py-mark-def-or-class
%c:comment-region

@MOVING POINT

\\[py-previous-statement]\t move to statement preceding point
\\[py-next-statement]\t move to statement following point
\\[py-goto-block-up]\t move up to start of current block
\\[py-beginning-of-def-or-class]\t move to start of def
\\[universal-argument] \\[py-beginning-of-def-or-class]\t move to start of class
\\[py-end-of-def-or-class]\t move to end of def
\\[universal-argument] \\[py-end-of-def-or-class]\t move to end of class

The first two move to one statement beyond the statement that contains
point.  A numeric prefix argument tells them to move that many
statements instead.  Blank lines, comment lines, and continuation lines
do not count as `statements' for these commands.  So, e.g., you can go
to the first code statement in a file by entering
\t\\[beginning-of-buffer]\t to move to the top of the file
\t\\[py-next-statement]\t to skip over initial comments and blank lines
Or do `\\[py-previous-statement]' with a huge prefix argument.
%c:py-previous-statement
%c:py-next-statement
%c:py-goto-block-up
%c:py-beginning-of-def-or-class
%c:py-end-of-def-or-class

@LITTLE-KNOWN EMACS COMMANDS PARTICULARLY USEFUL IN SRT MODE

`\\[indent-new-comment-line]' is handy for entering a multi-line comment.

`\\[set-selective-display]' with a `small' prefix arg is ideally suited for viewing the
overall class and def structure of a module.

`\\[back-to-indentation]' moves point to a line's first non-blank character.

`\\[indent-relative]' is handy for creating odd indentation.

@OTHER EMACS HINTS

If you don't like the default value of a variable, change its value to
whatever you do like by putting a `setq' line in your .emacs file.
E.g., to set the indentation increment to 4, put this line in your
.emacs:
\t(setq  pysrt-indent-offset  4)
To see the value of a variable, do `\\[describe-variable]' and enter the variable
name at the prompt.

When entering a key sequence like `C-c C-n', it is not necessary to
release the CONTROL key after doing the `C-c' part -- it suffices to
press the CONTROL key, press and release `c' (while still holding down
CONTROL), press and release `n' (while still holding down CONTROL), &
then release CONTROL.

Entering Srt mode calls with no arguments the value of the variable
`pysrt-mode-hook', if that value exists and is not nil; for backward
compatibility it also tries `py-mode-hook'; see the `Hooks' section of
the Elisp manual for details.

Obscure:  When pysrt-mode is first loaded, it looks for all bindings
to newline-and-indent in the global keymap, and shadows them with
local bindings to pysrt-newline-and-indent."))

(require 'info-look)
;; The info-look package does not always provide this function (it
;; appears this is the case with XEmacs 21.1)
(when (fboundp 'info-lookup-maybe-add-help)
  (info-lookup-maybe-add-help
   :mode 'pysrt-mode
   :regexp "[a-zA-Z0-9_]+"
   :doc-spec '(("(pysrt-lib)Module Index")
               ("(pysrt-lib)Class-Exception-Object Index")
               ("(pysrt-lib)Function-Method-Variable Index")
               ("(pysrt-lib)Miscellaneous Index")))
  )


;; Helper functions
(defvar pysrt-parse-state-re
  (concat
   "^[ \t]*\\(elif\\|else\\|while\\|def\\|class\\)\\>"
   "\\|"
   "^[^ #\t\n]"))

(defun pysrt-parse-state ()
  "Return the parse state at point (see `parse-partial-sexp' docs)."
  (save-excursion
    (let ((here (point))
          pps done)
      (while (not done)
        ;; back up to the first preceding line (if any; else start of
        ;; buffer) that begins with a popular Srt keyword, or a
        ;; non- whitespace and non-comment character.  These are good
        ;; places to start parsing to see whether where we started is
        ;; at a non-zero nesting level.  It may be slow for people who
        ;; write huge code blocks or huge lists ... tough beans.
        (re-search-backward pysrt-parse-state-re nil 'move)
        (beginning-of-line)
        ;; In XEmacs, we have a much better way to test for whether
        ;; we're in a triple-quoted string or not.  Emacs does not
        ;; have this built-in function, which is its loss because
        ;; without scanning from the beginning of the buffer, there's
        ;; no accurate way to determine this otherwise.
        (save-excursion (setq pps (parse-partial-sexp (point) here)))
        ;; make sure we don't land inside a triple-quoted string
        (setq done (or (not (nth 3 pps))
                       (bobp)))
        ;; Just go ahead and short circuit the test back to the
        ;; beginning of the buffer.  This will be slow, but not
        ;; nearly as slow as looping through many
        ;; re-search-backwards.
        (if (not done)
            (goto-char (point-min))))
      pps)))

(defun pysrt-nesting-level ()
  "Return the buffer position of the last unclosed enclosing list.
If nesting level is zero, return nil."
  (let ((status (pysrt-parse-state)))
    (if (zerop (car status))
        nil                             ; not in a nest
      (car (cdr status)))))             ; char# of open bracket

(defun pysrt-backslash-continuation-line-p ()
  "Return t iff preceding line ends with backslash that is not in a comment."
  (save-excursion
    (beginning-of-line)
    (and
     ;; use a cheap test first to avoid the regexp if possible
     ;; use 'eq' because char-after may return nil
     (eq (char-after (- (point) 2)) ?\\ )
     ;; make sure; since eq test passed, there is a preceding line
     (forward-line -1)                  ; always true -- side effect
     (looking-at pysrt-continued-re))))

(defun pysrt-continuation-line-p ()
  "Return t iff current line is a continuation line."
  (save-excursion
    (beginning-of-line)
    (or (pysrt-backslash-continuation-line-p)
        (pysrt-nesting-level))))

(defun pysrt-goto-beginning-of-tqs (delim)
  "Go to the beginning of the triple quoted string we find ourselves in.
DELIM is the TQS string delimiter character we're searching backwards
for."
  (let ((skip (and delim (make-string 1 delim)))
        (continue t))
    (when skip
      (save-excursion
        (while continue
          (pysrt-safe (search-backward skip))
          (setq continue (and (not (bobp))
                              (= (char-before) ?\\))))
        (if (and (= (char-before) delim)
                 (= (char-before (1- (point))) delim))
            (setq skip (make-string 3 delim))))
      ;; we're looking at a triple-quoted string
      (pysrt-safe (search-backward skip)))))

(defun pysrt-goto-initial-line ()
  "Go to the initial line of the current statement.
Usually this is the line we're on, but if we're on the 2nd or
following lines of a continuation block, we need to go up to the first
line of the block."
  ;; Tricky: We want to avoid quadratic-time behavior for long
  ;; continued blocks, whether of the backslash or open-bracket
  ;; varieties, or a mix of the two.  The following manages to do that
  ;; in the usual cases.
  ;;
  ;; Also, if we're sitting inside a triple quoted string, this will
  ;; drop us at the line that begins the string.
  (let (open-bracket-pos)
    (while (pysrt-continuation-line-p)
      (beginning-of-line)
      (if (pysrt-backslash-continuation-line-p)
          (while (pysrt-backslash-continuation-line-p)
            (forward-line -1))
        ;; else zip out of nested brackets/braces/parens
        (while (setq open-bracket-pos (pysrt-nesting-level))
          (goto-char open-bracket-pos)))))
  (beginning-of-line))

(defun pysrt-goto-beyond-final-line ()
  "Go to the point just beyond the fine line of the current statement.
Usually this is the start of the next line, but if this is a
multi-line statement we need to skip over the continuation lines."
  ;; Tricky: Again we need to be clever to avoid quadratic time
  ;; behavior.
  ;;
  ;; XXX: Not quite the right solution, but deals with multi-line doc
  ;; strings
  (if (looking-at (concat "[ \t]*\\(" pysrt-stringlit-re "\\)"))
      (goto-char (match-end 0)))
  ;;
  (forward-line 1)
  (let (state)
    (while (and (pysrt-continuation-line-p)
                (not (eobp)))
      ;; skip over the backslash flavor
      (while (and (pysrt-backslash-continuation-line-p)
                  (not (eobp)))
        (forward-line 1))
      ;; if in nest, zip to the end of the nest
      (setq state (pysrt-parse-state))
      (if (and (not (zerop (car state)))
               (not (eobp)))
          (progn
            (parse-partial-sexp (point) (point-max) 0 nil state)
            (forward-line 1))))))

(defun pysrt-statement-opens-block-p ()
  "Return t iff the current statement opens a block.
I.e., iff it ends with a colon that is not in a comment.  Point should
be at the start of a statement."
  (save-excursion
    (let ((start (point))
          (finish (progn (pysrt-goto-beyond-final-line) (1- (point))))
          (searching t)
          (answer nil)
          state)
      (goto-char start)
      (while searching
        ;; look for a colon with nothing after it except whitespace, and
        ;; maybe a comment
        (if (re-search-forward ":\\([ \t]\\|\\\\\n\\)*\\(#.*\\)?$"
                               finish t)
            (if (eq (point) finish)     ; note: no `else' clause; just
                                        ; keep searching if we're not at
                                        ; the end yet
                ;; sure looks like it opens a block -- but it might
                ;; be in a comment
                (progn
                  (setq searching nil)  ; search is done either way
                  (setq state (parse-partial-sexp start
                                                  (match-beginning 0)))
                  (setq answer (not (nth 4 state)))))
          ;; search failed: couldn't find another interesting colon
          (setq searching nil)))
      answer)))

(defun pysrt-statement-closes-block-p ()
  "Return t iff the current statement closes a block.
I.e., if the line starts with `return', `raise', `break', `continue',
and `pass'.  This doesn't catch embedded statements."
  (let ((here (point)))
    (pysrt-goto-initial-line)
    (back-to-indentation)
    (prog1
        (looking-at (concat pysrt-block-closing-keywords-re "\\>"))
      (goto-char here))))

(defun pysrt-goto-beyond-block ()
  "Go to point just beyond the final line of block begun by the current line.
This is the same as where `py-goto-beyond-final-line' goes unless
we're on colon line, in which case we go to the end of the block.
Assumes point is at the beginning of the line."
  (if (pysrt-statement-opens-block-p)
      (pysrt-mark-block nil 'just-move)
    (pysrt-goto-beyond-final-line)))

(defun pysrt-goto-statement-at-or-above ()
  "Go to the start of the first statement at or preceding point.
Return t if there is such a statement, otherwise nil.  `Statement'
does not include blank lines, comments, or continuation lines."
  (pysrt-goto-initial-line)
  (if (looking-at pysrt-blank-or-comment-re)
      ;; skip back over blank & comment lines
      ;; note:  will skip a blank or comment line that happens to be
      ;; a continuation line too
      (if (re-search-backward "^[ \t]*[^ \t#\n]" nil t)
          (progn (pysrt-goto-initial-line) t)
        nil)
    t))

(defun pysrt-goto-statement-below ()
  "Go to start of the first statement following the statement containing point.
Return t if there is such a statement, otherwise nil.  `Statement'
does not include blank lines, comments, or continuation lines."
  (beginning-of-line)
  (let ((start (point)))
    (pysrt-goto-beyond-final-line)
    (while (and
            (or (looking-at pysrt-blank-or-comment-re)
                (pysrt-in-literal))
            (not (eobp)))
      (forward-line 1))
    (if (eobp)
        (progn (goto-char start) nil)
      t)))

(defun pysrt-go-up-tree-to-keyword (key)
  "Go to begining of statement starting with KEY, at or preceding point.

KEY is a regular expression describing a Srt keyword.  Skip blank
lines and non-indenting comments.  If the statement found starts with
KEY, then stop, otherwise go back to first enclosing block starting
with KEY.  If successful, leave point at the start of the KEY line and
return t.  Otherwise, leave point at an undefined place and return nil."
  ;; skip blanks and non-indenting #
  (pysrt-goto-initial-line)
  (while (and
          (looking-at "[ \t]*\\($\\|#[^ \t\n]\\)")
          (zerop (forward-line -1)))    ; go back
    nil)
  (pysrt-goto-initial-line)
  (let* ((re (concat "[ \t]*" key "\\>"))
         (case-fold-search nil)         ; let* so looking-at sees this
         (found (looking-at re))
         (dead nil))
    (while (not (or found dead))
      (condition-case nil               ; in case no enclosing block
          (pysrt-goto-block-up 'no-mark)
        (error (setq dead t)))
      (or dead (setq found (looking-at re))))
    (beginning-of-line)
    found))

(defun pysrt-suck-up-leading-text ()
  "Return string in buffer from start of indentation to end of line.
Prefix with \"...\" if leading whitespace was skipped."
  (save-excursion
    (back-to-indentation)
    (concat
     (if (bolp) "" "...")
     (buffer-substring (point) (progn (end-of-line) (point))))))

(defun pysrt-suck-up-first-keyword ()
  "Return first keyword on the line as a Lisp symbol.
`Keyword' is defined (essentially) as the regular expression
([a-z]+).  Returns nil if none was found."
  (let ((case-fold-search nil))
    (if (looking-at "[ \t]*\\([a-z]+\\)\\>")
        (intern (buffer-substring (match-beginning 1) (match-end 1)))
      nil)))

(defun pysrt-current-defun ()
  "Srt value for `add-log-current-defun-function'.
This tells add-log.el how to find the current function/method/variable."
  (save-excursion

    ;; Move back to start of the current statement.

    (pysrt-goto-initial-line)
    (back-to-indentation)
    (while (and (or (looking-at pysrt-blank-or-comment-re)
                    (pysrt-in-literal))
                (not (bobp)))
      (backward-to-indentation 1))
    (pysrt-goto-initial-line)

    (let ((scopes "")
          (sep "")
          dead assignment)

      ;; Check for an assignment.  If this assignment exists inside a
      ;; def, it will be overwritten inside the while loop.  If it
      ;; exists at top lever or inside a class, it will be preserved.

      (when (looking-at "[ \t]*\\([a-zA-Z0-9_]+\\)[ \t]*=")
        (setq scopes (buffer-substring (match-beginning 1) (match-end 1)))
        (setq assignment t)
        (setq sep "."))

      ;; Prepend the name of each outer socpe (def or class).

      (while (not dead)
        (if (and (pysrt-go-up-tree-to-keyword "\\(class\\|def\\)")
                 (looking-at
                  "[ \t]*\\(class\\|def\\)[ \t]*\\([a-zA-Z0-9_]+\\)[ \t]*"))
            (let ((name (buffer-substring (match-beginning 2) (match-end 2))))
              (if (and assignment (looking-at "[ \t]*def"))
                  (setq scopes name)
                (setq scopes (concat name sep scopes))
                (setq sep "."))))
        (setq assignment nil)
        (condition-case nil             ; Terminate nicely at top level.
            (pysrt-goto-block-up 'no-mark)
          (error (setq dead t))))
      (if (string= scopes "")
          nil
        scopes))))



(defconst pysrt-help-address "pysrt-mode@srt.org"
  "Address accepting submission of bug reports.")

(defun pysrt-version ()
  "Echo the current version of `pysrt-mode' in the minibuffer."
  (interactive)
  (message "Using `pysrt-mode' version %s" pysrt-version)
  (pysrt-keep-region-active))

;; only works under Emacs 19
										;(eval-when-compile
										;  (require 'reporter))

(defun pysrt-submit-bug-report (enhancement-p)
  "Submit via mail a bug report on `pysrt-mode'.
With \\[universal-argument] (programmatically, argument ENHANCEMENT-P
non-nil) just submit an enhancement request."
  (interactive
   (list (not (y-or-n-p
               "Is this a bug report (hit `n' to send other comments)? "))))
  (let ((reporter-prompt-for-summary-p (if enhancement-p
                                           "(Very) brief summary: "
                                         t)))
    (require 'reporter)
    (reporter-submit-bug-report
     pysrt-help-address                    ;address
     (concat "pysrt-mode " pysrt-version) ;pkgname
     ;; varlist
     (if enhancement-p nil
       '(pysrt-pysrt-command
         pysrt-indent-offset
         pysrt-block-comment-prefix
         pysrt-temp-directory
         pysrt-beep-if-tab-change))
     nil                                ;pre-hooks
     nil                                ;post-hooks
     "Dear Barry,")                     ;salutation
    (if enhancement-p nil
      (set-mark (point))
      (insert
	   "Please replace this text with a sufficiently large code sample\n\
and an exact recipe so that I can reproduce your problem.  Failure\n\
to do so may mean a greater delay in fixing your bug.\n\n")
      (exchange-point-and-mark)
      (pysrt-keep-region-active))))


(defun pysrt-kill-emacs-hook ()
  "Delete files in `py-file-queue'.
These are Srt temporary files awaiting execution."
  (mapc #'(lambda (filename)
            (pysrt-safe (delete-file filename)))
        pysrt-file-queue))

;; arrange to kill temp files when Emacs exists
(add-hook 'kill-emacs-hook 'pysrt-kill-emacs-hook)
(add-hook 'comint-output-filter-functions 'pysrt-pdbtrack-track-stack-file)

;; Add a designator to the minor mode strings
(or (assq 'pysrt-pdbtrack-is-tracking-p minor-mode-alist)
    (push '(pysrt-pdbtrack-is-tracking-p pysrt-pdbtrack-minor-mode-string)
          minor-mode-alist))



;;; paragraph and string filling code from Bernhard Herzog
;;; see http://mail.srt.org/pipermail/pysrt-list/2002-May/103189.html

(defun pysrt-fill-comment (&optional justify)
  "Fill the comment paragraph around point"
  (let (;; Non-nil if the current line contains a comment.
        has-comment

        ;; If has-comment, the appropriate fill-prefix for the comment.
        comment-fill-prefix)

    ;; Figure out what kind of comment we are looking at.
    (save-excursion
      (beginning-of-line)
      (cond
       ;; A line with nothing but a comment on it?
       ((looking-at "[ \t]*#[# \t]*")
        (setq has-comment t
              comment-fill-prefix (buffer-substring (match-beginning 0)
                                                    (match-end 0))))

       ;; A line with some code, followed by a comment? Remember that the hash
       ;; which starts the comment shouldn't be part of a string or character.
       ((progn
          (while (not (looking-at "#\\|$"))
            (skip-chars-forward "^#\n\"'\\")
            (cond
             ((eq (char-after (point)) ?\\) (forward-char 2))
             ((memq (char-after (point)) '(?\" ?')) (forward-sexp 1))))
          (looking-at "#+[\t ]*"))
        (setq has-comment t)
        (setq comment-fill-prefix
              (concat (make-string (current-column) ? )
                      (buffer-substring (match-beginning 0) (match-end 0)))))))

    (if (not has-comment)
        (fill-paragraph justify)

      ;; Narrow to include only the comment, and then fill the region.
      (save-restriction
        (narrow-to-region

         ;; Find the first line we should include in the region to fill.
         (save-excursion
           (while (and (zerop (forward-line -1))
                       (looking-at "^[ \t]*#")))

           ;; We may have gone to far.  Go forward again.
           (or (looking-at "^[ \t]*#")
               (forward-line 1))
           (point))

         ;; Find the beginning of the first line past the region to fill.
         (save-excursion
           (while (progn (forward-line 1)
                         (looking-at "^[ \t]*#")))
           (point)))

        ;; Lines with only hashes on them can be paragraph boundaries.
        (let ((paragraph-start (concat paragraph-start "\\|[ \t#]*$"))
              (paragraph-separate (concat paragraph-separate "\\|[ \t#]*$"))
              (fill-prefix comment-fill-prefix))
          ;;(message "paragraph-start %S paragraph-separate %S"
          ;;paragraph-start paragraph-separate)
          (fill-paragraph justify))))
    t))


(defun pysrt-fill-string (start &optional justify)
  "Fill the paragraph around (point) in the string starting at start"
  ;; basic strategy: narrow to the string and call the default
  ;; implementation
  (let (;; the start of the string's contents
        string-start
        ;; the end of the string's contents
        string-end
        ;; length of the string's delimiter
        delim-length
        ;; The string delimiter
        delim
        )

    (save-excursion
      (goto-char start)
      (if (looking-at "\\('''\\|\"\"\"\\|'\\|\"\\)\\\\?\n?")
          (setq string-start (match-end 0)
                delim-length (- (match-end 1) (match-beginning 1))
                delim (buffer-substring-no-properties (match-beginning 1)
                                                      (match-end 1)))
        (error "The parameter start is not the beginning of a srt string"))

      ;; if the string is the first token on a line and doesn't start with
      ;; a newline, fill as if the string starts at the beginning of the
      ;; line. this helps with one line docstrings
      (save-excursion
        (beginning-of-line)
        (and (/= (char-before string-start) ?\n)
             (looking-at (concat "[ \t]*" delim))
             (setq string-start (point))))

      (forward-sexp (if (= delim-length 3) 2 1))

      ;; with both triple quoted strings and single/double quoted strings
      ;; we're now directly behind the first char of the end delimiter
      ;; (this doesn't work correctly when the triple quoted string
      ;; contains the quote mark itself). The end of the string's contents
      ;; is one less than point
      (setq string-end (1- (point))))

    ;; Narrow to the string's contents and fill the current paragraph
    (save-restriction
      (narrow-to-region string-start string-end)
      (let ((ends-with-newline (= (char-before (point-max)) ?\n)))
        (fill-paragraph justify)
        (if (and (not ends-with-newline)
                 (= (char-before (point-max)) ?\n))
            ;; the default fill-paragraph implementation has inserted a
            ;; newline at the end. Remove it again.
            (save-excursion
              (goto-char (point-max))
              (delete-char -1)))))

    ;; return t to indicate that we've done our work
    t))

(defun pysrt-fill-paragraph (&optional justify)
  "Like \\[fill-paragraph], but handle Srt comments and strings.
If any of the current line is a comment, fill the comment or the
paragraph of it that point is in, preserving the comment's indentation
and initial `#'s.
If point is inside a string, narrow to that string and fill.
"
  (interactive "P")
  ;; fill-paragraph will narrow incorrectly
  (save-restriction
    (widen)
    (let* ((bod (pysrt-point 'bod))
           (pps (parse-partial-sexp bod (point))))
      (cond
       ;; are we inside a comment or on a line with only whitespace before
       ;; the comment start?
       ((or (nth 4 pps)
            (save-excursion (beginning-of-line) (looking-at "[ \t]*#")))
        (pysrt-fill-comment justify))
       ;; are we inside a string?
       ((nth 3 pps)
        (pysrt-fill-string (nth 8 pps)))
       ;; are we at the opening quote of a string, or in the indentation?
       ((save-excursion
          (forward-word 1)
          (eq (pysrt-in-literal) 'string))
        (save-excursion
          (pysrt-fill-string (pysrt-point 'boi))))
       ;; are we at or after the closing quote of a string?
       ((save-excursion
          (backward-word 1)
          (eq (pysrt-in-literal) 'string))
        (save-excursion
          (pysrt-fill-string (pysrt-point 'boi))))
       ;; otherwise use the default
       (t
        (fill-paragraph justify))))))



(provide 'pysrt-mode)
;;; pysrt-mode.el ends here
