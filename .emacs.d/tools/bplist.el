;;; bplist.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yingchao.yang@icloud.com>

;;; Commentary:

;;; Code:

(require 'bindat)

;; constant variables.
(defconst bpl/MAGIC "bplist")
(defconst bpl/VERSION "00" )
(defconst bpl/NULL #x00 )
(defconst bpl/FALSE #x08 )
(defconst bpl/TRUE #x09 )
(defconst bpl/FILL #x0F )
(defconst bpl/UINT #x10 )
(defconst bpl/REAL #x20 )
(defconst bpl/DATE #x30 )
(defconst bpl/DATA #x40 )
(defconst bpl/STRING #x50 )
(defconst bpl/UNICODE #x60 )
(defconst bpl/UNK_x70 #x70 )
(defconst bpl/UID #x80 )
(defconst bpl/ARRAY #xA0 )
(defconst bpl/SET #xC0 )
(defconst bpl/DICT #xD0 )
(defconst bpl/MASK #xF0 )

;; Hash tables for constant description and parser/dumper...
(defun bpl/get-type-symbol (type)
  "Return description of TYPE."
  (or   (alist-get type
                   (list
                    (cons bpl/MAGIC  'MAGIC)
                    (cons bpl/VERSION  'VERSION)
                    (cons bpl/NULL  'NULL)
                    (cons bpl/FALSE  'FALSE)
                    (cons bpl/TRUE  'TRUE)
                    (cons bpl/FILL  'FILL)
                    (cons bpl/UINT  'UINT)
                    (cons bpl/REAL  'REAL)
                    (cons bpl/DATE  'DATE)
                    (cons bpl/DATA  'DATA)
                    (cons bpl/STRING  'STRING)
                    (cons bpl/UNICODE  'UNICODE)
                    (cons bpl/UNK_x70  'UNK_x70)
                    (cons bpl/UID  'UID)
                    (cons bpl/ARRAY  'ARRAY)
                    (cons bpl/SET  'SET)
                    (cons bpl/DICT  'DICT)
                    (cons bpl/MASK  'MASK)))
        'Unkown))

(defun bpl/get-type-name (type)
  "Return description of TYPE."
  (symbol-name (bpl/get-type-symbol type)))

(defun bpl/get-type-parser (type)
  "Return description of TYPE."
  (alist-get type
             (list
              (cons bpl/MAGIC  #'bpl/parse-magic)
              (cons bpl/VERSION  #'bpl/parse-version)
              (cons bpl/NULL  #'bpl/parse-null)
              (cons bpl/FALSE  #'bpl/parse-false)
              (cons bpl/TRUE  #'bpl/parse-true)
              (cons bpl/FILL  #'bpl/parse-fill)
              (cons bpl/UINT  #'bpl/parse-uint)
              (cons bpl/REAL  #'bpl/parse-real)
              (cons bpl/DATE  #'bpl/parse-date)
              (cons bpl/DATA  #'bpl/parse-data)
              (cons bpl/STRING  #'bpl/parse-string)
              (cons bpl/UNICODE  #'bpl/parse-unicode)
              (cons bpl/UNK_x70  #'bpl/parse-unk_x70)
              (cons bpl/UID  #'bpl/parse-uid)
              (cons bpl/ARRAY  #'bpl/parse-array)
              (cons bpl/SET  #'bpl/parse-set)
              (cons bpl/DICT  #'bpl/parse-dict)
              (cons bpl/MASK  #'bpl/parse-mask))))

;; Trailer spec
(defconst bpl/u64 '((high u32)(low u32)) "Specification for u64.")

(defconst bpl/trailer-spec
  '((reserved-4 u32)
    (reserved-2 u16)
    (offset-size u8)
    (ref-size u8)
    (num-objects struct bpl/u64)
    (root-object-index struct bpl/u64)
    (offset-table-offset struct bpl/u64)
    )
  "Trailer format for bpl.")

(defconst bpl-width-specs
  '((1  u8)
    (2  u16)
    (4  u32)
    (8  '(struct bpl/u64)))

  "Descriptions."
  )



(defun bpl/as-int (in)
  "Represent IN as int."
  (cond
   ((integerp in) in)
   ((and (listp in)
         (alist-get  'low in)
         (alist-get  'low in))

    (let ((low  (alist-get  'low in))
          (high (alist-get  'high in)))

      (logior (ash high 32) low))
    )

   (t (error "Wrong input: %S" in))))


(defvar bpl nil "Nil.")
(defvar entry-list nil "Nil.")

(defun bpl/trailer-get-filed (trailer field)
  "Return value of FIELD in TRAILER."
  (bpl/as-int (bindat-get-field trailer field)))


(defun bpl/read-bytes (path)
  "Read bytes from PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'binary)
    (insert-file-contents-literally path)
    (buffer-substring-no-properties (point-min) (point-max))))


(defun bpl/get-int-from-bpl (bpl pos size)
  "Get an integer from BPL, starting from POS of size SIZE."
  (let* ((int-spec (cond
                    ((= 1 size) '((val u8)))
                    ((= 2 size) '((val u16)))
                    ((= 4 size) '((val u32)))
                    ((= 8 size) '((val struct bpl/u64)))
                    (t (error "Wrong size: %d" size))
                    ))
         (bytes  (substring (plist-get bpl :raw) pos (+ pos size)))
         (unpacked (bindat-unpack int-spec bytes)))
    (PDEBUG "BYTES:" bytes ", UNPACKED: " unpacked)
    (bpl/as-int (alist-get 'val unpacked ))))

(defun bpl/parse-dict (bpl pos size index)
  "Parse dict from BPL, start from POS, with size SIZE."
  (let ((ref-size (plist-get bpl :ref-size))
        val)

    (dolist (i (number-sequence 0 (1- size)))
      (let ((key_idx (bpl/get-int-from-bpl bpl (+ pos (* i ref-size)) ref-size))
            (val_idx (bpl/get-int-from-bpl bpl (+ pos (* (+ i size) ref-size)) ref-size)))

        (PDEBUG "I: " i
          ", KEY_INDEX: " key_idx
          ", VAL_INDEX: " val_idx)



        (bpl/parse-at-index bpl key_idx)
        (bpl/parse-at-index bpl val_idx)

        (push (cons key_idx val_idx) val )))

    (PDEBUG "DICT-INDEX: " index ", FINAL VAL: " val
      ", ENTRY-LIST-CUR: " entry-list)


    (push (cons index
                (cons 'DICT val);; (plist-put ret :val val)
                ) entry-list)

    (PDEBUG "DICT-INDEX-B: " index ", FINAL VAL: " val
      ", ENTRY-LIST-CUR: " entry-list)))


(defun bpl/parse-string (bpl pos size index)
  "Parse string from BPL, start from POS, with size SIZE."
  (let ((ret '(:type STRING)))
    (PDEBUG "PARSE-STRING -- POS: " pos ", SIZE: " size)
    (push  (cons index
                 (cons 'STRING  (substring (plist-get bpl :raw)
                                           pos (+ pos size))))
           entry-list
           )))

(defun bpl/parse-data (bpl pos size index)
  "Parse string from BPL, start from POS, with size SIZE."
  (let ((ret '(:type DATA)))
    (PDEBUG "PARSE-DATA -- POS: " pos ", SIZE: " size)
    (push (cons index
                (cons 'DATA (substring (plist-get bpl :raw)
                                       pos (+ pos size))))
                 entry-list
                 )))

(defun bpl/parse-array (bpl pos size index)
  "Parse dict from BPL, start from POS, with size SIZE."
  (let ((ref-size (plist-get bpl :ref-size))
        val)

    (dolist (i (number-sequence 0 (1- size)))
      (let ((idx (bpl/get-int-from-bpl bpl (+ pos (* i ref-size)) ref-size)))

        (PDEBUG "ARRAY -- " "I: " i
          ", KEY_INDEX: " key_idx)

        (bpl/parse-at-index bpl idx)

        (push idx val )))

    (PDEBUG "ARRAY-INDEX: " index ", FINAL VAL: " val
      ", ENTRY-LIST-CUR: " entry-list)


    (push (cons index
                (cons 'ARRAY val);; (plist-put ret :val val)
                ) entry-list)

    (PDEBUG "ARRAY-INDEX-B: " index ", FINAL VAL: " val
      ", ENTRY-LIST-CUR: " entry-list)))

(defun bpl/parse-at-index (bpl index)
  "Parse entry at INDEX, add it to BPL, and return bpl."
  (let* ((offset-size (plist-get bpl :offset-size))
         (pos (bpl/get-int-from-bpl
               bpl
               (+ (plist-get bpl :offset-table-offset)
                  (* offset-size index))
               offset-size))
         (tmp (PDEBUG "NODE-OFFSET: " pos))
         (first-byte (bpl/get-int-from-bpl bpl pos 1))
         (pos (1+ pos)) ;; after first bytes
         (tmp (PDEBUG "FIRST: " (format "%X" first-byte)))
         (type (logand first-byte bpl/MASK))
         (size (logand first-byte bpl/FILL)))

    (PDEBUG
      "INDEX: " index
      ", TYPE: " (bpl/get-type-name type)
      ", SIZE: " size)

    (when (= size bpl/FILL)
      (unless (member type (list bpl/DATA bpl/STRING bpl/UNICODE bpl/ARRAY bpl/SET bpl/DICT))
        (error "Unexpected type: %s" (bpl/get-type-name type)))
      (let* ((next-obj (bpl/get-int-from-bpl bpl pos 1))
             (next-type (logand next-obj bpl/MASK))
             (next-size (logand next-obj bpl/FILL)))

        (PDEBUG "NEXT-TYPE: " (bpl/get-type-name next-type) ", NEXT-SIZE: " next-size)

        (unless (= next-type bpl/UINT)
          (error "Expecting UINT, but got: %s" (bpl/get-type-name next-type)))

        (setq pos (1+ pos)
              next-size (lsh 1 next-size))

        (if (> (+ pos next-size) (length (plist-get bpl :raw)))
            (error "Outside value range: %S -- %S"
                   (+ pos next-size) (length (plist-get bpl :raw))))

        (PDEBUG "POS before: " pos ", size: " size)

        (setq size (bpl/get-int-from-bpl bpl pos next-size)
              pos (+ pos next-size))

        (PDEBUG "POS after: " pos ", size: " size)))

    (PDEBUG "AAA: ENTRY-LIST before: " entry-list)

    (let ((func (bpl/get-type-parser type)))
      (unless func
        (error "Can't find parser for type: %s" (bpl/get-type-name type)))

      (aif (funcall func bpl pos size index)
          (progn
            (PDEBUG "ENTRY-LIST after: " entry-list)
            )
        (error "INDEX: " index " yields to NIL")))))


(defun bpl/parse-bytes (bytes)
  "Parse BYTES and return a bpl."
  (let (bpl entry-list)
    (unless (string= bpl/MAGIC (substring bytes 0 (length bpl/MAGIC)))
      (error "Not Apple binary property list"))

    (unless (string= bpl/VERSION (substring bytes (length bpl/MAGIC)
                                            (+ (length bpl/MAGIC) (length bpl/VERSION))))
      (error "Wrong version"))

    (setq bpl (plist-put bpl :raw bytes))

    (let* ((trailer (bindat-unpack bpl/trailer-spec (substring bytes -32)))
           (root-index (bpl/trailer-get-filed trailer 'root-object-index))
           (bpl (plist-put
                 (plist-put
                  (plist-put
                   (plist-put
                    (plist-put
                     (plist-put bpl :trailer trailer)
                     :ref-size (bpl/trailer-get-filed trailer 'ref-size))
                    :offset-size (bpl/trailer-get-filed trailer 'offset-size))
                   :num-objects (bpl/trailer-get-filed trailer 'num-objects))
                  :root-object-index (bpl/trailer-get-filed trailer 'root-object-index))
                 :offset-table-offset (bpl/trailer-get-filed trailer
                                                             'offset-table-offset)))
           (entry-list (bpl/parse-at-index
                        bpl
                        root-index)))

      (PDEBUG "ENTRY-LIST: " entry-list)
      (plist-put bpl :entries entry-list))))

(defun bpl/as-xml (bpl)
  "Return BPL as text."

  (defun bpl/entry-2-xml (entry level entries &optional tag)
    "Return text representation of ENTRY."
    ;; (PDEBUG "ENTRY: " entry)
    (let ((type (car entry))
          (val  (cdr  entry))
          (prefix (make-string (* level 4) ? )))

      (cond
       ((equal type 'DICT)
        (let* ((level (1+ level)))
          (print (concat prefix "<dict>"))

          (dolist (pair val)
            (let ((key-entry (alist-get (car pair) entries))
                  (val-entry (alist-get (cdr pair) entries)))

              (bpl/entry-2-xml key-entry level entries "key")
              (bpl/entry-2-xml val-entry level entries)

              ))

          (print (concat prefix "</dict>"))

          ))

       ((equal type 'STRING)
        (let ((tag-start (if tag (format "<%s>" tag) "<string>"))
              (tag-close (if tag (format "</%s>" tag) "</string>")))

          (print (concat prefix tag-start (cdr entry) tag-close))))

       ((equal type 'DATA)
        (let ((tag-start (if tag (format "<%s>" tag) "<data>"))
              (tag-close (if tag (format "</%s>" tag) "</data>")))
          (print (concat prefix tag-start (or (cdr entry) "");; (base64-encode-string (or (cdr entry) ""))
                         tag-close))))

       ((equal type 'ARRAY)
        (progn
          (print (concat prefix "<ARRAY>"))

          (dolist (idx val)
            (let ((entry (alist-get idx entries)))
              (bpl/entry-2-xml entry level entries)))

          (print (concat prefix "</ARRAY>"))))

       (t (print (format "%S" entry)))))
    )

  (concat
   "META: "
   (format "\n  ref-size            : %d" (plist-get bpl :ref-size))
   (format "\n  offset-size         : %d" (plist-get bpl :offset-size))
   (format "\n  num-objects         : %d" (plist-get bpl :num-objects))
   (format "\n  root-object-index   : %d" (plist-get bpl :root-object-index))
   (format "\n  offset-table-offset : %d" (plist-get bpl :offset-table-offset))

   "\nContent:\n"

   (let ((level 0)
         (entries  (plist-get bpl :entries)))

     (bpl/entry-2-xml
      (alist-get
       (plist-get bpl :root-object-index) entries)
      level
      entries))))

(defun bpl/get-width-info (val)
  "Description."
  (let ((ret   (catch 'info
                 (dolist (i '(1 2 4 8))
                   (when (< val (lsh 1 (* i 8)))
                     (throw 'info (cons i (alist-get i bpl-width-specs))))))) )

    (PDEBUG "VAL: " val ", RET: " ret)
    ret))

(defun bpl/dump-int (int)
  "Dump INT"
  (let* ((pack-info (bpl/get-width-info int))
         (pack-width (car pack-info))
         (sz-fmt '((size u8)))
         (sz `((size . ,(logior bpl/UINT (truncate (/ (log pack-width)(log 2)))))))
         (val-fmt `((value ,(cadr pack-info))))
         (val `((value . ,(logand int (1- (lsh 1 (* 8 (car pack-info)))))))))

    (PDEBUG "SZ: " sz-fmt " -- " sz)
    (PDEBUG "VAL: " val-fmt " -- " val)

    (insert (bindat-pack sz-fmt sz))
    (insert (bindat-pack val-fmt val)))
  )

(defun bpl/dump-type-size (type size)
  "Dump TYPE & SIZE."
  (PDEBUG "TYPE: " type
    "SIZE: " size)
  (let ((fmt '((marker u8)))
        (val `((marker . ,(logior type (if (< size 15) size #x0F))))))
    (PDEBUG "FMT: " fmt ", VAL: " val)
    (insert (bindat-pack fmt val)))


  (if (>= size 15)
      (bpl/dump-int size)))

(defun bpl/dump-entry (entry ref-fmt)
  "Dump ENTRY."
  (let ((type (car entry))
        (val  (cdr  entry)))
    (when entry
      (PDEBUG "DUMP: type --" type ", VAL: " val ", ENTRY: " entry)


      (cond
       ((equal type 'DICT)
        (bpl/dump-type-size bpl/DICT (length val))

        (let ((sorted (sort val (lambda (x y)
                                  (<= (car x) (car y))))) )

          ;; dump key-index
          (mapc (lambda (x)
                  (insert (let ((key_idx (car x)))
                            (bindat-pack `((val ,ref-fmt)) `((val . ,key_idx)))
                            ))) sorted)

          (mapc (lambda (x)
                  (insert (let ((val_idx (cdr x)))
                            (bindat-pack `((val ,ref-fmt)) `((val . ,val_idx)))
                            ))) sorted)))

       ((equal type 'STRING)
        (let ((len (length val)))
          (bpl/dump-type-size bpl/STRING len)
          (insert val)))

       ((equal type 'DATA)
        (let ((len (length val)))
          (bpl/dump-type-size bpl/DATA len)
          (insert val)))

       ((equal type 'ARRAY)
        (bpl/dump-type-size bpl/ARRAY (length val))

        (let ((sorted (sort val '<=)))

          (PDEBUG "SORTED: " sorted)


          ;; dump key-index
          (mapc (lambda (x)
                  (insert (bindat-pack `((val ,ref-fmt)) `((val . ,x))))) sorted)

          ))

       ((unless type)
        (message "NIL....")
        )

       (t (warn "Not impl: %s: %S" (bpl/get-type-name type) entry))))
    (1- (point)))
  )

(defun bpl/generate-u64 (val)
  "Generate data suitable to pack as u64."
  `((low . ,(logand val (1- (lsh 1 32))))
    (high . ,(lsh val -32)))
  )

(defun bpl/dump-file (bpl path)
  "Dump BPL to PATH."
  (PDEBUG "DUMP BPL: "bpl)

  (with-temp-file path
    (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'binary)
    (insert bpl/MAGIC bpl/VERSION)
    (let* ((entries (plist-get bpl :entries))
           (num-entries (length entries))
           (offsets '(8))
           (ref-info (bpl/get-width-info num-entries))
           (ref-size (car ref-info))
           (ref-fmt (cadr ref-info)))

      (PDEBUG "OFFSETS-INITIAL: " offsets)

      ;; wierd,....
      (while offsets
        (pop offsets)
        )

      (push 8 offsets)

      (unless ref-fmt
        (error "Failed to parse ref-szie & ref-fmt: length -- %d, got: %S"
               num-entries ref-info))

      (dolist (i (number-sequence 0 (1- num-entries)))
        (push (bpl/dump-entry (alist-get i entries) ref-fmt) offsets)
        (PDEBUG "OFFSETS: " offsets)

        )

      (pop offsets);; last offset is not used.

      ;; dump offset table & trailer
      (PDEBUG "OFFSETS: " offsets)
      (let* ((offset-table-offset (1- (point)))
             (offset-info (bpl/get-width-info offset-table-offset)))
        (PDEBUG "OFFSET-INFO: " offset-info)
        (dolist (offset (nreverse offsets))
          (insert (bindat-pack
                  `((offset ,(cadr offset-info)))
                  `((offset . ,offset)))))

        ;; dump trailer info.
        ;; ((offset-table-offset (low . 284) (high . 0))
        ;;  (root-object-index (low . 0) (high . 0))
        ;;  (num-objects (low . 13) (high . 0))
        ;;  (ref-size . 1)
        ;;  (offset-size . 2)
        ;;  (reserved-2 . 0)
        ;;   (reserved-4 . 0))

        (let ((trailer-data
               `((reserved-4 . 0)
                 (reserved-2 . 0)
                 (offset-size . ,(car offset-info))
                 (ref-size . ,(car (bpl/get-width-info num-entries)))
                 (num-objects . ,(bpl/generate-u64 num-entries))
                 (root-object-index . ,(bpl/generate-u64 (plist-get bpl :root-object-index)))
                 (offset-table-offset . ,(bpl/generate-u64 offset-table-offset)))))

          (PDEBUG "TRAILER: spec -- " bpl/trailer-spec
            ", data -- " trailer-data)

          (insert (bindat-pack
                   bpl/trailer-spec
                   trailer-data
                   )))))))


(defun bpl/retrive-resource-data (bpl)
  "Retrieve resource data from BPL."
  (let* ((root-index (plist-get bpl :root-object-index))
         (entries (plist-get bpl :entries))
         (root-tree (alist-get root-index entries)))

    (PDEBUG "ROOT-ENTRY: " root-tree)

    (catch 'p-found
      (dolist (pair (cdr root-tree))
        (PDEBUG "PAIR: "pair)
        (let* ((key (alist-get (car pair) entries ))
               (val (alist-get (cdr pair)  entries))
               )
          (PDEBUG "KEY: " key
            ", VAL: " val)

          (when (string= (cdr key) "WebMainResource")
            (unless (equal (car val) 'DICT)
              (error "value of key WebMainResource should be a dict."))

            (dolist (p (cdr val))
              (let ((k (alist-get (car p) entries))
                    (v (alist-get (cdr p) entries)))
                (when (string= (cdr k) "WebResourceData")
                  (unless (equal (car v) 'DATA)
                    (error "Expecting DATA, but got: %s" (bpl/get-type-name (car v))))

                  (throw 'p-found (cons (cdr p) v)))))))))))



(defvar curr-node nil "Nil.")
(defvar curr-bpl nil "Nil.")

(setq-default curr-node nil)


(defun open-webarchive ()
  "Description."
  (interactive)

  (let* ((in (or buffer-file-name
               (ivy-read "Find file: " (directory-files  default-directory t ".*\\.webarchive")
                         :action (lambda (cand)
                                   (interactive)
                                   (if (file-directory-p cand)
                                       (counsel-list-directory cand)
                                     cand
                                     ))
                         :caller 'counsel-list-directory)))
         (bytes (bpl/read-bytes in))
         (bpl (bpl/parse-bytes bytes))
         (buf (get-buffer-create (concat "EDIT-" in)))
         )

    (with-current-buffer buf
      (setq curr-bpl bpl)
      (setq curr-node (bpl/retrive-resource-data bpl))

      (erase-buffer)
      (insert (decode-coding-string (cddr curr-node) 'utf-8))

      (PDEBUG "BUF: "(current-buffer))
      (PDEBUG "CURR-NODE: " curr-node)

      (html-mode)
      (local-set-key (kbd "C-c C-c")
                     (lambda ()
                       (interactive)
                       (PDEBUG "BUF: "(current-buffer))

                       (message "updating content...")
                       (PDEBUG "CURR-NODE1: " curr-node)

                       (setf (cddr curr-node) (buffer-substring-no-properties (point-min)
                                                                              (point-max)))
                       ;; (PDEBUG "CURR-NODE2: " curr-node)
                       ;; (setf (alist-get (car curr-node) curr-bpl) curr-node)

                       (PDEBUG "CURR-NODE: " curr-node)

                       (let ((entries (plist-get curr-bpl :entries)) )
                         (setf (alist-get (car curr-node) entries) (cdr curr-node))
                         (plist-put curr-bpl :entries entries))

                       (kill-buffer)
                       (bpl/dump-file curr-bpl (expand-file-name "~/tmp/out_title.webarchive"))

                       )

                     )
      (pop-to-buffer buf)
      )

    )
  )




(provide 'bplist)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; bplist.el ends here
