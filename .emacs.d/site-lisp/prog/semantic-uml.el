;;; semantic-uml.el --- Brief introduction here. -*- lexical-binding: t; -*-

;; Author: YangYingchao <yangyingchao@gmail.com>
;;
;; Copyright (C) 2015 Yang,Ying-chao
;;
;; Author: Yang,Ying-chao <yangyingchao@gmail.com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Usage:
;; Open a file that can be analyzed by CEDET and select a region, calls uml/struct-to-dot
;; and it will generate a node which can be drawn by graphiviz.
;; (add-hook 'c-mode-common-hook
;; (lambda ()
;;              (local-set-key "\C-csD" 'uml/struct-to-dot)
;;              (local-set-key "\C-csd" 'uml/struct-to-puml)))

;;; Code:


 ;; Function used to add fields of struct into a dot file (for Graphviz).

(require 'eieio)
(require 'semantic/find)
(require 's)

(defmacro aif (test-form then-form &rest else-forms)
  "Like `if' but set the result of TEST-FORM in a temprary variable called `it'.
THEN-FORM and ELSE-FORMS are then excuted just like in `if'."
  (declare (indent 2) (debug t))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(defgroup uml nil
  "group name."
  :group 'tools)

(defcustom uml/extract-type 'all
  "Type of members to be extracted.
Possible choices:
`methods' -- extract member functions only.
`fields'  -- extract member fields only.
`all'     -- extract everything."
  :type '(radio (const :tag "Extract member methods only." methods)
                (const :tag "Extract member fields only." fields)
                (const :tag "Extract everything.." all))
  :group 'uml)


;;;;; Regular expressions to match a field of a struct.

(defclass uml/object-node ()
  ((name :initarg :name
         :initform nil
         :type (or null string))
   (type :initarg :type
         :type string
         :initform "")
   (funcs :initarg :funcs
          :type (or null list)
          :initform nil)
   (attrs :initarg :attrs
          :type (or null list)
          :initform nil)
   (subnodes :initarg :subnodes
             :initform nil
             :type (or null list))
   (parents :initarg :parents
            :initform nil
            :type (or null list)))
  :document "A Semantic object which can be stringified."
  )

(defclass uml/object-attr ()
  ((name :initarg :name
         :initform nil
         :type (or null string)
         )
   (type :initarg :type
         :type string
         :initform "")

   (visibility :initarg :visibility
               :initform 1 ;;visibility: 0 -- public, 1 -- private, 2 -- protected
               :type number
               )
   (params :initarg :params ;; For functions only...
           :initform nil
           :type (or null list))
   )
  :document "A Semantic object for member fields and member functions."
  )

;; Utilities

(defmacro uml/concat (src &rest sequences)
  "Concatenate SRC with all SEQUENCES, and set result to SRC."
  `(setq ,src (funcall 'concat ,src ,@sequences)))


(defmacro uml/append-item (list item)
  "Append LIST with ITEM, and set result to LIST."
  `(setq ,list (nreverse (cons ,item (nreverse ,list)))))

;;visibility: 0 -- public, 1 -- private, 2 -- protected

(defun uml/ele-to-obj-attr (ele)
  "Parse an element `ELE' and and generate a object-attr."
  (let ((name (semantic-tag-name ele))
        (type (semantic-tag-type ele))
        (modifier "")
        node params)
    (PDEBUG "ele" ele "name" name "type" type)

    (unless (or (semantic-tag-get-attribute ele :constructor-flag)
                (semantic-tag-get-attribute ele :destructor-flag)
                (member "static" (semantic-tag-get-attribute ele :typemodifiers)))
      (setq node (make-instance 'uml/object-attr))
      (when (semantic-tag-p type)
        (aif (semantic-tag-get-attribute type :template-specifier)
            (setq type (format "%s\\<%s\\>" (semantic-tag-name type)
                               (car (car it))))
          (setq type (semantic-tag-name type))))
      (case (semantic-tag-class ele)
        ('function ;; member functions
         (setq params (semantic-tag-get-attribute ele :arguments)))
        ('variable
         t
         ;; (print "variable")
         ))

      (cond
       ((semantic-tag-get-attribute ele :template-specifier)
        (PDEBUG "template" ele)
        )
       ((semantic-tag-get-attribute ele :functionpointer-flag)
        (PDEBUG "function pointer" ele)
        )
       ((semantic-tag-get-attribute ele :pointer)
        (setq modifier "*"))
       ((semantic-tag-get-attribute ele :dereference)
        (setq modifier "[]")))

      (when (stringp type)
        (setq type (concat type modifier)))

      (progn
        (oset node :name name)
        (oset node :type (or type ""))
        (oset node :visibility visibility)
        (oset node :params params)))
    node))

(defun Tag-To-ObjNode (tag)
  "Parse semantic TAG and convert it into a uml/object-node which can be stringified later."
  (let* ((type (semantic-tag-get-attribute tag :type))
         (attrs (semantic-tag-get-attribute tag :members))
         (visibility 0) ;;visibility: 0 -- public, 1 -- private, 2 -- protected
         node
         func-list attr-list subnodes
         ele)

    (defun uml/parse-func-args (pl)
      (let (res)
        (dolist (item pl)
          (add-to-list 'res (semantic-tag-get-attribute tag :type) t )
          res)))

    ;; start of real parsing...
    (PDEBUG "TAG: " tag )
    (when (string= type "typedef")
      (let ((tmp-tag (semantic-tag-get-attribute tag :typedef)))
      (setq type (semantic-tag-get-attribute tmp-tag :type)
            attrs (semantic-tag-get-attribute tmp-tag :members))))

    ;;todo: Add more case handling if necessary.
    (when (and (> (length (semantic-tag-name tag)) 0) ;; filter out empty names..
               attrs)
      (setq node (make-instance 'uml/object-node))
      (oset node :name (semantic-tag-name tag))
      (PDEBUG "Tag:" tag "name:" (semantic-tag-name tag))
      (while (setq ele (pop attrs)) ;; Walk through all attributes of this Tag.
        (case (semantic-tag-class ele)
          ('function ;; member functions
           (unless (equal uml/extract-type 'fields)
             (aif (uml/ele-to-obj-attr ele)
                 (add-to-list 'func-list it t))))
          ('variable ;; memer fields
           (progn
             (unless (equal uml/extract-type 'methods)
               (aif (uml/ele-to-obj-attr ele)
                   (add-to-list 'attr-list it t)))))
          ('label    ;; Lables (public/protected/private)
           (aif (semantic-tag-name ele)
               (cond
                ((string= it "public")
                 (setq visibility 0))
                ((string= it "protected")
                 (setq visibility 2))
                ((string= it "private")
                 (setq visibility 1))
                (t
                 (message (format "Skip lable (%s), should be added later." it))))))
          ('type  ;; Other embedded types, will be processed later.
           (add-to-list 'subnodes (Tag-To-ObjNode (semantic-tag-copy ele))))
          (t (message (format "Skip type (%s), should be added later."
                              (semantic-tag-class ele))))))

      (oset node :type type)
      (oset node :funcs func-list)
      (oset node :attrs attr-list)
      (oset node :subnodes subnodes)
      (oset node :parents
            (cons (remove "object" (semantic-tag-type-superclasses tag))
                  (semantic-tag-type-interfaces tag))))
    (PDEBUG "NODE: " node)
  node))

 ;; plantuml support

(defun uml/puml-fmt-funcs (funcs)
  "Format functions (FUNCS)."
  (let (result)
    (dolist (func funcs)
      (PDEBUG "UML/FUNC: " func)
      (let* ((name (oref func :name))
             (type (oref func :type))
             (visibility (oref func :visibility))
             (prefix (case visibility
                       (0 "+")
                       (1 "-")
                       (2 "#"))))

        (add-to-list 'result (format "%s %s %s()" prefix type name) t)))
    (mapconcat 'identity result "\n")))

(defun uml/puml-fmt-attrs (attrs parent-type)
  "Format member fields (ATTRS).   `PARENT-TYPE' is type of parent node."
  (let ((format-func (cond
                      ((string= parent-type "enum") (lambda (prefix type name) name))
                      (t (lambda (prefix type name) (format "%s %s %s" prefix type name)))))
        result)
    (dolist (attr attrs)
      (PDEBUG "UML/FIELD: " attr)
      (let* ((name (oref attr :name))
             (type (oref attr :type))
             (visibility (oref attr :visibility))
             (prefix (cl-case visibility
                       (0 "+")
                       (1 "-")
                       (2 "#"))))

        (if (string-match (rx (+? nonl) "::" (group (+ nonl)) eow) type)
            (setq type (match-string 1 type)))

        (add-to-list 'result (funcall format-func prefix type name) t)))
    (mapconcat 'identity result "\n")))

(defvar uml/stringified-nodes nil "List of nodes has been stringified.")
(defun uml/node-to-puml (node)
  "Convert NODE to xml string which can be displayed with DIA."
  (PDEBUG "PUML-Node: " node)
  (let* ((name  (or (oref node :name) "Unamed Object"))
         (funcs (oref node :funcs))
         (attrs (oref node :attrs))
         (subnodes (oref node :subnodes))
         (parents (oref node :parents))
         (type (oref node :type))
         result)

    (cond
     ((string= type "namespace")
      (PDEBUG
        "Node IGNORED, reason: is namespace, Details: name: " name ", type: " type
        ", list: " uml/stringified-nodes))
     ((member name uml/stringified-nodes)
      (PDEBUG
        "Node IGNORED, reason: already in stringfied-nodes, Details: name: "
        name ", type: " type
        ", list: " uml/stringified-nodes))
     (t
      (add-to-list 'result (format "%s %s {"
                                   (cond
                                    ((string= type "enum") "enum")
                                    (t "class"))
                                   name) )
      (when funcs
        (add-to-list 'result  (uml/puml-fmt-funcs funcs) t))

      (when attrs
        (add-to-list 'result  (uml/puml-fmt-attrs attrs type) t))

      ;; tail
      (add-to-list 'result  "}\n" t)

      ;; Links
      (dolist (parent (car parents)) ;; super classs..
        (add-to-list 'result (format "%s <|-- %s" parent name) t))
      (dolist (parent (car parents)) ;; interfaces..
        (add-to-list 'result (format "%s <|-- %s" parent name) t))
      (add-to-list 'uml/stringified-nodes name))
     )

    (dolist (subnode subnodes)
      (aif (and subnode
                (uml/node-to-puml subnode))
          (add-to-list 'result it)))

    (PDEBUG "XNODE:" node "Result: " result)

    ;; Concatenate them.
    (s-replace-all '(("\\<" . "<") ("\\>" . ">")) (mapconcat 'identity result "\n"))))

(defun uml/cleanup-orphans ()
  "Clean up orphans nodes."
  (interactive)
  ;; refresh stringified nodes.
  (uml/parse-stringfied-nodes t)
  (let (deleted-nodes)
    ;; first, save original nodes, if something goes wrong, we can use it to
    ;; restore.
    (copy-rectangle-as-kill (point-min) (point-max))
    (dolist (node uml/stringified-nodes)
      (save-excursion
        (goto-char (point-min))
        (unless (search-forward-regexp
                 (eval `(rx
                         (* space)
                         (or
                          (: (+? (or alnum "_")) (* space)
                             (* (or "<" "|" "*" "o")) (+  (or  "-" "."))
                             (* space) ,node eow)
                          (: bow ,node (* space)
                             (* (or "<" "|" "*" "o")) (+ (or  "-" "."))
                             (* space) (+? nonl)
                             ))))
                 nil t)
          ;; no link was found, remove this node.
          (goto-char (point-min))
          (when (search-forward-regexp
                 (eval `(rx bol (+ nonl) ,node (* space) "{" (+? anything) "}"))
                 nil t)
            (kill-region (match-beginning 0)
                             (if (> (point-max) (match-end 0))
                                 (1+ (match-end 0))
                               (match-end 0)))
            (add-to-list 'deleted-nodes node)))))

    ;; parse current buffer again to refresh stringfied-nodes.
    (uml/parse-stringfied-nodes t)
    (if deleted-nodes
        (message "Deleted nodes: %s" (s-join ", " deleted-nodes))
      (message "No orphan was found."))))

(defun uml/get-nodes-semantic (start end)
  "Get list of ObjNode from region (START, END) via semantic."
  (unless semantic-mode
    (semantic-mode 1))

  (aif (semantic-find-tag-by-overlay start)
      (mapcar 'Tag-To-ObjNode it)
    (error "No tags found!")))

(defun uml/get-nodes-lsp (start end)
  "Get list of ObjNode from region (START, END) via semantic."
  (when (and (bound-and-true-p lsp-mode)
             (lsp--capability "documentSymbolProvider"))
    (let ((symbols  (lsp--get-document-symbols))
          (l_start (line-number-at-pos start))
          (l_end (line-number-at-pos end))
          nodes cand)

      (PDEBUG "POS:" l_start l_end)

      (defun uml/parse-item (item)
        "Description."
        (let* ((range (gethash "range" item))
               (start (1+ (gethash "line" (gethash "start" range))))
               (end (1+ (gethash "line" (gethash "end" range))))
               (kind (alist-get (gethash "kind" item) lsp--symbol-kind))
               (children (gethash "children" item))
               (yc/debug-log-limit -1))

          (when (and (<= start l_start)
                     (>= end l_end))
            (if (and (member kind
                              '("Struct" "Class" "Enum"))
                     (or (not cand)
                         (and
                          (> start (gethash "line" (gethash "start" (gethash "range" cand))))
                          (< end (gethash "line" (gethash "end" (gethash "range" cand)))))))
                (setq cand item))

            (if children
                (seq-map 'uml/parse-item children)))))

      (mapc 'uml/parse-item symbols)

      (let ((yc/debug-log-limit 256))
        (PDEBUG "CAND:" cand
          "HASHP: " (hash-table-p cand)
          ;; "CHILDREN:" (gethash "children" cand)
          ))

      (when cand
        (let* ((children (gethash "children" cand))
               (node (make-instance 'uml/object-node))
               func-list attr-list parent subnodes)

          (PDEBUG
            "NAME:" (gethash "name" cand)
            "KIND: " (gethash "kind" cand))

          (seq-map (lambda (field)
                     (let ((name )
                           (detail )
                           (attr (make-instance 'uml/object-attr)))

                       (when (= (gethash "kind" field) 8 )
                         (oset attr :name (gethash "name" field))
                         (oset attr :type (car (string-split (gethash "detail" field) " ")))
                         (oset attr :visibility '0 ;; FIXME: visibility
                               )
                         (oset attr :params nil)
                         (add-to-list 'attr-list attr t))))
                   children)

          (oset node :name (gethash "name" cand))
          (oset node :type (alist-get (gethash "kind" cand) lsp--symbol-kind))

          (oset node :funcs func-list)
          (oset node :attrs attr-list)
          (oset node :subnodes subnodes)
          (oset node :parents parent)
          (add-to-list 'nodes node )
          ))
      nodes))) 


(defun uml/get-objnode-list (start end)
  "Get list of ObjNode from region (START, END)."
  (or (uml/get-nodes-lsp start end)
      (uml/get-nodes-semantic start end)))

;;;###autoload
(defun uml/struct-to-puml (start end)
  "Generated a plantuml for tags between START and END."
  (interactive "rp")
  (save-excursion
    (aif (uml/get-objnode-list start end)
        (let ((ret  (mapconcat 'uml/node-to-puml (cl-remove-if-not 'identity it) "\n")))
              (kill-new ret)
              (PDEBUG "RET: " ret))

      (error "Failed to format tags!")))
  (deactivate-mark)
  (message "Finished, node copied to killing-ring."))

(defun uml/struct-to-puml-fields-only (start end)
  "Similar with `uml/struct-to-puml', but ignore member-functions."
  (interactive "rp")
  (let ((uml/extract-type 'fields))
    (uml/struct-to-puml start end)))

 ;; dot support, used to plot relationship between structures, e.g linked list.

(defun uml/dot-fmt-attrs (attrs)
  "Format member fields (ATTRS)."
  (let ((field 0)
        result)
    (dolist (attr attrs)
      (let* ((name (oref attr :name))
             (str (format "<f%d>%s" (setq field (1+ field))  name)))
        (PDEBUG "UML/ATTR: " attr "RESULT: " str)
        (add-to-list 'result str t)))
    (concat (mapconcat 'identity result "|\\\n") "\\")))

(cdsq uml/dot-ignored-types '("enum" "namespace"))

(defun uml/node-to-dot (node)
  "Convert NODE to xml string which can be displayed with DIA."
  (PDEBUG "dot-Node: " node)
  (let* ((name  (or (oref node :name) "Unamed Object"))
         (funcs (oref node :funcs))
         (attrs (oref node :attrs))
         (subnodes (oref node :subnodes))
         (parents (oref node :parents))
         (type (oref node :type))
         result)

    (if (or (member name uml/stringified-nodes)
            (member type uml/dot-ignored-types))
        (PDEBUG "Node ignored, name:" name ", type: " type ", list: "
          uml/stringified-nodes)

      (add-to-list 'result (format "node_%s [shape=record label=\"{%s|\\" name
                                   name) )
      ;; (when funcs
      ;;   (add-to-list 'result  (uml/dot-fmt-funcs funcs) t))
      (when attrs
        (add-to-list 'result  (uml/dot-fmt-attrs attrs) t))

      ;; tail
      (add-to-list 'result  "}\"];" t)

      ;; ;; Links
      ;; (dolist (parent (car parents)) ;; super classs..
      ;;   (add-to-list 'result (format "%s <|-- %s" parent name) t))
      ;; (dolist (parent (car parents)) ;; interfaces..
      ;;   (add-to-list 'result (format "%s <|-- %s" parent name) t))
      (add-to-list 'uml/stringified-nodes name)
      )

    (dolist (subnode subnodes)
      (aif subnode
          (push (uml/node-to-dot it) result)))

    (PDEBUG "XNODE:" node "Result: " result)

    ;; Concatenate them.
    (mapconcat 'identity result "\n")))

;;;###autoload
(defun uml/struct-to-dot (start end)
  "Generated a plantuml for tags between START and END."
  (interactive "rp")
  (save-excursion
    (let ((tags (semantic-find-tag-by-overlay start))
          strs)
      (setq uml/stringified-nodes nil)
      (if (not tags)
          (error "No tags found!")
        (dolist (tag tags)
          (aif (Tag-To-ObjNode tag)
              (push (uml/node-to-dot it) strs)))
        (if strs
            (kill-new (mapconcat 'identity strs "\n"))
          (error "Failed to format tags!")))))
  (deactivate-mark)
  (message "Finished, node copied to killing-ring."))


(defun uml/parse-stringfied-nodes (&optional silent)
  "Parse all stringfied nodes.
If `silent' is nil, print collected nodes before exit."
  (interactive)
  (setq uml/stringified-nodes nil)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp
            (rx bol (* space)
                (or "enum" "class" "interface")
                (+ space)
                (group (+? nonl))
                (* space)
                "{" eol) nil t)
      (add-to-list 'uml/stringified-nodes (match-string-no-properties 1))))
  (unless silent
    (message "Stringfied notes: %s" (s-join ", " uml/stringified-nodes))))


(provide 'semantic-uml)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; semantic-uml.el ends here
