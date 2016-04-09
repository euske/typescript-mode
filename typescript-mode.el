;;; typescript-mode.el -*- lexical-binding: t -*-

;;; This is an attempt to create a lean typescript-mode
;;; by using SMIE parser. It's not working yet.

;;; The overall coding style is based on swift-mode.
;;; cf. https://github.com/chrisbarrett/swift-mode

(require 'rx)
(require 'smie)

(defgroup typescript nil
  "Configuration for typescript-mode."
  :group 'languages
  :prefix "typescript-")

(defcustom typescript-indent-offset 4
  "Defines the indentation offset for Typescript code."
  :group 'typescript
  :type 'integer)


;;;  Lexers
;;;
(defvar typescript-smie--unary-operators
  '("++" "--"
    ))
(defvar typescript-smie--unary-operators-regexp
  (regexp-opt typescript-smie--unary-operators))

(defvar typescript-smie--binary-operators
  '("=" "+=" "-=" "*=" "/=" "%=" "&=" "^=" "|=" "<<=" ">>=" ">>>="
    "?" "||" "&&" "|" "^" "&" "==" "!=" "===" "!=="
    "<" "<=" ">" ">=" "<<" ">>" ">>>"
    "+" "-" "*" "/" "%"
    ))
(defvar typescript-smie--binary-operators-regexp
  (regexp-opt typescript-smie--binary-operators))

(defun typescript-smie--forward-token ()
  (forward-comment (point-max))
  (cond
   ((looking-at typescript-smie--unary-operators-regexp)
    (goto-char (match-end 0))
    "++")
   ((looking-at typescript-smie--binary-operators-regexp)
    (goto-char (match-end 0))
    "+")
   (t (smie-default-forward-token))
   ))

(defun typescript-smie--backward-token ()
  (forward-comment (- (point)))
  (cond
   ((looking-back typescript-smie--unary-operators-regexp (- (point) 4) t)
    (goto-char (match-beginning 0))
    "++")
   ((looking-back typescript-smie--binary-operators-regexp (- (point) 4) t)
    (goto-char (match-beginning 0))
    "+")
   (t (smie-default-backward-token))
   ))


;;;  Grammar
;;;
(defconst typescript-smie-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    
    (smie-bnf->prec2
     '((id)
       (inst ("{" insts "}")
	     ("if" exp "{" inst "}")
	     ("else" "{" inst "}")
	     (id "=" exp)
	     (exp))
       (insts (insts ";" insts) (inst))
       (exp (exp "++")
	    (exp "+" exp)
	    (exp "." id)
	    ("(" exps ")"))
       (exps (exps "," exps) (exp)))
     '((assoc ";"))
     '((assoc ","))
     '((assoc "++") (assoc "+"))
    ))))


;;;  Indentation Rules
;;;
(defun verbose-typescript-smie-rules (kind token)
  (let ((value (typescript-smie-rules kind token)))
    (message "%s '%s'; sibling-p:%s parent:%s hanging:%s == %s" kind token
             (ignore-errors (smie-rule-sibling-p))
             (ignore-errors smie--parent)
             (ignore-errors (smie-rule-hanging-p))
             value)
    value))

(defun typescript-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) typescript-indent-offset)
    (`(,_ . ",") (smie-rule-separator kind))
    (`(:after . "=") typescript-indent-offset)
    (`(:before . ,(or `"(" `"{"))
     (if (smie-rule-hanging-p) (smie-rule-parent)))
    (`(:before . "if")
     (and (not (smie-rule-bolp)) (smie-rule-prev-p "else")
	  (smie-rule-parent)))))


;;;  Font-lock Rules
;;;

(defvar typescript-mode--type-decl-keywords
  '("class" "interface" "enum" "namespace"))

(defvar typescript-mode--variable-decl-keywords
  '("let" "var" "const"))

(defvar typescript-mode--function-decl-keywords
  '("function"))

(defvar typescript-mode--context-variables-keywords
  '("this" "super"))

(defvar typescript-mode--operator-keywords
  '("new" "instanceof" "typeof"))

(defvar typescript-mode--statement-keywords
  '("break" "case" "continue" "default" "do" "else" 
    "if" "for" "return" "switch" "while"
    "import" "declare"))

(defvar typescript-mode--contextual-keywords
  '("public" "private" "static" "export" "extends"))

(defvar typescript-mode--keywords
  (append typescript-mode--type-decl-keywords
          typescript-mode--variable-decl-keywords
          typescript-mode--function-decl-keywords
          typescript-mode--context-variables-keywords
          typescript-mode--operator-keywords
          typescript-mode--statement-keywords
          typescript-mode--contextual-keywords)
  "Keywords used in the Typescript language.")

(defvar typescript-mode--basic-types
  '("any" "boolean" "number" "string" "void" "Array" "Object"))

(defvar typescript-mode--constants
  '("true" "false" "null" "undefined" "Infinity" "NaN"))

(defvar typescript-font-lock-keywords
  `(
    ;; Keywords
    (,(rx-to-string
       `(and bow (or ,@typescript-mode--keywords) eow)
       t)
     0 font-lock-keyword-face)
    
    ;; Types
    (,(rx-to-string
       `(and bow (or ,@typescript-mode--type-decl-keywords) eow
	     (+ space)
	     (group bow (+ word) eow))
       t)
     1 font-lock-type-face)
    (,(rx-to-string
       `(and "<"
	     (* space)
	     (group bow (+ word) eow)
	     (* space)
	     ">")
       t)
     1 font-lock-type-face)
    (,(rx (+ word)
	  (* space)
	  ":"
	  (* space)
	  (group (* "[") (* space) bow (+ word) eow (* space) (* "]")))
     1 font-lock-type-face)
    
    ;; Variables
    (,(rx-to-string
       `(and bow (or ,@typescript-mode--variable-decl-keywords) eow
	     (+ space)
	     (group bow (+ word) eow))
       t)
       1 font-lock-variable-name-face)
    (,(rx bol
	  (* space)
	  (group bow (+ word) eow)
	  (* space)
	  ":")
     1 font-lock-variable-name-face)
    
    ;; Functions
    (,(rx bow "function" eow
	  (+ space)
	  (group bow (+ word) eow))
     1 font-lock-function-name-face)
    (,(rx bol
	  (* space)
	  (group bow (+ word) eow)
	  (* space)
	  "(")
     1 font-lock-function-name-face)
    
    ;; Constants
    (,(rx-to-string
       `(and bow (or ,@typescript-mode--constants) eow))
     0 font-lock-constant-face)
    ))


;;;  Mode definition
;;;
(defvar typescript-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; Operators
    (dolist (i '(?+ ?- ?* ?/ ?& ?| ?^ ?< ?> ?~))
      (modify-syntax-entry i "." table))

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; Additional symbols
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?? "_" table)

    ;; Comments
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?\n "> b"    table)

    ;; Parenthesis, braces and brackets
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)

    table))

(defvar typescript-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Key map for typescript mode.")


;;;###autoload
(define-derived-mode typescript-mode prog-mode "Typescript"
  "Major mode for Typescript programming language.

\\<typescript-mode-map>"
  :group 'typescript
  :syntax-table typescript-mode-syntax-table
  (setq font-lock-defaults '((typescript-font-lock-keywords) nil nil))

  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (smie-setup typescript-smie-grammar
	      ;;'typescript-smie-rules
	      'verbose-typescript-smie-rules
              :forward-token 'typescript-smie--forward-token
              :backward-token 'typescript-smie--backward-token))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(provide 'typescript-mode)

;;; typescript-mode.el ends here
