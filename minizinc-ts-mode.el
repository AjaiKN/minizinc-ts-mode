;;; minizinc-ts-mode.el --- Major mode for the MiniZinc constraint modeling language -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Ajai Khatri Nelson
;;
;; Author: Ajai Khatri Nelson <emacs@ajai.dev>
;; Maintainer: Ajai Khatri Nelson <emacs@ajai.dev>
;; Version: 0.1-pre
;; Keywords: languages
;; Homepage: https://github.com/AjaiKN/minizinc-ts-mode
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Major mode for the MiniZinc constraint modeling language using tree-sitter.
;;
;;; Code:

(require 'treesit)

;;;; Custom variables

(defgroup minizinc-ts nil
  "Major mode for the MiniZinc constraint modeling language."
  :group 'languages)

(defcustom minizinc-ts-program "minizinc"
  "The path to the MiniZinc executable."
  :type 'file
  :group 'minizinc-ts)

;;;###autoload (put 'minizinc-ts-solver 'risky-local-variable t)
(defcustom minizinc-ts-solver nil
  "Which solver to use.

If this is a string, it will be passed to minizinc's --solver
argument.

If this is nil, always ask whenever you solve a buffer for the
first time. (Your preference will be saved in a buffer-local
variable.)

If this is t, always leave out the --solver argument (i.e., let
MiniZinc decide which solver to use).

If this is a function, that function's result will be used
instead.

Note that if a prefix argument is provided to
`minizinc-ts-solve', then it'll always ask which solver to use,
even if this variable is specified."
  :type '(choice
          (const :tag "Ask every time" nil)
          (const :tag "Default (let MiniZinc choose)" t)
          (string :tag "Solver ID/tag")
          (function :tag "Function"))
  :group 'minizinc-ts
  :risky t
  :safe (lambda (v)
          (or (booleanp v)
              (and
               (stringp v)
               (string-match-p (rx bos (* (any "a-z" "A-Z" "0-9" ?\. ?\-)) eos)
                               v)))))

(defcustom minizinc-ts-compile-function #'compilation-start
  "The function to use when running `minizinc-ts-solve'.

This function is passed a single argument, the shell command to
run."
  :type 'function
  :group 'minizinc-ts)

(defcustom minizinc-ts-indent-offset 2
  "The basic indentation offset in `minizinc-ts-mode'."
  :type 'natnum
  :safe #'natnump
  :group 'minizinc-ts)

;;;; CLI stuff

(defun minizinc-ts--solver (&optional should-ask)
  "Figure out which solver we should use. See `minizinc-ts-solver'.

If SHOULD-ASK is non-nil, always ask the user to explicitly
choose a solver, even if `minizinc-ts-solver' is specified."
  (or (and (not should-ask)
           (if (and (functionp minizinc-ts-solver) (not (booleanp minizinc-ts-solver)))
               (funcall minizinc-ts-solver)
             minizinc-ts-solver))
      (setq-local minizinc-ts-solver (minizinc-ts--ask-for-solver))))

(defun minizinc-ts--solvers ()
  "Return an alist of available solvers.

The key is a description of the solver, and the value is an ID
that can be passed to minizinc."
  (with-temp-buffer
    (call-process minizinc-ts-program nil (current-buffer) nil "--solvers")
    (goto-char (point-min))
    (search-forward "Available solver configurations:")
    (forward-line)
    (delete-region (point-min) (point))
    (when (search-forward-regexp (rx bol (not whitespace)))
      (forward-char -1)
      (delete-region (point) (point-max)))
    (goto-char (point-min))
    (cl-loop until (eobp)
             collect (let ((line (thing-at-point 'line))
                           (trimmed (string-trim (thing-at-point 'line))))
                       (string-match (rx "(" (group (+ (not (any ",)")))) (any ",)")) line)
                       (cons trimmed (match-string 1 line)))
             do (forward-line))))

(defun minizinc-ts--ask-for-solver ()
  "Ask the user to choose a solver."
  (let ((solvers (minizinc-ts--solvers)))
    (alist-get (completing-read "Solver: " solvers) solvers nil nil #'equal)))

(defun minizinc-ts-solve (file &optional solver)
  "Ask minizinc to solve the FILE using SOLVER.

SOLVER, a string, is passed to minizinc's --solver argument.
If SOLVER is nil or t, the --solver argument is not passed.

Interactively, if a prefix argument is provided, asks which
solver to use. Otherwise, the solver depends on the value of
`minizinc-ts-solver'."
  (interactive
   (list (progn
           (when (and (buffer-modified-p)
                      (y-or-n-p "Save this buffer first?"))
             (save-buffer))
           (or buffer-file-name
               (user-error "Current buffer doesn't have a file")))
         (minizinc-ts--solver current-prefix-arg))
   minizinc-ts-mode)
  (funcall minizinc-ts-compile-function
           (format "%s %s %s"
                   (shell-quote-argument minizinc-ts-program)
                   (if (and solver (not (eq solver t)))
                       (concat "--solver " (shell-quote-argument solver))
                     "")
                   (shell-quote-argument file))))

;;;; Commands

(defvar-keymap minizinc-ts-mode-map
  "C-c C-c" #'minizinc-ts-solve
  "C-c C-s" #'minizinc-ts-solve)

(defvar minizinc-ts-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; comments "% ...\n"
    (modify-syntax-entry ?% "< b" table)
    (modify-syntax-entry ?\n "> b" table)

    table))

;;;###autoload
(define-derived-mode minizinc-ts-mode prog-mode "MiniZinc TS"
  "Major mode for the MiniZinc constraint modeling language."
  :group 'minizinc-ts
  (setq-local comment-start "%"
              comment-end ""
              comment-start-skip (rx (one-or-more "%") (zero-or-more blank))
              tab-width minizinc-ts-indent-offset)

  (when (treesit-ready-p 'minizinc)
    (treesit-parser-create 'minizinc)
    (minizinc-ts--ts-setup)))

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.[mdfo]zn\\'" #'minizinc-ts-mode))

(add-to-list 'treesit-language-source-alist
             '(minizinc . ("https://github.com/shackle-rs/shackle" nil "parsers/tree-sitter-minizinc/src")))

;;;###autoload
(defun minizinc-ts-install-treesit-grammar ()
  "Install the tree-sitter grammar for the MiniZinc constraint modeling language.

Uses `treesit-install-language-grammar'."
  (interactive)
  (treesit-install-language-grammar 'minizinc))

;;;; Private

(defvar minizinc-ts--ts-font-lock-rules
  '(:language minizinc
    :override t
    :feature comments
    ((line_comment) @font-lock-comment-face
     (block_comment) @font-lock-comment-face)

    :language minizinc
    :override t
    :feature types
    ((primitive_type)      @font-lock-type-face
     (type_inst_id)        @font-lock-type-face
     (type_inst_enum_id)   @font-lock-type-face
     (enumeration "enum"   @font-lock-type-face)
     (array_type  "array"  @font-lock-type-face)
     (set_type    "set"    @font-lock-type-face)
     (tuple_type  "tuple"  @font-lock-type-face)
     (record_type "record" @font-lock-type-face)
     (operation_type "op"  @font-lock-type-face)
     (any_type)            @font-lock-type-face)

    :language minizinc
    :override t
    :feature var-use
    ((identifier) @font-lock-variable-use-face)

    :language minizinc
    :override t
    :feature var-declarations
    ((declaration
      name: _ @font-lock-variable-name-face))

    :language minizinc
    :override t
    :feature keywords
    ("var"        @font-lock-keyword-face
     "constraint" @font-lock-keyword-face
     "solve"      @font-lock-keyword-face
     "satisfy"    @font-lock-keyword-face
     "maximize"   @font-lock-keyword-face
     "minimize"   @font-lock-keyword-face
     "output"     @font-lock-keyword-face
     "par"        @font-lock-keyword-face
     "of"         @font-lock-keyword-face
     "where"      @font-lock-keyword-face
     "ann"        @font-lock-keyword-face
     "annotation" @font-lock-keyword-face
     "any"        @font-lock-keyword-face
     "function"   @font-lock-keyword-face
     "include"    @font-lock-keyword-face
     "op"         @font-lock-keyword-face
     "predicate"  @font-lock-keyword-face
     "record"     @font-lock-keyword-face
     "test"       @font-lock-keyword-face
     "tuple"      @font-lock-keyword-face
     "type"       @font-lock-keyword-face
     "if"         @font-lock-keyword-face
     "else"       @font-lock-keyword-face
     "elseif"       @font-lock-keyword-face
     "endif"      @font-lock-keyword-face
     "false"      @font-lock-keyword-face
     "true"       @font-lock-keyword-face
     "in"         @font-lock-keyword-face
     "then"       @font-lock-keyword-face
     "opt"        @font-lock-keyword-face
     "let"        @font-lock-keyword-face)

    :language minizinc
    :override t
    :feature operators
    ((infix_operator
      operator: _ @font-lock-operator-face))

    :language minizinc
    :override t
    :feature delimiters
    (":" @font-lock-delimiter-face
     ";" @font-lock-delimiter-face
     "," @font-lock-delimiter-face)

    :language minizinc
    :override t
    :feature brackets
    ("(" @font-lock-bracket-face
     ")" @font-lock-bracket-face
     "[" @font-lock-bracket-face
     "]" @font-lock-bracket-face
     "{" @font-lock-bracket-face
     "}" @font-lock-bracket-face)

    :language minizinc
    :override t
    :feature function-calls
    ((generator_call
      function: _ @font-lock-function-call-face)
     (call
      function: _ @font-lock-function-call-face)
     (quoted_identifier) @font-lock-function-call-face)

    :language minizinc
    :override t
    :feature misc-punctuation
    ("=" @font-lock-misc-punctuation-face)

    :language minizinc
    :override t
    :feature numbers
    ((integer_literal) @font-lock-number-face
     (float_literal) @font-lock-number-face
     (infinity) @font-lock-number-face)

    :language minizinc
    :override t
    :feature strings
    ((string_literal) @font-lock-string-face
     (string_characters) @font-lock-string-face
     (escape_sequence) @font-lock-escape-face
     (string_interpolation
      "\"" @font-lock-string-face
      "\\(" @font-lock-escape-face
      ")" @font-lock-escape-face))

    :language minizinc
    :override t
    :feature array-labels
    ((_ column_index: (identifier) @font-lock-constant-face)
     (_        index: (identifier) @font-lock-constant-face))

    :language minizinc
    :override t
    :feature property-declaration
    ((record_type_field name: _ @font-lock-property-name-face))

    :language minizinc
    :override t
    :feature property-use
    ((record_access field: _ @font-lock-property-use-face)
     (tuple_access  field: _ @font-lock-property-use-face))

    :language minizinc
    :override t
    :feature annotations
    ((_ annotation: _ @font-lock-preprocessor-face)
     "::" @font-lock-preprocessor-face))
  "The rules for syntax highlighting MiniZinc code based on tree-sitter.

This is passed to `treesit-font-lock-rules' and assigned to
`treesit-font-lock-settings' in `minizinc-ts--ts-setup'.")

;; As best as I can tell, minizinc doesn't really have a consistent indentation
;; convention, so these are just some reasonable-seeming rules.
(defvar minizinc-ts--ts-indent-rules
  `(;; line up "|"s in matrices
    ((n-p-gp ,(rx "|") ,(rx "array_literal_2d") nil) parent 1)
    ;; line up closing brackets with start of line of opening brackets
    ((n-p-gp ,(rx (or "]" "}" ")")) nil nil) parent-bol 0)
    ;; semicolon should be indented
    ((n-p-gp ,(rx ";") nil nil) parent-bol minizinc-ts-indent-offset)
    ;; all top-level things should be at indent level 0
    ((parent-is "source_file") parent-bol 0)
    ;; Everything else should be indented one level further then its parent:
    (catch-all parent-bol minizinc-ts-indent-offset))
  "Rules for indenting MiniZinc code based on tree-sitter.

This is assigned to an entry of `treesit-simple-indent-rules'.")

(defun minizinc-ts--ts-setup ()
  "Setup Tree Sitter for the MiniZinc mode."
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules minizinc-ts--ts-font-lock-rules))

  (setq-local treesit-font-lock-feature-list
              '((comments var-declarations)
                (keywords strings types)
                (numbers)
                (operators delimiters array-labels var-use function-calls misc-punctuation property-declaration property-use annotations)))

  (setf (alist-get 'minizinc treesit-simple-indent-rules)
        minizinc-ts--ts-indent-rules)

  (treesit-major-mode-setup))

;;;; Footer

(provide 'minizinc-ts-mode)
;;; minizinc-ts-mode.el ends here
