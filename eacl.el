;;; eacl.el --- Emacs auto-complete line by GNU grep

;; Copyright (C) 2017 Chen Bin
;;
;; Version: 0.0.3
;; Keywords: autocomplete line
;; Author: Chen Bin <chenbin DOT sh AT gmail DOT com>
;; URL: http://github.com/redguardtoo/eapl
;; Package-Requires: ((ivy "0.9.1") (emacs "24.3"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Mutiple commands are provided to grep files in the project to get
;; Multiple commands are provided to grep files in the project to get
;; auto complete candidates.
;; The keyword to grep is the text from line beginning to current cursor.
;; Project is *automatically* detected if Git/Mercurial/Subversion is used.
;; You can override the project root by setting `eacl-project-root',
;;
;; List of commands,
;;
;; `eacl-complete-line' complete line.  You could assign key binding
;; "C-x C-l" to this command.
;;
;; `eacl-complete-statement' completes statement which ends with ";".
;;
;; `eacl-complete-snippet' completes snippets which ends with "}".
;;
;; `eacl-complete-tag' completes HTML tag which ends with ">".
;;
;; GNU Grep, Emacs 24.3 and counsel (https://github.com/abo-abo/swiper)
;; are required.
;;
;; Please use HomeBrew (https://brew.sh/) to install GNU Grep on macOS.
;; Then insert `(setq eacl-grep-program "ggrep")' into "~/.emacs".
;; The bundled "BSD Grep" on macOS is too outdated to use.


;;; Code:
(require 'ivy)
(require 'cl-lib)

(defgroup eacl nil
  "eacl")

(defcustom eacl-statement-regex "[^;]*;"
  "Regular expression to match the statement."
  :type 'string
  :group 'eacl)

(defcustom eacl-grep-program "grep"
  "GNU Grep program."
  :type 'string
  :group 'eacl)

(defcustom eacl-project-root nil
  "Project root.  If it's nil project root is detected automatically."
  :type 'string
  :group 'eacl)

(defcustom eacl-project-file '(".svn" ".hg" ".git")
  "The file/directory used to locate project root."
  :type '(repeat sexp)
  :group 'eacl)

(defvar eacl-keyword-start nil
  "The start position of multi-line keyword.  Internal variable.")

(defcustom eacl-grep-ignore-dirs
  '(".git"
    ".bzr"
    ".svn"
    "bower_components"
    "node_modules"
    ".sass-cache"
    ".cache"
    "test"
    "tests"
    ".metadata"
    "logs")
  "Directories to ignore when grepping."
  :type '(repeat sexp)
  :group 'eacl)

(defcustom eacl-grep-ignore-file-exts
  '("log"
    "properties"
    "session"
    "swp")
  "File extensions to ignore when grepping."
  :type '(repeat sexp)
  :group 'eacl)

(defcustom eacl-grep-ignore-file-names
  '("TAGS"
    "tags"
    "GTAGS"
    "GPATH"
    ".bookmarks.el"
    "*.svg"
    "history"
    "#*#"
    "*.min.js"
    "*bundle*.js"
    "*vendor*.js"
    "*.min.css"
    "*~")
  "File names to ignore when grepping."
  :type '(repeat sexp)
  :group 'eacl)

;;;###autoload
(defun eacl-get-project-root ()
  "Get project root."
  (or eacl-project-root
      (cl-some (apply-partially 'locate-dominating-file
                                default-directory)
               eacl-project-file)))

;;;###autoload
(defun eacl-current-line ()
  "Current line."
  (buffer-substring-no-properties (line-beginning-position)
                                  (point)))

(defun eacl-trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun eacl-encode(s)
  "Encode S."
  (message "eacl-encode called")
  ;; encode "{}[]"
  (setq s (replace-regexp-in-string "\"" "\\\\\"" s))
  (setq s (replace-regexp-in-string "\\?" "\\\\\?" s))
  (setq s (replace-regexp-in-string "\\$" "\\\\x24" s))
  (setq s (replace-regexp-in-string "\\*" "\\\\\*" s))
  (setq s (replace-regexp-in-string "\\." "\\\\\." s))
  (setq s (replace-regexp-in-string "\\[" "\\\\\[" s))
  (setq s (replace-regexp-in-string "\\]" "\\\\\]" s))
  (setq s (replace-regexp-in-string "\n" "[[:space:]]" s))
  (setq s (replace-regexp-in-string "\r" "[[:space:]]" s))
  ;; don't know how to pass "(" to shell, so a little noise here
  (setq s (replace-regexp-in-string "\(" "." s))
  (setq s (replace-regexp-in-string "\)" "." s))
  s)

(defun eacl-grep-exclude-opts ()
  "Create grep exclude options."
  (concat (mapconcat (lambda (e) (format "--exclude-dir='%s'" e))
                     eacl-grep-ignore-dirs " ")
          " "
          (mapconcat (lambda (e) (format "--exclude='*.%s'" e))
                     eacl-grep-ignore-file-exts " ")
          " "
          (mapconcat (lambda (e) (format "--exclude='%s'" e))
                     eacl-grep-ignore-file-names " ")))

;;;###autoload
(defun eacl-get-keyword (cur-line)
  "Get trimmed keyword from CUR-LINE."
  (message "eacl-get-keyword called")
  (let* ((keyword (replace-regexp-in-string "^[ \t]*"
                                            ""
                                            cur-line)))
    (eacl-encode keyword)))

(defun eacl-replace-text (content start is-multiline)
  "Insert CONTENT from START to current point if IS-MULTILINE is t."
  (delete-region start (if is-multiline (point) (line-end-position)))
  (insert content))

(defun eacl-create-candidate-summary (s)
  "If S is too wide to fit into the screen, return pair summary and S."
  (message "eacl-create-candidate-summary called")
  (let* ((w (frame-width))
         ;; display kill ring item in one line
         (key (replace-regexp-in-string "[ \t]*[\n\r]+[ \t]*" "\\\\n" s)))
    ;; strip the whitespace
    (setq key (replace-regexp-in-string "^[ \t]+" "" key))
    ;; fit to the minibuffer width
    (if (> (length key) w)
        (setq key (concat (substring key 0 (- w 4)) "...")))
    (cons key s)))

(defun eacl-complete-line-or-statement (regex cur-line keyword start)
  "Complete line or statement according to REGEX.
If REGEX is nil, we only complete current line.
CUR-LINE and KEYWORD are also required.
If REGEX is not nil, complete statement."
  (let* ((default-directory (or (eacl-get-project-root) default-directory))
         (cmd-format-opts (if regex "%s -rshPzoI %s \"%s\" *"
                            "%s -rshEI %s \"%s\" *"))
         (cmd (format cmd-format-opts
                      eacl-grep-program
                      (eacl-grep-exclude-opts)
                      (if regex (concat keyword regex)
                        keyword)))
         ;; Please note grep's "-z" will output null character at the end of each candidate
         (sep (if regex "\x0" "[\r\n]+"))
         (collection (split-string (shell-command-to-string cmd) sep t "[ \t\r\n]+"))
         (rlt t))
    ;; (message "cmd=%s collection length=%s sep=%s" cmd (length collection) sep)
    (when collection
      (setq collection (delq nil (delete-dups collection)))
      (cond
       ((= 1 (length collection))
        ;; insert only candidate
        (cond
         ((string= (car collection) (buffer-substring-no-properties start (point)))
          (message "%s===%s" (length (car collection)) (buffer-substring-no-properties start (point)))
          (setq rlt nil))
         (t
          (message "%s===%s" (length (car collection)) (length (buffer-substring-no-properties start (point))))
          (eacl-replace-text (car collection) start regex))))
       ((> (length collection) 1)
        ;; uniq
        (if regex
          (setq collection (mapcar 'eacl-create-candidate-summary collection)))
        (ivy-read "candidates:"
                  collection
                  :action (lambda (l)
                            (if (consp l) (setq l (cdr l)))
                            (eacl-replace-text l start regex))))))
    (unless collection (setq rlt nil))
    rlt))

;;;###autoload
(defun eacl-complete-line ()
  "Complete line by grepping in project.
The keyword to grep is the text from line beginning to current cursor."
  (interactive)
  (let* ((cur-line (eacl-current-line))
         (start (eacl-line-beginning-position))
         (keyword (eacl-get-keyword cur-line)))
    (eacl-complete-line-or-statement nil cur-line keyword start)))

;;;###autoload
(defun eacl-complete-statement (&optional keyword)
  "Complete statement which end with pattern `eacl-statement-regex'.
The KEYWORD to grep is the text from line beginning to current cursor if not provided."
  (interactive)
  (let* ((cur-line (eacl-current-line))
         (start (eacl-line-beginning-position))
         (keyword (eacl-get-keyword cur-line)))
    (eacl-complete-line-or-statement "[^;]*;" cur-line keyword start)))

;;;###autoload
(defun eacl-complete-snippet ()
  "Complete snippet which ends with \"}\".
The keyword to grep is the text from line beginning to current cursor."
  (interactive)
  (let* ((cur-line (eacl-current-line))
         (start (eacl-line-beginning-position))
         (keyword (eacl-get-keyword cur-line)))
    (eacl-complete-line-or-statement "[^}]*}" cur-line keyword start)))

(defun eacl-line-beginning-position ()
  (save-excursion (back-to-indentation) (point)))

;;;###autoload
(defun eacl-complete-tag ()
  "Complete snippet which ends with \">\".
The keyword to grep is the text from line beginning to current cursor."
  (interactive)
  (let* ((cur-line (eacl-current-line))
         (keyword (eacl-get-keyword cur-line))
         (start (eacl-line-beginning-position))
         (continue t))
    (while continue
      (unless (eacl-complete-line-or-statement "[^>]*>" cur-line keyword start)
        (message "Auto-completion done!")
        (setq continue nil))
      (cond
       ((and continue (yes-or-no-p "Continue?"))
        (setq keyword (eacl-encode (eacl-trim-left (buffer-substring-no-properties start (point))))))
       (t
        (setq continue nil))))))

(provide 'eacl)
;;; eacl.el ends here

