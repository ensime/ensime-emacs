;;; ensime-sbt.el --- SBT support for ENSIME
;;
;;;; License
;;
;;     Copyright (C) 2008 Raymond Paul Racine
;;     Portions Copyright (C) Free Software Foundation
;;     Portions Copyright (C) 2010 Aemon Cannon
;;
;;     Authors: Luke Amdor, Raymond Racine, Aemon Cannon
;;
;;     This file includes code from slime.el of the SLIME project
;;     (also licensend under the GNU General Public License.) The
;;     following copyrights therefore apply:
;;
;;     Copyright (C) 2003  Eric Marsden, Luke Gorrie, Helmut Eller
;;     Copyright (C) 2004,2005,2006  Luke Gorrie, Helmut Eller
;;     Copyright (C) 2007,2008,2009  Helmut Eller, Tobias C. Rittweiler
;;
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.


;; Support for running sbt in inferior mode.
;; 20090918 Suggestions from Florian Hars
;; - Removed global manipulations.
;; - Removed colorization attempts to use base sbt anis colorization.

(eval-when-compile
  (require 'cl)
  (require 'ensime-macros))

(require 'sbt-mode)
(require 'hydra)

(defgroup ensime-sbt nil
  "Support for sbt build REPL."
  :group 'ensime
  :prefix "ensime-sbt-")

(defcustom ensime-sbt-perform-on-save nil
  "Which (if any) sbt action to perform when a file is saved."
  :type '(choice (const nil) string)
  :group 'ensime-sbt)

(defun ensime-sbt ()
  "Switch to the sbt shell (create if necessary) if or if already there, back.
   If already there but the process is dead, restart the process. "
  (interactive)
  (ensime-with-conn-interactive
   conn
   (with-current-buffer (sbt-start)
     (setq ensime-buffer-connection conn)
     (add-hook 'ensime-source-buffer-saved-hook 'ensime-sbt-maybe-auto-compile)
     (add-hook 'comint-output-filter-functions 'ensime-inf-postoutput-filter))))

(defun ensime-sbt-maybe-auto-compile ()
  (when (and
         (ensime-connected-p)
         ensime-sbt-perform-on-save
         (get-buffer (sbt:buffer-name)))
    (sbt-command ensime-sbt-perform-on-save)))

(defun ensime-sbt-switch ()
  (interactive)
  (ensime-sbt))

(defun ensime-sbt-do-compile ()
  (interactive)
  (sbt-command "compile"))

(defun ensime-sbt-do-run ()
  (interactive)
  (sbt-command "run"))

(defun ensime-sbt-do-clean ()
  (interactive)
  (sbt-command "clean"))

(defun ensime-sbt-do-gen-ensime ()
  (interactive)
  (sbt-command "gen-ensime"))

(defun ensime-sbt-do-package ()
  (interactive)
  (sbt-command "package"))

;; shameless copypasta from magit-utils.el
;; added one little thing to eval the dynamic submodule binding
(defmacro ensime-sbt-read-char-prompt (prompt verbose &rest clauses)
  "Prompts and reads for a character defined in CLAUSES.

CLAUSES can either be an associative list of list clauses
(char prompt string), or it can be something that when evaluated
returns an associative list of the same structure.  Prompting for
modules is done dynamically, and uses the latter, while other
prompting requires no evaluation and is done up front."
  (let* ((first-clause (car clauses))
	 (clauses (if (and (sequencep first-clause)
			   (= 3 (length first-clause)))
		      clauses
		    (eval clauses))))
    (declare (indent 2)
	     (debug (form form &rest (characterp form body))))
    `(pcase (read-char-choice
	     (concat ,prompt
		     ,(concat (mapconcat 'cadr clauses ", ")
			      (and verbose ", or [C-g] to abort") " "))
	     ',(mapcar 'car clauses))
       ,@(--map `(,(car it) ,@(cddr it)) clauses))))

(defun ensime-sbt-module-prompt (module abbrev li ri)
  "Makes the module prompt string.

SUBPROJECT is the module name, i.e. `core' ABBREV is the
abbreviation character for the prompt, i.e. `?o' LI AND RI are
the indices to the left and right of the ABBREV character in the
string.

Results in the same module name, but with brackets enclosing the
abbreviation character."
  (let ((left (s-left li module))
	(prompt-abbrev (char-to-string abbrev))
	(right (-> (- (length module) ri) (s-right module))))
    (s-concat left "[" prompt-abbrev "]" right)))

(defun ensime-sbt-prompt-char-indices (module li ri module-prompt-clauses)
  "Finds the char abbreviation and indices of this MODULE.

MODULE is the module name, i.e. `core'.
MODULE-PROMPT-CLAUSES is a
potentially empty associative list that is of the form
  (char module-prompt module).

Looks at the character defined at left and right indices (`LI'
and `RI'), makes sure that character isn't already used as a key
in the MODULE-PROMPT-CLAUSES If no key is defined for that
character use it, otherwise move on to the next character and try
again."
  (let ((abbrev (-> (substring module li ri) string-to-char)))
    (if (eq nil (assoc abbrev module-prompt-clauses))
	(list abbrev li ri)
      (ensime-sbt-prompt-char-indices module (+ 1 li) (+ 1 ri) module-prompt-clauses))))

(defun ensime-sbt-module-prompt-clauses ()
  "Creates the module prompt associative list, used during prompting.

From ensime config file, get subproject names, and transform them
into an associative list with elements
  (char-abbrev module-prompt-string module-name)"
  (let* ((config (-> (ensime-connection) ensime-config))
	 (subprojects (->> (plist-get config :subprojects)
			   (-map (lambda (sp) (plist-get sp :name)))))
	 (module-clauses
	  (let (prompt-clauses)
	    (-each subprojects
	      (lambda (sp)
		(let* ((ai (ensime-sbt-prompt-char-indices sp 0 1 prompt-clauses))
		       (prompt (apply 'ensime-sbt-module-prompt sp ai))
		       (new-clauses (cons (list (car ai) prompt sp) prompt-clauses)))
		  (setq prompt-clauses new-clauses))))
	    prompt-clauses)))
    module-clauses))

(defun ensime-sbt-prompt-for-test ()
  "Prompt sequence when `*-test-dwim' can't figure out what to do."
  (let ((module
	 (ensime-sbt-read-char-prompt
	  "Do you want to run from module " t ensime-sbt-module-prompt-clauses))
	(source-set
	 (ensime-sbt-read-char-prompt "Do you want to run from " t
				      (?t "[t]est" "")
				      (?i "[i]t" "it:")
				      (?f "[f]un" "fun:")))
	(task
	 (ensime-sbt-read-char-prompt "Do you want to run " t
				      (?t "[t]est" "test")
				      (?o "test-[o]nly" "testOnly")
				      (?q "test-[q]uick" "testQuick"))))
    (concat module "/" source-set task)))

(defun ensime-sbt-find-subproject (file-name source-set)
  (when source-set
    (let* ((config (-> (ensime-connection) ensime-config))
	   (subprojects (plist-get config :subprojects))
	   (matches-subproject-dir? (lambda (dir) (string-match-p dir file-name)))
	   (find-subproject (lambda (sp)
			      (-any matches-subproject-dir? (plist-get sp :source-roots)))))
      (-> (-find find-subproject subprojects) (plist-get :name)))))

(defun ensime-sbt-test-dwim (command)
  (let* ((file-name (or buffer-file-name default-directory))
	 (source-set (cond
		      ((string-match-p "src/test" file-name) "")
		      ((string-match-p "src/it" file-name) "it:")
		      ((string-match-p "src/fun" file-name) "fun:"))))
    (if source-set
	(-> (ensime-sbt-find-subproject file-name source-set)
	    (concat "/" source-set command)
	    sbt-command)
      (-> (ensime-sbt-prompt-for-test) sbt-command))))

(defun ensime-sbt-do-test-dwim ()
  (interactive)
  (ensime-sbt-test-dwim "test"))

(defun ensime-sbt-do-test-quick-dwim ()
  (interactive)
  (ensime-sbt-test-dwim "testQuick"))

(defun ensime-sbt-do-test-only-dwim ()
  (interactive)
  (let* ((impl-class
	  (or (ensime-top-level-class-closest-to-point)
	      (return (message "Could not find top-level class"))))
	 (cleaned-class (replace-regexp-in-string "<empty>\\." "" impl-class))
	 (command (concat "test-only" " " cleaned-class)))
    (ensime-sbt-test-dwim command)))


(provide 'ensime-sbt)

;; Local Variables:
;; End:

