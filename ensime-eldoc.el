;;; ensime-eldoc.el --- ELDoc Support for ensime

;;; Commentary:

;;; Code:

(require 'eldoc)
(require 'ensime-client)
(require 'ensime-model)
(require 'ensime-notes)

(defun eldoc-ensime-info ()
  "ELDoc backend for ensime."
  ;; The response from `ensime-rpc-symbol-at-point' has the type info but,
  ;; its sligthly different from the one obtained with `ensime-type-at-point'
  ;; Using the underlying `ensime-rpc-get-type-at-point' to maintain consistency
  (when (ensime-connected-p)
    (let* ((error-msgs (append (ensime-errors-at (point))
                               (ensime-implicit-notes-at (point)))))
      (if error-msgs
          (eldoc-message error-msgs)
        (let*  ((symbol (ensime-rpc-symbol-at-point))
                (type (ensime-rpc-get-type-at-point))
                (name (ensime-symbol-local-name symbol))
                (type-name (ensime-type-name-with-args type)))
          (when (and type (not (string= type-name "<none>")))
            (eldoc-message (concat name ": " type-name))))))))

(provide 'ensime-eldoc)

;;; ensime-eldoc.el ends here
