(require 'websocket)

(defun ensime-websocket-hook-on-open (websocket)
  ""
  (message "websocket open"))

(defun ensime-websocket-hook-on-message (websocket frame)
  ""
  (message "ws frame: %S" (websocket-frame-text frame)))

(defun ensime-websocket-hook-on-close (websocket)
  ""
  (message "websocket closed")
  (setq ensime-websocket-connection nil))

(defun ensime-websocket-connect (host port)
  ""
  (interactive)
  (websocket-open
   (format "ws://%s:%s/websocket" host port)
   :protocols '("swanky")
   :on-open #'ensime-websocket-hook-on-open
   :on-message #'ensime-websocket-hook-on-message
   :on-close #'ensime-websocket-hool-on-close))


(defun ensime-websocket-send-request (connection request)
  ""
  (interactive)
  (websocket-send-text connection (format "%S" `(:req ,request :call-id -1))))

(defun ensime-websocket-send-test-message ()
  ""
  (interactive)
  (ensime-websocket-send-request ensime-websocket-connection '(:ensime-api-public-symbol-search-req (:keywords ("foo" "bar") :max-results 10))))

(provide 'ensime-websocket)
