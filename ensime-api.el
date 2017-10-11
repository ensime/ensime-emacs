(require 'eieio)

(ensime-marshal-defclass ensime-api-offset-range ()
  ((from :initarg :from
         :type number)
   (to :initarg :to
       :type number)))

(ensime-marshal-defclass ensime-api-implicit-info-req ()
  ((file :initarg :file
         :type string)
   (range :initarg :range
          :type ensime-api-offset-range)))

(ensime-marshal-defclass ensime-api-unload-files-req ()
                     ((source :initarg :source
                              :type list)
                      (remove :initarg :remove
                              :type boolean)))

(ensime-marshal-defclass ensime-api-reload-strategy () ()
                     :abstract t)
(ensime-marshal-defclass ensime-api-unload-all (ensime-api-reload-strategy) ())
(ensime-marshal-defclass ensime-api-load-project (ensime-api-reload-strategy) ())
(ensime-marshal-defclass ensime-api-keep-loaded (ensime-api-reload-strategy) ())

(ensime-marshal-defclass ensime-api-restart-scala-compiler-req ()
                     ((id :initarg :id
                          :type string)
                      (strategy :initarg :strategy
                                :type ensime-api-reload-strategy)))

(ensime-marshal-defclass ensime-api-public-symbol-search-req ()
                     ((keywords :initarg :keywords
                                :type list)
                      (max-results :initarg :max-results
                                   :type number)))

(ensime-marshal-defclass ensime-api-import-suggestions-req ()
                     ((file :initarg :file
                            :type string)
                      (point :initarg :point
                             :type number)
                      (names :initarg :names
                             :type list)
                      (max-results :initarg :max-results
                                   :type number)))

(ensime-marshal-defclass ensime-api-fqn-of-symbol-at-point-req ()
                     ((file :initarg :file
                            :type string)
                      (point :initarg :point
                             :type number)))

(ensime-marshal-defclass ensime-api-fqn-of-type-at-point-req ()
                     ((file :initarg :file
                            :type string)
                      (point :initarg :point
                             :type number)))

(ensime-marshal-defclass ensime-api-uses-of-symbol-at-point-req ()
                     ((file :initarg :file
                            :type string)
                      (point :initarg :point
                             :type number)))

(ensime-marshal-defclass ensime-api-hierarchy-of-type-at-point-req ()
                     ((file :initarg :file
                            :type string)
                      (point :initarg :point
                             :type number)))

(ensime-marshal-defclass ensime-api-find-usages ()
                     ((fqn :initarg :fqn
                           :type string)))

(ensime-marshal-defclass ensime-api-find-hierarchy ()
                     ((fqn :initarg :fqn
                           :type string)))

(ensime-marshal-defclass ensime-api-doc-uri-at-point-req ()
                     ((file :initarg :file
                            :type string)
                      (point :initarg :point
                             :type ensime-api-offset-range)))

(ensime-marshal-defclass ensime-api-completions-req ()
                     ((file :initarg :file
                            :type string)
                      (point :initarg :point
                             :type number)
                      (max-results :initarg :max-results
                                   :type number)
                      (case-sens :initarg :case-sens
                                 :type boolean)
                      (reload :initarg :reload
                              :type boolean)))

(ensime-marshal-defclass ensime-api-type-at-point-req ()
                     ((file :initarg :file
                            :type string)
                      (range :initarg :range
                             :type ensime-api-offset-range)))

(ensime-marshal-defclass ensime-api-symbol-at-point-req ()
                     ((file :initarg :file
                            :type string)
                      (point :initarg :point
                             :type number)))

(ensime-marshal-defclass ensime-api-refactor-desc () ()
                         :abstract t)

(ensime-marshal-defclass ensime-api-inline-local-refactor-desc (ensime-api-refactor-desc)
                         ((file :initarg :file
                                :type string)
                          (start :initarg :start
                                 :type number)
                          (end :initarg :end
                               :type number)))

(ensime-marshal-defclass ensime-api-rename-refactor-desc (ensime-api-refactor-desc)
                         ((new-name :initarg :new-name
                                    :type string)
                          (file :initarg :file
                                :type string)
                          (start :initarg :start
                                 :type number)
                          (end :initarg :end
                               :type number)))

(ensime-marshal-defclass ensime-api-extract-method-refactor-desc (ensime-api-refactor-desc)
                         ((method-name :initarg :method-name
                                       :type string)
                          (file :initarg :file
                                :type string)
                          (start :initarg :start
                                 :type number)
                          (end :initarg :end
                               :type number)))

(ensime-marshal-defclass ensime-api-extract-local-refactor-desc (ensime-api-refactor-desc)
                         ((name :initarg :name
                                         :type string)
                          (file :initarg :file
                                :type string)
                          (start :initarg :start
                                 :type number)
                          (end :initarg :end
                               :type number)))

(ensime-marshal-defclass ensime-api-organise-imports-refactor-desc (ensime-api-refactor-desc)
                         ((file :initarg :file
                                :type string)))

(ensime-marshal-defclass ensime-api-add-import-refactor-desc (ensime-api-refactor-desc)
                         ((qualified-name :initarg :qualified-name
                                          :type string)
                          (file :initarg :file
                                :type string)))

(ensime-marshal-defclass ensime-api-expand-match-cases-desc (ensime-api-refactor-desc)
                         ((file :initarg :file
                               :type string)
                         (start :initarg :start
                                :type number)
                         (end :initarg :end
                              :type number)))

(ensime-marshal-defclass ensime-api-refactor-req ()
                         ((proc-id :initarg :proc-id
                                   :type number)
                          (params :initarg :params
                                  :type ensime-api-refactor-desc)
                          (interactive :initarg :interactive
                                       :type boolean)))

(ensime-marshal-defclass ensime-api-source-symbol () ()
                         :abstract t)
(ensime-marshal-defclass ensime-api-object-symbol (ensime-api-source-symbol) ())
(ensime-marshal-defclass ensime-api-class-symbol (ensime-api-source-symbol) ())
(ensime-marshal-defclass ensime-api-trait-symbol (ensime-api-source-symbol) ())
(ensime-marshal-defclass ensime-api-package-symbol (ensime-api-source-symbol) ())
(ensime-marshal-defclass ensime-api-constructor-symbol (ensime-api-source-symbol) ())
(ensime-marshal-defclass ensime-api-imported-name-symbol (ensime-api-source-symbol) ())
(ensime-marshal-defclass ensime-api-type-param-symbol (ensime-api-source-symbol) ())
(ensime-marshal-defclass ensime-api-param-symbol (ensime-api-source-symbol) ())
(ensime-marshal-defclass ensime-api-var-field-symbol (ensime-api-source-symbol) ())
(ensime-marshal-defclass ensime-api-val-field-symbol (ensime-api-source-symbol) ())
(ensime-marshal-defclass ensime-api-operator-field-symbol (ensime-api-source-symbol) ())
(ensime-marshal-defclass ensime-api-var-symbol (ensime-api-source-symbol) ())
(ensime-marshal-defclass ensime-api-val-symbol (ensime-api-source-symbol) ())
(ensime-marshal-defclass ensime-api-function-call-symbol (ensime-api-source-symbol) ())
(ensime-marshal-defclass ensime-api-implicit-conversion-symbol (ensime-api-source-symbol) ())
(ensime-marshal-defclass ensime-api-implicit-params-symbol (ensime-api-source-symbol) ())
(ensime-marshal-defclass ensime-api-deprecated-symbol (ensime-api-source-symbol) ())

(ensime-marshal-defclass ensime-api-symbol-designations-req ()
                         ((file :initarg :file
                                :type string)
                          (start :initarg :start
                                 :type number)
                          (end :initarg :end
                               :type number)
                          (requested-types :initarg :requested-types
                                           :type ensime-api-source-symbol)))

(ensime-marshal-defclass ensime-api-expand-selection-req ()
                         ((file :initarg :file
                                :type string)
                          (start :initarg :start
                                 :type number)
                          (end :initarg :end
                               :type number)))

(ensime-marshal-defclass ensime-api-structure-view-req ()
                         ((file-info :initarg :file-info
                                     :type string)))

(ensime-marshal-defclass ensime-api-debug-active-vm-req () ())

(ensime-marshal-defclass ensime-api-debug-attach-req ()
                         ((hostname :initarg :hostname
                                    :type string)
                          (port :initarg :port
                                :type number)))

(ensime-marshal-defclass ensime-api-debug-stop-req () ())

(ensime-marshal-defclass ensime-api-debug-set-break-req ()
                         ((file :initarg :file
                                :type string)
                          (line :initarg :line
                                :type number)))

(ensime-marshal-defclass ensime-api-debug-clear-break-req ()
                         ((file :initarg :file
                                :type string)
                          (line :initarg :line
                                :type number)))

(ensime-marshal-defclass ensime-api-debug-clear-all-breaks-req () ())

(ensime-marshal-defclass ensime-api-debug-list-breakpoints-req () ())

(ensime-marshal-defclass ensime-api-debug-run-req () ())

(ensime-marshal-defclass ensime-api-debug-continue-req ()
                         ((thread-id :initarg :thread-id
                                     :type number)))

(ensime-marshal-defclass ensime-api-debug-step-req ()
                         ((thread-id :initarg :thread-id
                                     :type number)))

(ensime-marshal-defclass ensime-api-debug-next-req ()
                         ((thread-id :initarg :thread-id
                                     :type number)))

(ensime-marshal-defclass ensime-api-debug-step-out-req ()
                         ((thread-id :initarg :thread-id
                                     :type number)))

(ensime-marshal-defclass ensime-api-debug-locate-name-req ()
                         ((thread-id :initarg :thread-id
                                     :type number)
                          (name :initarg :name
                                :type string)))

(ensime-marshal-defclass ensime-api-debug-location () ()
                         :abstract t)
(ensime-marshal-defclass ensime-api-debug-object-reference (ensime-api-debug-location)
                         ((object-id :initarg :object-id
                                     :type number)))
(ensime-marshal-defclass ensime-api-debug-stack-slot (ensime-api-debug-location)
                         ((debug-thread-id :initarg :debug-thread-id
                                           :type number)
                          (frame :initarg :frame
                                 :type number)
                          (offset :initarg :offset
                                  :type number)))
(ensime-marshal-defclass ensime-api-debug-array-element (ensime-api-debug-location)
                         ((object-id :initarg :object-id
                                     :type number)
                          (index :initarg :index
                                 :type number)))
(ensime-marshal-defclass ensime-api-debug-object-field (ensime-api-debug-location)
                         ((object-id :initarg :object-id
                                     :type number)
                          (field :initarg :field
                                 :type string)))

(ensime-marshal-defclass ensime-api-debug-value-req ()
                         ((loc :initarg :loc
                               :type ensime-api-debug-location)))

(ensime-marshal-defclass ensime-api-debug-to-string-req ()
                         ((thread-id :initarg :thread-id
                                     :type string)
                          (loc :initarg :loc
                               :type ensime-api-debug-location)))

(ensime-marshal-defclass ensime-api-debug-set-value-req ()
                         ((loc :initarg :loc
                               :type ensime-api-debug-location)
                          (new-value :initarg :new-value
                                     :type string)))

(ensime-marshal-defclass ensime-api-debug-backtrace-req ()
                         ((thread-id :initarg :thread-id
                                     :type string)
                          (index :initarg :index
                                 :type number)
                          (count :initarg :count
                                 :type number)))
