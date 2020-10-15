(cl:defpackage :cl-resect
  (:nicknames :resect)
  (:use :cl)
  (:export #:with-iterator
           #:docollection
           #:with-options
           #:with-translation-unit
           #:parse))
(cl:in-package :cl-resect)

;;;
;;; UTIL
;;;
(defmacro with-iterator ((iterator) collection &body body)
  (alexandria:once-only (collection)
    `(let ((,iterator (%resect:collection-iterator ,collection)))
       (unwind-protect
            (progn ,@body)
         (%resect:iterator-free ,iterator)))))


(defmacro docollection ((element collection) &body body)
  (alexandria:with-gensyms (iterator)
    `(with-iterator (,iterator) ,collection
       (loop while (%resect:iterator-next ,iterator)
             do (let ((,element (%resect:iterator-value ,iterator)))
                  ,@body)))))


(defmacro with-options ((opts &key include-paths
                                framework-paths
                                language
                                standard
                                target
                                single-header-mode)
                        &body body)
  (alexandria:with-gensyms (path)
    (alexandria:once-only (language standard target)
      `(let ((,opts (%resect:make-options)))
         (unwind-protect
              (progn
                ,@(when include-paths
                    `((loop for ,path in ,include-paths
                            do (%resect:options-add-include-path ,opts (namestring ,path)))))
                ,@(when framework-paths
                    `((loop for ,path in ,framework-paths
                            do (%resect:options-add-include-path ,opts (namestring ,path)))))
                ,@(when language
                    `((when ,language
                        (%resect:options-add-language ,opts ,language))))
                ,@(when standard
                    `((when ,standard
                        (%resect:options-add-standard ,opts ,standard))))
                ,@(when target
                    `((when ,target
                        (%resect:options-add-target ,opts ,target))))
                ,@(when single-header-mode
                    `((when ,single-header-mode
                        (%resect:options-enable-single-header-mode ,opts))))
                ,@body)
           (%resect:destroy-options ,opts))))))


(defun parse (filename &key include-paths
                         framework-paths
                         language
                         standard
                         target
                         single-header-mode)
  (with-options (opts :include-paths include-paths
                      :framework-paths framework-paths
                      :language language
                      :standard standard
                      :target target
                      :single-header-mode single-header-mode)
    (%resect:parse (namestring filename) opts)))


(defmacro with-translation-unit ((unit filename &key include-paths
                                                  framework-paths
                                                  language
                                                  standard
                                                  target
                                                  single-header-mode)
                                 &body body)
  `(let ((,unit (parse ,filename :include-paths ,include-paths
                                 :framework-paths ,framework-paths
                                 :language ,language
                                 :standard ,standard
                                 :target ,target
                                 :single-header-mode ,single-header-mode)))
     (unwind-protect
          (progn ,@body)
       (%resect:free ,unit))))
