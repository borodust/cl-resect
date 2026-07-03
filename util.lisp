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
                                resource-paths
                                language
                                standard
                                target
                                single-header-mode
                                diagnostics
                                include-definitions
                                include-sources
                                exclude-definitions
                                exclude-sources
                                enforce-definitions
                                enforce-sources
                                ignore-definitions
                                ignore-sources
                                defines
                                diagnostics-level
				visibility)
                        &body body)
  (alexandria:with-gensyms (path val name)
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
                ,@(when resource-paths
                    `((loop for ,path in ,resource-paths
                            do (%resect:options-add-resource-path ,opts (namestring ,path)))))
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
                ,@(when diagnostics
                    `((when ,diagnostics
                        (%resect:options-enable-diagnostics ,opts))))
                ,@(when include-definitions
                    `((loop for ,val in ,include-definitions
                            do (%resect:options-include-definition ,opts ,val))))
                ,@(when include-sources
                    `((loop for ,val in ,include-sources
                            do (%resect:options-include-source ,opts (namestring ,val)))))
                ,@(when exclude-definitions
                    `((loop for ,val in ,exclude-definitions
                            do (%resect:options-exclude-definition ,opts ,val))))
                ,@(when exclude-sources
                    `((loop for ,val in ,exclude-sources
                            do (%resect:options-exclude-source ,opts (namestring ,val)))))
                ,@(when enforce-definitions
                    `((loop for ,val in ,enforce-definitions
                            do (%resect:options-enforce-definition ,opts ,val))))
                ,@(when enforce-sources
                    `((loop for ,val in ,enforce-sources
                            do (%resect:options-enforce-source ,opts (namestring ,val)))))
                ,@(when ignore-definitions
                    `((loop for ,val in ,ignore-definitions
                            do (%resect:options-ignore-definition ,opts ,val))))
                ,@(when ignore-sources
                    `((loop for ,val in ,ignore-sources
                            do (%resect:options-ignore-source ,opts (namestring ,val)))))
                ,@(when defines
                    `((loop for (,name ,val) in ,defines
                            do (%resect:options-add-define ,opts ,name ,val))))
                ,@(when diagnostics-level
                    `((when ,diagnostics-level
                        (%resect:options-diagnostics-level ,opts ,diagnostics-level))))
		,@(when visibility
		    `((when ,visibility
			(%resect:options-visibility ,opts ,visibility))))
                ,@body)
           (%resect:destroy-options ,opts))))))


(defun parse (filename &key include-paths
                         framework-paths
                         resource-paths
                         language
                         standard
                         target
                         single-header-mode
                         (diagnostics t)
			 diagnostics-level
                         intrinsics
                         include-definitions
                         include-sources
                         exclude-definitions
                         exclude-sources
                         enforce-definitions
                         enforce-sources
                         ignore-definitions
                         ignore-sources
                         defines
			 visibility)
  (with-options (opts :language language
                      :standard standard
                      :target target
                      :include-paths include-paths
                      :framework-paths framework-paths
                      :resource-paths resource-paths
                      :single-header-mode single-header-mode
                      :diagnostics diagnostics
		      :diagnostics-level diagnostics-level
                      :include-definitions include-definitions
                      :include-sources include-sources
                      :exclude-definitions exclude-definitions
                      :exclude-sources exclude-sources
                      :enforce-definitions enforce-definitions
                      :enforce-sources enforce-sources
                      :ignore-definitions ignore-definitions
                      :ignore-sources ignore-sources
                      :defines defines
		      :visibility visibility)
    (loop for intrinsic in intrinsics
          do (%resect:options-enable-intrinsic opts intrinsic))
    (%resect:parse (namestring filename) opts)))


(defmacro with-translation-unit ((unit filename &key include-paths
                                                  framework-paths
                                                  resource-paths
                                                  language
                                                  standard
                                                  target
                                                  single-header-mode
                                                  (diagnostics t)
						  diagnostics-level
                                                  intrinsics
                                                  include-definitions
                                                  include-sources
                                                  exclude-definitions
                                                  exclude-sources
                                                  enforce-definitions
                                                  enforce-sources
                                                  ignore-definitions
                                                  ignore-sources
                                                  defines
						  visibility)
                                 &body body)
  `(let ((,unit (parse ,filename :include-paths ,include-paths
                                 :framework-paths ,framework-paths
                                 :resource-paths ,resource-paths
                                 :language ,language
                                 :standard ,standard
                                 :target ,target
                                 :single-header-mode ,single-header-mode
                                 :diagnostics ,diagnostics
				 :diagnostics-level ,diagnostics-level
                                 :intrinsics ,intrinsics
                                 :include-definitions ,include-definitions
                                 :include-sources ,include-sources
                                 :exclude-definitions ,exclude-definitions
                                 :exclude-sources ,exclude-sources
                                 :enforce-definitions ,enforce-definitions
                                 :enforce-sources ,enforce-sources
                                 :ignore-definitions ,ignore-definitions
                                 :ignore-sources ,ignore-sources
                                 :defines ,defines
				 :visibility ,visibility)))
     (unwind-protect
          (progn ,@body)
       (%resect:free ,unit))))
