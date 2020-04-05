(asdf:defsystem :cl-resect
  :description "Wrapper for libresect"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :depends-on (:alexandria :cffi)
  :serial t
  :components ((:file "resect")
               (:file "util")))
