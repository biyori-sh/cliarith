(defsystem "cliarith"
  :description "Simple interval arithmetic in Common Lisp"
  :author "biyori-sh"
  :license "MIT"
  :depends-on ()
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "interval-arithmetic")))))
