(asdf:defsystem "hx-app"
  :author "garlic0x1"
  :depends-on (
               ;; lang
               :cl-annot
               :binding-arrows
               :defstar

               ;; db
               :mito

               ;; web
               :woo
               :clack
               :lack-middleware-session
               :lack-session-store-dbi
               :ningle
               :cl-pass
               :hiccl)
  :components
  ((:module "src"
    :components ((:file "database")
                 (:file "utils")
                 (:file "components")
                 (:file "web")
                 (:file "server")))))
