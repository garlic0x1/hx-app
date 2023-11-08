(defpackage :hx-app/server
  (:nicknames :server)
  (:use :cl)
  (:import-from #:lack.session.store.dbi #:make-dbi-store)
  (:export #:start! #:stop! #:restart!))
(in-package :hx-app/server)

;; ----------------------------------------------------------------------------
(defvar *server* nil)

;; ----------------------------------------------------------------------------
(defun clackup (app)
  (clack:clackup
   (lack:builder
    (:session :store (make-dbi-store :connector (lambda () mito:*connection*)))
    app)
   :server :woo))

;; ----------------------------------------------------------------------------
(defun start! ()
  (unless *server*
    (db:connect)
    (setf *server* (clackup web:*app*))))

;; ----------------------------------------------------------------------------
(defun stop! ()
  (when *server*
    (clack:stop *server*)
    (setf *server* nil)
    (mito:disconnect-toplevel)))

;; ----------------------------------------------------------------------------
(defun restart! ()
  (stop!)
  (start!))
