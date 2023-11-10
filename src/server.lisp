(defpackage :hx-app/server
  (:nicknames :server)
  (:use :cl)
  (:import-from #:lack.session.store.dbi #:make-dbi-store)
  (:export #:start! #:stop! #:restart!))
(in-package :hx-app/server)

;; ----------------------------------------------------------------------------
(defvar *server* nil)

;; ----------------------------------------------------------------------------
(defun clackup (app &key public? join?)
  (clack:clackup
   (lack:builder
    (:session :store (make-dbi-store :connector (lambda () mito:*connection*)))
    (:static :path "/static/" :root #P"./static/")
    app)
   :address (if public? "0.0.0.0" "127.0.0.1")
   :use-thread (not join?)
   :server :woo))

;; ----------------------------------------------------------------------------
(defun start! (&key public? join?)
  (unless *server*
    (db:connect)
    (setf *server* (clackup web:*app* :public? public? :join? join?))))

;; ----------------------------------------------------------------------------
(defun stop! ()
  (when *server*
    (clack:stop *server*)
    (setf *server* nil)
    (mito:disconnect-toplevel)))

;; ----------------------------------------------------------------------------
(defun restart! (&key public? join?)
  (stop!)
  (start! :public? public? :join? join?))
