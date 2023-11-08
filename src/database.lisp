(uiop:define-package :hx-app/database
  (:nicknames :db)
  (:use :cl :cl-annot.class)
  (:export #:migrate #:connect))
(in-package :hx-app/database)
(cl-annot:enable-annot-syntax)

;;
;; Utilities
;;

;; ----------------------------------------------------------------------------
(defvar tables (make-hash-table))

;; ----------------------------------------------------------------------------
(defun add-table (table)
  (setf (gethash table tables) t))

;; ----------------------------------------------------------------------------
(defun migrate (&key recreate?)
  (loop :for table :being :the :hash-key :of tables
        :do (if recreate?
                (mito:recreate-table table)
                (mito:ensure-table-exists table))))

;; ----------------------------------------------------------------------------
(defun connect ()
  (mito:connect-toplevel
   :sqlite3
   :database-name "/tmp/garlic-vulner.sqlite3"))

;;
;; Data models
;;

;; ----------------------------------------------------------------------------
@export-class
(mito:deftable session ()
  ((id
    :col-type (:char 72)
    :primary-key t
    :initarg id
    :accessor session-id)
   (session-data
    :col-type (or :null :text)
    :initarg data
    :accessor session-data))
  (:table-name "sessions")
  (:record-timestamps nil))
(add-table 'session)

;; ----------------------------------------------------------------------------
@export-class
(mito:deftable user ()
  ((name
    :col-type (:varchar 64)
    :unique-key t
    :initarg :name
    :accessor user-name)
   (email
    :col-type (:varchar 256)
    :initarg :email
    :accessor user-email)
   (password
    :col-type (:varchar 256)
    :initarg :password
    :accessor user-password)
   (role
    :col-type (:varchar 32)
    :initarg :role
    :initform "basic"
    :accessor user-role)))
(add-table 'user)

;; ----------------------------------------------------------------------------
@export-class
(mito:deftable post ()
  ((user
    :col-type user
    :references user
    :initarg :user
    :accessor post-user)
   (message
    :col-type :text
    :initarg :message
    :accessor post-message)))
(add-table 'post)

;; ----------------------------------------------------------------------------
@export-class
(mito:deftable post-like ()
  ((user
    :col-type user
    :references user
    :initarg :user
    :accessor post-like-user)
   (post
    :col-type post
    :references post
    :initarg :post
    :accessor post-like-post)))
(add-table 'post-like)
