(defpackage :hx-app/utils
  (:nicknames :hx/utils)
  (:use :cl :alexandria :lack.response)
  (:export #:param #:send-redirect))
(in-package :hx-app/utils)

;; ----------------------------------------------------------------------------
(defun param (key params)
  "Get a value from request parameters"
  (assoc-value params key :test #'string-equal))

;; ----------------------------------------------------------------------------
(defmacro add-header (resp header)
  `(setf (response-headers ,resp)
         (append (response-headers ,resp)
                 ,header)))

;; ----------------------------------------------------------------------------
(defmacro hx-redirect (resp url)
  `(progn
     (setf (response-headers ,resp)
           (append (response-headers ,resp)
                   (list :hx-redirect ,url)))
     nil))
