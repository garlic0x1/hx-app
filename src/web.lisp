(defpackage :hx-app/web
  (:nicknames :web)
  (:use :cl :alexandria :binding-arrows :hx-app/utils :ningle)
  (:import-from #:defstar #:lambda*)
  (:import-from #:cl-pass #:check-password #:hash)
  (:export #:*app*))
(in-package :hx-app/web)

;; ----------------------------------------------------------------------------
(defvar *app* (make-instance 'app))

;;
;; Requirements
;;

;; ----------------------------------------------------------------------------
(setf (requirement *app* :auth)
      (lambda* (_)
        (when-let* ((uid (gethash :user-id *session*))
                    (dao (mito:find-dao 'db:user :id uid)))
          (values t dao))))

;;
;; Endpoints
;;

;; ----------------------------------------------------------------------------
(setf (route *app* "/" :method :GET :auth t)
      (lambda* (_)
        (hiccl:render nil
          (comps:htmx-page
           comps:top-bar-auth
           '(:div#view)))))

;; ----------------------------------------------------------------------------
(setf (route *app* "/" :method :GET)
      (lambda* (_)
        (hiccl:render nil
          (comps:htmx-page
           comps:top-bar-unauth
           '(:div#view)))))

;; ----------------------------------------------------------------------------
(setf (route *app* "/rxss" :method :GET)
      (lambda (params)
        (hiccl:render nil
          `(:<>
            (:a :href "/rxss?attr=hi&text=world" "/rxss?attr=hi&text=world")
            (:div :attr-sink ,(param :attr params)
                  ,(param :text params))))))

;; ----------------------------------------------------------------------------
(setf (route *app* "/feed" :method :GET)
      (lambda* (_)
        (hiccl:render nil
          `(:<> ,@(mito:select-dao 'db:post)))))

;; ----------------------------------------------------------------------------
(setf (route *app* "/post" :method :GET :auth t)
      (lambda* (_)
        (hiccl:render nil
          comps:post-form)))

;; ----------------------------------------------------------------------------
(setf (route *app* "/post" :method :GET)
      (lambda* (_)
        (hiccl:render nil
          `(:div "You must be logged in to post"))))

;; ----------------------------------------------------------------------------
(setf (route *app* "/post/:id/like" :method :POST :auth t)
      (lambda (params)
        (ignore-errors
         (mito:create-dao
          'db:post-like
          :post (mito:find-dao 'db:post :id (param :id params))
          :user (param :auth params)))))

;; ----------------------------------------------------------------------------
(setf (route *app* "/post/:id" :method :GET)
      (lambda (params)
        (hiccl:render nil
          (mito:find-dao 'db:post :id (param :id params)))))

;; ----------------------------------------------------------------------------
(setf (route *app* "/post" :method :POST :auth t)
      (lambda (params)
        (when-let* ((user (param :auth params))
                    (msg (param :message params)))
          (mito:create-dao 'db:post :user user :message msg))))

;; ----------------------------------------------------------------------------
(setf (route *app* "/profile" :method :GET :auth t)
      (lambda* (_)
        (hiccl:render nil
          (mito:find-dao 'db:user :id (gethash :user-id *session*)))))

;; ----------------------------------------------------------------------------
(setf (route *app* "/profile" :method :GET)
      (lambda* (_)
        (hiccl:render nil
          `(:div "You must be logged in to view profile"))))

;;
;; Auth endpoints
;;

;; ----------------------------------------------------------------------------
(setf (route *app* "/login" :method :GET)
      (lambda* (_)
        (hiccl:render nil
          comps:login-form)))

;; ----------------------------------------------------------------------------
(setf (route *app* "/login" :method :POST)
      (lambda (params)
        (when-let* ((name (param :username params))
                    (pass (param :password params))
                    (dao (mito:find-dao 'db:user :name name)))
          (when (check-password pass (db:user-password dao))
            (setf (gethash :user-id *session*) (mito:object-id dao))
            (hx-redirect *response* "/")))))

;; ----------------------------------------------------------------------------
(setf (route *app* "/signup" :method :GET)
      (lambda* (_)
        (hiccl:render nil
          comps:signup-form)))

;; ----------------------------------------------------------------------------
(setf (route *app* "/signup" :method :POST)
      (lambda (params)
        (let* ((name (param :username params))
               (pass (param :password params))
               (email (param :email params))
               (dao (mito:create-dao 'db:user :name name :password (hash pass) :email email)))
          (setf (gethash :user-id *session*) (mito:object-id dao))
          (hx-redirect *response* "/"))))

;; ----------------------------------------------------------------------------
(setf (route *app* "/logout" :method :POST)
      (lambda* (_)
        (setf (gethash :user-id *session*) nil)
        (hx-redirect *response* "http://localhost:5000/")))

;; ----------------------------------------------------------------------------
(setf (route *app* "/logout" :method :GET)
      (lambda* (_)
        (hiccl:render nil
          comps:logout-form)))
