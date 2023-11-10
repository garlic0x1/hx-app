(defpackage :hx-app/components
  (:nicknames :comps)
  (:use :cl)
  (:export
   #:htmx-page
   #:top-bar-auth
   #:top-bar-unauth
   #:login-form
   #:signup-form
   #:logout-form
   #:post-form))
(in-package :hx-app/components)

;;
;; Hiccl method extensions
;;

;; ----------------------------------------------------------------------------
(defmethod hiccl::render-form (out (obj db:post))
  "Render a post object as HTML"
  (hiccl:render out
    `(:div.post
      (:div.user "User: " ,(db:user-name (db:post-user obj)))
      (:div.message "Message:" ,(db:post-message obj))
      (:div.likes "Likes:" ,(mito:count-dao 'db:post-like :post obj))
      (:button :hx-target "#flash"
               :hx-post ,(format nil "/post/~a/like" (mito:object-id obj))
               "Like"))))

;; ----------------------------------------------------------------------------
(defmethod hiccl::render-form (out (obj db:user))
  "Render a user object as HTML"
  (hiccl:render out
    `(:div.user
      (:div.name "Name: " ,(db:user-name obj))
      (:div.email "Email: " ,(db:user-email obj))
      (:div.role "Role: " ,(db:user-role obj)))))

;;
;; Function Components
;;

;; ----------------------------------------------------------------------------
(defun htmx-page (&rest content)
  `(:html.no-js
    (:head
     (:script :src "https://unpkg.com/htmx.org@1.9.8")
     (:link :rel "stylesheet" :href "https://unpkg.com/sakura.css/css/sakura-dark.css"))
    (:body#page ,@content)
    (:footer#flash)))

;;
;; Static components
;;

;; ----------------------------------------------------------------------------
(defparameter top-bar-unauth
  '(:nav.top-bar
    (:a :href "https://github.com/garlic0x1/hx-app" "Source Code")
    (:a :hx-target "#view" :hx-get "/rxss" "XSS test page")
    (:a :hx-target "#view" :hx-get "/login" "Log in")
    (:a :hx-target "#view" :hx-get "/signup" "Sign up")
    (:a :hx-target "#view" :hx-get "/feed" :hx-trigger "load, click" "Feed")))

;; ----------------------------------------------------------------------------
(defparameter top-bar-auth
  '(:nav.top-bar
    (:a :href "https://github.com/garlic0x1/hx-app" "Source Code")
    (:a :hx-target "#view" :hx-get "/rxss" "XSS test page")
    (:a :hx-target "#view" :hx-get "/profile" "Profile")
    (:a :hx-target "#view" :hx-get "/logout" "Log out")
    (:a :hx-target "#view" :hx-get "/feed" :hx-trigger "load, click" "Feed")
    (:a :hx-target "#view" :hx-get "/post" "Post")))

;; ----------------------------------------------------------------------------
(defparameter login-form
  '(:form.login
    :hx-target "#flash"
    :hx-post "/login"
    (:h1 "Login")
    (:br) (:input :type "text" :name "username")
    (:br) (:input :type "password" :name "password")
    (:br) (:input :type "submit")))

;; ----------------------------------------------------------------------------
(defparameter signup-form
  '(:form.signup
    :hx-post "/signup"
    (:h1 "Sign up")
    (:br) (:input :type "text" :name "username" :placeholder "username")
    (:br) (:input :type "text" :name "email" :placeholder "email")
    (:br) (:input :type "password" :name "password" :placeholder "password")
    (:br) (:input :type "submit")))

;; ----------------------------------------------------------------------------
(defparameter logout-form
  '(:form.logout
    :hx-post "/logout"
    (:h1 "Are you sure?")
    (:br) (:input :type "submit" "yes")))

;; ----------------------------------------------------------------------------
(defparameter post-form
  '(:<>
    (:iframe#dummy :name "dummy" :style "display: none;")
    (:form.signup
     :hx-post "/post" :hx-target "#dummy"
     (:h1 "Make a post")
     (:br) (:input :type "text" :name "message")
     (:br) (:input :type "submit"))))
