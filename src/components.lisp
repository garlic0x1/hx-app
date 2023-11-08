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
      (:div.message "Message:" ,(db:post-message obj)))))

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
     (:script :src "https://unpkg.com/htmx.org@1.9.5")
     (:script :src "https://unpkg.com/htmx.org/dist/ext/ws.js")
     (:link :rel "stylesheet" :href "https://unpkg.com/sakura.css/css/sakura-dark.css"))
    (:body#page ,@content)))

;;
;; Static components
;;

;; ----------------------------------------------------------------------------
(defparameter top-bar-unauth
  '(:nav.top-bar
    (:a :hx-target "#view" :hx-get "/login" "Log in")
    (:a :hx-target "#view" :hx-get "/signup" "Sign up")
    (:a :hx-target "#view" :hx-get "/feed" "Feed")))

;; ----------------------------------------------------------------------------
(defparameter top-bar-auth
  '(:nav.top-bar
    (:a :hx-target "#view" :hx-get "/profile" "Profile")
    (:a :hx-target "#view" :hx-get "/logout" "Log out")
    (:a :hx-target "#view" :hx-get "/feed" "Feed")
    (:a :hx-target "#view" :hx-get "/post" "Post")))

;; ----------------------------------------------------------------------------
(defparameter login-form
  '(:<>
    (:iframe#dummy :name "dummy" :style "display: none;")
    (:form.login
     :method "POST" :action "/login" :target "dummy"
     (:h1 "Login")
     (:br) (:input :type "text" :name "username")
     (:br) (:input :type "password" :name "password")
     (:br) (:input :type "submit"))))

;; ----------------------------------------------------------------------------
(defparameter signup-form
  '(:<>
    (:iframe#dummy :name "dummy" :style "display: none;")
    (:form.signup
     :method "POST" :action "/signup" :target "dummy"
     (:h1 "Sign up")
     (:br) (:input :type "text" :name "username" :placeholder "username")
     (:br) (:input :type "text" :name "email" :placeholder "email")
     (:br) (:input :type "password" :name "password" :placeholder "password")
     (:br) (:input :type "submit"))))

;; ----------------------------------------------------------------------------
(defparameter logout-form
  '(:<>
    (:iframe#dummy :name "dummy" :style "display: none;")
    (:form.logout
     :method "POST" :action "/logout" :target "dummy"
     (:h1 "Are you sure?")
     (:br) (:input :type "submit" "yes"))))

;; ----------------------------------------------------------------------------
(defparameter post-form
  '(:<>
    (:iframe#dummy :name "dummy" :style "display: none;")
    (:form.signup
     :method "POST" :action "/post" :target "dummy"
     (:h1 "Make a post")
     (:br) (:input :type "text" :name "message")
     (:br) (:input :type "submit"))))
