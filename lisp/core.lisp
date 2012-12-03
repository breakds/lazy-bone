;;;; core.lisp
;;;; Description: core functions/macros for lazy-bone framework
;;;; Author: BreakDS
;;;; Date: Sun Dec  2 13:53:42 CST 2012

(in-package #:breakds.lazy-bone)


;;; +---------- Data Structures ----------+

;; Bone: A backbone class instance represent a backbone.js class that can be
;; compiled to javascript declaration and defintion
;; +----------
;; | name: symbol representing the identity of the class
;; | base: symbol representing the idenetty of the base class
;; | members: plist of (symbol, paren-script) pair

(defstruct bone
  (name 'noname)
  (base '*object)
  (members nil))

(defmacro compile-bone (obj)
  `(ps:ps (ps:defvar (bone-name ,obj)
            ((ps:chain (bone-base ,obj) extend)
             (ps:create ,@(bone-members obj))))))

(defmacro test (obj)


;; + ---------- Global Variables ---------- +

(defparameter *namepsace* (make-hash-table))





