;;; let-recur.el --- Simplified implementation of recur -*- lexical-binding:t -*-
;; Copyright (C) 2012  Evan Izaksonas-Smith

;; Author: Evan Izaksonas-Smith <izak0002 at umn dot edu>
;; Maintainer: Evan Izaksonas-Smith
;; Created: 1st Januay 2012
;; Version: 0.0.5
;; Keywords: lisp
;; Description: A simplified let-recur mechanic.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A simplified let-recur mechanic.  Though I have extensive work on
;; full TCO, this is provided as a simple way to achieve one of the
;; most common uses in a direct and simple fashion.  It is not sutable
;; for any case but strictly tail recursive self recursion.

;;; Code:

(defun let-recur--transform (form)
  "Blindly looks for :recur in the function slot of an sexp.
Will blithely transform it in places where it will break your
code.  So don't put the keyword :recur in your code unless you
are certain it is a tail call."
  (pcase form
    ( `(:recur . ,args)
      `(throw :recur (list ,@args)))
    ( `(,func . ,args)
      `(,func ,@(mapcar 'let-recur--transform args)))
    ( _ form)))

(defmacro let-recur (bindings &rest body)
  "Like `let' but acts as a lambda named :recur, however :recur
will only work correctly in the tail-call position, so this is
only suitable for recursive functions that are guaranteed to be
strictly tail recursive.  Using the keyword :recur anywhere else
in the body will yield undefined behavior."
  (declare (indent 2))
  (setf (car (last body))
	(let-recur--transform (car (last body))))
  (let ((arglist (mapcar 'car bindings))
        (args (make-symbol "args")))
    `(let ((,args (list ,@(mapcar 'cadr bindings))))
       (catch :return
         (while t
           (setq ,args
                 (catch :recur
                   (throw :return
                          (apply (lambda ,arglist ,@body)
                                 ,args)))))))))

(defmacro pcase-recur (exp &rest cases)
  "Like `pcase' but acts as a lambda named :recur, however :recur
will only work correctly in the tail-call position, so this is
only suitable for recursive functions that are guaranteed to be
strictly tail recursive.  Using the keyword :recur anywhere else
in the body will yield undefined behavior."
  (declare (indent 1))
  (mapc (lambda (body)
          (setf (car (last body))
		(let-recur--transform (car (last body)))))
        cases)
  (let ((args (make-symbol "args")))
    `(let ((,args ,exp))
       (catch :return
         (while t
           (setq ,args
                 (catch :recur
                   (throw :return
                          (pcase ,args ,@cases)))))))))

;;; examples:
;; (defun preverse (list)
;;   (pcase-recur `(,list)
;;     (`(nil ,reverse) reverse)
;;     (`((,head \, tail) ,reverse) (:recur tail `(,head ,@reverse)))
;;     (`(,sequence) (:recur sequence nil))))
;;
;;
;; thanks to wasamasa:
;; (defun factorial-lr (n)
;;   (let-recur ((n n) (acc 1))
;;       (cond
;;        ((= 0 n) acc)
;;        (t (:recur (1- n) (* acc n))))))
;;
;; (defun factorial-pr (n)
;;     (pcase-recur `(,n 1)
;;       (`(0 ,acc) acc)
;;       (`(,n ,acc) (:recur (1- n) (* acc n)))))



(provide 'let-recur)
;;; let-recur.el ends here
