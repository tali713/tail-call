;;; let-recur.el --- Simplified implementation of recur
;; Copyright (C) 2012  Evan Izaksonas-Smith

;; Author: Evan Izaksonas-Smith <izak0002 at umn dot edu>
;; Maintainer: Evan Izaksonas-Smith
;; Created: 1st Januay 2012
;; Version: 0.0.1
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
(defun let-recur--tc
  "Utility function, not intended for external use."
  (&rest args) (throw :recur args))

(defun let-recur--transform (form)
  "Blindly looks for :recur in the function slot of an sexp.
Will blithely transform it in places where it will break your
code.  So don't put the keyword :recur in your code unless you
are certain it is a tail call."
  (if (consp form)
      (if (eq :recur (car form))
        `(let-recur--tc ,@(cdr form))
        (cons (car form) (mapcar 'let-recur--transform (cdr form))))
      form))             

(defmacro let-recur (bindings &rest body)
  "Like let but acts as a lambda named :recur, however :recur will
only work correctly in the atil-call position, so this is only
suitable for recursive functions that are guaranteed to be
strictly tail recursive.  Using the keyword :recur anywhere else
in the body will yield undefined behavior."
  (declare (indent 2))
  (setcar (last body)
          (let-recur--transform (car (last body))))
  (let ((arglist (mapcar #'first bindings))
        (args (cl-gensym))
        (real-call (cl-gensym)))
    `(cl-labels
         ((recur (&rest ,args)
                 (let ((,real-call ,args))
                   (catch :return
                     (while t
                       (setq ,real-call
                             (catch :recur
                               (throw :return
                                      (apply (lambda ,arglist ,@body)
                                             ,real-call)))))))))
       (apply #'recur (list ,@(mapcar #'second bindings))))))


(provide 'let-recur)
;;; let-recur.el ends here
