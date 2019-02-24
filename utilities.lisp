;;; Flexichain
;;; Utility functions
;;;
;;; Copyright (C) 2003-2004  Robert Strandh (strandh@labri.fr)
;;; Copyright (C) 2003-2004  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA


(cl:in-package :flexichain)

(defun square (x)
  "Returns the square of the number X."
  (* x x))

(defun find-if-2 (predicate sequence)
  "Searches the sequence for an element that satisfies PREDICATE.
Returns the element found or NIL of none was found, and a boolean
indicating whether an element was found or not."
  (let ((position (position-if predicate sequence)))
    (if (null position)
        (values nil nil)
        (values (elt sequence position) t))))

;;;; Weak Pointers
;;;; This looks like the same code as in trivial-garbage, can that be used?

#+openmcl
(defvar *weak-pointers* (cl:make-hash-table :test 'eq :weak :value)
  "Weak value hash-table mapping between pseudo weak pointers and its values.")

#+(or allegro openmcl lispworks)
(defstruct (weak-pointer (:constructor %make-weak-pointer))
  #-openmcl pointer)

(defun make-weak-pointer (object)
  "Creates a new weak pointer which points to @code{object}. For
   portability reasons, @code{object} must not be @code{nil}."
  (assert (not (null object)))
  #+sbcl (sb-ext:make-weak-pointer object)
  #+(or cmu scl) (ext:make-weak-pointer object)
  #+clisp (ext:make-weak-pointer object)
  #+abcl (ext:make-weak-reference object)
  #+ecl (ext:make-weak-pointer object)
  #+allegro
  (let ((wv (excl:weak-vector 1)))
    (setf (svref wv 0) object)
    (%make-weak-pointer :pointer wv))
  #+openmcl
  (let ((wp (%make-weak-pointer)))
    (setf (gethash wp *weak-pointers*) object)
    wp)
  #+corman (ccl:make-weak-pointer object)
  #+lispworks
  (let ((array (make-array 1 :weak t)))
    (setf (svref array 0) object)
    (%make-weak-pointer :pointer array))
  #+clasp (core:make-weak-pointer object))

(defun weak-pointer-value (weak-pointer)
  "If @code{weak-pointer} is valid, returns its value. Otherwise,
   returns @code{nil}."
  #+sbcl (values (sb-ext:weak-pointer-value weak-pointer))
  #+(or cmu scl) (values (ext:weak-pointer-value weak-pointer))
  #+clisp (values (ext:weak-pointer-value weak-pointer))
  #+abcl (values (ext:weak-reference-value weak-pointer))
  #+ecl (values (ext:weak-pointer-value weak-pointer))
  #+allegro (svref (weak-pointer-pointer weak-pointer) 0)
  #+openmcl (values (gethash weak-pointer *weak-pointers*))
  #+corman (ccl:weak-pointer-obj weak-pointer)
  #+lispworks (svref (weak-pointer-pointer weak-pointer) 0)
  #+clasp (core:weak-pointer-value weak-pointer))

#-(or :sbcl :cmu :clisp :allegro :openmcl :corman :lispworks :ecl :clasp)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (warn "No support for weak pointers in this implementation. ~
         Things may get big and slow."))
