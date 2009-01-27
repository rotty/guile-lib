;; (container delay-tree) -- a nodal tree with promises
;; Copyright (C) 2003,2004  Andy Wingo <wingo at pobox dot com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;A delay tree is a superset of a nodal tree (see (container
;;nodal-tree)). It extends nodal trees to allow any entry of the node to
;;be a promise created with the @code{delay} operator.
;;
;;; Code:

(define-module (container delay-tree)
  #:use-module (container nodal-tree)
  #:export (force-ref))

(define (force-ref node field)
  "Access a field in a node of a delay tree. If the value of the field
is a promise, the promise will be forced, and the value will be replaced
with the forced value."
  (let ((val (node-ref node field)))
    (and val
         (if (promise? val)
             (begin
               (node-set! node field (force val))
               (node-ref node field))
             val))))
