;; guile-lib
;; Copyright (C) 2004 Andy Wingo <wingo at pobox dot com>

;; This program is free software; you can redistribute it and/or    
;; modify it under the terms of the GNU General Public License as   
;; published by the Free Software Foundation; either version 2 of   
;; the License, or (at your option) any later version.              
;;                                                                  
;; This program is distributed in the hope that it will be useful,  
;; but WITHOUT ANY WARRANTY; without even the implied warranty of   
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
;; GNU General Public License for more details.                     
;;                                                                  
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:
;;
;;Integration between texinfo, structured text, and guile's help system.
;;        
;;; Code:

(define-module (text structured help)
  :use-module (text structured plain-text)
  :use-module (text structured texinfo)
  :use-module (ice-9 documentation)
  :use-module (scheme session))

(define (stext-help-handler value)
  (let ((docs (object-documentation value)))
    (if (list? docs) ;; pretty lame check for stext, eh...
        (stext->plain-text (list '(stext) docs "\n")) ;; ghetto! i want to
						 ;; move to sxml.
        #f)))

(define (texinfo-help-handler value)
  (let ((docs (object-documentation value)))
    (if (and (string? docs) (string-index docs #\@)) ;; another lame check
        (stext->plain-text
         (list '(stext) ;; yikes
               (with-input-from-string docs
                 texinfo->stext)
               "\n"))
        #f)))

(add-value-help-handler! texinfo-help-handler)
(add-value-help-handler! stext-help-handler)

;;; arch-tag: 99d453bd-cc00-4ca0-b5b7-f018dc1ae0e5
