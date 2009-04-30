;;; Copyright (c) 2009, Volkan YAZICI <volkan.yazici@gmail.com>
;;; All rights reserved.

;;; Redistribution  and  use  in  source  and  binary  forms,  with  or  without
;;; modification, are permitted provided that the following conditions are met:

;;; - Redistributions  of source code  must retain  the above  copyright notice,
;;;   this list of conditions and the following disclaimer.

;;; - Redistributions in binary form  must reproduce the above copyright notice,
;;;   this list of conditions and  the following disclaimer in the documentation
;;;   and/or other materials provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY  THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY  EXPRESS OR IMPLIED WARRANTIES,  INCLUDING, BUT NOT  LIMITED TO, THE
;;; IMPLIED WARRANTIES  OF MERCHANTABILITY AND FITNESS FOR  A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO  EVENT SHALL THE  COPYRIGHT OWNER OR  CONTRIBUTORS BE
;;; LIABLE  FOR  ANY  DIRECT,   INDIRECT,  INCIDENTAL,  SPECIAL,  EXEMPLARY,  OR
;;; CONSEQUENTIAL  DAMAGES  (INCLUDING,  BUT  NOT  LIMITED  TO,  PROCUREMENT  OF
;;; SUBSTITUTE GOODS  OR SERVICES;  LOSS OF USE,  DATA, OR PROFITS;  OR BUSINESS
;;; INTERRUPTION)  HOWEVER CAUSED  AND ON  ANY THEORY  OF LIABILITY,  WHETHER IN
;;; CONTRACT,  STRICT LIABILITY,  OR  TORT (INCLUDING  NEGLIGENCE OR  OTHERWISE)
;;; ARISING IN ANY WAY  OUT OF THE USE OF THIS SOFTWARE,  EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.


(in-package :patron)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Management Routines
;;;

(defun rotate-patron-state (patron target-state)
  "Switches `STATE' slot of `PATRON' to specified `TARGET-STATE'."
  (with-lock (state-lock-of patron)
    (assert
     (eql (state-of patron)
          (ecase target-state
            (:inactive :active)
            (:active :inactive))))
    (setf (state-of patron) target-state)))

(defun start-patron (patron)
  "After switching `STATE' to `:ACTIVE', starts `WORKERS's and `KEEPER's in
order."
  (prog1 patron
    (rotate-patron-state patron :active)
    (start-workers patron)
    (start-keepers patron)))

(defun stop-patron (patron &key wait kill)
  "After switching `STATE' to `:INACTIVE', stops `KEEPER's and `WORKER's in
order. For related effects of keyword arguments see documentation of
`STOP-KEEPERS' and `STOP-WORKERS' functions."
  (prog1 patron
    (rotate-patron-state patron :inactive)
    (stop-keepers patron :wait wait)
    (stop-workers patron :wait wait :kill kill)))
