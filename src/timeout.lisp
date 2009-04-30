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
;;; Timeout Abstraction Layer
;;;

#+sbcl
(defun %with-timeout (duration body)
  (with-unique-names (_duration)
    `(let ((,_duration ,duration))
       (handler-case (sb-ext:with-timeout ,_duration (progn ,@body))
         (sb-ext:timeout () (error 'timeout-condition :duration ,_duration))))))

#+ccl
(defun %with-timeout (duration body)
  (with-unique-names (_duration _semaphore _result _process)
    `(let* ((,_duration ,duration)
            (,_semaphore (ccl:make-semaphore))
            (,_result)
            (,_process
             (ccl:process-run-function
              ,(format nil "Timed Process ~S" _process)
              (lambda ()
                (setf ,_result (multiple-value-list (progn ,@body)))
                (ccl:signal-semaphore ,_semaphore)))))
       (cond ((ccl:timed-wait-on-semaphore ,_semaphore ,_duration)
              (values-list ,_result))
             (t
              (ccl:process-kill ,_process)
              (error 'timeout-condition :duration ,_duration))))))

(defmacro with-timeout (duration &body body)
  "Execute `BODY' for no more than specified `DURATION'. In case of timeout,
function throws a `TIMEOUT-CONDITION'."
  (unless (fboundp '%with-timeout)
    (error "`WITH-TIMEOUT' is not supported!"))
  (%with-timeout duration body))
