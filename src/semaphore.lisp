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
;;; Semaphore Abstraction Layer
;;;

(defun semaphore-wait (semaphore)
  #+ccl (ccl:wait-on-semaphore semaphore)
  #+sbcl (sb-thread:wait-on-semaphore semaphore)
  #-(or ccl sbcl) (error "`SEMAPHORE-WAIT' is not supported!"))

(defun semaphore-timed-wait (semaphore duration)
  #+ccl (ccl:timed-wait-on-semaphore semaphore duration)
  #+sbcl (handler-case
             (sb-ext:with-timeout duration
               (sb-thread:wait-on-semaphore semaphore))
           (sb-ext:timeout ()))
  #-(or ccl sbcl) (error "`SEMAPHORE-TIMED-WAIT' is not supported!"))

(defun semaphore-signal (semaphore)
  #+ccl (ccl:signal-semaphore semaphore)
  #+sbcl (sb-thread:signal-semaphore semaphore)
  #-(or ccl sbcl) (error "`SEMAPHORE-SIGNAL' is not supported!"))

(defun semaphore-make (&optional (count 0))
  (prog1-let
      (semaphore
       #+ccl (ccl:make-semaphore)
       #+sbcl (sb-thread:make-semaphore)
       #-(or ccl sbcl) (error "`MAKE-SEMAPHORE' is not supported!"))
    (loop repeat count do (semaphore-signal semaphore))))
