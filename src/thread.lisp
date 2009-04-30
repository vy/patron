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
;;; Thread & Locking Abstraction Layer
;;;

(defun lock-make ()
  (bt:make-lock))

(defmacro with-lock (lock &body body)
  `(bt:with-lock-held (,lock) ,@body))

(defun thread-start (thread)
  (prog1 thread
    (let ((semaphore (semaphore-make)))
      (bt:make-thread
       (lambda ()
         (setf (id-of thread) (bt:current-thread))
         (semaphore-signal semaphore)
         (funcall (function-of thread))))
      (semaphore-wait semaphore))))

(defun thread-current ()
  (bt:current-thread))

(defun thread-alive-p (thread)
  (bt:thread-alive-p (id-of thread)))

(defun thread-interrupt (thread function)
  (bt:interrupt-thread (id-of thread) function))

(defun thread-join (thread)
  #+ccl (ccl:join-process (id-of thread))
  #+sbcl (bt:join-thread (id-of thread))
  #-(or ccl sbcl) (error "`THREAD-JOIN' is not supported!"))

(defmacro without-interrupts (&body body)
  #+ccl `(ccl:without-interrupts ,@body)
  #+sbcl `(sb-sys:without-interrupts ,@body)
  #-(or ccl sbcl) (error "`WITHOUT-INTERRUPTS' is not supported!"))
