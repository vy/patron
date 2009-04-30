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
;;; Job Submission Routines
;;;

(defun submit-job (patron job)
  "Submit given `JOB' into the job queue of `PATRON'. Function works in a
blocking manner and returns inserted `JOB', or throws a `TIMEOUT-CONDITION'."
  (setf (submit-time-of job) (get-universal-time))
  (or (queue-timed-push (jobs-of patron) job (worker-timeout-duration-of patron))
      (error 'timeout-condition :duration (worker-timeout-duration-of patron))))
