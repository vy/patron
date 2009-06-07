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
;;; Common Error Reporting Related Definitions
;;;

(defvar *error-stream* *debug-io*
  "Generic stream used by default error reporting functions.")

(define-condition error-condition (error)
  ((time
    :initform      (get-universal-time)
    :reader        time-of
    :type          (integer 1 *)
    :documentation "Time condition instance is created."))
  (:documentation "Generic wrapper condition for application specific
  conditions."))

(defun time->string (time)
  (destructuring-bind (s min h day mon y dow dlp z)
      (multiple-value-list (decode-universal-time time))
    (declare (ignore dow dlp z))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D" y mon day h min s)))

(define-condition kill-condition (error-condition)
  ()
  (:documentation "Condition passed to the `CONDITION' slot of a `KEEPER'/`JOB'
  while killing a keeper/worker."))

(defmethod print-object ((kill-condition kill-condition) stream)
  (print-unreadable-object (kill-condition stream :type t)
    (format stream ":TIME ~S" (time->string (time-of kill-condition)))))

(define-condition timeout-condition (error-condition)
  ((duration
    :initarg       :duration
    :reader        duration-of
    :type          (integer 1 *)
    :documentation "Elapsed duration before condition is raised."))
  (:documentation "Condition thrown when the duration specified in the
  `WITH-TIMEOUT' is exceeded."))

(defmethod print-object ((timeout-condition timeout-condition) stream)
  (print-unreadable-object (timeout-condition stream :type t)
    (format stream ":DURATION ~A :TIME ~S"
            (duration-of timeout-condition)
            (time->string (time-of timeout-condition)))))

(defun default-error-report (condition)
  "Default function for reporting errors."
  (format *error-stream* "~A - ~S~%"
          (time->string
           (if (and (slot-exists-p condition 'time)
                    (slot-boundp condition 'time))
               (time-of condition)
               (get-universal-time)))
          condition))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Queue Related Definitions
;;;

(defclass queue ()
  ((head
    :accessor      head-of
    :type          list)
   (tail
    :accessor      tail-of
    :type          list)
   (size
    :initarg       :size
    :reader        size-of
    :type          (integer 1 *)
    :documentation "Maximum # of items allowed in the queue.")
   (pop-lock
    :accessor      pop-lock-of
    :documentation "Lock serializing pop operations.")
   (pop-semaphore
    :accessor      pop-semaphore-of
    :documentation "Semaphore blocking pop operations while queue is empty.")
   (push-lock
    :accessor      push-lock-of
    :documentation "Lock serializing push operations.")
   (push-semaphore
    :accessor push-semaphore-of
    :documentation "Semaphore blocking push operations while queue is full."))
  (:documentation "Size bounded two-lock concurrent FIFO queue."))

(defmethod print-object ((queue queue) stream)
  (print-unreadable-object (queue stream :identity t :type t)
    (format stream ":HEAD ~S :TAIL ~S :SIZE ~A"
            (head-of queue) (tail-of queue) (size-of queue))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Job Related Definitions
;;;

(defclass job ()
  ((function
    :initform      (error "Missing `FUNCTION'!")
    :initarg       :function
    :reader        function-of
    :type          function
    :documentation "Function will be called to start the execution.")
   (result-report-function
    :initarg       :result-report-function
    :reader        result-report-function-of
    :type          function
    :documentation "Function will be called to report the result.")
   (error-report-function
    :initform      #'default-error-report
    :initarg       :error-report-function
    :reader        error-report-function-of
    :type          function
    :documentation "Function will be called to report an error.")
   (submit-time
    :accessor      submit-time-of
    :documentation "Job queue entrance time.")
   (start-time
    :accessor      start-time-of
    :documentation "Job execution start time.")
   (finish-time
    :accessor      finish-time-of
    :documentation "Job execution finish time.")
   (condition
    :accessor      condition-of
    :documentation "Signaled condition in case of a failure.")
   (result
    :accessor      result-of
    :documentation "Job result in case of no failure.")))

(defmethod print-object ((job job) stream)
  (print-unreadable-object (job stream :identity t :type t)
    (format
     stream
     (concatenate
      'string
      ":FUNCTION ~S"
      "~@[ :START-TIME ~S~]"
      "~@[ :FINISH-TIME ~S~]"
      "~@[ :CONDITION ~S~]"
      "~@[ :RESULT ~S~]")
     (function-of job)
     (when (slot-boundp job 'start-time) (time->string (start-time-of job)))
     (when (slot-boundp job 'finish-time) (time->string (finish-time-of job)))
     (when (slot-boundp job 'condition) (condition-of job))
     (when (slot-boundp job 'result) (result-of job)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Thread Related Defitions
;;;

(defclass thread ()
  ((id
    :accessor      id-of
    :documentation "Implementation dependent thread identifier.")
   (function
    :accessor      function-of
    :documentation "Function executed by current thread.")))

(defmethod print-object ((thread thread) stream)
  (print-unreadable-object (thread stream :identity t :type t)
    (format stream "~@[ :ID ~S~]~@[ :FUNCTION ~S~]"
            (when (slot-boundp thread 'id) (id-of thread))
            (when (slot-boundp thread 'function) (function-of thread)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Worker Related Definitions
;;;

(defclass worker (thread)
  ((last-start-time
    :accessor      last-start-time-of
    :documentation "Last time worker started a job.")
   (last-finish-time
    :accessor      last-finish-time-of
    :documentation "Last time worker finished a job.")
   (n-failures
    :initform      0
    :accessor      n-failures-of
    :type          (integer 0 *)
    :documentation "# of failed processings.")
   (fail-duration
    :initform      0
    :accessor      fail-duration-of
    :type          (integer 0 *)
    :documentation "Total duration spend on failed processings.")
   (busy-duration
    :initform      0
    :accessor      busy-duration-of
    :type          (integer 0 *)
    :documentation "Total non-idle duration.")
   (idle-duration
    :initform      0
    :accessor      idle-duration-of
    :type          (integer 0 *)
    :documentation "Total duration worker stayed idle.")))

(defmethod print-object ((worker worker) stream)
  (print-unreadable-object (worker stream :identity t :type t)
    (format
     stream ":LAST-START-TIME ~S :LAST-FINISH-TIME ~S :IDLE-DURATION ~A"
     (time->string (last-start-time-of worker))
     (time->string (last-finish-time-of worker))
     (idle-duration-of worker))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Keeper Related Definitions
;;;

(defclass keeper (thread)
  ((start-time
    :initform      (get-universal-time)
    :accessor      start-time-of
    :documentation "Birth date.")
   (finish-time
    :accessor      finish-time-of
    :documentation "Exit/Crash date.")
   (condition
    :initform      nil
    :accessor      condition-of
    :documentation "Condition catched in case of a crash.")
   (n-keeper-failures
    :initform      0
    :accessor      n-keeper-failures-of
    :documentation "# of `KEEPER' failures found.")
   (n-worker-failures
    :initform      0
    :accessor      n-worker-failures-of
    :documentation "# of `WORKER' failures found.")))

(defmethod print-object ((keeper keeper) stream)
  (print-unreadable-object (keeper stream :identity t :type t)
    (format stream
            (concatenate
             'string
             ":START-TIME ~S"
             "~@[ :FINISH-TIME ~S~]"
             "~@[ :CONDITION ~S~]"
             " :N-KEEPER-FAILURES ~A"
             " :N-WORKER-FAILURES ~A")
            (time->string (start-time-of keeper))
            (when (slot-boundp keeper 'finish-time)
              (time->string (finish-time-of keeper)))
            (when (slot-boundp keeper 'condition) (condition-of keeper))
            (n-keeper-failures-of keeper)
            (n-worker-failures-of keeper))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Patron Related Definitions
;;;

(defclass patron ()
  ((state
    :initform      :inactive
    :accessor      state-of
    :type          keyword
    :documentation "State of the patron; either `ACTIVE', or `INACTIVE'.")
   (state-lock
    :initform      (make-lock)
    :reader        state-lock-of
    :documentation "Synchronization primitive for `STATE' slot.")
   (error-report-function
    :initform      #'default-error-report
    :initarg       :error-report-function
    :reader        error-report-function-of
    :type          function
    :documentation "Will get called for management related errors -- e.g when
    found a dead worker, keeper, etc.")
   ;; `JOB' Specific Slots ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (jobs
    :accessor      jobs-of
    :type          queue
    :documentation "FIFO queue of `JOB's waiting to be processed.")
   (job-capacity
    :initform      (error "Missing `JOB-CAPACITY'!")
    :initarg       :job-capacity
    :reader        job-capacity-of
    :type          (integer 1 *)
    :documentation "Upper limit on the job queue size.")
   ;; `WORKER' Specific Slots ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (workers
    :accessor      workers-of
    :type          (simple-array worker (*))
    :documentation "Vector of serving `WORKER's.")
   (worker-capacity
    :initform      (error "Missing `WORKER-CAPACITY'!")
    :initarg       :worker-capacity
    :reader        worker-capacity-of
    :type          (integer 1 *)
    :documentation "Number of serving `WORKER's.")
   (worker-timeout-duration
    :initform      (error "Missing `WORKER-TIMEOUT-DURATION'!")
    :initarg       :worker-timeout-duration
    :reader        worker-timeout-duration-of
    :type          (integer 1 *)
    :documentation "Time limit on the work processing duration.")
   ;; Keeper Specific Slots ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (keepers
    :accessor      keepers-of
    :documentation "`KEEPER' couple for `WORKER's and each other.")
   (keeper-timeout-duration
    :initform      5
    :initarg       :keeper-timeout-duration
    :reader        keeper-timeout-duration-of
    :type          (integer 1 *)
    :documentation "Wait period for keepers.")))

(defmethod print-object ((patron patron) stream)
  (print-unreadable-object (patron stream :identity t :type t)
    (format
     stream ":WORKER-CAPACITY ~A :JOB-CAPACITY ~A :WORKER-TIMEOUT-DURATION ~A"
     (worker-capacity-of patron) (job-capacity-of patron)
     (worker-timeout-duration-of patron))))
