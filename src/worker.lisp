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
;;; Worker Routines
;;;

(defmethod shared-initialize :after ((worker worker) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (prog1 worker
    ;; Initialize `LAST-START-TIME' and `LAST-FINISH-TIME' slots.
    (let ((time (get-universal-time)))
      (setf (last-start-time-of worker) time
            (last-finish-time-of worker) time))))

(defun worker (worker patron)
  "Worker function to execute the next available job in the queue. Function
infinitely tries to pop `JOB' from the queue until it receives a `NIL' job.

During every job processing iteration, function resets `LAST-START-TIME',
`LAST-FINISH-TIME', `N-FAILURES', `FAIL-DURATION', `BUSY-DURATION', and
`IDLE-DURATION' slots of the `WORKER' accordingly.

`START-TIME' and `FINISH-TIME' slots of the `JOB' is assigned respectively
before and after the execution of the `FUNCTION' slot of the `JOB'. After
execution, if there doesn't occur any errors, `RESULT' slot of the `JOB' is set
accordingly and `RESULT-REPORT-FUNCTION' is called with `JOB' as argument. In
case of an error, `CONDITION' slot is set and `ERROR-REPORT-FUNCTION' is
called."
  (loop for job = (queue-pop (jobs-of patron))
        while job
        do (progn
             ;; Update `IDLE-DURATION', `LAST-START-TIME', and `START-TIME'
             ;; slots.
             (without-interrupts
               (let ((time (get-universal-time)))
                 (incf (idle-duration-of worker)
                       (- time (last-finish-time-of worker)))
                 (setf (last-start-time-of worker) time
                       (start-time-of job) time)))
             (handler-case
                 (progn
                   ;; Run `FUNCTION' with time boundary.
                   (setf (result-of job)
                         (with-timeout (worker-timeout-duration-of patron)
                           (funcall (function-of job))))
                   ;; Update `LAST-FINISH-TIME', `BUSY-DURATION', and
                   ;; `FINISH-TIME' slots.
                   (without-interrupts
                     (let ((time (get-universal-time)))
                       (incf (busy-duration-of worker)
                             (- time (last-start-time-of worker)))
                       (setf (last-finish-time-of worker) time
                             (finish-time-of job) time)))
                   ;; Submit the result.
                   (funcall (result-report-function-of job) job))
               ;; Possible failure reasons: timeout condition or runtime error.
               (error (c)
                 (without-interrupts
                   ;; We could have missed updating `LAST-FINISH-TIME', if so,
                   ;; do it now.
                   (unless (< (last-start-time-of worker)
                              (last-finish-time-of worker))
                     (setf (last-finish-time-of worker) (get-universal-time)))
                   ;; Update `N-FAILURES', `FAIL-DURATION', and `BUSY-DURATION'
                   ;; slots.
                   (incf (n-failures-of worker))
                   (let ((duration
                          (- (last-finish-time-of worker)
                             (last-start-time-of worker))))
                     (incf (fail-duration-of worker) duration)
                     (incf (busy-duration-of worker) duration)))
                 ;; Set the `CONDITION' slot and submit the error.
                 (setf (condition-of job) c)
                 (funcall (error-report-function-of job) job))))))

(defun make-worker (patron)
  (prog1-let (worker (make-instance 'worker))
    (setf (function-of worker) (lambda () (worker worker patron)))))

(defun wait-worker (worker)
  (thread-join worker))

(defun kill-worker (worker)
  (thread-interrupt
   worker
   (lambda ()
     ;; This condition should be catched by the `ERROR' handler of the main
     ;; `HANDLER-CASE' expression in `WORKER' function appropriately.
     (error 'kill-condition))))

(defun start-workers (patron)
  "Fills `WORKERS' and `JOBS' slots of the given `PATRON' appropriately and
spawns workers. Function returns supplied `PATRON'."
  (prog1 patron
    ;; TODO: Instead of allocating a job queue and a workers array from scratch
    ;; everytime, try to use existing objects if there is any.
    (setf (jobs-of patron) (make-instance 'queue :size (job-capacity-of patron))
          (workers-of patron) (make-array (list (worker-capacity-of patron))))
    (dotimes (i (worker-capacity-of patron))
      (let ((i i)) (setf (elt (workers-of patron) i) (make-worker patron))))
    (map nil #'thread-start (workers-of patron))))

(defun stop-workers (patron &key kill wait)
  "Stops workers by pushing `NIL' jobs to the queue as much as total # of
workers. Function blocks until there is enough space in the job queue to push
dummy `NIL's. Function finally returns supplied `PATRON'.

If `KILL' is true, function will try to terminate every worker that is still
alive and report jobs about the situation via `ERROR-SUBMIT-FUNCTION'.
`CONDITION' slot of the `JOB' will set to `KILL-CONDITION'.

If `WAIT' is true, function will wait (at most `WORKER-TIMEOUT-DURATION') for
`WORKER's to exit."
  (prog1 patron
    (loop repeat (length (workers-of patron))
          do (queue-push (jobs-of patron) nil))
    (when kill (map nil #'kill-worker (workers-of patron)))
    (when wait (map nil #'wait-worker (workers-of patron)))))

(defun worker-stats (patron)
  "Returns a property list of minimum, maximum, and average statistics of
`N-FAILURES', `FAIL-DURATION', `BUSY-DURATION', and `IDLE-DURATION' slots among
workers. Function blocks job queue while gathering statistics."
  (with-blocking-queue-operations (jobs-of patron)
    (loop for worker across (workers-of patron)
          collect (n-failures-of worker) into n-failures
          collect (fail-duration-of worker) into fail-durations
          collect (busy-duration-of worker) into busy-durations
          collect (idle-duration-of worker) into idle-durations
          finally (return
                    (let ((n-workers (length (workers-of patron))))
                      (list :min-failures       (reduce #'min n-failures)
                            :max-failures       (reduce #'max n-failures)
                            :avg-n-failures     (/ (reduce #'+ n-failures)
                                                   n-workers)
                            :sum-n-failures     (reduce #'+ n-failures)
                            :min-fail-durations (reduce #'min fail-durations)
                            :max-fail-durations (reduce #'max fail-durations)
                            :avg-fail-durations (/ (reduce #'+ fail-durations)
                                                   n-workers)
                            :min-busy-durations (reduce #'min busy-durations)
                            :max-busy-durations (reduce #'max busy-durations)
                            :avg-busy-durations (/ (reduce #'+ busy-durations)
                                                   n-workers)
                            :min-idle-durations (reduce #'min idle-durations)
                            :max-idle-durations (reduce #'max idle-durations)
                            :avg-idle-durations (/ (reduce #'+ idle-durations)
                                                   n-workers)))))))
