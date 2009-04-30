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
;;; Keeper Routines
;;;

(defun keeper (keeper patron)
  "Keeper function to ensure the existence of its parent `KEEPER' and `WORKER's.

In case of a dead `KEEPER'/`WORKER' instance is found,
`N-KEEPER-FAILURES'/`N-WORKER-FAILURES' slot is incremented and
`ERROR-REPORT-FUNCTION' the `PATRON' is called with the inactive instance as
argument.

Function loops infinitely by checking if `STATE' is still `:ACTIVE' before every
`KEEPER-TIMEOUT-DURATION' interval. In case of an error, `CONDITION' slot of the
`KEEPER' is filled appropriately."
  (handler-case
      ;; Loop over `STATE' of `PATRON'.
      (loop with keeper-pos = (position keeper (keepers-of patron))
            with partner = (elt (keepers-of patron) (if (zerop keeper-pos) 1 0))
            for state = (with-lock (state-lock-of patron) (state-of patron))
            while (eql state :active)
            do (progn
                 ;; Check if our partner `KEEPER' is in mood.
                 (without-interrupts
                   (unless (thread-alive-p partner)
                     (thread-start partner)
                     (incf (n-keeper-failures-of keeper))
                     (funcall (error-report-function-of patron) partner)))
                 ;; Check if `WORKER's are in good shape.
                 (when (zerop keeper-pos)
                   (loop for worker across (workers-of patron)
                         unless (thread-alive-p worker)
                         do (progn
                              (thread-start worker)
                              (incf (n-worker-failures-of keeper))
                              (funcall (error-report-function-of patron)
                                       worker))))
                 ;; Time to take a nap.
                 (sleep (keeper-timeout-duration-of patron))))
    ;; Record the error and exit.
    (error (c) (setf (condition-of keeper) c)))
  (setf (finish-time-of keeper) (get-universal-time)))

(defun make-keeper (patron)
  "Make an appropriate `KEEPER' instance with a specific wrapper function around
`KEEPER' function."
  (prog1-let (keeper (make-instance 'keeper))
    (setf (function-of keeper) (lambda () (keeper keeper patron)))))

(defun wait-keeper (keeper)
  "Wait for `KEEPER' to exit."
  (thread-join keeper))

(defun kill-keeper (keeper)
  (thread-interrupt
   keeper
   (lambda ()
     ;; This condition should be catched by the `ERROR' handler of the main
     ;; `HANDLER-CASE' expression in `KEEPER' function appropriately.
     (error 'kill-condition))))

(defun start-keepers (patron)
  "Starts `KEEPER's and waits for them to wake up. Function returns given
`PATRON'."
  (prog1 patron
    ;; TODO: Instead of allocating a keeper couple from scratch, try to use
    ;; existing ones if there is any.
    (setf (keepers-of patron)
          (coerce (loop repeat 2 collect (make-keeper patron))
                  '(simple-array keeper (2))))
    ;; We need to hold the `STATE' lock to avoid first `KEEPER' trying to wake
    ;; its parent up while creating both `KEEPER' threads for the first time.
    (with-lock (state-lock-of patron)
      (map nil #'thread-start (keepers-of patron)))))

(defun stop-keepers (patron &key kill wait)
  "Function does nothing -- assuming `STATE' is switched to `:INACTIVE',
`KEEPER' function will exit in the next loop round. Function returns given
`PATRON'.

If `KILL' is true, function will try to terminate every keeper via throwing a
`KILL-CONDITION'. `CONDITION' slot of related `KEEPER's will get set to this
condition appropriately.

If `WAIT' is true, function will wait (at most `KEEPER-TIMEOUT-DURATION') for
`KEEPER's to exit."
  (prog1 patron
    (when kill (map nil #'kill-keeper (keepers-of patron)))
    (when wait (map nil #'wait-keeper (keepers-of patron)))))
