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
;;; Size Bounded Two-Way Blocking FIFO Queue
;;;

;;; Algorithm  is adopted  from "Simple,  Fast, and  Practical  Non-Blocking and
;;; Blocking Concurrent Queue Algorithms" paper  by Maged M. Michael and Michael
;;; L.   Scott.  (It's   told  that   algorithm  performs   well   on  dedicated
;;; multiprocessors under high contention.  Useful for multiprocessors without a
;;; universal atomic primitive. YMMV.)

(defmethod shared-initialize :after ((queue queue) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (prog1 queue
    (setf (head-of queue) (list t)
          (tail-of queue) (head-of queue)
          (pop-lock-of queue) (lock-make)
          (pop-semaphore-of queue) (semaphore-make)
          (push-lock-of queue) (lock-make)
          (push-semaphore-of queue) (semaphore-make (size-of queue)))))

(defun %queue-pop (queue &optional default)
  "Pops an item from the given `QUEUE'. Function returns `DEFAULT' in case of no
available elements found."
  (with-lock (pop-lock-of queue)
    (let ((head (cdr (head-of queue))))
      (if head
          (prog1 (car head)
            (setf (head-of queue) head))
          default))))

(defun queue-pop (queue)
  "Pops an item from the given `QUEUE'. Function blocks if there isn't any
available item in the queue."
  (semaphore-wait (pop-semaphore-of queue))
  (prog1 (%queue-pop queue)
    (semaphore-signal (push-semaphore-of queue))))

(defun queue-timed-pop (queue duration &optional timeout)
  "Works like `QUEUE-POP', but function returns `TIMEOUT' if no available
elements found in given `DURATION'."
  (if (semaphore-timed-wait (pop-semaphore-of queue) duration)
      (prog1 (%queue-pop queue timeout)
        (semaphore-signal (push-semaphore-of queue)))
      timeout))

(defun %queue-push (queue item)
  "Pushes given `ITEM' into the `QUEUE'. Function returns supplied `ITEM'."
  (with-lock (push-lock-of queue)
    (prog1 item
      (let ((tail (tail-of queue)))
        (setf (cdr tail) (list item)
              (tail-of queue) (cdr tail))))))

(defun queue-push (queue item)
  "Tries to push given `ITEM' into the `QUEUE'. If queue size gets exceeded,
function blocks until at least an item is consumed from the queue. Function
returns supplied `ITEM'."
  (semaphore-wait (push-semaphore-of queue))
  (prog1 (%queue-push queue item)
    (semaphore-signal (pop-semaphore-of queue))))

(defun queue-timed-push (queue item duration &optional timeout)
  "Works like `QUEUE-PUSH', but function returns `TIMEOUT' if no push occurs in
given `DURATION'."
  (if (semaphore-timed-wait (push-semaphore-of queue) duration)
      (prog1 (%queue-push queue item)
        (semaphore-signal (pop-semaphore-of queue)))
      timeout))

(defmacro with-blocking-queue-operations (queue &body body)
  "Function blocks any physical push/pop operations on the `QUEUE' while
execution `BODY'."
  `(with-lock (push-lock-of ,queue)
     (with-lock (pop-lock-of ,queue)
       ,@body)))
