           ____   ____ ______ ____   ___  ____
    !!  ! |    \ /    |      T    \ /   \|    \  !!!!
    !!!   |  o  Y  o  |      |  D  Y     Y  _  l  ! !!!
    !!!!  |   _/|     l_   __j     |  O  |  |  |   !!!!
    !!:   |  |  |  _  | |  | |    \|     |  |  |  !: :!
    :!:   |  |  |  |  | |  | |  .  l     !  |  |   : ::
     ::   l__j  l__j__j l__j l__j\_j\___/l__j__j   :  :

# Abstract

Patron is a multi-consumer/multi-producer thread pooling library written in Common Lisp with flexibility and performance in mind. You simply create a `PATRON` with a job queue of fixed size, specify a fixed number of `WORKER` threads and start submitting your `JOB`s into the work queue.

While Patron is written in portable Common Lisp in mind, because of some platform specific features[1], it currently works on [SBCL](http://www.sbcl.org/) and [CCL](http://openmcl.clozure.com/) platforms.  As a side note, Patron currently depends on [bordeaux-threads](http://common-lisp.net/project/bordeaux-threads/) library for common threading functionalities.

[1] Semaphores, missing threading features (`THREAD-JOIN`, `WITHOUT-INTERRUPTS`, etc.) in bordeaux-threads, `WITH-TIMEOUT` macro.

# Example

Below  basic example should  get you  to a  point where  you can  start creating thread pools in minutes.

    (defvar *stream* *standard-output*)
    
    (defvar *stream-lock* (patron:make-lock))
    
    (defun safe-format (fmt &rest args)
      (patron:with-lock *stream-lock*
        (apply #'format *stream* fmt args)))
    
    (defun thread-stats (patron)
      (safe-format
       "Keepers: ~{~A~^ ~}~%Workers: ~{~A~^ ~}~%"
       (map 'list #'patron:thread-alive-p (patron::keepers-of patron))
       (map 'list #'patron:thread-alive-p (patron::workers-of patron))))
    
    (let ((state (make-random-state)))
      (defun job ()
        (let ((duration (random 5 state)))
          (safe-format "  ~S => Sleeping... [~A]~%" (patron:current-thread) duration)
          (sleep duration)
          (safe-format "  ~S => Done!~%" (patron:current-thread)))))
    
    (defun report-result (job)
      (safe-format "RESULT: JOB: ~S~%" job))
    
    (defun patron-test ()
      (let* ((patron
              (make-instance
               'patron:patron
               :worker-capacity 3
               :job-capacity 32
               :worker-timeout-duration 3)))
        (safe-format "Starting...~%")
        (patron:start-patron patron)
        (sleep 1.0)
        (thread-stats patron)
        (safe-format "Submitting jobs...~%")
        (loop repeat 5
              do (patron:submit-job
                  patron
                  (make-instance
                       'patron:job
                       :function #'job
                       :result-report-function #'report-result)))
        (safe-format "Submitted.~%")
        (safe-format "Stopping...~%")
        (patron:stop-patron patron :wait t)
        (safe-format "Stopped.~%")
        (thread-stats patron)))
    
    ; Starting...
    ; Keepers: T T
    ; Workers: T T T
    ; Submitting jobs...
    ; Submitted.
    ; Stopping...
    ;   #<SB-THREAD:THREAD "Anonymous" RUNNING {10040E8D21}> => Sleeping... [2]
    ;   #<SB-THREAD:THREAD "Anonymous" RUNNING {10040E8F71}> => Sleeping... [2]
    ;   #<SB-THREAD:THREAD "Anonymous" RUNNING {10040EA1D1}> => Sleeping... [3]
    ;   #<SB-THREAD:THREAD "Anonymous" RUNNING {10040E8D21}> => Done!
    ; RESULT: JOB: #<PATRON:JOB :FUNCTION #<FUNCTION TEST::JOB> :START-TIME "2009-04-30 14:41:49" :FINISH-TIME "2009-04-30 14:41:51" {1004155561}>
    ;   #<SB-THREAD:THREAD "Anonymous" RUNNING {10040E8D21}> => Sleeping... [3]
    ;   #<SB-THREAD:THREAD "Anonymous" RUNNING {10040E8F71}> => Done!
    ; RESULT: JOB: #<PATRON:JOB :FUNCTION #<FUNCTION TEST::JOB> :START-TIME "2009-04-30 14:41:49" :FINISH-TIME "2009-04-30 14:41:51" {1004155891}>
    ;   #<SB-THREAD:THREAD "Anonymous" RUNNING {10040E8F71}> => Sleeping... [2]
    ; 2009-04-30 14:41:52 - #<PATRON:JOB :FUNCTION #<FUNCTION TEST::JOB> :START-TIME "2009-04-30 14:41:49" :CONDITION #<PATRON::TIMEOUT-CONDITION :DURATION 3 :TIME "2009-04-30 14:41:52"> {1004155BC1}>
    ;   #<SB-THREAD:THREAD "Anonymous" RUNNING {10040E8F71}> => Done!
    ; RESULT: JOB: #<PATRON:JOB :FUNCTION #<FUNCTION TEST::JOB> :START-TIME "2009-04-30 14:41:51" :FINISH-TIME "2009-04-30 14:41:53" {1004156231}>
    ; 2009-04-30 14:41:54 - #<PATRON:JOB :FUNCTION #<FUNCTION TEST::JOB> :START-TIME "2009-04-30 14:41:51" :CONDITION #<PATRON::TIMEOUT-CONDITION :DURATION 3 :TIME "2009-04-30 14:41:54"> {1004155EF1}>
    ; Stopped.
    ; Keepers: NIL NIL
    ; Workers: NIL NIL NIL

# Documentation

Before going  into the syntatical  details, here is  a general figure  about the inner workings of Patron.

* Queue operations take action in a blocking manner and are wrapped by `WITH-TIMEOUT` statements.
* There are no busy waits, synchronized access is supplied using semaphores.
* Using a two-lock concurrent queue algorithm, consumer and producer lockings are separated from each other for performance purposes .
* There are two keeper threads where each keeper is ensuring its partners existence and first (master) keeper ensuring the existence of specified number of worker threads.

While Patron source code is fully documented, below you'll find the documentation excerpts from the source code for exported symbols.

    [Condition] timeout-condition (error-condition)
      [Slot] time - Time condition instance is created.
      [Slot] duration - Elapsed duration before condition is raised.

> Condition  thrown  when  the  duration  specified  in  the  `WITH-TIMEOUT`  is exceeded. (`TIME` slot is inherited from `ERROR-CONDITION`.)

    [Function] default-error-report (condition)

> Default function for reporting errors.

    [Class] thread ()
      [Slot] id - Implementation dependent thread identifier.
      [Slot] function - Function executed by current thread.

    [Function] make-lock ()

    [Macro] with-lock (lock &body body)

    [Function] thread-start (thread)

    [Function] current-thread ()

    [Function] thread-alive-p (thread)

    [Function] thread-interrupt (thread function)

    [Function] thread-join (thread)

    [Macro] without-interrupts (&body body)

    [Class] job ()
      [Slot] function - Function will be called to start the execution.
      [Slot] result-report-function - Function will be called to report the result.
      [Slot] error-report-function - Function will be called to report an error.
      [Slot] submit-time - Job queue entrance time.
      [Slot] start-time - Job execution start time.
      [Slot] finish-time - Job execution finish time.
      [Slot] condition - Signaled condition in case of a failure.
      [Slot] result - Job result in case of no failure.

    [Class] patron ()
      [Slot] error-report-function - Will get called for management related
             errors -- e.g when found a dead worker, keeper, etc.
      [Slot] job-capacity - Upper limit on the job queue size.
      [Slot] worker-capacity - Number of serving `WORKER's.
      [Slot] worker-timeout-duration - Time limit on the work processing duration.
      [Slot] keeper-timeout-duration - Wait period for keepers.

    [Function] submit-job (patron job)

> Submit given `JOB` into the job queue of `PATRON`. Function works in a blocking manner and returns inserted `JOB`, or throws a `TIMEOUT-CONDITION`.

    [Function] worker-stats (patron)

> Returns a property list of minimum, maximum, and average statistics of `N-FAILURES`, `FAIL-DURATION`, `BUSY-DURATION`, and `IDLE-DURATION` slots among workers. Function blocks job queue while gathering statistics.

    [Function] start-patron (patron)

> After switching `STATE` to `:ACTIVE`, starts `WORKERS`s and `KEEPER`s in order.

    [Function] stop-patron (patron &key wait kill)

> After switching `STATE` to `:INACTIVE`, stops `KEEPER`s and `WORKER`s in order. For related effects of keyword arguments see documentation of `STOP-KEEPERS` and `STOP-WORKERS` functions.
