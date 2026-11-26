(in-package :data-ui)

;; Interim request serialization tests (backlog item "Interim request
;; serialization"). The gate serializes every dispatched request except
;; the exact /health script name. The suite proves exclusion with the
;; gate's own entered-dispatch counter (request-serialization-entered-count,
;; synchronized), never h:acceptor-requests-in-progress (which counts
;; pre-dispatch states: TCP accept, header read, the taskmaster
;; thread-count wait).
;;
;; The hook is global server-thread-visible state: tests install it with
;; setf and restore the saved global value in unwind-protect, on every
;; exit path. A dynamic (let) binding in the client thread would be
;; invisible to Hunchentoot worker threads.
;;
;; with-model never stops the acceptor, so a worker left blocked in the
;; hook would wedge every later HTTP suite in this image. Every wait is
;; therefore bounded, and with-rsq-hook releases the permit, restores
;; the hook, and joins the clients before the test thread moves on.
;;
;; /health is fetched with dr:*text-content-types* bound to
;; (("text" . nil)): rest-tests.lisp setfs the special to
;; '((nil . "json")) at load time, so the image default turns the
;; text/html body into octets.

(def-suite request-serialization-suite
  :description "The interim request-serialization gate: at most one
dispatched request at a time, /health exempt, abort releases the
mutex, and the queue signal is the gate's own entered-dispatch
counter.")

(in-suite request-serialization-suite)

(defparameter *rsq-deadline-seconds* 10
  "How long the test thread waits for an event it expects promptly.")

(defparameter *rsq-join-seconds* 15
  "Deadline for joining one client thread.")

(defparameter *rsq-block-seconds* 30
  "Backstop for a hook entry blocked on the release permit.")

(defun rsq-entered ()
  "The gate's synchronized entered-dispatch count — never a bare
symbol-value read from this thread."
  (request-serialization-entered-count))

(defun rsq-count (state key)
  "Read the counter KEY of STATE under the state mutex."
  (sb-thread:with-mutex ((getf state :state-mutex))
    (getf state key)))

(defun rsq-bump (state key &optional (delta 1))
  "Add DELTA to the counter KEY of STATE under the state mutex."
  (sb-thread:with-mutex ((getf state :state-mutex))
    (incf (getf state key) delta)))

(defun rsq-wait-until (predicate)
  "Poll PREDICATE with sb-thread:thread-yield until it returns true or
*rsq-deadline-seconds* elapses; return the final value."
  (let ((deadline (+ (get-internal-real-time)
                    (* *rsq-deadline-seconds*
                       internal-time-units-per-second))))
    (loop for value = (funcall predicate)
      until (or value (>= (get-internal-real-time) deadline))
      do (sb-thread:thread-yield)
      finally (return value))))

(defun rsq-wait-for-entry (state)
  "Wait (bounded) for the next hook entry: the hook signals the
counting semaphore once per entry."
  (sb-thread:wait-on-semaphore (getf state :counting)
    :timeout *rsq-deadline-seconds*))

(defun rsq-release (state)
  "Signal the release permit: a hook entry blocked on it runs on."
  (sb-thread:signal-semaphore (getf state :release)))

(defun rsq-record-entry (state)
  "Record one hook entry under the state mutex: the entry count and
the overlap detectors — :active (hook bodies in flight right now) and
:max-active (their high-water mark, which must never exceed 1)."
  (sb-thread:with-mutex ((getf state :state-mutex))
    (when (plusp (getf state :active))
      (incf (getf state :exclusion-failures)))
    (incf (getf state :active))
    (setf (getf state :max-active)
      (max (getf state :max-active) (getf state :active)))
    (incf (getf state :hook-entries))))

(defun rsq-block-first-entry (state)
  "Block the first hook entry on the release permit — and only the
first: the exclusion test signals the permit once, so a second entry
that blocked too would strand its worker. The wait itself is bounded,
so even an unsignaled permit cannot park a worker for good."
  (let ((block (sb-thread:with-mutex ((getf state :state-mutex))
                 (when (getf state :block-first-p)
                   (setf (getf state :block-first-p) nil)
                   t))))
    (when block
      (sb-thread:wait-on-semaphore (getf state :release)
        :timeout *rsq-block-seconds*))))

(defun rsq-with-state (blockp)
  "The state plist for one test: the :active / :max-active /
:hook-entries / :exclusion-failures counters under their :state-mutex,
the :counting semaphore (one signal per hook entry), the :release
permit, :block-first-p (consumed by the one hook entry that may
block), the pending :clients, and the :hook itself. With BLOCKP the
hook blocks the first request on the release permit (the exclusion
test); without it the hook only records its entry and exit."
  (let ((state (list :active 0
                     :max-active 0
                     :hook-entries 0
                     :exclusion-failures 0
                     :block-first-p blockp
                     :state-mutex (sb-thread:make-mutex
                                    :name "rsq-state")
                     :counting (sb-thread:make-semaphore :count 0)
                     :release (sb-thread:make-semaphore :count 0)
                     :clients nil
                     :hook nil)))
    (setf (getf state :hook)
      (lambda (request)
        (declare (ignore request))
        (rsq-record-entry state)
        (sb-thread:signal-semaphore (getf state :counting))
        (rsq-block-first-entry state)
        (rsq-bump state :active -1)))
    state))

(defun rsq-make-thread (state thunk)
  "Start THUNK on a thread registered as a pending client of STATE, so
with-rsq-hook joins whatever outlives the test; returns the thread."
  (sb-thread:with-mutex ((getf state :state-mutex))
    (let ((thread (sb-thread:make-thread thunk :name "rsq-client")))
      (push thread (getf state :clients))
      thread)))

(defun rsq-make-client (state url)
  "Client thread making one http-get request to URL; returns the
thread. The result is the http-get plist (:data :code :meta)."
  (rsq-make-thread state (lambda () (http-get (make-url url)))))

(defun rsq-join (state thread)
  "Join THREAD within *rsq-join-seconds* and drop it from the pending
clients of STATE. Returns the thread's first value, or the keyword
naming the abnormal join: :timeout (the client outlived the join
deadline) or :abort (its function signalled)."
  (unwind-protect
      (multiple-value-bind (result status)
          (sb-thread:join-thread thread :default nil
            :timeout *rsq-join-seconds*)
        (or status result))
    (sb-thread:with-mutex ((getf state :state-mutex))
      (setf (getf state :clients)
        (remove thread (getf state :clients))))))

(defun rsq-fetch (state thunk)
  "Run THUNK on a client thread of STATE and wait for its result.
Drakma has no read timeout on SBCL, so the bounded join is what keeps
the test thread moving."
  (rsq-join state (rsq-make-thread state thunk)))

(defun rsq-join-assert (result)
  "Assert the client join was ordinary — neither timed out nor
aborted. Returns RESULT."
  (is (listp result)
    "client join must neither time out nor abort, got ~a" result)
  result)

(defun rsq-code (result)
  "The HTTP status of a client result, or nil when the join was
abnormal (callers compare with eql, so that stays a failure and never
an error)."
  (when (listp result) (getf result :code)))

(defun rsq-health (state)
  "GET /health on a client thread; returns the result list (:code C
:data BODY), or the keyword naming the abnormal join. Binds
dr:*text-content-types* to ((\"text\" . nil)) — not http-get, whose
((nil . \"json\")) binding turns the text/html body into octets and
then fails in json-to-plist."
  (rsq-fetch state
    (lambda ()
      (multiple-value-bind (body code)
          (let ((dr:*text-content-types* '(("text" . nil))))
            (dr:http-request (make-url "/health") :method :get))
        (list :code code :data (if (stringp body) body ""))))))

(defun rsq-join-clients (state)
  "Cleanup for with-rsq-hook: bounded-join every client still pending
and report one that outlives the deadline as a failure. Such a client
is blocked on its own socket and holds no lock, so it cannot hold up
the next HTTP suite; the gate-empty check after it is what guards the
image."
  (dolist (thread (rsq-count state :clients))
    (let ((result (rsq-join state thread)))
      (unless (listp result)
        (is nil "client ~a outlived its join deadline: ~a" thread result)))))

(defmacro with-rsq-hook (state &body body)
  "Install the hook of STATE, run BODY, and clean up in the plan's
order: signal the release permit first (a hook-blocked worker runs to
completion and releases the gate), then restore the saved global hook,
then bounded-join the client threads — and finally wait (bounded) for
the gate to empty."
  (let ((saved (gensym "saved")))
    `(let ((,saved *request-serialization-test-hook*))
       (unwind-protect
           (progn
             (setf *request-serialization-test-hook* (getf ,state :hook))
             ,@body)
         (rsq-release ,state)
         (setf *request-serialization-test-hook* ,saved)
         (rsq-join-clients ,state)
         (is-true (rsq-wait-until (lambda () (zerop (rsq-entered))))
           "the serialization gate must empty before the suite moves on")))))

(test requests-serialize-and-health-bypasses-lock
  (let ((state (rsq-with-state t)))
    (with-rsq-hook state
      ;; The first request enters the hook and blocks there, holding
      ;; the serialization mutex.
      (let ((first (rsq-make-client state "/api/public-info")))
        (is-true (rsq-wait-for-entry state)
          "first request must enter the hook")
        (is (= 1 (rsq-count state :hook-entries)))
        (is (= 1 (rsq-entered)))
        ;; The second request enters dispatch and queues on the gate; it
        ;; never reaches the hook. The positive signal is the gate's own
        ;; entered-dispatch counter reaching 2 while hook entries remain
        ;; 1.
        (let ((second (rsq-make-client state "/api/public-info")))
          (is-true (rsq-wait-until (lambda () (>= (rsq-entered) 2)))
            "second request must enter dispatch within the deadline")
          (is (= 1 (rsq-count state :hook-entries))
            "second request must not enter the hook while the first is blocked")
          (is (= 1 (rsq-count state :max-active)))
          ;; /health bypasses the gate while the first request holds it:
          ;; 200 with the OK body, and the counter never moved.
          (let ((health (rsq-health state)))
            (rsq-join-assert health)
            (is (eql h:+http-ok+ (rsq-code health)))
            (when (listp health)
              (is (equal (format nil "OK~%") (getf health :data)))))
          (is (= 2 (rsq-entered)))
          ;; Release the first handler: both clients complete, and the
          ;; second then runs through the hook once.
          (rsq-release state)
          (let ((result (rsq-join-assert (rsq-join state first))))
            (is (eql h:+http-ok+ (rsq-code result))))
          (is-true (rsq-wait-for-entry state)
            "second request must enter the hook after the release")
          (let ((result (rsq-join-assert (rsq-join state second))))
            (is (eql h:+http-ok+ (rsq-code result))))
          (is (= 2 (rsq-count state :hook-entries)))
          (is (= 1 (rsq-count state :max-active))
            "max-active must never exceed 1")
          (is (= 0 (rsq-count state :exclusion-failures))
            "two hook bodies must never overlap")
          (is (= 0 (rsq-entered))))))
    (is (= 0 (rsq-count state :active)))))

(test abort-releases-request-mutex
  ;; One-shot hook: abort-request-handler throws Hunchentoot's
  ;; handler-done out of the with-mutex body; the gate must release, and
  ;; a subsequent request must run through the real handler within a
  ;; bounded timeout.
  (let ((state (rsq-with-state nil)))
    (setf (getf state :hook)
      (lambda (request)
        (declare (ignore request))
        ;; One shot: the follow-up request below must reach the real
        ;; handler.
        (setf *request-serialization-test-hook* nil)
        (rsq-bump state :hook-entries)
        (sb-thread:signal-semaphore (getf state :counting))
        ;; The JSON error is the production abort reply (abort-request):
        ;; http-get can only parse a JSON body.
        (h:abort-request-handler
          (make-json-error h:+http-bad-request+ "aborted"))))
    (with-rsq-hook state
      (let ((aborted (rsq-make-client state "/api/public-info")))
        (is-true (rsq-wait-for-entry state)
          "aborting request must enter the hook")
        (is (eql h:+http-bad-request+
              (rsq-code (rsq-join-assert (rsq-join state aborted))))))
      (is (null *request-serialization-test-hook*)
        "the one-shot hook must remove itself")
      ;; The gate is free: a subsequent request completes within the
      ;; bounded timeout through the real handler, and the counter
      ;; drained back to 0.
      (let ((follow (rsq-join-assert
                      (rsq-join state
                        (rsq-make-client state "/api/public-info")))))
        (is (eql h:+http-ok+ (rsq-code follow))))
      (is (= 0 (rsq-entered))))
    (is (= 1 (rsq-count state :hook-entries))
      "the follow-up request must not enter the hook")))
