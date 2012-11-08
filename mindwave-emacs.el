;;; mindwave-emacs.el --- Neurosky mindwave support

;; Copyright (C) 2012 Jonathan Arkell

;; Author: Jonathan Arkell <jonnay@jonnay.net>
;; Created: 16 June 2012
;; Keywords: comint mindwave
;; Version 0.1.5

;; This file is not part of GNU Emacs.
;; Released under the GPL     

;;; Commentary: 
;; Please see the org-file that this was generated from. 


;;; Code:

(require 'json)
(require 'cl)

(defvar mindwave-debug nil)

(defcustom mindwave-connect-with-serial nil
  "Whether or not to connect with the serial port."
  :type 'boolean
  :group 'mindwave-emacs)

(defcustom mindwave-serial-port "/dev/tty.MindWave"
  "Serial port that the mindwave is connected to."
  :type 'string
  :group 'mindwave-emacs)

(defcustom mindwave-poor-signal-level 50
  "The signal level that mindwave-emacs should stop running hooks at.

The mindwave API sends a poorSignal level hook whenever it 
senses connection problems.  This is generally between 0 and
200.

  0   - Best connection
  200 - completely off the users head. (get it?)"
  :type 'integer
  :group 'mindwave-emacs)

(defvar mindwave-host "localhost")
(defvar mindwave-port 13854)

(defvar mindwave-appName "mindwave-emacs")
(defvar mindwave-appKey (sha1 mindwave-appName))

(defconst mindwave-serial-baud 57600)
(defconst mindwave-auth-key 0000)

(defvar mindwave-buffer nil "Variable to store the buffer connected to the process.")
(defvar mindwave-process nil "Process that mindwave is connected.")

(defalias 'mindwave-connect 'mindwave-get-buffer)

(defun mindwave-get-buffer ()
  "Returns a mindwave buffer.
The connection type is dependent on the `mindwvave-connection-type' variable."
  (if mindwave-connect-with-serial
      (setq mindwave-buffer (process-buffer (mindwave-make-serial-process)))
    (mindwave-thinkgear-buffer)))

(defun mindwave-thinkgear-buffer ()
  "Returns the buffer for the mindwave connection"
  (if (and mindwave-process (process-live-p mindwave-process))
      mindwave-process
      (if (progn
        (setq mindwave-buffer (make-comint "mindwave" (cons mindwave-host mindwave-port)))
        (setq mindwave-process (get-buffer-process mindwave-buffer))
        (save-excursion
          (set-buffer mindwave-buffer)
          (buffer-disable-undo mindwave-buffer)
          (sleep-for 1)
          (mindwave-authorize)
          (sleep-for 1)
          (mindwave-get-raw nil)
          (sleep-for 1)
          (add-hook 'comint-preoutput-filter-functions 'mindwave-comint-filter-function nil t))
        mindwave-buffer))))

(defun mindwave-make-serial-process ()
  "Creates a serial process for mindwave, or returns the current one if it exists.
Note that this function assumes that you'll only ever have one mindwave connected."
  (setq mindwave-serial--bad-packets 0)
  (setq mindwave-serial--total-packets 0)
  (if (and mindwave-process
           (process-live-p mindwave-process))
      mindwave-process
    (setq mindwave-process 
          (apply 'make-serial-process 
                 :port mindwave-serial-port
                 :speed mindwave-serial-baud
                 :coding-system 'binary
                 :filter 'mindwave-serial/filter-function
                 :buffer "*mindwave*"
                 '()))))

(defun mindwave-send-string (str)
  "Helper function to send STRING directly to the mindwave.
Please use `mindwave-authorize' or `mindwave-get-raw' for user-level configuration."
  (if mindwave-connect-with-serial
      (process-send-string mindwave-process str)
      (comint-send-string mindwave-process str)))

(defvar mindwave-hook '() "Hooks to run when mindwave gets standard input\nShould be a in a list that conforms to the json output.")
(defvar mindwave-blink-hook '() "Hooks to run when mindwave gets blink input")
(defvar mindwave-e-sense-hook '() "Hooks to run when mindwave gets an eSense(tm) reading")
(defvar mindwave-eeg-power-hook '() "Hooks to run when mindwave gets an eegPower reading")

;; In the spirit of 3 strikes and refactor, once you touch this, or mindwave-serial/filter-function
;; make sure to refactor them to a common function.

(defun mindwave-serial/filter-function (process output)
  "Sends input to parser and triggers the hooks."
  (loop for brain
        in (mindwave-serial/parse-packets process output)
        do
        (mindwave-if-in-list 'blinkStrength  brain
          (mindwave/set-current 'blinkStrength  mw-result)
          (run-hook-with-args 'mindwave-blink-hook mw-result))
        (run-hook-with-args 'mindwave-hook brain)
        (if (and (assoc 'poorSignalLevel brain)
                 (> (cdr (assoc 'poorSignalLevel brain))
                    mindwave-poor-signal-level))
            (progn 
              (when (assoc 'poorSignalLevel brain)
                (mindwave/set-current 'poorSignalLevel (cdr (assoc 'poorSignalLevel brain)))
                (run-hook-with-args 'mindwave-poor-signal-hook 
                                    (cdr (assoc 'poorSignalLevel brain)))))
          (progn
            (mindwave-if-in-list 'poorSignalLevel brain
              (mindwave/set-current 'poorSignalLevel mw-result)
              (run-hook-with-args 'mindwave-poor-signal-hook mw-result))
            (mindwave-if-in-list 'eSense brain
              (mindwave/set-current 'eSense mw-result)
              (run-hook-with-args mindwave-e-sense-hook mw-result))                
            (mindwave-if-in-list 'eegPower brain
              (mindwave/set-current 'eegPower mw-result)
              (run-hook-with-args 'mindwave-eeg-power-hook mw-result)
              (mindwave/brain-ring-update brain)))))
  (when mindwave-debug
    (with-current-buffer "*mindwave*"
      (goto-char (point-max))
      (if mindwave-debug (insert output)))))

(defvar mindwave-serial--partial-packet nil)

(defvar mindwave-serial--bad-packets 0)
(defvar mindwave-serial--total-packets 0)

(defun mindwave-serial/parse-packets (process output)
  "Lower level serial filter function.
  Returns a lit of mindwave-emacs compatible lists, with no guarantee of order.
  Note that this also inserts the raw eeg into the raw eeg ring."
  (when (not (null mindwave-serial--partial-packet))
    (setq output (concat mindwave-serial--partial-packet output))
    (setq mindwave-serial--partial-packet nil))
  (let ((pos 0)
        (end (length output))
        (dlen 0)
        (packet '())
        (poorSignalLevel '())
        (eSense '())
        (eegPower '())
        (blink '())
        (raw nil)
        (out-packets '()))
    (setq mindwave-serial--partial-packet
          (catch 'partial-packet-received
            (while (< pos end)
              (when  (<= end (+ pos 3))
                (setq mindwave-serial--partial-packet (substring output pos))
                (throw 'partial-packet-received (substring output pos)))
              (if (and (= #xAA
                          (aref output pos))
                       (= #xAA
                          (aref output (+ 1 pos))))
                  (let ((plen (aref output (+ 2 pos))))
                    (when (<= end (+ pos 3 plen))
                      (setq mindwave-serial--partial-packet (substring output pos))
                      (throw 'partial-packet-received (substring output pos)))
                    (setq mindwave-serial--total-packets (1+ mindwave-serial--total-packets))
                    (if (and (not (= 0 plen))
                             (= (aref output (+ 3 plen pos))
                                (mindwave-serial/checksum-bytestream
                                 (substring output 
                                            (+ pos 3) 
                                            (+ plen pos 3)))))
                        (mindwave-serial--parse-data)
                      (progn 
                        (setq mindwave-serial--bad-packets (1+ mindwave-serial--bad-packets))
                        (if mindwave-debug (message "Mindwave: Checksum doesn't match, or got a zero length packet. [%s]" (substring output pos (+ 3 plen pos))))
                        (setq pos (1+ pos)))))
                (progn 
                  (setq pos (1+ pos)))))
            nil))
    (nreverse (delq nil out-packets))))

(defmacro mindwave-serial--parse-data ()
  '(progn 
     (setq pos (+ 3 pos))
     (let ((innerlen (+ pos plen)))
       (while (< pos innerlen)
         (if (> #x80 (aref output pos))
             (progn
               (case (aref output pos)
                 (#x02 (add-to-list 'packet (cons 'poorSignalLevel 
                                                  (aref output (+ 1 pos)))))
                 (#x04 (add-to-list 'eSense (cons 'attention 
                                                  (aref output (+ 1 pos)))))
                 (#x05 (add-to-list 'eSense (cons 'meditation 
                                                  (aref output (+ 1 pos))) t))
                 (#x16 (add-to-list 'packet (cons 'blinkStrength 
                                                  (aref output (+ 1 pos))) t))
                 (t (message "Mindwave: unknown unibyte type %s with data %s"
                             (aref output pos)
                             (aref output (+ 1 pos)))))
               (setq pos (+ 2 pos)))
           (progn 
             (case (aref output pos)
               (#x80 (setq raw 
                           (mindwave-serial/2byte-sword-to-int (aref output (+ 2 pos))
                                                               (aref output (+ 3 pos)))))
               (#x83 (setq eegPower
                           (list (mindwave-serial/make-eeg-list 'delta 2)
                                 (mindwave-serial/make-eeg-list 'theta 5)
                                 (mindwave-serial/make-eeg-list 'lowAlpha 8)
                                 (mindwave-serial/make-eeg-list 'highAlpha 11)
                                 (mindwave-serial/make-eeg-list 'lowBeta 14)
                                 (mindwave-serial/make-eeg-list 'highBeta 17)
                                 (mindwave-serial/make-eeg-list 'lowGamma 20)
                                 (mindwave-serial/make-eeg-list 'highGamma 23))))
               (208 (message "Mindwave Got Packet with id 208. Full packet: %s" (append output '()))) ;; Not entirely sure what these packets mean
               (210 (message "Mindwave Got Packet with id 210. Full packet: %s" (append output '()))) ;;
               (212 (message "Mindwave Got Packet with id 212. Full packet: %s" (append output '()))) ;;
               (t (message "Mindwave unknown multibyte type %s" (aref output pos))))
             (setq pos (+ pos 
                          (aref output (+ pos 1)) 
                          2)))))
       (setq out-packets 
             (cons (append packet 
                           (if (not (null eSense))
                               (cons (cons 'eSense eSense)
                                     '()))                          
                           (if (not (null eegPower))
                               (cons (cons 'eegPower eegPower)
                                     '())))
                   out-packets))
       (if raw (ring-insert mindwave-eeg-ring raw))
       (setq raw '()
             eSense '()
             eegPower '()
             packet '()
             pos (+ pos 1)))))

(defmacro mindwave-serial/make-eeg-list (eegName b1)
  (let ((b2 (+ 1 b1))
        (b3 (+ 2 b1)))
    `(cons ,eegName (mindwave-serial/3byte-uword-to-int (aref output (+ ,b1 pos))
                                                        (aref output (+ ,b2 pos))
                                                        (aref output (+ ,b3 pos))))))

(ert-deftest mindwave-serial/make-eeg-list-test ()
  ""
  (should (equal '(cons 'highGamma 
                        (mindwave/3byte-uword-to-int (aref output (+ 23 pos))
                                                     (aref output (+ 24 pos))
                                                     (aref output (+ 25 pos))))
                 (macroexpand '(mindwave-serial/make-eeg-list 'highGamma 23)))))

(defmacro mindwave-serial/checksum-bytestream (stream)
  "do a checksum calculation on a bytestream"                                      
  `(lognot (logior -256 (mod (reduce #'(lambda (x y) (mod (+ x y) 256)) 
                                      stream) 
                             256))))


(ert-deftest mindwave-serial/checksum-test ()
    "test the checksum macro"
  (should (= (mindwave-serial/checksum-bytestream (concat (list 0))) 255))
  (should (= (mindwave-serial/checksum-bytestream (concat (list 255))) 0))
  (should (= (mindwave-serial/checksum-bytestream (concat (list 255 1))) 255))
  (should (= (mindwave-serial/checksum-bytestream (concat (list 255 1 255 1 255 1))) 255))
  (should (= (mindwave-serial/checksum-bytestream (concat (list 0 1))) 254))
  (should (= (mindwave-serial/checksum-bytestream (concat (list #x02 #x00 #x83 #x18 #x00 #x00 #x94 #x00 #x00 #x42 #x00 #x00 #x0B #x00 #x00 #x64 #x00 #x00 #x4D #x00 #x00 #x3D #x00 #x00 #x07 #x00 #x00 #x05 #x04 #x0D #x05 #x3D))) #x34)))


(defmacro mindwave-serial/2byte-sword-to-int (byte1 byte2)
  "Perform a 2's compliment on a pair of bytes"
  `(if (> ,byte1 127)
       (logior -65536 (+ (* 256 ,byte1) ,byte2))
     (+ (* 256 ,byte1) ,byte2)))

(ert-deftest mindwave-2s-compliment-test ()
    "Test 2s compliment macros"
  (should (= (mindwave-serial/2byte-sword-to-int 0 1) 1))
  (should (= (mindwave-serial/2byte-sword-to-int 0 2) 2))
  (should (= (mindwave-serial/2byte-sword-to-int 0 127) 127))
  (should (= (mindwave-serial/2byte-sword-to-int 0 128) 128))
  (should (= (mindwave-serial/2byte-sword-to-int 0 #x80) 128))
  (should (= (mindwave-serial/2byte-sword-to-int 1 0) 256))
  (should (= (mindwave-serial/2byte-sword-to-int #b11111111 #b11111111) -1)) 
  (should (= (mindwave-serial/2byte-sword-to-int #b11111111 #b11111011) -5))
  (should (= (mindwave-serial/2byte-sword-to-int #b11111111 #b11000000) -64))
  (should (= (mindwave-serial/2byte-sword-to-int #b11111111 #b10000000) -128))
  (should (= (mindwave-serial/2byte-sword-to-int #b11111111 #b00000000) -256))
  (should (= (mindwave-serial/2byte-sword-to-int #b11000000 #b00000000) -16384))
  (should (= (mindwave-serial/2byte-sword-to-int #b10000000 #b00000000) -32768)))

(defmacro mindwave-serial/3byte-uword-to-int (byte1 byte2 byte3)
  `(+ (* #x010000 ,byte1)
      (* #x000100 ,byte2)
      ,byte3))

(ert-deftest mindwave-3byte-uword ()
    "Test 3 byte uword macros"
  (should (= (mindwave-serial/3byte-uword-to-int 0 0 1) 1))
  (should (= (mindwave-serial/3byte-uword-to-int 0 1 0) 256))
  (should (= (mindwave-serial/3byte-uword-to-int 1 0 0) 65536))
  (should (= (mindwave-serial/3byte-uword-to-int 255 255 255) #xffffff)))

(eval-when-compile
  (require 'cl))

(defun mindwave-if-in-list-run-hook (key list hook &rest funcs)
  (when (assoc key list)
    (when (not (null funcs))
      (dolist (func funcs) 
              (apply func (cdr (assoc key list)))))
    (run-hook-with-args hook (cdr (assoc key list)))))

(defmacro mindwave-if-in-list (key list &rest forms)
  "Helper macro to bind the mw-result to (assoc KEY LIST) and run FORMS"
  (declare (indent 2))
  `(let ((mw-result (assoc ,key ,list)))
     (if mw-result
         (progn 
           (setq mw-result  (cdr mw-result))
           ,@forms)
       nil)))

(ert-deftest mindwave/test-if-in-list ()
    ""
  (let ((r nil))
    (mindwave-if-in-list 'a '() (setq r 't))
    (should (not r)))
  (let ((r nil))
    (debug)
    (mindwave-if-in-list 'a '((a 1)) (setq r mw-result))
    (should r)))

;; In the spirit of 3 strikes and refactor, once you touch this, or mindwave-serial/filter-function
;; make sure to refactor them to a common function.
(defun mindwave-comint-filter-function (output)
  "A helper hook to pass off output to the apropriate hooks"
  (when (and (stringp output) 
             (string= (substring output 0 1) "{"))   
    (loop for out 
          in (split-string output "\C-j" t)
          do
          (let ((brain (json-read-from-string out)))
            (mindwave-if-in-list 'blinkStrength  brain
              (mindwave/set-current 'blinkStrength  mw-result)
              (run-hook-with-args 'mindwave-blink-hook mw-result))
            (run-hook-with-args 'mindwave-hook brain)
            (if (and (assoc 'poorSignalLevel brain)
                     (> (cdr (assoc 'poorSignalLevel brain))
                        mindwave-poor-signal-level))
                (progn 
                  (when (assoc 'poorSignalLevel brain)
                    (mindwave/set-current 'poorSignalLevel (cdr (assoc 'poorSignalLevel brain)))
                    (run-hook-with-args 'mindwave-poor-signal-hook 
                                        (cdr (assoc 'poorSignalLevel brain)))))
              (progn
                (mindwave-if-in-list-run-hook 'rawEeg brain 'mindwave-raw-hook)
                (mindwave-if-in-list 'poorSignalLevel brain
                  (mindwave/set-current 'poorSignalLevel mw-result)
                  (run-hook-with-args 'mindwave-poor-signal-hook mw-result))
                (mindwave-if-in-list 'eSense brain
                  (mindwave/set-current 'eSense mw-result)
                  (run-hook-with-args mindwave-e-sense-hook mw-result))                
                (mindwave-if-in-list 'eegPower brain
                  (mindwave/set-current 'eegPower mw-result)
                  (run-hook-with-args 'mindwave-eeg-power-hook mw-result)
                  (mindwave/brain-ring-update brain)))))))
  (if mindwave-debug output ""))

(defvar mindwave/current '((poorSignalLevel . 200)
                           (eSense . ((attention . 0)
                                      (meditation . 0)))
                           (eegPower . ((delta      . 0)
                                        (theta      . 0)
                                        (lowAlpha   . 0)
                                        (highAlpha  . 0)
                                        (lowBeta    . 0)
                                        (highBeta   . 0)
                                        (lowGamma   . 0)
                                        (highGamma  . 0)))
                           (blinkStrength . 0))
  "The last known values from the mindwave headset.")

(defun mindwave/set-current (key val)
  (setq mindwave/current (list (if (equal key 'poorSignalLevel)
                                   (cons key val)
                                   (assoc 'poorSignalLevel mindwave/current))
                               (if (equal key 'eSense)
                                   (cons key val)
                                   (assoc 'eSense mindwave/current))
                               (if (equal key 'eegPower)
                                   (cons key val)
                                   (assoc 'eegPower mindwave/current))
                               (if (equal key 'blinkStrength)
                                   (cons key val)
                                   (assoc 'blinkStrength mindwave/current)))))

(ert-deftest mindwave/current-setters ()
  (setq mindwave/current '((poorSignalLevel . 200)
                           (eSense . ((attention . 0)
                                      (meditation . 0)))
                           (eegPower . ((delta      . 0)
                                        (theta      . 0)
                                        (lowAlpha   . 0)
                                        (highAlpha  . 0)
                                        (lowBeta    . 0)
                                        (highBeta   . 0)
                                        (lowGamma   . 0)
                                        (highGamma  . 0)))
                           (blinkStrength . 0)))
  (mindwave/set-current 'blinkStrength 255)
  (should (equal (assoc 'blinkStrength mindwave/current)
                 '(blinkStrength . 255)))

  (should (equal mindwave/current
                 '((poorSignalLevel . 200)
                   (eSense . ((attention . 0)
                              (meditation . 0)))
                   (eegPower . ((delta      . 0)
                                (theta      . 0)
                                (lowAlpha   . 0)
                                (highAlpha  . 0)
                                (lowBeta    . 0)
                                (highBeta   . 0)
                                (lowGamma   . 0)
                                (highGamma  . 0)))
                   (blinkStrength . 255))))
  (mindwave/set-current 'eegPower '(((delta      . 1)
                                     (theta      . 1)
                                     (lowAlpha   . 1)
                                     (highAlpha  . 1)
                                     (lowBeta    . 1)
                                     (highBeta   . 1)
                                     (lowGamma   . 1)
                                     (highGamma  . 1))))
  (should (equal mindwave/current
                 '((poorSignalLevel . 200)
                   (eSense . ((attention . 0)
                              (meditation . 0)))
                   (eegPower . ((delta      . 1)
                                (theta      . 1)
                                (lowAlpha   . 1)
                                (highAlpha  . 1)
                                (lowBeta    . 1)
                                (highBeta   . 1)
                                (lowGamma   . 1)
                                (highGamma  . 1)))
                   (blinkStrength . 255))))
  (mindwave/set-current 'eegPower '(((delta      . 2)
                                     (theta      . 2)
                                     (lowAlpha   . 2)
                                     (highAlpha  . 2)
                                     (lowBeta    . 2)
                                     (highBeta   . 2)
                                     (lowGamma   . 2)
                                     (highGamma  . 2))))
  (should (equal mindwave/current
                 '((poorSignalLevel . 200)
                   (eSense . ((attention . 0)
                              (meditation . 0)))
                   (eegPower . ((delta      . 2)
                                (theta      . 2)
                                (lowAlpha   . 2)
                                (highAlpha  . 2)
                                (lowBeta    . 2)
                                (highBeta   . 2)
                                (lowGamma   . 2)
                                (highGamma  . 2)))
                   (blinkStrength . 0))))

  (setq mindwave/current '((poorSignalLevel . 200)
                           (eSense . ((attention . 0)
                                      (meditation . 0)))
                           (eegPower . ((delta      . 0)
                                        (theta      . 0)
                                        (lowAlpha   . 0)
                                        (highAlpha  . 0)
                                        (lowBeta    . 0)
                                        (highBeta   . 0)
                                        (lowGamma   . 0)
                                        (highGamma  . 0)))
                           (blinkStrength . 0)))
  )

(defconst mindwave/brain-ring-size 30)

(defvar mindwave/brain-ring (make-ring mindwave/brain-ring-size))
(defvar mindwave/brain-ring-reset-counter 0)

(defvar mindwave/brain-ring-full-hook '() "Hook to call when the brain ring is full")

(defun mindwave/access-in (outer-key inner-key list)
  "Access the value of INNER-KEY from OUTER-KEY of alist LIST"
  (cdr (assoc inner-key (cdr (assoc outer-key list)))))

(ert-deftest mindwave/test-access-in ()
  (should (equal (mindwave/access-in 'outer 
                                     'inner 
                                     '((outer1 . (inner1 . 0))
                                       (outer . ((inner . 23)))))
                 23)))

(defun mindwave/make-brain-ring (meditation attention delta theta lowAlpha highAlpha lowBeta highBeta lowGamma highGamma &optional poorSignalLevel)
  "convenience function to make a valid brain ring entry"
  `((poorSignalLevel . ,(or poorSignalLevel 0))
    (eSense . ((meditation  . ,meditation)
               (attention   . ,attention)))
    (eegPower . ((delta     . ,delta) 
                 (theta     . ,theta)
                 (lowAlpha  . ,lowAlpha)
                 (highAlpha . ,highAlpha)
                 (lowBeta   . ,lowBeta)
                 (highBeta  . ,highBeta)
                 (lowGamma  . ,lowGamma)
                 (highGamma . ,highGamma)))))

(ert-deftest mindwave/make-brain-ring ()
  "Maker tests. Super simple,"
  (should (equal (mindwave/make-brain-ring 0 0 0  0  0  0  0   0 0  0)
                 (mindwave/make-brain-ring 0 0 0  0  0  0  0   0 0  0  0)))
  (should (equal (mindwave/make-brain-ring 2 3 5 23 32 46 92 184 7 13 21)
                 (mindwave/make-brain-ring 2 3 5 23 32 46 92 184 7 13 21))))

(defun mindwave/make-single-val-brain-ring (val)
  "Convenience function to make a brain ring of a single value VAL.
  Useful for dealing with averages."
  (mindwave/make-brain-ring val 
                            val val val val val val val val
                            val val))

(ert-deftest mindwave/make-brain-ring ()
  "Maker tests. Super simple,"
  (should (equal (mindwave/make-brain-ring 0 0 0  0  0  0  0   0 0  0)
                 (mindwave/make-single-val-brain-ring 0)))
  (should (equal (mindwave/make-brain-ring 1 1 1 1 1 1 1 1 1 1 1)
                 (mindwave/make-single-val-brain-ring 1))))

;; Averaging the brain ring can be a little dicey since we expect poorSignalLevel to be 0
(defun mindwave/safe-div (dividend divisor) 
  "Another division function safe to use with averaging. 0 save-div 0 = 0"
  (if (= 0 divisor)
      0
    (/ dividend divisor)))


(ert-deftest mindwave/safe-div () 
  "test out safe div"
  (should (equal (mindwave/safe-div 0 0) 0))
  (should (equal (mindwave/safe-div 3 0) 0))
  (should (equal (mindwave/safe-div 0 3) 0)))

(defun mindwave/brain-ring-apply (fn ring1 ring2)
  "Takes the \"brain-rings\" RING1 and RING2 and runs FN on it's guts"
  (mindwave/make-brain-ring
   (funcall fn (mindwave/access-in 'eSense 'meditation ring1)
               (mindwave/access-in 'eSense 'meditation ring2))
   (funcall fn (mindwave/access-in 'eSense 'attention ring1)
               (mindwave/access-in 'eSense 'attention ring2))
   (funcall fn (mindwave/access-in 'eegPower 'delta ring1)
               (mindwave/access-in 'eegPower 'delta ring2))
   (funcall fn (mindwave/access-in 'eegPower 'theta ring1)
               (mindwave/access-in 'eegPower 'theta ring2))
   (funcall fn (mindwave/access-in 'eegPower 'lowAlpha ring1)
               (mindwave/access-in 'eegPower 'lowAlpha ring2))
   (funcall fn (mindwave/access-in 'eegPower 'highAlpha ring1)
               (mindwave/access-in 'eegPower 'highAlpha ring2))
   (funcall fn (mindwave/access-in 'eegPower 'lowBeta ring1)
               (mindwave/access-in 'eegPower 'lowBeta ring2))
   (funcall fn (mindwave/access-in 'eegPower 'highBeta ring1)
               (mindwave/access-in 'eegPower 'highBeta ring2))
   (funcall fn (mindwave/access-in 'eegPower 'lowGamma ring1)
               (mindwave/access-in 'eegPower 'lowGamma ring2))
   (funcall fn (mindwave/access-in 'eegPower 'highGamma ring1)
               (mindwave/access-in 'eegPower 'highGamma ring2))
   (funcall fn (cdr (assoc 'poorSignalLevel ring1))
               (cdr (assoc 'poorSignalLevel ring2)))))

(ert-deftest mindwave/test-brain-ring-add ()
  (should (equal (mindwave/make-brain-ring 0 0 0 0 0 0 0 0 0 0)
                 (mindwave/brain-ring-apply '+ 
                                            (mindwave/make-brain-ring 0 0 0 0 0 0 0 0 0 0)
                                            (mindwave/make-brain-ring 0 0 0 0 0 0 0 0 0 0))))
  (should (equal (mindwave/make-brain-ring 1 2 3 4 5 6 7 8 9 10)
                 (mindwave/brain-ring-apply '+
                                            (mindwave/make-brain-ring 1 2 3 4 5 6 7 8 9 10)
                                            (mindwave/make-brain-ring 0 0 0 0 0 0 0 0 0 0))))
  (should (equal (mindwave/make-brain-ring 2 3 4 5 6 7 8 9 10 11)
                 (mindwave/brain-ring-apply '+
                                            (mindwave/make-brain-ring 1 2 3 4 5 6 7 8 9 10)
                                            (mindwave/make-brain-ring 1 1 1 1 1 1 1 1 1 1))))
  (should (equal (mindwave/make-brain-ring 2 3 4 5 6 7 8 9 10 11 12)
                 (mindwave/brain-ring-apply '+
                                            (mindwave/make-brain-ring 1 2 3 4 5 6 7 8 9 10 11)
                                            (mindwave/make-brain-ring 1 1 1 1 1 1 1 1 1 1 1)))))

(defun mindwave/brain-ring-update (brain)
  "Keep a running tally of your neurological state."
  (when (and (assoc 'eSense brain)
             (assoc 'eegPower brain)
             (assoc 'poorSignalLevel brain)
             (> mindwave-poor-signal-level
                (cdr (assoc 'poorSignalLevel brain))))
    (ring-insert mindwave/brain-ring  brain)
    (when (>= (ring-length mindwave/brain-ring) 
              mindwave/brain-ring-size)
      (let ((new-ring (make-ring mindwave/brain-ring-size))
            (s mindwave/brain-ring-size)
            (collapsed-ring (reduce #'(lambda (brain total) 
                                        (mindwave/brain-ring-apply '+ brain total)) 
                                    (ring-elements mindwave/brain-ring)
                                    :initial-value (mindwave/make-brain-ring 0 0 0 0 0 0 0 0 0 0))))
        (setq mindwave/brain-ring new-ring)
        (run-hook-with-args 'mindwave/brain-ring-full-hook
                            (mindwave/brain-ring-apply 'mindwave/safe-div 
                                                       collapsed-ring 
                                                       (mindwave/make-brain-ring s s s s s s s s s s)))))))

(defcustom mindwave-eeg-ring-size 2048
  "Size of the eeg ring to store.
Generally the sampling frequency of a mindwave is 512hz, so to get 
an 8 second ring, you would want a size of 4096."
  :group 'mindwave
  :type 'int )

(defvar mindwave-eeg-ring (make-ring mindwave-eeg-ring-size)
  "A ring full of mindwave raw eeg values.")

(defun mindwave-get-raw (raw)
  "Return raw output from mindwave.
RAW is a boolean value as to whether or not to listen for raw values"
  (mindwave-send-string (json-encode `(("enableRawOutput" . ,(if raw t json-false))
                                      ("format" . "Json")))))

(defvar mindwave-authorized-p nil "whether or not app is authorized")

(defun mindwave-authorize () 
  "provides an autorization request to the mindwave server"
  (mindwave-send-string (json-encode `(("appName" . ,mindwave-appName) 
                                       ("appKey" . ,mindwave-appKey)))))

(provide 'mindwave-emacs)

;;; mindwave-emacs.el ends here
