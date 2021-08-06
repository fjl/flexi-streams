(uiop:with-muffled-loader-conditions ()
  (uiop:with-muffled-compiler-conditions ()
    (let ((*load-verbose* nil)
          (*compile-verbose* nil)
          (*load-print* nil)
          (*compile-print* nil)
          (*error-output* (make-broadcast-stream)))
      (asdf:load-system "flexi-streams" :verbose nil))))

(defpackage #:flexi-streams-bench
  (:use #:cl #:flexi-streams)
  (:export #:run))

(in-package #:flexi-streams-bench)

(defun read-file-bytes (file)
  (with-open-file (stream file :element-type 'octet)
    (let ((s (make-array (file-length stream) :element-type 'octet)))
      (read-sequence s stream)
      s)))

(defstruct result
  (n      0)
  (start  0)
  (end    0)
  (name   nil)
  (output ""))

(defmacro make-benchmark ((n &key name) &body body)
  (let ((b (gensym "B")))
    `(lambda (,n)
       (let ((,b (make-result :name ,name :n ,n)))
         (setf (result-output ,b)
               (flet ((reset-timer () (setf (result-start ,b) (get-internal-real-time))))
                 (with-output-to-string (*trace-output*)
                   (time
                    (progn
                      (setf (result-start ,b) (get-internal-real-time))
                      ,@body
                      (setf (result-end ,b) (get-internal-real-time)))))))
         ,b))))

(defun print-result (result &optional (out *standard-output*))
  (let* ((d-internal (- (result-end result) (result-start result)))
         (int-ns     (/ internal-time-units-per-second (* 1000 1000 1000)))
         (ns-op      (round (/ (/ d-internal (result-n result)) int-ns))))
    (format out "Benchmark~A ~A ~A ns/op~%" (result-name result) (result-n result) ns-op)
    (format out "~<;; ~@;~A~:>~%" (list (string-trim '(#\Newline) (result-output result))))))

(defun read-char-benchmark (name &key file format (n-chars 200))
  (make-benchmark (n-times :name name)
    (let ((content (read-file-bytes file))
          (format  (apply #'make-external-format format)))
      (when (< (char-length content :external-format format) n-chars)
        (error "Input file ~A is too short, need at least ~A characters." file n-chars))
      (reset-timer)
      (loop repeat n-times do
        (let* ((bytes-stream (make-in-memory-input-stream content))
               (char-stream  (make-flexi-stream bytes-stream :external-format format)))
          (loop repeat n-chars do
            (read-char char-stream)))))))

(defun read-sequence-string-benchmark (name &key file format (n-chars 500))
  (make-benchmark (n-times :name name)
    (let ((content (read-file-bytes file))
          (format  (apply #'make-external-format format)))
      (when (< (char-length content :external-format format) n-chars)
        (error "Input file ~A is too short, need at least ~A characters." file n-chars))
      (let ((result (make-string n-chars)))
        (reset-timer)
        (loop repeat n-times do
          (let* ((bytes-stream (make-in-memory-input-stream content))
                 (char-stream  (make-flexi-stream bytes-stream :external-format format)))
            (read-sequence result char-stream)))))))

(defun octets-to-string-benchmark (name &key file format (n-chars 200))
  (make-benchmark (n-times :name name)
    (let* ((content (read-file-bytes file))
           (format (apply #'make-external-format format))
           (all-chars (octets-to-string content :external-format format)))
      (when (< (length all-chars) n-chars)
        (error "Input file ~A is too short, need at least ~A characters." file n-chars))
      (let ((n-bytes (octet-length all-chars :external-format format :end n-chars)))
        (reset-timer)
        (loop repeat n-times do
          (octets-to-string content :external-format format :end n-bytes))))))

(defparameter *benchmarks*
  (list (read-char-benchmark
         "READ-CHAR-200/latin1_lf"
         :file "test/kafka_latin1_lf.txt"
         :format '(:latin-1 :eol-style :lf)
         :n-chars 200)
        (read-char-benchmark
         "READ-CHAR-200/utf8_lf"
         :file "test/unicode_demo_utf8_lf.txt"
         :format '(:utf-8 :eol-style :lf)
         :n-chars 200)
        (read-char-benchmark
         "READ-CHAR-200/utf16_lf"
         :file "test/unicode_demo_ucs2_lf_le.txt"
         :format '(:utf-16le :eol-style :lf)
         :n-chars 200)
        (read-char-benchmark
         "READ-CHAR-200/utf32_lf"
         :file "test/unicode_demo_ucs4_lf_le.txt"
         :format '(:utf-32le :eol-style :lf)
         :n-chars 200)
        (read-sequence-string-benchmark
         "READ-SEQUENCE-STRING-500/latin1_lf"
         :file "test/kafka_latin1_lf.txt"
         :format '(:latin-1 :eol-style :lf)
         :n-chars 500)
        (read-sequence-string-benchmark
         "READ-SEQUENCE-STRING-500/utf8_lf"
         :file "test/unicode_demo_utf8_lf.txt"
         :format '(:utf-8 :eol-style :lf)
         :n-chars 500)
        (read-sequence-string-benchmark
         "READ-SEQUENCE-STRING-500/utf16_lf"
         :file "test/unicode_demo_ucs2_lf_le.txt"
         :format '(:utf-16le :eol-style :lf)
         :n-chars 500)
        (read-sequence-string-benchmark
         "READ-SEQUENCE-STRING-500/utf32_lf"
         :file "test/unicode_demo_ucs4_lf_le.txt"
         :format '(:utf-32le :eol-style :lf)
         :n-chars 1000)
        (octets-to-string-benchmark
         "OCTETS-TO-STRING-1000/latin1_lf"
         :file "test/kafka_latin1_lf.txt"
         :format '(:latin-1 :eol-style :lf)
         :n-chars 1000)
        (octets-to-string-benchmark
         "OCTETS-TO-STRING-1000/latin1_crlf"
         :file "test/kafka_latin1_crlf.txt"
         :format '(:latin-1 :eol-style :crlf)
         :n-chars 1000)
        (octets-to-string-benchmark
         "OCTETS-TO-STRING-1000/utf8_lf"
         :file "test/unicode_demo_utf8_lf.txt"
         :format '(:utf-8 :eol-style :lf)
         :n-chars 1000)
        (octets-to-string-benchmark
         "OCTETS-TO-STRING-1000/utf16_lf"
         :file "test/unicode_demo_ucs2_lf_le.txt"
         :format '(:utf-16le :eol-style :lf)
         :n-chars 1000)
        (octets-to-string-benchmark
         "OCTETS-TO-STRING-1000/utf32_lf"
         :file "test/unicode_demo_ucs4_lf_le.txt"
         :format '(:utf-32le :eol-style :lf)
         :n-chars 1000)))

(defun run (&key (n 1000) (count 1))
  (format t "~&lisp: ~A ~A~%" (lisp-implementation-type) (lisp-implementation-version))
  (format t "~&arch: ~A~%" (machine-type))
  (dolist (b *benchmarks*)
    (dotimes (i count)
      (print-result (funcall b n)))))
