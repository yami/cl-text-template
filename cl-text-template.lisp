;; Introduction
;; ------------
;; A simple text template libray which reassemles perl's Text::Template.
;; 
;; Philosophy
;; ----------
;; Same as Text::Template, except the language here is Common Lisp.
;;
;; Usage
;; -----
;; 1. compile a template
;;       (compile-template-from-file "sample.tmpl")
;;    or (compile-template-from-string "1 + 2 = {(+ 1 2)}")  
;; 
;; 2. fill in the compiled template
;;       (fill-in-template (compile-template-from-string "area = {(* width height)}")
;;                         '(("WIDTH" . 2.3) ("HEIGHT" . 4.5)))
;;
;; TODO
;; ----
;; 1. evaluate the variables in a private package
;; 2. error handling/reporting
;; 3. packaging and others

(defclass compiled-template ()
  ((segments
    :initform nil
    :accessor segments)))

(defclass segment ()
  ((tag :initarg :tag)))

(defclass text-segment (segment)
  ((text
    :initarg :text
    :accessor text)))

(defclass code-segment (segment)
  ((code :initarg :code
         :accessor code)))



(defmethod print-object ((segment text-segment) s)
  (format s "{text-segment :text \"~a\"}" (text segment)))

(defmethod print-object ((segment code-segment) s)
  (format s "{code-segment :code \"~a\"}" (code segment)))


(defmethod print-object ((template compiled-template) s)
  (format s "{compiled-tempalte :segments ~a}" (segments template)))


(defgeneric fill-in-segment (segment)
  (:documentation "yes"))

(defmethod fill-in-segment ((segment text-segment))
  (text segment))


(defmethod fill-in-segment ((segment code-segment))
  (format nil "~a" (eval (read-from-string (code segment)))))

(defun append-segment (compiled-template segment)
  (let ((segments (segments compiled-template)))
    (setf (segments compiled-template) (append segments (list segment)))))


(defun read-file-to-string (filename)
  (with-open-file (s filename)
    (let* ((len  (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

(defun compile-template-from-file (filename)
  (let ((string (read-file-to-string filename)))
    (compile-template-from-string string)))

(defun compile-template-from-string (string)
  (let ((start 0)
        (context-state 'text)
        (not-eof t)
        (compiled-template (make-instance 'compiled-template)))
    (loop
       while
         not-eof
       do
         (multiple-value-bind (tag pos content)
             (next-segment string start)
           (case tag
             ((code-start)
              (append-segment compiled-template (make-instance 'text-segment :tag 'text :text content))
              (setf context-state 'code))

             ((code-end)
              (append-segment compiled-template (make-instance 'code-segment :tag 'code :code content))
              (setf context-state 'text))

             ((eof)
              (append-segment compiled-template (make-instance 'text-segment :tag 'text :text content))
              (setf not-eof nil)))
           (setf start (+ pos 1))))
    compiled-template))



(defvar *code-start* #\{)
(defvar *code-end* #\})
(defvar *code-escape* #\\)

(defun next-segment (string start)
  (let ((string-max (- (length string) 1))
        segment-tag
        segment-pos
        (segment-content "")
        (current-item "")
        escaped)
    (loop for index from start to string-max
       while
         (not segment-tag)
       do
         (let ((current-char (char string index)))
           (cond
             ((char= *code-start* current-char)
              (if (not escaped)
                  (progn (setf segment-tag 'code-start)
                         (setf segment-pos index)
                         (setf current-item ""))
                  (setf current-item (string *code-start*))))
             
             ((char= *code-end* current-char)
              (if (not escaped)
                  (progn (setf segment-tag 'code-end)
                         (setf segment-pos index)
                         (setf current-item ""))
                  (setf current-item (string *code-end*))))
             
             ((char= *code-escape* current-char)
              (if (not escaped)
                  (setf escaped t)
                  (progn (setf escaped nil)
                         (setf current-item (string *code-escape*)))))
             
             (t
              (setf escaped nil)
              (setf current-item (string current-char))))

           (when (not (string= current-item ""))
             (setf segment-content (concatenate 'string segment-content current-item)))))
         
    (if segment-tag
        (values segment-tag segment-pos segment-content)
        (values 'eof (+ string-max 1) segment-content))))


(defun fill-in-template (compiled-template &optional (name-values nil))
  (let ((segments (segments compiled-template))
        (result ""))
    (mapcar #'(lambda (name-value)
                (set (intern (string-upcase (car name-value))) (cdr name-value)))
            name-values)
    
    (setf result (apply #'concatenate 'string
                        (mapcar #'fill-in-segment segments)))
    
    (mapcar #'(lambda (name-value)
                (unintern (find-symbol (string-upcase (car name-value)))))
            name-values)
    
    result))