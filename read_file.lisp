(load "eliza_util")
(defconstant dash_label "-------------")

(defun str_replace(str_in s r)
	(let ((pos_start (search s str_in )))
		(if (not (eq pos_start nil))	
			(concatenate 'string (subseq str_in 0 pos_start) r 
								(subseq str_in (+ pos_start (length s) ))
			)
			str_in
		)
	)
)

(defun load_words_file(filename)
	;(mapcar #'flatten (parse_line (read_file filename)))
	(parse_line2 (read_file filename))
)

(defun read_file(filename)
	;(setf ht (make-hash-table))
	;(setf this_label "")
	(with-open-file (stream filename)
		 (loop for line = (read-line stream nil 'foo)
				   until (eq line 'foo)
				collect  line
		 )
	)
)


(defun new_label( val list_out)
    (cons  (string_to_list val) list_out )
)
(defun new_item( val list_out)
	(cons (append (first list_out) (string_to_list val)) (rest list_out)) 
)

(defun parse_line2(list_in  &optional(list_out '()) )
   (declare (NOTINLINE parse_line2)) ; and the compiler can't ignore NOTINLINE
	(if (not (null list_in))
		(let* ((first_list (first list_in ))(rest_list (rest list_in))(label (is_label first_list)))
			;(print first_list)
			(if (eq rest_list nil) 
				(if	(eq label nil) 
					(new_item  first_list list_out)
					(new_label label list_out)
				)	
				(parse_line2 rest_list (parse_line2 (list first_list) list_out))
			)
		)
		list_out	
	)
)


(defun is_label(str_in)
	(if (> (length str_in) (length dash_label))
		(let* ((pos_start (length dash_label))
				(pos_end (search dash_label str_in :start2 pos_start))
			)
			;(print (length str_in))
			;(print (length dash_label))
			(if (not (eq pos_end nil))
				(str_replace (subseq str_in pos_start pos_end) " " "_")
			;	(subseq str_in pos_start pos_end)
			)
		)
	)
)


