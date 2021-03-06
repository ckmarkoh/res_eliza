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

(defun load_raw_file2(filename)
	(parse_line2 (read_file filename))
)


(defun load_raw_file1(filename)
	(parse_line1 (read_file filename))
)

(defun read_file(filename)
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

(defun parse_line1(list_in  &optional(list_out '((0))) )
   (declare (NOTINLINE parse_line1)) ; and the compiler can't ignore NOTINLINE
	(if (not (null list_in))
		(let* ((first_list (first list_in ))(rest_list (rest list_in))(label (is_label1 first_list)))
			(if (eq rest_list nil) 
				(if	(eq label nil) 
					(new_item  (remove_str_char first_list '(#\[ #\])) list_out)
					(new_label (write-to-string (length list_out)) list_out)
				)	
				(parse_line1 rest_list (parse_line1 (list first_list) list_out))
			)
		)
		list_out	
	)
)

(defun is_label1(str_in)
	 (not (eq (search dash_label str_in ) nil) )
)

(defun parse_line2(list_in  &optional(list_out '()) )
   (declare (NOTINLINE parse_line2)) ; and the compiler can't ignore NOTINLINE
	(if (not (null list_in))
		(let* ((first_list (first list_in ))(rest_list (rest list_in))(label (is_label2 first_list)))
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


(defun is_label2(str_in)
	(if (> (length str_in) (length dash_label))
		(let* ((pos_start (length dash_label))
				(pos_end (search dash_label str_in :start2 pos_start))
			)
			(if (not (eq pos_end nil))
				(str_replace (subseq str_in pos_start pos_end) " " "_")
			)
		)
	)
)


