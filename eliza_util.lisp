(defconstant fail nil 
	"Indicates pat_match failure"
)

(defconstant no_bindings '((t . t )) 
	"Indicates pat_match success, with no variables."
)

(defun flat2(lst)
	(if (listp lst)
		(mappend #'mklist lst)
		lst
	)
)

(defun cross_product (fn xlist ylist)
	(mappend #'(lambda (y)
				(mapcar #'(lambda (x) (funcall fn  x  y)) xlist)
				)
		ylist
	)
)

(defun rec_cross_product(fn all_list)
	(cond 
		((= (length all_list) 2) 
			(mapcar #'flat2 (cross_product fn (first all_list) (first (last all_list)) ))
		)
		((> (length all_list) 2) 
			(mapcar #'flat2 (cross_product fn (first all_list) (rec_cross_product fn (rest all_list)) ))
		)
		(t nil)
	)
)

(defun list_to_string (lst)
	(string-downcase (princ-to-string lst) )
)

(defun string_to_list (str)
	(if (not (streamp str))
		(string_to_list (make-string-input-stream str))
		(if (listen str)
			(cons (read str) (string_to_list str)) 
			nil
		)
	)
)

(defun remove_str_char(str_to_rm str_a)
		(dolist (char_a str_a)
			(setf str_to_rm (remove char_a str_to_rm))
		)
		str_to_rm
)

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

(defun starts-with (list x)
	"Is x a list whose first element is x?"
	(and (consp list) (eql (first list) x))
)

(defun variable_p (x)
	"Is x a variable? (a symbol beginning with '?')?"
	 (and (symbolp x) (equal (char (symbol-name x ) 0 ) #\?))
)

(defun get_binding (var bindings)
	"Find a (variable. value) pair in a binding list."
	(assoc var bindings)
)

(defun binding_val (bindings)
	"Get the value part of a single binding."
	(cdr bindings)
)

(defun lookup (var bindings)
	"Get the value part (for var) from a binding list."
	(binding_val (get_binding var bindings))
)

(defun extend_bindings(var val bindings)
	"Add a (var . value) pair to a binding list."
	(cons (cons var val) 
		(if (eq bindings no_bindings)
			nil
			bindings)
	)
)

(defun pat_match (pattern input &optional (bindings no_bindings))
	"Match pattern against input in the context of the bindings."
	(cond ((eq bindings fail) fail)
		((variable_p pattern) (match_variable pattern input bindings))
		((eql pattern input) bindings)
		((segment_pattern_p pattern) (segment_match pattern input bindings))
		((and (consp pattern) (consp input)) 
			(pat_match (rest pattern) (rest input)
						(pat_match (first pattern) (first input) bindings)))
		(t fail)
	)	
)

(defun match_variable (var input bindings)
	"Does VAR match input? Uses (or updates) and returns bindings."
	(let ((binding (get_binding var bindings)))
		(cond ((not binding) (extend_bindings var input bindings))
			((equal input (binding_val binding)) bindings)
			(t fail)
		)
	)
)
		

(defun segment_pattern_p (pattern)
	"Is this a segment matching pattern: ((?* var) .pat)"
	(and (consp pattern)
		(starts-with (first pattern) '?*)
	)
)

(defun segment_match (pattern input bindings &optional (start 0))
	"Match the segment pattern ((?* var) .pat) against input."
	(let ((var (second (first pattern))) (pat (rest pattern)))
		(if (null pat)
			(match_variable var input bindings)
			(let ((pos (position (first pat) input :start start :test #'equal)))
				(if (null pos)
					fail
					(let ((b2 (pat_match pat (subseq input pos)
							(match_variable var (subseq input 0 pos) bindings))))
						(if (eq b2 fail)
							(segment_match pattern input bindings (+ pos 1))
							b2
						)
					)
				)
			)
		)
	)
)

(defun rule_pattern (rule) (first rule))
(defun rule_responses (rule) (rest rule))

(defun flatten(lst)
	"Append together elements (or lists) in the list."
	(if (null lst)
		'(Sorry... I don't understand what you are saying. ) ;TODO response to nil result. 
		(mappend #'mklist lst)
	)
)

(defun mklist(x)
	"Return x if it is a list, otherwise (x)."
	(if (listp x)
		x
		(list x)
	)
)
(defun mappend (fn lst)
	"Apply fn to each element of list and append the results."
	(apply #'append (mapcar fn lst)	)
)
	
(defun random_elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices)))
)

(defun rule_pattern (rule) (first rule))
(defun rule_responses (rule) (rest rule))


