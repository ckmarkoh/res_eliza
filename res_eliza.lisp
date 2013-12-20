(load "read_file")
(defconstant txt_dir "txt/")
(defconstant txt_rules "ReasoningRules.txt")
;(defconstant txt_rules "SimpleRules.txt")
(defconstant txt_toks "ReasoningToks.txt")
(defconstant txt_proper "ProperNouns.txt")

;	(defconstant txt_rules "rr.txt")
;	(defconstant txt_toks "rs.txt")
;	(defconstant txt_proper "pn.txt")
(defconstant res_pat_symbol 
		'(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z)
)

(defun load_rules()
	(sort	(load_raw_file1 (concatenate 'string txt_dir txt_rules)) #'> :key #'length)
)

(defun load_words()
	(append	(load_raw_file2 (concatenate 'string txt_dir txt_proper))
			(load_raw_file2 (concatenate 'string txt_dir txt_toks))
	)
)


(defun eliza()
	"Respond to user input using pattern matching rules."
	(let  ((all_rules (load_rules)) (all_words (load_words)))
		(loop 
			(print 'eliza>)
			(write (flatten (use_res_rules (read) all_rules all_words))  )
		)
	)
)

(defun use_res_rules(input all_rules all_words)
	(declare (NOTINLINE use_res_rules)) 
	"Find some rule with which to transform the input."
	(some #'(lambda (rule)
			(some #'(lambda (word)
					(let* ((result (pat_match (gen_res_pat word) input))
						  (result_match(lookup '?B result) )
						  (rest_rule (rest rule))
						  (list_word (list word))
						)
						(cond 
							((eq result nil) nil)
							((and (eq result_match nil)(= (length rest_rule) 1)) list_word)
							(t (use_res_rules_aux input rest_rule all_words list_word))
						)
					)
			)(lookup (nth 0 (rest rule)) all_words ))
	) all_rules)
)

(defun use_res_rules_aux(input this_rule all_words out_word)
	(declare (NOTINLINE use_res_rules_aux))
	(if (>= (length out_word) (length this_rule) )
		out_word
		(some #'(lambda (word)
				(let* ((result (pat_match (gen_res_pat word) input))   
					  (result_match(lookup '?A result) )
					  (list_word (append out_word (list word) ))
					  )
					(cond 
						((eq result nil) nil)
						((and (eq result_match nil) (= (length out_word) (- (length this_rule) 1))) 
							list_word
						 )
						(t (use_res_rules_aux result_match this_rule all_words list_word)
						)
					)
				)
		)(lookup (nth (length out_word) this_rule) all_words ))
	)
)

(defun rm_variable_p(lst)
	(mapcar #'(lambda(s)
		(if (not (consp s))
			s
		)	
	)
	lst)	
)



(defun gen_res_pat (input_pat)
	(list '(?* ?X) input_pat '(?* ?A))
)

(defun gen_list(y)
	(list '?* (nth y res_pat_symbol) ) 
)
