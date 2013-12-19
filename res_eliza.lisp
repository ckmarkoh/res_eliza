(load "read_file")
(defconstant txt_dir "txt/")
;	(defconstant txt_rules "ReasoningRules.txt")
;	(defconstant txt_toks "ReasoningToks.txt")
;	(defconstant txt_proper "ProperNouns.txt")

(defconstant txt_rules "rr.txt")
(defconstant txt_toks "rs.txt")
(defconstant txt_proper "pn.txt")
(defconstant res_pat_symbol 
		'(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z)
)

(defun load_rules()
	(load_raw_file1 (concatenate 'string txt_dir txt_rules))
)
(defun load_words()
	(append	(load_raw_file2 (concatenate 'string txt_dir txt_proper))
			(load_raw_file2 (concatenate 'string txt_dir txt_toks))
	)
)


(defun eliza()
	"Respond to user input using pattern matching rules."
	(let ((res_rules (output_res_rules)))
		(loop 
			(print 'eliza>)
			(write  (flatten (use_res_rules (read) res_rules))  )
		)
	)
)

(defun use_res_rules(input res_rules)
	"Find some rule with which to transform the input."
	(some #'(lambda (rule)
				(let ((result (pat_match rule input)))
					(if (not (eq result nil))
							(rm_variable_p	rule)
					)
				)
	) res_rules)
)

(defun rm_variable_p(lst)
	(mapcar #'(lambda(s)
		(if (not (consp s))
			s
		)	
	)
	lst)	
)



(defun output_res_rules() ;TODO
	(let  ((all_rules (load_rules)) (all_words (load_words)))
		(gen_res_pat	(gen_rules all_rules all_words))
	)
)

(defun gen_rules (all_rules all_words) ;TODO
	(mappend #'(lambda(rules)
					(rec_cross_product #'list 
						(mapcar #'(lambda(rule)
									(lookup rule all_words)	
								)	
						(rest rules)
						)
					)
				) 
		all_rules)  
)
(defun gen_res_pat (all_res_pats)
	(mapcar #'(lambda(res_pats)
		(flat2	(loop for res_pat in res_pats 
				for y from 0
				collect ( if (<  y (- (length res_pats) 1) )
							(list (gen_list y) res_pat )
							(list (gen_list y) res_pat (gen_list (+ y 1)) )
						) 
			))
;			(mappend #'(lambda(res_pat)
;								
;						)	
;			res_pats)
			)
	all_res_pats)
)

(defun gen_list(y)
	(list '?* (nth y res_pat_symbol) ) 
)
;	(defun test_cross_product()
;		(rec_cross_product #'list '((1 2 3) (a b c ) (x y z) (8 9)))
;	)
