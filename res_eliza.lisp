(load "read_file")
(defconstant txt_dir "txt/")
;	(defconstant txt_rules "ReasoningRules.txt")
;	(defconstant txt_toks "ReasoningToks.txt")
;	(defconstant txt_proper "ProperNouns.txt")

(defconstant txt_rules "rr.txt")
(defconstant txt_toks "rs.txt")
(defconstant txt_proper "pn.txt")

(defun load_rules()
	(load_raw_file1 (concatenate 'string txt_dir txt_rules))
)
(defun load_words()
	(append	(load_raw_file2 (concatenate 'string txt_dir txt_proper))
			(load_raw_file2 (concatenate 'string txt_dir txt_toks))
	)
)
(defun res_eliza() ;TODO
	(let  ((all_rules (load_rules)) (all_words (load_words)))
		(print all_rules)
		(print all_words)
	
	)
)
;
;	(defun gen_rules (all_rules all_words) ;TODO
;		(mappend #'(lambda(rules)(
;		
;		
;						)
;					) 
;			all_rules
;		)  
;	)
;	(defun test_cross_product()
;		(rec_cross_product #'list '((1 2 3) (a b c ) (x y z) (8 9)))
;	)
