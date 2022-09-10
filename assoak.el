
(provide 'assoak)

(defun assoak-plist-to-alist (pl)
  "Converts property list PL to an association list."
  (if (null pl)
      nil
    (let ((results nil)
	  (cursor pl))
      (while (not (null cursor))
	(setq results (cons (cons (car cursor) (cadr cursor))
			    results))
	(setq cursor (cddr cursor)))
      (nreverse results))))

(defun assoak-alist-to-plist (ass)
  "Converts the association list ASS to a property list."
  (if (null ass)
      nil
    (let ((results nil)
	  (cursor ass))
      (while (not (null cursor))
	(setq results (cons (cdr (car cursor))
			    (cons (car (car cursor))
				  results)))
	(setq cursor (cdr cursor)))
      (nreverse results))))

(defun assoak-shallow-merge-alists (a1 a2)
  "Shallow merges 2 association lists (A1 & A2).
   Any keys shared between both result in A2's value
   overwriting A1's value."
  (let ((results nil)
	(cursor1 a1)
	(cursor2 a2))
    (while (not (null cursor1))
      (let* ((pair1 (car cursor1))
	     (pair2 (assoc (car pair1) cursor2)))
	(setq results (cons (cons (car pair1)
				  (if pair2 (cdr pair2) (cdr pair1)))
			    results))
	(setq cursor1 (cdr cursor1))
	(when pair2 (assoc-delete-all (car pair1) cursor2))))
    (if (null cursor2)
	(nreverse results)
      (append (nreverse results) cursor2))))

(defun assoak-merge-plists (p1 p2)
  "Merges 2 property lists (P1 & P2).
   Any shared keys are overwritten by P2's value."
  (assoak-alist-to-plist
   (assoak-shallow-merge-alists (assoak-plist-to-alist p1)
				(assoak-plist-to-alist p2))))
