;;; init-macro.el --- Summary macro file -*- lexical-binding: t -*-

;;; Commentary:
;; macro

;;; Code:

;; M-x kmacro-start-macro-or-insert-counter (f3)
;; M-x kmacro-end-or-call-macro (f4)
;; M-x name-last-kbd-marco
;; M-x insert-kbd-marco

(fset 'gb2312-buffer
      (kmacro-lambda-form [?\M-x ?r ?e ?v ?e ?r ?t ?- ?b ?u ?f ?f ?e ?r ?- ?w ?i ?t ?h ?- ?c ?o ?d ?i ?n ?g ?- ?s ?y ?s ?t ?e tab return ?g ?b ?2 ?3 ?1 ?2 return ?y ?\M-x ?s ?e ?t ?- ?b ?u ?f ?f ?e ?r ?- ?f ?i ?l ?e ?- ?c ?o ?d ?i ?n ?g return ?g ?b ?2 ?3 ?1 ?2 return] 0 "%d"))

;; copy from https://emacs.stackexchange.com/questions/54500/how-to-add-a-locally-override-the-message-function
(defmacro with-temp-advice (fn-orig where fn-advice &rest body)
  "Execute BODY with advice temporarily enabled."
  `
  (let ((fn-advice-var ,fn-advice))
    (unwind-protect
      (progn
        (advice-add ,fn-orig ,where fn-advice-var)
        ,@body)
      (advice-remove ,fn-orig fn-advice-var))))

(defmacro with-message-suffix (suffix &rest body)
  "Add text after the message output.
Argument SUFFIX is the text to add at the start of the message.
Optional argument BODY runs with the message suffix."
  (declare (indent 1))
  `
  (with-temp-advice
    'message
    :around
    (lambda (fn-orig arg &rest args)
      (apply fn-orig (append (list (concat arg "%s")) args (list ,suffix))))
    ,@body))

(provide 'init-macro)
;;; init-macro.el ends here
