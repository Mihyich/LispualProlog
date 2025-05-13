(load "identification.lisp")

(defun substitute-term (term sub)
"Рекурсивно применить подстановку к терм"
    (format t "~a: ~a~%" term (identify-term term))
    (cond
        (
            (null sub) term
        )
        (
            (variable-term-p term)
            (or 
                (cdr (assoc term sub))
                term
            )
        )
        (
            (compound-term-p term)
            (cons (car term) (mapcar (lambda (x) (substitute-term x sub)) (cdr term)))
        )
        (
            (list-term-p term)
            (mapcar (lambda (x) (substitute-term x sub)) term)
        )
        (
            t term
        )
    )
)

(defun occurs-in (var term sub)
"Проверяет, содержится ли переменная в терме с учётом подстановки"
    (let
        (
            (term (substitute-term term sub))
        )
        (cond
            ((equal var term) t)
            ((compound-term-p term) (some (lambda (x) (occurs-in var x sub)) (cdr term)))
            ((list-term-p term) (some (lambda (x) (occurs-in var x sub)) term))
        )
    )
)

(defun extend-subst (var term sub)
"Добавляет подстановку var → term, проверяя occurs-in"
    (and
        (occurs-in var term sub)
        (cons (cons var term) sub)
    )
)

(defun unify-variable (var term sub)
"Унифицирует переменную с термом"
    (cond 
        ((assoc var sub) (unify (cdr (assoc var sub)) term sub))
        ((and (variable-term-p term)) (assoc term sub)) 
        ((unify var (cdr (assoc term sub))) sub)
        (t (extend-subst var term sub))
    )
)

; (defun unify (term1 term2 &optional (sub nil))
; "Унификация с поддержкой составных термов и списков"
;     (let 
;         (
;             (term1 (substitute-term term1 sub))
;             (term2 (substitute-term term2 sub))
;         )
;         (cond 
;             (
;                 (equal term1 term2) sub
;             )
;             (
;                 (variable-term-p term1)
;                 (unify-variable term1 term2 sub)
;             )
;             (
;                 (variable-term-p term2) 
;                 (unify-variable term2 term1 sub)
;             )
;             (
;                 (and 
;                     (compound-term-p term1) 
;                     (compound-term-p term2)
;                 )
;                 (when 
;                     (eq (car term1) (car term2))
;                     (unify-lists (cdr term1) (cdr term2) sub)
;                 )
;             )
;             (
;                 (and 
;                     (list-term-p term1)
;                     (list-term-p term2)
;                 )
;                 (unify-lists term1 term2 sub)
;             )
;         )
;     )
; )

; (defun unify-lists (list1 list2 sub)
;     (if
;         (or
;             (null list1)
;             (null list2)
;         )
;         (and
;             (null list1)
;             (null list2)
;             sub
;         )
;         (let
;             (
;                 (new-sub (unify (car list1) (car list2) sub))
;             )
;             (and
;                 new-sub
;                 (unify-lists (cdr list1) (cdr list2) new-sub)
;             )
;         )
;     )
; )

(defvar sub '((?X . 42) (?Y . 52)))
(defvar term '(f ?X ?Y (g 1 2 3) (f ?X ?Y (f ?Y ?X) (G R I Z L Y (1 ?X ?Y 4) (5 ?X ?Y 8)))))
(defvar uterm (substitute-term term sub))

(print term)
(print uterm)