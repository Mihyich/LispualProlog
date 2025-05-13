(defun constant-term-p (term)
"Символ - константа?"
	(or
		(null term)    ; пустой список - тоже константа
		(numberp term) ; число
		(stringp term) ; строка
		(and
			(symbolp term)               ; символ,
			(not (variable-term-p term)) ; но не переменная
		)
	)
)

(defun variable-term-p (term)
"Символ - переменная?"
	(and 
		(symbolp term)                          ; символ
		(char= (char (symbol-name term) 0) #\?) ; начинается с '?'
	)
)

(defun list-term-p (term)
"Символ - это список (Прологовский, '(a . b) - список, но не для пролога)?"
	(and
		(consp term) ; это список
		(not (symbolp (car term)))  ; критерий - НЕ символ на первой позиции
		(every #'term-p (cdr term)) ; все остальные элементы валидные термы
	)
)

(defun compound-term-p (term)
"Символ - составной терм?"
	(and
		(consp term)                       ; это список
		(atom (car term))                  ; car - атом
		(not (variable-term-p (car term))) ; но не переменная
		(every #'term-p (cdr term))        ; все остальные элементы валидные термы
	)
)

(defun term-p (term)
"Символ - допустимый терм?"
	(or
		(constant-term-p term) ; это константа
		(variable-term-p term) ; это переменная
		(list-term-p term)     ; это список
		(compound-term-p term) ; это составной терм
	)
)

(defun identify-term (term)
"Определить терм"
	(cond
		((constant-term-p term) 'constant)
		((variable-term-p term) 'variable)
		((list-term-p term) 'list)
		((compound-term-p term) 'compound)
	)
)

; (defvar term nil)
; (format t "~a~%" term)
; (format t "~a~%~%~%" (identify-term term))

; (defvar term1 nil)
; (defvar term2 "?GrzilyBear")
; (defvar term3 1)
; (defvar term4 -1)
; (defvar term5 0)
; (defvar term6 1234.1234)
; (defvar term7 -1234.1234)
; (defvar term8 0.0)
; (defvar term9 `a)
; (defvar term10 'abc)
; (defvar term11 '?GrizlyBearGoyda)
; (defvar term12 '())
; (defvar term13 '(a b c))
; (defvar term14 '(1 2 3))
; (defvar term15 '((a) (b) (c)))
; (defvar term16 '((1 2 3) (a b c)))
; (defvar term17 '(grizly (f 2 3 (x 5 3)) goyda))
; (defvar term18 '(?grizly (f 2 3 (g 5 3)) goyda))

; (format t "~a: ~a~%" term1 (identify-term term1))
; (format t "~a: ~a~%" term2 (identify-term term2))
; (format t "~a: ~a~%" term3 (identify-term term3))
; (format t "~a: ~a~%" term4 (identify-term term4))
; (format t "~a: ~a~%" term5 (identify-term term5))
; (format t "~a: ~a~%" term6 (identify-term term6))
; (format t "~a: ~a~%" term7 (identify-term term7))
; (format t "~a: ~a~%" term8 (identify-term term8))
; (format t "~a: ~a~%" term9 (identify-term term9))
; (format t "~a: ~a~%" term10 (identify-term term10))
; (format t "~a: ~a~%" term11 (identify-term term11))
; (format t "~a: ~a~%" term12 (identify-term term12))
; (format t "~a: ~a~%" term13 (identify-term term13))
; (format t "~a: ~a~%" term14 (identify-term term14))
; (format t "~a: ~a~%" term15 (identify-term term15))
; (format t "~a: ~a~%" term16 (identify-term term16))
; (format t "~a: ~a~%" term17 (identify-term term17))
; (format t "~a: ~a~%" term18 (identify-term term18))