(defvar *db* nil "База данных правил Prolog")

;; Структура для представления правил
(defstruct rule head body)

;; Добавление фактов и правил в базу данных
(defun <- (head &rest body)
  (push (make-rule :head head :body body) *db*)
  head)

;; Унификация
(defun unify (x y &optional bindings)
  (cond ((eql x y) bindings)
        ((variable-p x) (unify-variable x y bindings))
        ((variable-p y) (unify-variable y x bindings))
        ((and (consp x) (consp y))
         (unify (rest x) (rest y) (unify (first x) (first y) bindings)))
        (t nil)))

(defun unify-variable (var x bindings)
  (let ((binding (assoc var bindings)))
    (cond (binding (unify (cdr binding) x bindings))
          ((occurs-check var x bindings) nil)
          (t (cons (cons var x) bindings)))))

(defun occurs-check (var x bindings)
  (cond ((variable-p x) (or (eql var x)
                            (let ((binding (assoc x bindings)))
                              (and binding (occurs-check var (cdr binding) bindings)))))
        ((consp x) (or (occurs-check var (first x) bindings)
                       (occurs-check var (rest x) bindings)))
        (t nil)))

(defun variable-p (x)
  (and (symbolp x) (char= (char (symbol-name x) 0) #\?)))

;; Интерпретатор Prolog
(defun prove (goal &optional bindings)
  (dolist (rule *db*)
    (let ((new-bindings (unify goal (rule-head rule) bindings)))
      (when new-bindings
        (if (null (rule-body rule))
            (return-from prove new-bindings)
            (progn
              (setq bindings new-bindings)
              (if (prove-all (rule-body rule) bindings)
                  (return-from prove bindings))))))
  nil)

(defun prove-all (goals &optional bindings)
  (cond ((null goals) bindings)
        ((eq (car goals) '!) (prove-all (cdr goals) bindings)) ; обработка cut
        (t (let ((new-bindings (prove (subst-bindings bindings (car goals)) bindings)))
             (when new-bindings
               (prove-all (cdr goals) new-bindings))))))

(defun subst-bindings (bindings x)
  (if (atom x)
      (let ((binding (assoc x bindings)))
        (if binding (subst-bindings bindings (cdr binding)) x))
      (cons (subst-bindings bindings (car x))
            (subst-bindings bindings (cdr x)))))

;; Интерфейс для запросов
(defmacro ?- (&rest goals)
  `(query ',goals))

(defun query (goals)
  (let ((bindings (prove-all goals)))
    (if bindings
        (print-bindings bindings)
        (format t "No.~%"))))

(defun print-bindings (bindings)
  (if (null bindings)
      (format t "Yes.~%")
      (dolist (binding bindings)
        (format t "~a = ~a~%" (car binding) (cdr binding)))))

;; Пример использования
(defun test-prolog ()
  ;; Очистка базы данных
  (setq *db* nil)
  
  ;; Определение некоторых фактов и правил
  (<- 'father 'john 'bob)
  (<- 'father 'bob 'mike)
  (<- 'mother 'mary 'bob)
  (<- 'male 'john)
  (<- 'male 'bob)
  (<- 'male 'mike)
  (<- 'female 'mary)
  
  ;; Определение правила ancestor
  (<- 'ancestor '?x '?y) 'father '?x '?y)
  (<- 'ancestor '?x '?y) 'father '?x '?z) 'ancestor '?z '?y)
  
  ;; Выполнение запросов
  (format t "~%Testing father relations:~%")
  (?- (father john bob))
  (?- (father bob mike))
  (?- (father john mike))
  
  (format t "~%Testing ancestor relations:~%")
  (?- (ancestor john bob))
  (?- (ancestor john mike))
  (?- (ancestor mike john)))
