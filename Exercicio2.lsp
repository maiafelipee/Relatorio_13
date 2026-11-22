(defun le-texto (pergunta)
  (format t "~a" pergunta)
  (finish-output)
  (read-line))

(defun le-numero (pergunta)
 (format t "~a" pergunta)
 (finish-output)
 (read-from-string (read-line)))

(defstruct criatura nome bioma profundidade perigo)

(defun texto-do-perigo (perigo)
  (case perigo
    (:inofensiva "Inofensiva")
    (:agressiva "Agressiva")
    (:leviata "Leviata: mantenha distancia")
    (otherwise "Desconhecido")))

(defun descreve-criatura (c)
  (format nil "~a | Bioma: ~a | Profundidade: ~a m | Perigo: ~a"
          (criatura-nome c)
          (criatura-bioma c)
          (criatura-profundidade c)
          (texto-do-perigo (criatura-perigo c))))

(defun abaixo-de (metros catalogo)
  (remove-if-not
   (lambda (c) (> (criatura-profundidade c) metros))
   catalogo))

(defparameter *catalogo* nil)

(defun le-perigo ()
 (let ((p (le-texto "Perigo (inofensiva/agressiva/leviata): ")))
    (cond ((string-equal p "inofensiva") :inofensiva)
          ((string-equal p "agressiva") :agressiva)
          ((string-equal p "leviata") :leviata)
          (t :desconhecido))))

(defun main ()
  (let ((catalogo nil))
    (dotimes (i 4)
      (format t "~%Criatura ~a~%" (+ i 1))
      (push (make-criatura
              :nome (le-texto "Nome: ")
              :bioma (le-texto "Bioma: ")
              :profundidade (le-numero "Profundidade em metros: ")
              :perigo (le-perigo))
            catalogo))

    (setf catalogo (nreverse catalogo))
    (setf *catalogo* catalogo)

    (format t "~%--- Catalogo completo ---~%")
    (dolist (texto (mapcar #'descreve-criatura *catalogo*))
       (format t "~a~%" texto))

    (let* ((limite (le-numero "~%Profundidade minima para pesquisa: "))
           (filtradas (abaixo-de limite *catalogo*)))

      (format t "~%--- Criaturas mais profundas ---~%")
      (dolist (c filtradas)
        (format t "~a~%" (descreve-criatura c)))

      (format t "~%Quantidade encontrada: ~a~%"
              (length filtradas)))))

(main)
