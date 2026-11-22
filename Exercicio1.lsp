(defun le-texto (pergunta)
  (format t "~a" pergunta)
  (finish-output)
  (read-line))

(defun le-numero (pergunta)
 (format t "~a" pergunta)
 (finish-output)
 (read-from-string (read-line)))

(defstruct erva nome preco-por-grama toxicidade)

(defun classifica-erva (erva)
  (cond ((>= (erva-toxicidade erva) 7) "veneno")
       ((>= (erva-toxicidade erva) 4) "use com cuidado")
        (t "segura")))

(defun preco-da-dose (erva gramas)
  (* (erva-preco-por-grama erva) gramas))

(defun descreve-erva (erva)
  (format nil "~a - R$~,2f por grama - ~a"
          (erva-nome erva)
          (erva-preco-por-grama erva)
          (classifica-erva erva)))

(defun aviso (erva)
  (if (>= (erva-toxicidade erva) 7)
      (format nil "Maomao vai testar ~a em si mesma antes."
              (erva-nome erva))
    (format nil "~a liberada para o paciente."
            (erva-nome erva))))

(defun main ()
 (let ((ervas nil))
    (dotimes (i 3)
      (format t "~%Erva ~a~%" (+ i 1))
      (push (make-erva
              :nome (le-texto "Nome: ")
              :preco-por-grama (le-numero "Preco por grama: ")
              :toxicidade (le-numero "Toxicidade (0 a 10): "))
            ervas))

    (setf ervas (nreverse ervas))

    (dolist (erva ervas)
       (format t "~%~a~%" (descreve-erva erva))
       (format t "~a~%" (aviso erva)))

    (let ((gramas (le-numero
                   "~%Quantidade de gramas para calcular a dose: ")))
      (format t "~%Preco da dose: R$~,2f~%"
              (preco-da-dose (car ervas) gramas)))))

(main)
