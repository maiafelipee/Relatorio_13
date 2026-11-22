(defun le-texto (pergunta)
  (format t "~a" pergunta)
  (finish-output)
  (read-line))

(defun le-numero (pergunta)
 (format t "~a" pergunta)
 (finish-output)
 (read-from-string (read-line)))

(defstruct garota-magica nome escuridao bruxas)

(defun escuridao-total (grupo)
  (if (null grupo)
      0
      (+ (garota-magica-escuridao (car grupo))
         (escuridao-total (cdr grupo)))))

(defun total-de-bruxas (grupo)
  (reduce #'+ (mapcar #'garota-magica-bruxas grupo)
          :initial-value 0))

(defun purifica (g quantidade)
  (make-garota-magica
   :nome (garota-magica-nome g)
   :escuridao (if (>= quantidade (garota-magica-escuridao g))
                  0
                  (- (garota-magica-escuridao g) quantidade))
   :bruxas (garota-magica-bruxas g)))

(defun le-garota (numero)
  (format t "~%Garota magica ~a~%" numero)
  (make-garota-magica
    :nome (le-texto "Nome: ")
    :escuridao (le-numero "Escuridao da joia (0 a 100): ")
    :bruxas (le-numero "Quantidade de bruxas derrotadas: ")))

(defun main ()
  (let* ((grupo (list (le-garota 1)
                      (le-garota 2)
                      (le-garota 3)))
         (nova (le-garota "nova"))
         (grupo-com-nova (cons nova grupo))
         (original (car grupo))
         (quantidade (le-numero "~%Quanto deseja purificar da primeira garota? "))
         (purificada (purifica original quantidade)))

    (format t "~%Escuridao total do grupo original: ~a~%"
            (escuridao-total grupo))
    (format t "Total de bruxas derrotadas: ~a~%"
            (total-de-bruxas grupo))

    (format t "Escuridao total com a nova garota: ~a~%"
            (escuridao-total grupo-com-nova))

    (format t "~%Primeira garota: ~a~%"
            (garota-magica-nome original))
    (format t "Escuridao original: ~a~%"
            (garota-magica-escuridao original))
    (format t "Escuridao depois da purificacao: ~a~%"
            (garota-magica-escuridao purificada))))

(main)
