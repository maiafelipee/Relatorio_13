(defun le-texto (pergunta)
  (format t "~a" pergunta)
  (finish-output)
  (read-line))

(defun le-numero (pergunta)
 (format t "~a" pergunta)
 (finish-output)
 (read-from-string (read-line)))

(defstruct dominio nome elemento dificuldade mora)

(defun bonus-elemental (elemento)
  (case elemento
    (:electro 1.5)
    (:hydro 1.2)
    (otherwise 1)))

(defun recompensa-final (d)
  (* (dominio-mora d) (bonus-elemental (dominio-elemento d))))

(defun rota (lista dificuldade-maxima)
  (let* ((possiveis
          (remove-if-not
           (lambda (d) (<= (dominio-dificuldade d) dificuldade-maxima))
           lista))
         (textos
          (mapcar (lambda (d)
                    (format nil "~a - Recompensa final: ~,2f mora"
                            (dominio-nome d)
                            (recompensa-final d)))
                  possiveis)))
    textos))

(defun le-elemento ()
 (let ((e (le-texto "Elemento (pyro/hydro/electro/cryo/anemo): ")))
   (intern (string-upcase e) :keyword)))

(defun main ()
  (let ((lista nil))
    (dotimes (i 5)
      (format t "~%Dominio ~a~%" (+ i 1))
      (push (make-dominio
              :nome (le-texto "Nome: ")
              :elemento (le-elemento)
              :dificuldade (le-numero "Dificuldade (1 a 5): ")
              :mora (le-numero "Recompensa base em mora: "))
            lista))

    (setf lista (nreverse lista))

    (let ((maxima (le-numero
                   "~%Dificuldade maxima que consegue enfrentar (1 a 5): ")))
      (format t "~%--- Rota disponivel ---~%")
      (dolist (texto (rota lista maxima))
        (format t "~a~%" texto)))

    (format t "~%Total de mora da lista inteira: ~,2f~%"
            (reduce #'+ (mapcar #'recompensa-final lista)
                    :initial-value 0))

    (let* ((original (car lista))
           (copia (copy-dominio original))
           (nova-mora (le-numero "~%Digite a nova mora para a copia: ")))

      (setf (dominio-mora copia) nova-mora)
      (format t "~%Mora da copia: ~a~%" (dominio-mora copia))
      (format t "Mora do original: ~a~%" (dominio-mora original)))))

(main)
