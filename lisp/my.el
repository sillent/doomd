;;; lisp/my.el -*- lexical-binding: t; -*-

(defun my/delete-inside-sexp ()
  "Удалить содержимое текущего парного выражения, оставив delimiters.

Point может находиться внутри выражения, на открывающем или
закрывающем delimiter. Если подходящего парного выражения нет,
ничего не делать и вывести сообщение."
  (interactive)
  (condition-case nil
      (let* ((ppss (syntax-ppss))
             (string-start (nth 8 ppss))
             beg end)

        (cond
         ;; Внутри строки: \"foo\" или 'foo', если mode считает это строкой.
         ((nth 3 ppss)
          (setq beg string-start)
          (setq end (scan-sexps beg 1)))

         ;; Point прямо на открывающем delimiter.
         ((eq (char-syntax (or (char-after) 0)) ?\()
          (setq beg (point))
          (setq end (scan-sexps beg 1)))

         ;; Point прямо на закрывающем delimiter.
         ((eq (char-syntax (or (char-after) 0)) ?\))
          (setq end (1+ (point)))
          (setq beg (scan-sexps end -1)))

         ;; Point находится внутри (...) [...] {...} и т.п.
         ((nth 1 ppss)
          (setq beg (nth 1 ppss))
          (setq end (scan-sexps beg 1)))

         (t
          (user-error "Point не находится внутри парного sexp")))

        ;; Удаляем только внутренности, delimiters оставляем.
        (when (and beg end (> (- end beg) 1))
          (delete-region (1+ beg) (1- end))))

    (scan-error
     (message "Не удалось найти парный delimiter"))
    (user-error
     (message "Point не находится внутри парного sexp"))))

(defun my/delete-pair (delimiter &optional around)
  "Удалить содержимое окружающей пары DELIMITER.

Если AROUND nil — оставить delimiters.
Если AROUND non-nil — удалить всё, включая delimiters.

DELIMITER можно указывать любой стороной пары."
  (let* ((pairs '((?\( . ?\))
                  (?\[ . ?\])
                  (?\{ . ?\})
                  (?<  . ?>)
                  (?\" . ?\")
                  (?'  . ?')))
         (pair (or (assq delimiter pairs)
                   (seq-find
                    (lambda (pair)
                      (= (cdr pair) delimiter))
                    pairs))))

    (if (not pair)
        (message "Символ `%c' не является известным delimiter" delimiter)

      (let* ((open (car pair))
             (close (cdr pair))
             (origin (point))
             beg end)

        ;; Если point стоит прямо на closing delimiter,
        ;; считаем его частью окружающей пары.
        (when (eq (char-after origin) close)
          (setq origin (1+ origin)))

        (save-excursion
          (goto-char origin)

          (catch 'found
            ;; Point прямо на opener.
            (when (eq (char-after) open)
              (condition-case nil
                  (let ((candidate-end (scan-sexps (point) 1)))
                    (when (and candidate-end
                               (eq (char-before candidate-end) close))
                      (setq beg (point)
                            end candidate-end)
                      (throw 'found t)))
                (scan-error nil)))

            ;; Ищем подходящий opener слева.
            (while (search-backward (char-to-string open) nil t)
              (let ((candidate-beg (point)))
                (condition-case nil
                    (let ((candidate-end
                           (scan-sexps candidate-beg 1)))
                      (when (and candidate-end
                                 (>= candidate-end origin)
                                 (eq (char-before candidate-end) close))
                        (setq beg candidate-beg
                              end candidate-end)
                        (throw 'found t)))
                  (scan-error nil))))))

        (if (and beg end)
            (if around
                ;; delete around
                (delete-region beg end)

              ;; delete inside
              (delete-region (1+ beg) (1- end)))

          (message "Не найдена окружающая пара `%c%c'"
                   open close))))))


(defun my/delete-inside-pair (delimiter)
  "Удалить содержимое окружающей пары, оставив delimiters."
  (interactive
   (list (read-char "Delete inside pair: ")))
  (my/delete-pair delimiter nil))


(defun my/delete-around-pair (delimiter)
  "Удалить окружающую пару целиком вместе с delimiters."
  (interactive
   (list (read-char "Delete around pair: ")))
  (my/delete-pair delimiter t))

(defun my/wrap-with-pair (delimiter)
  "Обрамить активный region или point парой DELIMITER.

Если region активен — вставить delimiters вокруг выделенного текста.
Если region не активен — вставить пустую пару и оставить point внутри.

Можно передать любую сторону пары:
  ( или )
  [ или ]
  { или }
  < или >
  \" или '
"
  (interactive
   (list (read-char "Wrap with pair: ")))

  (let* ((pairs '((?\( . ?\))
                  (?\[ . ?\])
                  (?\{ . ?\})
                  (?<  . ?>)
                  (?\" . ?\")
                  (?'  . ?')))
         (pair (or (assq delimiter pairs)
                   (seq-find
                    (lambda (pair)
                      (= (cdr pair) delimiter))
                    pairs))))

    (if (not pair)
        (message "Символ `%c' не является известным delimiter" delimiter)

      (let ((open (car pair))
            (close (cdr pair)))

        (if (use-region-p)
            ;; Оборачиваем region.
            (let ((beg (region-beginning))
                  (end (region-end)))

              ;; Сначала вставляем справа, чтобы позиция beg не сдвинулась.
              (save-excursion
                (goto-char end)
                (insert close))

              (save-excursion
                (goto-char beg)
                (insert open))

              ;; Оставляем region внутри новой пары.
              (goto-char (1+ beg))
              (set-mark (1+ end))
              (activate-mark))

          ;; Region нет — вставляем пару в point.
          (insert open close)

          ;; Возвращаем point между delimiters.
          (backward-char 1))))))

(defun my/directory-files-recursively (directory regexp &optional follow-symlinks)
  "Рекурсивно найти файлы в DIRECTORY с помощью `fd'.

REGEXP интерпретируется командой `fd'.
Возвращает список абсолютных путей.

Если FOLLOW-SYMLINKS non-nil, следовать по символическим ссылкам."
  (let ((fd (executable-find "fd")))
    (unless fd
      (user-error "Команда `fd' не найдена в PATH"))

    (let ((directory (expand-file-name directory))
          args)

      (unless (file-directory-p directory)
        (user-error "Директория не существует: %s" directory))

      (setq args
            (append
             (list
              "--type" "f"
              "--absolute-path"
              "--hidden"
              "--no-ignore"
              "--color" "never")

             (when follow-symlinks
               (list "--follow"))

             (list regexp directory)))

      (with-temp-buffer
        (let ((exit-code
               (apply #'process-file
                      fd
                      nil
                      t
                      nil
                      args)))
          (unless (zerop exit-code)
            (error
             "fd завершился с кодом %s: %s"
             exit-code
             (string-trim (buffer-string))))

          (split-string
           (buffer-string)
           "\n"
           t))))))
