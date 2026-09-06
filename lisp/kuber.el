(require 'cl-lib)
(require 'subr-x)

(defvar my/k8s-kube-directory
  (expand-file-name "~/.kube/")
  "Корневая директория с kubeconfig-файлами.")


(defun my/k8s-config-current-context (file)
  "Прочитать `current-context' напрямую из kubeconfig FILE."
  (when (file-regular-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))

      (when (re-search-forward
             "^current-context:[ \t]*\\(.+?\\)[ \t]*$"
             nil
             t)
        (string-trim
         (match-string-no-properties 1)
         "[ \t\"']+"
         "[ \t\"']+")))))


(defun my/k8s-kubeconfig-p (file)
  "Вернуть non-nil, если FILE похож на Kubernetes kubeconfig."
  (when (file-regular-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))

      (and
       (re-search-forward "^apiVersion:[ \t]*v1[ \t]*$" nil t)

       (progn
         (goto-char (point-min))
         (re-search-forward "^clusters:[ \t]*$" nil t))

       (progn
         (goto-char (point-min))
         (re-search-forward "^contexts:[ \t]*$" nil t))

       (progn
         (goto-char (point-min))
         (re-search-forward "^current-context:" nil t))))))


(defun my/k8s-find-kubeconfigs ()
  "Найти все *.yaml и *.yml kubeconfig внутри `my/k8s-kube-directory'.

Возвращает список:

  ((CONTEXT . FILE)
   ...)"
  (let ((files
         (my/directory-files-recursively
          my/k8s-kube-directory
          "\\.ya?ml$"))
        result)

    (dolist (file files)
      (when (my/k8s-kubeconfig-p file)
        (when-let ((context
                    (my/k8s-config-current-context file)))
          (push
           (cons context file)
           result))))

    (sort
     result
     (lambda (a b)
       (string-lessp
        (car a)
        (car b))))))


(defun my/k8s-production-p (kubeconfig context)
  "Вернуть non-nil, если KUBECONFIG или CONTEXT похож на production."
  (let ((case-fold-search t))
    (or
     (and context
          (string-match-p
           "\\b\\(prod\\|production\\)\\b"
           context))

     (string-match-p
      "\\b\\(prod\\|production\\)\\b"
      kubeconfig))))


(defun my/k8s-update-header ()
  "Обновить Kubernetes header-line текущего Eshell."
  (when (bound-and-true-p my/k8s-kubeconfig)
    (let* ((kubeconfig my/k8s-kubeconfig)

           (context
            (or
             (my/k8s-config-current-context kubeconfig)
             "<NO CONTEXT>"))

           (production
            (my/k8s-production-p
             kubeconfig
             context))

           (config-relative-name
            (file-relative-name
             kubeconfig
             my/k8s-kube-directory)))

      (setq-local
       header-line-format
       (list
        " "
        (propertize
         (if production

             (format
              " !!! PRODUCTION !!!   ☸ %s   │   %s "
              context
              config-relative-name)

           (format
            " ☸ %s   │   %s "
            context
            config-relative-name))

         'face

         (if production

             '(:foreground "white"
               :background "red3"
               :weight ultra-bold
               :height 1.5
               :box (:line-width 3
                     :color "red"))

           '(:weight bold
             :height 1.25
             :inherit font-lock-keyword-face)))))

      (force-mode-line-update t))))


(defun my/k8s-open-eshell (kubeconfig)
  "Открыть отдельный Eshell для KUBECONFIG."
  (let* ((kubeconfig
          (expand-file-name kubeconfig))

         (context
          (or
           (my/k8s-config-current-context kubeconfig)
           "unknown"))

         (buffer-name
          (format "*k8s:%s*" context)))

    ;; Создаём новый независимый eshell.
    (eshell t)

    ;; Например:
    ;;
    ;; *k8s:production-eu*
    ;;
    (rename-buffer buffer-name t)

    ;; Делаем environment локальным для этого буфера.
    ;; Благодаря этому KUBECONFIG не попадёт в другие eshell.
    (setq-local
     process-environment
     (copy-sequence process-environment))

    (setenv
     "KUBECONFIG"
     kubeconfig
     t)

    ;; Запоминаем kubeconfig в buffer-local variable.
    (setq-local
     my/k8s-kubeconfig
     kubeconfig)

    ;; После каждой команды перечитываем current-context.
    ;; Это позволяет обновить шапку после:
    ;;
    ;; kubectl config use-context ...
    ;;
    (add-hook
     'eshell-post-command-hook
     #'my/k8s-update-header
     nil
     t)

    ;; Рисуем шапку сразу.
    (my/k8s-update-header)))


(defun my/k8s-eshell ()
  "Выбрать найденный kubeconfig и открыть для него Eshell."
  (interactive)

  (unless (file-directory-p my/k8s-kube-directory)
    (user-error
     "Директория %s не существует"
     my/k8s-kube-directory))

  (let ((configs
         (my/k8s-find-kubeconfigs)))

    (unless configs
      (user-error
       "Kubeconfig-файлы в %s не найдены"
       my/k8s-kube-directory))

    (let* ((choice
            (completing-read
             "Kubernetes context: "
             configs
             nil
             t))

           (kubeconfig
            (cdr
             (assoc choice configs))))

      (my/k8s-open-eshell
       kubeconfig))))


(map! "C-c k" #'my/k8s-eshell)
