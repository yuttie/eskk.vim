(get-buffer-create "*rom-kana-table*")
(with-current-buffer "*rom-kana-table*"
  (delete-region (point-min) (point-max))
  (let ((found-seqs nil))
    (dolist (rule-list (list skk-rom-kana-rule-list skk-rom-kana-base-rule-list))
      (dolist (rule rule-list)
        (let* ((seq (car rule))
               (next (cadr rule))
               (output (caddr rule))
               ;; string representations
               (seq-str
                (cond
                 ((stringp seq)
                  (format (if (string-match-p "'" seq) "\"%s\"" "'%s'") seq))
                 ((eq seq 'skk-kakutei-key)
                  (ignore))
                 ((symbolp seq)
                  (error "Unknown symbol found in seq: %s" (symbol-name seq)))
                 (t (error "Unknown type of seq: %s" seq))))
               (next-str
                (cond
                 ((stringp next)
                  (format (if (string-match-p "'" next) "\"%s\"" "'%s'") next))
                 ((null next)
                  "''")
                 (t (error "Unknown type of next: %s" next))))
               (output-str
                (cond
                 ((stringp output)
                  (format (if (string-match-p "'" output) "\"%s\"" "'%s'") output))
                 ((consp output)
                  (format (if (string-match-p "'" (cdr output)) "\"%s\"" "'%s'") (cdr output)))
                 ((functionp output)
                  (ignore)))))
          (unless (or (functionp output) (member seq found-seqs))
            (push seq found-seqs)
            (insert (format "    \\   %s: [%s, %s],\n" seq-str output-str next-str))))))))
