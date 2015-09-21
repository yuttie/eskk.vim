(let ((found-seqs nil))
  (dolist (rule-list (list skk-rom-kana-rule-list skk-rom-kana-base-rule-list))
    (dolist (rule rule-list)
      (let* ((seq (car rule))
             (next (cadr rule))
             (output (caddr rule))
             ;; string representations
             (seq-str
              (format (if (string-match-p "'" seq) "\"%s\"" "'%s'") seq))
             (next-str
              (cond
               ((stringp next)
                (format (if (string-match-p "'" next) "\"%s\"" "'%s'") next))
               (t "''")))
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
          (message "    \\   %s: [%s, %s]," seq-str output-str next-str))))))
