(nconc auto-mode-alist
       '(("\\.[hg]s$"  . haskell-mode)
         ("\\.hi$"     . haskell-mode)
		 ("\\.hs-boot$" . haskell-mode)
         ("\\.hsc$"     . haskell-mode)
		 ("\\.nhs$" . haskell-mode)
         ("\\.npl$" . haskell-mode)
         ("\\.l[hg]s$" . literate-haskell-mode)))
(autoload 'haskell-mode "haskell-mode"
   "Major mode for editing Haskell scripts." t)
(autoload 'literate-haskell-mode "haskell-mode"
   "Major mode for editing literate Haskell scripts." t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-font-lock)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-hugs)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
(global-set-key [(control meta down-mouse-3)] 'imenu)

;(set-default-font "-apple-dejavu sans mono-medium-r-normal--0-0-0-0-m-0-mac-roman")
(setq mac-allow-anti-aliasing t)

;(setq haskell-font-lock-symbols 'unicode)
  (defun unicode-symbol (name)
    "Translate a symbolic name for a Unicode character -- e.g., LEFT-ARROW
  or GREATER-THAN into an actual Unicode character code. "
    (decode-char 'ucs (case name
                        ;; arrows
                        ('left-arrow 8592)
                        ('up-arrow 8593)
                        ('right-arrow 8594)
                        ('down-arrow 8595)

                        ;; boxes
                        ('double-vertical-bar #X2551)
						('proportion 8759)
						('compose 9675)
						;('compose #X2218)
						('forall 8704)
						('multiply #X2A09)
						('integral #X2A0D)
						('function #X192)
						('sum #X2211)
						;('divide

                        ;; relational operators
                        ('equal #X003d)
                        ('not-equal #X2260)
                        ('identical #X2261)
                        ('not-identical #X2262)
                        ('less-than #X003c)
                        ('greater-than #X003e)
                        ('less-than-or-equal-to #X2264)
                        ('greater-than-or-equal-to #X2265)

                        ;; logical operators
                        ('logical-and #X2227)
                        ('logical-or #X2228)
                        ('logical-neg #X00AC)

                        ;; misc
                        ('nil #X2205)
                        ('horizontal-ellipsis #X2026)
                        ('double-exclamation #X203C)
                        ('prime #X2032)
                        ('double-prime #X2033)
                        ('for-all #X2200)
                        ('there-exists #X2203)
                        ('element-of #X2208)

                        ;; mathematical operators
                        ('square-root #X221A)
                        ('squared #X00B2)
                        ('cubed #X00B3)
						('elem #X2208)
						('notElem #X2209)
						('increment #X2206)
						('nabla #X2207)
						('angle #X2220)
						('measured-angle #X2221)

						('union #X2A42)
						('intersection #X2A43)
						('sine-wave #X223F)
						('subset #X2282)
						('superset #X2283)
						('notSubset #X2284)
						('notSuperset #X2285)
						('then #X21D2)

						('circle-plus #X2295)
						('circle-minus #X2296)
						('circle-times #X2297)
						('circle-division #X2298)
						('circle-dot #X2299)
						('circle-ring #X229A)
						('circle-asterisk #X229B)
						('circle-equals #X229C)

                        ;; letters
                        ('lambda #X03BB)
                        ('alpha #X03B1)
                        ('beta #X03B2)
                        ('gamma #X03B3)
                        ('delta #X03B4))))

  (defun substitute-pattern-with-unicode (pattern symbol)
    "Add a font lock hook to replace the matched part of PATTERN with the
  Unicode symbol SYMBOL looked up with UNICODE-SYMBOL."
    (interactive)
    (font-lock-add-keywords
     nil `((,pattern (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                              ,(unicode-symbol symbol))
                              nil))))))

  (defun substitute-patterns-with-unicode (patterns)
    "Call SUBSTITUTE-PATTERN-WITH-UNICODE repeatedly."
    (mapcar #'(lambda (x)
                (substitute-pattern-with-unicode (car x)
                                                 (cdr x)))
            patterns))

  (defun haskell-unicode ()
    (interactive)
    (substitute-patterns-with-unicode
     (list ;(cons "\\(<->\\)" 'circle-minus)
		   ;(cons "\\(<\\*>\\)" 'circle-times)
		   ;(cons "\\(<\\+>\\)" 'circle-plus)
		   ;(cons "\\(</>\\)" 'circle-division)
		   ;(cons "\\(<\\.>\\)" 'circle-dot)
		   ;(cons "\\(<=>\\)" 'circle-equals)

		   ;(cons "\\(<-\\)" 'left-arrow)
           ;(cons "\\(->\\)" 'right-arrow)
           ;(cons "\\(==\\)" 'identical)
           ;(cons "\\(/=\\)" 'not-identical)
	  ;(cons "\\(()\\)" 'nil)
           (cons "\\<\\(sqrt\\)\\>" 'square-root)
           ;(cons "\\(&&\\)" 'logical-and)
           ;(cons "\\(||\\)" 'logical-or)
		   ;(cons "\\(::\\)" 'proportion)
           ;(cons "\\<\\(not\\)\\>" 'logical-neg)
           ;(cons "\\(>\\)\\[^=\\]" 'greater-than)
           ;(cons "\\(<\\)\\[^=\\]" 'less-than)
           ;(cons "\\(>=\\)" 'greater-than-or-equal-to)
           ;(cons "\\(<=\\)" 'less-than-or-equal-to)
    ;       (cons "\\<\\(alpha\\)\\>" 'alpha)
    ;       (cons "\\<\\(beta\\)\\>" 'beta)
   ;        (cons "\\<\\(gamma\\)\\>" 'gamma)
   ;        (cons "\\<\\(delta\\)\\>" 'increment)  ; 'delta
;		   (cons "\\<\\(nabla\\)\\>" 'nabla)
		   ;(cons "\\<\\(elem\\)\\>" 'elem)
		   ;(cons "\\<\\(notElem\\)\\>" 'notElem)
  ;         (cons "\\(''\\)" 'double-prime)
   ;        (cons "\\('\\)" 'prime)
		   ;(cons "\\<\\(forall\\)\\>" 'forall)
		   (cons "\\(\\\\\\)" 'lambda)
           ;(cons "\\(!!\\)" 'double-exclamation)
		   ;(cons "\\(\\*\\)" 'multiply)
		   (cons "\\<\\(f\\)\\>" 'function)
		   ;(cons "\\(=>\\)" 'then)
		   ;;(cons "\\(/\\)" 'divide)
		   (cons "\\<\\(union\\)\\>" 'union)
		   (cons "\\<\\(intersection\\)\\>" 'intersection)
		   ;(cons "\\<\\(angle\\)\\>" 'angle)
		   ;(cons "\\<\\(mangle\\)\\>" 'measured-angle)
		   ;(cons "\\<\\(sin\\)\\>" 'sine-wave)
		   ;(cons "\\<\\(sum\\)\\>" 'sum)
           ;(cons "\\(\\.\\.\\)" 'horizontal-ellipsis)
		   ;(cons "\\(\\.\\)" 'compose)
		   )))

  (add-hook 'haskell-mode-hook 'haskell-unicode)

 (defun pretty-greek ()
  (let ((greek '("alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta" "theta" "iota" "kappa" "lambda" "mu" "nu" "xi" "omicron" "pi" "rho" "sigma_final" "sigma" "tau" "upsilon" "phi" "chi" "psi" "omega")))
  ;(let ((greek '("a" "b" "g" "d" "e" "j" "h" "c" "i" "k" "l" "m" "n" "xi" "o" "p" "r" "z" "s" "t" "u" "v" "x" "y" "w")))
    (loop for word in greek
          for code = 97 then (+ 1 code)
          do  (let ((greek-char (make-char 'greek-iso8859-7 code)))
                (font-lock-add-keywords nil
                                        `((,(concatenate 'string "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[a-zA-Z]")
                                           (0 (progn (decompose-region (match-beginning 2) (match-end 2))
                                                     nil)))))
                (font-lock-add-keywords nil
                                        `((,(concatenate 'string "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[^a-zA-Z]")
                                           (0 (progn (compose-region (match-beginning 2) (match-end 2)
                                                                     ,greek-char)
                                                     nil)))))))))
  ;(add-hook 'haskell-mode-hook 'pretty-greek)



(defun my-haskell-mode-hook () "Local defaults for Haskell mode"
  ;(setq sml-indent-level 2) ; conserve on horizontal space
  ;(setq words-include-escape t) ; \ loses word break status
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  ;(setq indent-tabs-mode t)) ; always indent with tabs
  )
(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

(remove-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; Just use tab-stop indentation, 2-space tabs

(add-hook 'haskell-mode-hook
          (lambda ()
            (turn-on-haskell-doc-mode)
            (turn-on-haskell-simple-indent)
            (setq indent-line-function 'tab-to-tab-stop)
            (setq tab-stop-list
                  (loop for i from 4 upto 120 by 4 collect i))
            (local-set-key (kbd "RET") 'newline-and-indent-relative)))

(defun newline-and-indent-relative ()
  (interactive)
  (newline)
  (indent-to-column (save-excursion
                      (forward-line -1)
                      (back-to-indentation)
                      (current-column))))

(setq-default indent-tabs-mode nil)
(global-set-key (kbd "C-x a r") 'align-regexp)

(global-set-key (kbd "C-c c") 'recompile)