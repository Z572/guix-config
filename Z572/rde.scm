;; -*- mode: scheme -*-
(add-to-load-path (format #f "~a/.config/guix/config/" (getenv "HOME")))
(use-modules (guix) (gnu)
             (srfi srfi-26)
             (ice-9 format)
             (guix transformations))
(use-package-modules linux emacs-xyz)
(use-system-modules keyboard)
(use-modules (Z572)
             (gnu home)
             ((guix licenses) #:prefix license:)
             (guix git-download)
             (guix build-system emacs)
             (gnu home-services)
             (gnu home-services gnupg)
             (gnu home-services state)
             (gnu home-services mcron)
             (gnu home-services password-utils)
             (gnu home-services shells)
             (gnu home-services shellutils)
             (gnu home-services files)
             (gnu home-services ssh)
             (gnu home-services emacs)
             (gnu home-services version-control))

;; Add Intimate information

;; (save-module-excursion
;;  (lambda () (primitive-load (format #f "~a/.config/guix/config/Z572.scm" (getenv "HOME"))
;;                             ;; (string-append (dirname (current-filename)) "/../Z572.scm")
;;                             )))


(define-public z-emacs-rime
  (let ((commit "519e6eb3b5e8e668c2835d27f54fcf5776242576")
        (revision "1"))
    (package
      (inherit emacs-rime)
      (name "emacs-rime")
      (version (git-version "1.0.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/DogLooksGood/emacs-rime")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1q0kzdy3nxswsriq4fxr00wmw43x737dd7pnkf9g6v8hd7gsqkzc")))))))

(define-public emacs-leaf-keywords
  ;;latest git tag is on May 29, 2019, 1.1.0, but leaf-keywords.el version is
  ;;2.0.5
  (let ((commit "4146621f4ae80ef0c30160337119441c1f6334b6")
        (revision "0"))
    (package
      (name "emacs-leaf-keywords")
      (version (git-version "2.0.5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/conao3/leaf-keywords.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "16iv1cvlky2gij1ndx2d6q8l35axm72bx52n6v5y3h21aibj197n"))))
      (build-system emacs-build-system)
      (propagated-inputs `(("emacs-leaf" ,emacs-leaf)))
      (home-page
       "https://github.com/conao3/leaf-keywords.el")
      (synopsis
       "Additional leaf.el keywords for external packages")
      (description
       "@code{leaf-keywords} provide additional keywords for leaf.el defines
keywords that are dependent on an external package.")
      (license license:gpl3+))))

(define-public emacs-easy-escape
  (package
    (name "emacs-easy-escape")
    (version "20161209.1544")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://melpa.org/packages/easy-escape-"
             version
             ".el"))
       (sha256
        (base32
         "1z9xc3w5l2v2mhrncqsbbkax2jbd83p812svjhfr7zdlidd9v9l6"))))
    (build-system emacs-build-system)
    (home-page
     "https://github.com/cpitclaudel/easy-escape")
    (synopsis
     "Improve readability of escape characters in regular expressions")
    (description
     "`easy-escape-minor-mode' uses syntax highlighting and composition to make ELisp regular
expressions more readable.  More precisely, it hides double backslashes
preceding regexp specials (`()|'), composes other double backslashes into
single ones, and applies a special face to each.  The underlying buffer text
is not modified.

For example, `easy-escape` prettifies this:
  \"\\\\(?:\\\\_<\\\\\\\
ewcommand\\\\_>\\\\s-*\\\\)?\"
into this (`^' indicates a different color):
  \"(?:\\_<\\\
ewcommand\\_>\\s-*)?\".
   ^                        ^

The default is to use a single \\ character instead of two, and to hide
backslashes preceding parentheses or `|'.  The escape character and its color
can be customized using `easy-escape-face' and `easy-escape-character' (which see), and backslashes
before ()| can be shown by disabling `easy-escape-hide-escapes-before-delimiters'.

Suggested setup:
  (add-hook 'lisp-mode-hook 'easy-escape-minor-mode)

NOTE: If you find the distinction between the fontified double-slash and the
single slash too subtle, try the following:

* Adjust the foreground of `easy-escape-face'
* Set `easy-escape-character' to a different character.
")
    (license #f)))


(define emacs-winum
  (package
    (name "emacs-winum")
    (version "20190911.1607")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://melpa.org/packages/winum-"
             version
             ".el"))
       (sha256
        (base32
         "0dwg195ab9dyz53zqrj3i4mbz8isv69n8m0251fpl6va01rsrhgs"))))
    (build-system emacs-build-system)
    (propagated-inputs `(("emacs-dash" ,emacs-dash)))
    (home-page "http://github.com/deb0ch/winum.el")
    (synopsis
     "Navigate windows and frames using numbers.")
    (description
     "
Window numbers for Emacs: Navigate your windows and frames using numbers.

This package is an extended and actively maintained version of the
https://github.com/nschum/window-numbering.el package by Nikolaj Schumacher,
with some ideas and code taken from https://github.com/abo-abo/ace-window.

This version brings, among other things, support for number sets across multiple
frames, giving the user a smoother experience of multi-screen Emacs.
")
    (license #f)))

;; (define-public emacs-straight
;;   (package
;;     (name "emacs-straight")
;;     (version "33dbc6")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/raxod502/straight.el")
;;              (commit "33dbc6eac2a46462f6ec466a053144c1850b84d6")
;;              ))
;;        (sha256
;;         (base32
;;          "1kcq0z7f8474320w120rlb8dkqj5pk66bjhk6yi0c5h6wyx5kkz6"))
;;        (file-name (git-file-name name version))))
;;     (inputs `(("git" ,git-minimal)))
;;     (build-system emacs-build-system)
;;     (home-page
;;      "https://github.com/raxod502/straight.el")
;;     (synopsis "")
;;     (description "")
;;     (license #f)))

(define-public emacs-meow
  (package
    (name "emacs-meow")
    (version "20210511.314")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://melpa.org/packages/meow-"
             version
             ".tar"))
       (sha256
        (base32
         "0jmb5vwqv7vmnlq1lpc6676z69533rzs6xgly30079la4bh8mi4z"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash) ("emacs-s" ,emacs-s)))
    (home-page
     "https://www.github.com/DogLooksGood/meow")
    (synopsis "Modal Editing On Wheel")
    (description
     "Enable `meow-global-mode' to activate modal editing.
")
    (license #f)))

(define sample-he
  (home-environment
   ;; (keyboard-layout
   ;;  (keyboard-layout
   ;;   "us" #:options
   ;;   '("ctrl:nocaps"
   ;;     "shift:both_capslock")))
   (home-directory (getenv "HOME"))
   (packages
    (append
     (list)
     (map (compose specification->package+output symbol->string)
          '(python-language-server
            python
            guile-hall
            ;;guile-bash
            fontmanager
            fzf
            sbcl
            sbcl-slynk
            okular
            tree
            translate-shell
            telegram-desktop
            tintin++
            isync
            okular
            krita
            hsetroot
            mu
            libreoffice
            xmobar
            rust:cargo
            rust:out
            rust:rustfmt
            zip
            tidy
            gettext
            picom
            inkscape
            ccls))))
   (services
    (list
     (simple-service
      'my-env home-environment-variables-service-type
      `(("QT_IM_MODULE" . "ibus")
        ("GTK_IM_MODULE" . "ibus")
        ("XMODIFIERS" . "@im=ibus")
        ("GUIX_GTK3_IM_MODULE_FILE" . "/run/current-system/profile/lib/gtk-3.0/3.0.0/immodules-gtk3.cache")
        ("GUIX_GTK2_IM_MODULE_FILE" . "/run/current-system/profile/lib/gtk-2.0/2.10.0/immodules-gtk2.cache")
        ("GUIX_GITHUB_TOKEN" . ,z-guix-github-token
         ;; "c108c863fe50516c1af2be509d0e8d04ddb3fbfe"
         )))
     (simple-service
      'set-brightness-on-login home-run-on-first-login-service-type
      #~(system* #$(file-append light "/bin/light") "-S" "90"))
     (simple-service 'inputrc-config home-files-service-type
                     (list `("inputrc" ,(plain-file
                                         "inputrc"
                                         (format #f "~@{~?\n~}"
                                                 "~:@{~a: ~a\n~}"
                                                 '(("C-p" history-search-backward)
                                                   ("C-n" history-search-forward)
                                                   ("M-p" history-search-backward)
                                                   ("M-n" history-search-forward)
                                                   ("C-a" beginning-of-line)
                                                   ("Tab" menu-complete)
                                                   ;;("C-e" eng-of-line)
                                                   ("\\e[5~" beginning-of-history)
                                                   ("\\e[6~" end-of-history)
                                                   ("\\e[3~" delete-char)
                                                   ("\\e[2~" quoted-insert))
                                                 "~:@{set ~a ~:[Off~;On~]~%~}"
                                                 '((colored-stats #t)
                                                   (colored-completion-prefix #t)
                                                   (convert-meta #f)
                                                   (meta-flag #t)
                                                   (expand-tilde #t)
                                                   (input-meta #t)
                                                   (output-meta #t)
                                                   (completion-ignore-case #t)
                                                   ;;(completion-prefix-display-length 3)
                                                   (revert-all-at-newline #t)
                                                   (menu-complete-display-prefix #t)
                                                   (completion-map-case #t)
                                                   (bind-tty-special-chars #f)
                                                   (mark-symlinked-directories #t)
                                                   (skip-completed-text #t)
                                                   (show-all-if-ambiguous #t)
                                                   (show-all-if-unmodified #t)
                                                   (visible-stats #t)))))))
     (simple-service
      'translate-shell-config home-files-service-type
      (list `("config/translate-shell/init.trans"
              ,(plain-file "translate-shell-conf" ":verbose false\n"))))
     (service home-state-service-type
              (list (state-git (string-append (getenv "HOME") "/gits/z-guix-bot")
                               "https://github.com/Z572/z-guix-bot")
                    (state-git (string-append (getenv "HOME") "/gits/rde")
                               "https://github.com/Z572/rde")))

     (service home-mcron-service-type
              (home-mcron-configuration
               (jobs
                (list
                 #~(job
                    '(next-minute
                      (range 0 60 10))
                    (lambda ()
                      (system* #$(file-append (specification->package "isync") "/bin/mbsync")  "-a"))
                    "mbsync")))))
     (service home-ssh-service-type
              (home-ssh-configuration
               (extra-config
                (list (ssh-host "guixcn"
                                '((hostname . "shanghai.guix.org.cn")
                                  (user . "Z572")
                                  (port . 22)
                                  (identity-file "~/.ssh/Z572_guixcn_ed25519")))))))
     (service home-git-service-type
              (home-git-configuration
               (ignore '(".envrc"))
               (config
                `((pull ((rebase . #t)))
                  (user ((name . ,z-name)
                         (email . ,z-email)))
                  (gpg ((program . ,(file-append (specification->package "gnupg") "/bin/gpg"))))
                  (github ((user . ,z-name)))
                  (gitlab ((user . ,z-name)))))))
     (service home-zsh-autosuggestions-service-type)
     (service (service-type
               (name 'home-bash-direnv)
               (extensions
                (list
                 (service-extension
                  home-profile-service-type
                  (const (list (specification->package "direnv"))))
                 (service-extension
                  home-bash-service-type
                  (const (home-bash-extension
                          (bashrc (list #~(format #f "eval \"$(~a hook bash)\""
                                                  #$(file-append (specification->package "direnv") "/bin/direnv" )))))))))
               (default-value #f)
               (description "Enables @code{direnv} for @code{bash}.  Adds hook to
@file{.bashrc} and installs a package in the profile.")))
     ;; (service (service-type
     ;;                (name 'home-bash-guile-bash)
     ;;                (extensions
     ;;                 (list
     ;;                  (service-extension
     ;;                   home-profile-service-type
     ;;                   (const (list (specification->package "guile-bash"))))
     ;;                  (service-extension
     ;;                   home-bash-service-type
     ;;                   (const (home-bash-extension
     ;;                           (bashrc (list #~(string-append "enable -f " #$(file-append (specification->package "guile-bash") "/lib/bash/libguile-bash.so" )
     ;;                                                          " scm
     ;; "
     ;;                                                          "GUILE_AUTO_COMPILE=0 GUILE_LOAD_PATH=$GUILE_LOAD_PATH:/home/x/.guix-home-environment/profile/share/guile/site/3.0 "
     ;;                                                          "builtin scm \"$HOME\"/.bash.d/bash.scm"))))))))
     ;;                (default-value #f)
     ;;                (description "Enables @code{direnv} for @code{bash}.  Adds hook to
     ;; @file{.bashrc} and installs a package in the profile.")))
     (service home-zsh-direnv-service-type)
     (service home-bash-service-type
              (home-bash-configuration
               (guix-defaults? #t)
               (bashrc '("\
shopt -s autocd
shopt -s checkwinsize
"))))
     (service home-emacs-service-type
              (home-emacs-configuration
               (package (specification->package+output "emacs-next"))
               (server-mode? #t)
               (elisp-packages
                (list emacs-all-the-icons
                      emacs-leaf-keywords
                      emacs-avy
                      emacs-blackout
                      emacs-cider
                      ;;emacs-straight
                                        ;emacs-cmake-mode
                      emacs-company-box
                      emacs-ctrlf
                      emacs-haskell-mode
                      emacs-leaf
                      emacs-debbugs
                      emacs-meow
                      emacs-dimmer
                      emacs-easy-escape
                      emacs-diredfl
                      emacs-doom-modeline
                      emacs-eglot
                      emacs-emojify
                      emacs-envrc
                      emacs-flycheck
                      emacs-flycheck-guile
                      emacs-go-mode
                      emacs-guix
                      emacs-helpful
                      emacs-highlight-defined
                      emacs-hl-todo
                      emacs-macrostep
                      emacs-forge
                      emacs-magit
                      emacs-magit-todos
                      emacs-nix-mode
                      emacs-no-littering
                      emacs-nov-el
                      emacs-olivetti
                      emacs-page-break-lines
                      emacs-paren-face
                      emacs-pdf-tools
                      emacs-prescient
                      emacs-repology
                      emacs-winum
                      emacs-leetcode
                      emacs-ebdb
                      emacs-restart-emacs
                      z-emacs-rime
                      emacs-rust-mode
                      emacs-selectrum
                      emacs-shackle
                      emacs-sly
                      emacs-symbol-overlay
                      emacs-telega
                      emacs-vterm
                      emacs-web-mode
                      emacs-which-key
                      emacs-ws-butler
                      emacs-yasnippet
                      emacs-yasnippet-snippets
                      emacs-aggressive-indent))))
     (service home-zsh-service-type
              (home-zsh-configuration
               (xdg-flavor? #t)
               (zshenv (list "\
HISTSIZE=50000
HISTFILE=\"$HOME/.zsh_history\"
SAVEHIST=10000
"))))
     (service home-zsh-plugin-manager-service-type)
     (service home-gnupg-service-type
              (home-gnupg-configuration
               (gpg-config
                (home-gpg-configuration
                 (extra-config
                  '((cert-digest-algo . "SHA256")
                    (default-preference-list . ("SHA512"
                                                "SHA384"
                                                "SHA256"
                                                "SHA224"
                                                "AES256"
                                                "AES192"
                                                "Uncompressed"))
                    (with-fingerprint? . #t)))))
               (gpg-agent-config
                (home-gpg-agent-configuration
                 (ssh-agent? #t)
                 (pinentry-flavor 'emacs)
                 (extra-options '("--verbose"))
                 (extra-config
                  '((max-cache-ttl . 86400)))))))
     (service home-password-store-service-type
              (home-password-store-configuration
               (browserpass-native? #t)))))))

sample-he
