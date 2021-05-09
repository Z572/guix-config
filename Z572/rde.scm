;; -*- mode: scheme -*-
(use-modules (guix) (gnu)
             (srfi srfi-26)
             (guix transformations))
(use-package-modules linux emacs-xyz)
(use-system-modules keyboard)
(use-modules (gnu home)
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
          (base32 "1q0kzdy3nxswsriq4fxr00wmw43x737dd7pnkf9g6v8hd7gsqkzc"))))
      (build-system emacs-build-system)
      (arguments
       '(#:include (cons "\\.so$" %default-include)
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-rime-data-path
             (lambda* (#:key inputs #:allow-other-keys)
               (make-file-writable "rime.el")
               (emacs-substitute-variables "rime.el"
                 ("rime-share-data-dir"
                  (string-append (assoc-ref inputs "rime-data")
                                 "/share/rime-data")))
               #t))
           (add-before 'install 'build-emacs-module
             (lambda _
               (invoke "make" "lib")))))))))

(define emacs-leaf-keywords
  (package
    (name "emacs-leaf-keywords")
    (version "20210222.1243")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://melpa.org/packages/leaf-keywords-"
             version
             ".tar"))
       (sha256
        (base32
         "1g4d5h9kbsbnq4pp9q36fcpc4v89k7xcnwyhi1mca2v3kan84144"))))
    (build-system emacs-build-system)
    (propagated-inputs `(("emacs-leaf" ,emacs-leaf)))
    (home-page
     "https://github.com/conao3/leaf-keywords.el")
    (synopsis
     "Additional leaf.el keywords for external packages")
    (description
     "leaf-keywords.el provide additional keywords for leaf.el
This package defines keywords that are dependent on an external package.

More information is [[https://github.com/conao3/leaf-keywords.el][here]]
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

(define sample-he
  (home-environment
   (keyboard-layout
    (keyboard-layout
     "us" #:options
     '("ctrl:nocaps"
       "shift:both_capslock")))
   (home-directory (getenv "HOME"))
   (packages
    (append
     (list)
     (map (compose specification->package+output symbol->string)
          '(python-language-server
            python
            guile-hall
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
      '(("QT_IM_MODULE" . "ibus")
        ("GTK_IM_MODULE" . "ibus")
        ("XMODIFIERS" . "@im=ibus")
        ("GUIX_GTK3_IM_MODULE_FILE" . "/run/current-system/profile/lib/gtk-3.0/3.0.0/immodules-gtk3.cache")
        ("GUIX_GTK2_IM_MODULE_FILE" . "/run/current-system/profile/lib/gtk-2.0/2.10.0/immodules-gtk2.cache")
        ("GUIX_GITHUB_TOKEN" . "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"
         ;; "c108c863fe50516c1af2be509d0e8d04ddb3fbfe"
         )))
     (simple-service
      'set-brightness-on-login home-run-on-first-login-service-type
      #~(system* #$(file-append light "/bin/light") "-S" "90"))
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
                  (user ((name . "Z572")
                         (email . "873216071@qq.com")))
                  (gpg ((program . ,(file-append (specification->package "gnupg") "/bin/gpg"))))
                  (github ((user . "Z572")))
                  (gitlab ((user . "Z572")))))))
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
                          (bashrc (list "eval \"$(direnv hook bash)\"")))))))
               (default-value #f)
               (description "Enables @code{direnv} for @code{bash}.  Adds hook to
@file{.bashrc} and installs a package in the profile.")))
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
                      emacs-dimmer
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
                      emacs-leetcode
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
