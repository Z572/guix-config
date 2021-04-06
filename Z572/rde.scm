;; -*- mode: scheme -*-
(use-modules (guix) (gnu)
             (srfi srfi-26)
             (guix transformations))
(use-package-modules linux)
(use-system-modules keyboard)
(use-modules (gnu home)
             (gnu home-services)
             (gnu home-services gnupg)
             (gnu home-services state)
             (gnu home-services mcron)
             (gnu home-services shells)
             (gnu home-services shellutils)
             (gnu home-services files)
             (gnu home-services emacs)
             (gnu home-services version-control))

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
            krita
            fontmanager
            fzf
            sbcl
            sbcl-slynk
            okular
            tree
            translate-shell
            direnv
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
            guile-hall
            picom
            inkscape
            ccls))))
   (services
    (list
     (simple-service
      'my-env home-environment-vars-service-type
      '(("GUIX_GTK3_IM_MODULE_FILE" . "/run/current-system/profile/lib/gtk-3.0/3.0.0/immodules-gtk3.cache")
        ("GUIX_GTK2_IM_MODULE_FILE" . "/run/current-system/profile/lib/gtk-2.0/2.10.0/immodules-gtk2.cache")
        ("GUIX_GITHUB_TOKEN" . "c108c863fe50516c1af2be509d0e8d04ddb3fbfe")))
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

     ;; (service home-mcron-service-type
     ;;          (home-mcron-configuration
     ;;           (jobs
     ;;            (list
     ;;             #~(job
     ;;                next-hour-from
     ;;                (string-append #$(specification->package "isync") "/bin/mbsync" " -a")
     ;;                ;; (lambda ()
     ;;                ;;   (call-with-output-file
     ;;                ;;       "/tmp/ffaf"
     ;;                ;;     (cut display "Mcron service" <>)))
     ;;                "mbsync")))))
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
     (service home-zsh-direnv-service-type)
     (service home-emacs-service-type
              (home-emacs-configuration
               (package (specification->package+output "emacs-next"))
               (elisp-packages (map (compose specification->package+output symbol->string)
                                    '(emacs-all-the-icons
                                      emacs-avy
                                      emacs-blackout
                                      emacs-cider
                                      emacs-cmake-mode
                                      emacs-company-box
                                      emacs-ctrlf
                                      emacs-haskell-mode
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
                                      emacs-next
                                      emacs-nix-mode
                                      emacs-no-littering
                                      emacs-nov-el
                                      emacs-olivetti
                                      emacs-page-break-lines
                                      emacs-paren-face
                                      emacs-pdf-tools
                                      emacs-prescient
                                      emacs-repology
                                      emacs-restart-emacs
                                      emacs-rime
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
                                      emacs-aggressive-indent)))))
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
                  '((max-cache-ttl . 86400)))))))))))

sample-he
