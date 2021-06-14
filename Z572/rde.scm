;; -*- mode: scheme -*-
(add-to-load-path (format #f "~a/.config/guix/config/" (getenv "HOME")))
(use-modules (guix) (gnu)
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 format)
             (guix transformations))
(use-package-modules linux emacs-xyz guile guile-xyz gnupg rust-apps shellutils)
(use-system-modules keyboard)
(use-modules (Z572)
             (gnu home)
             ((guix licenses) #:prefix license:)
             (guix gexp)
             (guix modules)
             (guix git-download)
             (guix build-system emacs)
             (guix build-system copy)
             (gnu home-services)
             (gnu home-services gnupg)
             (gnu home-services state)
             (gnu home-services mcron)
             (gnu home-services password-utils)
             (gnu home-services shells)
             (gnu home-services shellutils)
             (gnu home-services web-browsers)
             (gnu home-services keyboard)
             (gnu home-services files)
             (gnu home-services fontutils)
             (gnu home-services ssh)
             (gnu home-services emacs)
             (gnu home-services version-control)
             (guix channels)
             (flat packages emacs)
             (gnu packages emacs)
             (guix inferior))

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

(define-public emacs-bing-dict
  (package
    (name "emacs-bing-dict")
    (version "20200216.110")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://melpa.org/packages/bing-dict-"
             version
             ".tar"))
       (sha256
        (base32
         "04x5jjlzizwdqw5ikrqx4kfw1djr3hmm2k79sm3mf49mn87k18hd"))))
    (build-system emacs-build-system)
    (home-page
     "https://github.com/cute-jumper/bing-dict.el")
    (synopsis
     "Minimalists' English-Chinese Bing dictionary")
    (description
     "A **minimalists'** Emacs extension to search http://www.bing.com/dict.
Support English to Chinese and Chinese to English.")
    (license #f)))

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

(define-public emacs-highlight-quoted
  (package
    (name "emacs-highlight-quoted")
    (version "20140916.1822")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://melpa.org/packages/highlight-quoted-"
             version
             ".el"))
       (sha256
        (base32
         "12d21lx97fh7hv00n02bfacn62g3dv99176fxdkrf7zkzcnpv75h"))))
    (build-system emacs-build-system)
    (home-page
     "https://github.com/Fanael/highlight-quoted")
    (synopsis
     "Highlight Lisp quotes and quoted symbols")
    (description
     "Minor mode proving highlight of Lisp quotes and quoted symbols.
")
    (license #f)))

(define-public emacs-cmake-mode
  (package
    (name "emacs-cmake-mode")
    (version "20210104.1831")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://melpa.org/packages/cmake-mode-"
             version
             ".el"))
       (sha256
        (base32
         "1qwfrz1n64g0mwzbh1ldqvav0qv19k4rd15mn6448vkkmrayygi2"))))
    (build-system emacs-build-system)
    (home-page "unspecified")
    (synopsis "major-mode for editing CMake sources")
    (description
     "Provides syntax highlighting and indentation for CMakeLists.txt and
*.cmake source files.

Add this code to your .emacs file to use the mode:

 (setq load-path (cons (expand-file-name \"/dir/with/cmake-mode\") load-path))
 (require 'cmake-mode)\n
;------------------------------------------------------------------------------
")
    (license #f)))

(define-public z-emacs
  (let ((commit "7673b6b9eb0af3add73e1614a466f142092b00aa")
        (revision "0"))
    (package
      (inherit emacs-pgtk-native-comp)
      (name "z-emacs")
      (version (git-version "28.0.50" revision commit))
      (source
       (origin
         (inherit (package-source emacs-pgtk-native-comp))
         (method git-fetch)
         (uri (git-reference
               (url "https://git.savannah.gnu.org/git/emacs.git/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1r1y3x570v1j5zcwp9qwz4dhcc03zp8xsfnz94wvrf9vr8c2ix24")))))))

(define (make-rust-src rust)
  (package
    (inherit rust)
    (name "rust-src")
    (outputs '("out"))
    (arguments
     (substitute-keyword-arguments (package-arguments rust)
       ((#:phases phases)
        `(modify-phases ,phases
           (delete 'configure)
           (delete 'enable-codegen-tests)
           (delete 'override-jemalloc)
           (delete 'build)
           (delete 'check)
           (delete 'mkdir-prefix-paths)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((src (string-append (assoc-ref outputs "out")
                                         "/lib/rustlib/src/rust")))
                 (mkdir-p src)
                 (copy-recursively "./library" (string-append src "/library"))
                 (copy-recursively "./src" (string-append src "/src"))
                 ;; (copy-file "Cargo.toml" (string-append src "/Cargo.toml"))
                 #t)))
           (delete 'wrap-rustc)
           (delete 'delete-install-logs)))))))

(define sample-he
  (home-environment
   (home-directory (getenv "HOME"))
   (packages
    (append
     (list (make-rust-src (specification->package "rust@1.52")))
     (map (compose list specification->package+output symbol->string)
          '(python-language-server
            python
            guile-hall
            ;;guile-bash
            fontmanager
            fzf
            tokei
            sbcl
            neofetch
            dunst
            rust-analyzer
            libnotify
            rsync
            sbcl-slynk
            okular
            tree
            syncthing
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
        ("GUIX_GITHUB_TOKEN" . ,z-guix-github-token)))
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
     (simple-service
      'mpv-config home-files-service-type
      (list `("config/mpv/mpv.conf"
              ,(plain-file "mpv.conf" "vo=x11,drm\n"))))
     (simple-service
      'mail-signature home-files-service-type
      (list `("signature"
              ,(plain-file "signature" z-email-signature))))
     (service home-state-service-type
              (list (state-git (string-append (getenv "HOME") "/gits/z-guix-bot")
                               "https://github.com/Z572/z-guix-bot")
                    (state-git (string-append (getenv "HOME") "/gits/rde")
                               "https://github.com/Z572/rde")))

     ;; (service home-fontconfig-service-type)
     (service home-mcron-service-type
              (home-mcron-configuration
               (jobs
                (list
                 #~(job
                    '(next-minute
                      (range 0 60 10))
                    #$(file-append (specification->package "isync") "/bin/mbsync -a")
                    "mbsync")))))
     (service home-ssh-service-type
              (home-ssh-configuration
               (extra-config
                (list (ssh-host
                       (host "guixcn")
                       (options
                        '((hostname . "shanghai.guix.org.cn")
                          (user . "Z572")
                          (port . 22)
                          (identity-file "~/.ssh/Z572_guixcn_ed25519"))))))))
     (service home-git-service-type
              (home-git-configuration
               (ignore '(".envrc"))
               (config
                `((pull ((rebase . #t)))
                  (user ((name . ,z-name)
                         (email . ,z-email)))
                  (gpg ((program . ,(file-append gnupg "/bin/gpg"))))
                  (github ((user . ,z-name)))
                  (gitlab ((user . ,z-name)))))))
     (service home-zsh-autosuggestions-service-type)
     (service (service-type
               (name 'home-bash-direnv)
               (extensions
                (list
                 (service-extension
                  home-profile-service-type
                  (const (list direnv)))
                 (service-extension
                  home-bash-service-type
                  (const
                   (home-bash-extension
                    (bashrc (list
                             #~(format #f "eval \"$(~a hook bash)\""
                                       #$(file-append direnv "/bin/direnv")))))))))
               (default-value #f)
               (description "Enables @code{direnv} for @code{bash}.  Adds hook to
@file{.bashrc} and installs a package in the profile.")))
     (service (service-type
               (name 'home-bash-zoxide)
               (extensions
                (list
                 (service-extension
                  home-profile-service-type
                  (const (list zoxide)))
                 (service-extension
                  home-bash-service-type
                  (const
                   (home-bash-extension
                    (bashrc (list
                             #~(begin (use-modules (ice-9 format))
                                      (format #f "alias zoxide=~a~%eval ~S"
                                              #$(file-append zoxide "/bin/zoxide")
                                              "$(zoxide init bash)")))))))))
               (default-value #f)
               (description "Enables @code{zoxide} for @code{bash}.  Adds hook to
@file{.bashrc} and installs a package in the profile.")))
     (service home-zsh-direnv-service-type)
     (service home-icecat-service-type
              (home-icecat-configuration
               (profiles
                (list (icecat-profile
                       (default? #t)
                       (name "default")
                       (id 0)
                       (settings '((browser.urlbar.shortcuts.history . #t)
                                   (browser.fullscreen.autohide . #t)
                                   (toolkit.legacyUserProfileCustomizations.stylesheets . #t)
                                   (svg.context-properties.content.enabled . #t)))
                       (user-chrome "\
#TabsToolbar { visibility: collapse !important; }")
                       (user-content "\
:root{ scrollbar-width: none !important; }"))
                      (icecat-profile
                       (default? #f)
                       (name "github")
                       (id 1)
                       (settings '((browser.urlbar.shortcuts.bookmarks . #f)
                                   (browser.fullscreen.autohide . #t))))))))
     (service home-bash-service-type
              (home-bash-configuration
               (guix-defaults? #t)
               (bashrc '("\
case $- in
   *i*) ;;
     *) return;;
esac
HISTCONTROL=ignoreboth
shopt -s histappend
HISTSIZE=5000
HISTFILEIZE=2000
shopt -s autocd
shopt -s checkwinsize
"))))
     (service home-emacs-service-type
              (home-emacs-configuration
               (package z-emacs)
               (server-mode? #t)
               (elisp-packages
                (list emacs-leaf-keywords
                      emacs-avy
                      emacs-blackout
                      emacs-ctrlf
                      emacs-debbugs

                      ;;emacs-meow
                      emacs-xr
                      emacs-dimmer
                      emacs-easy-escape
                      emacs-diredfl
                      emacs-doom-modeline
                      emacs-eglot
                      emacs-emojify
                      emacs-envrc
                      emacs-flycheck
                      emacs-flycheck-guile
                      emacs-guix
                      emacs-highlight-quoted
                      emacs-cmake-mode
                      emacs-prescient
                      emacs-helpful
                      emacs-highlight-defined
                      emacs-hl-todo
                      emacs-macrostep
                      emacs-forge
                      emacs-magit
                      emacs-calfw
                      emacs-magit-todos
                      emacs-symbol-overlay
                      emacs-telega
                      emacs-vterm
                      emacs-selectrum
                      emacs-no-littering
                      emacs-nov-el
                      emacs-bing-dict
                      emacs-olivetti
                      emacs-page-break-lines
                      emacs-paren-face
                      emacs-go-mode
                      emacs-pdf-tools
                      emacs-repology
                      emacs-winum
                      emacs-leetcode
                      emacs-ebdb
                      emacs-restart-emacs
                      z-emacs-rime
                      emacs-rust-mode
                      emacs-cider
                      emacs-haskell-mode
                      emacs-nix-mode
                      emacs-shackle
                      emacs-sly


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
                    (default-preference-list
                      . ("SHA512"
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
               (browserpass-native? #t)))
     (service home-keyboard-service-type
              (keyboard-layout
               "us" #:options
               '("ctrl:nocaps"
                 "shift:both_capslock")))))))

sample-he
