(define-module (Z572 system)
  #:autoload (guix build-system copy) (copy-build-system)
  #:autoload (guix build-system emacs) (emacs-build-system)
  #:autoload (guix build-system trivial) (trivial-build-system)
  #:autoload (ice-9 pretty-print) (pretty-print)
  #:autoload (srfi srfi-26) (cut)
  #:autoload (srfi srfi-1) (first)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu)
  #:use-module (gnu packages)
  ;; #:use-module (gnu packages base)
  ;; #:use-module (gnu packages bash)
  ;; #:use-module (gnu packages certs)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages linux)
  ;; #:use-module (gnu packages mpd)
  ;; #:use-module (gnu packages shells)
  ;; #:use-module (gnu packages webkit)
  ;; #:use-module (gnu packages wm)
  #:use-module (gnu services admin)
  #:use-module (gnu services audio)
  #:use-module (gnu services desktop)
  #:use-module (gnu services linux)
  #:use-module (gnu services mcron)
  #:use-module (gnu services networking)
  #:use-module (gnu services pm)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services sound)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services xorg)
  #:use-module (gnu services sddm)
  #:use-module (gnu system nss)
  #:use-module (guix channels)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix inferior)
  #:use-module (guix licenses)
  #:use-module (guix memoization)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module (guix transformations)
  #:use-module (guix utils)
  #:use-module (nongnu packages linux)
  #:use-module (flat packages emacs))

(define chili-sddm-theme
  (package
    (name "chili-sddm-theme")
    (version "0.1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/MarianArlt/sddm-chili")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "036fxsa7m8ymmp3p40z671z163y6fcsa9a641lrxdrw225ssq5f3"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (sddm-themes (string-append out "/share/sddm/themes")))
           (mkdir-p sddm-themes)
           (copy-recursively (assoc-ref %build-inputs "source")
                             (string-append sddm-themes "/chili"))))))
    (home-page "https://github.com/MarianArlt/sddm-chili")
    (synopsis "Chili theme for SDDM")
    (description "Spice up the login experience for your users, your family and
yourself.  Chili reduces all the clutter and leaves you with a clean, easy to
use, login interface with a modern yet classy touch.")
    (license license:gpl3)))

(define-public google-hosts
  (let ((commit "8ff01be91c4a70604f83e5cf0a3dd595fe8868b0")
        (revision "1"))
    (package
      (name "googlehosts")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/googlehosts/hosts")
               (commit commit)))
         (file-name (git-file-name name commit))
         (sha256
          (base32
           "07bf23fp9l3xcw2jf3rxjkb4729948k6a3vgqvizs9v4glhwmj1b"))))
      (build-system copy-build-system)
      (home-page "https://github.com/googlehosts/hosts/")
      (synopsis "hosts of googlehosts")
      (description "hosts of googlehosts")
      (license "file://LICENSE"))))

(define-public %z-emacs
  (let* ((z-inherit emacs-native-comp)
         (z-revision "1")
         (z-commit "71e93eda873a775c016a9871a2e245d8662b69dd")
         (z-checksum "15z5fvqvvrch57lcn560y2a5wgj0yg954wl5qdgibf906yk5q86y"))
    (package/inherit
     z-inherit
     (name "fast-emacs")
     (version (git-version "28.0.50" z-revision z-commit))
     (source
      (origin
        (inherit (package-source z-inherit))
        (uri (git-reference
              (url "https://gitclone.com/github.com/geza-herman/emacs")
              (commit z-commit)))
        (file-name (git-file-name name version))
        (sha256 (base32 z-checksum)))))))

(define %z-substitute-urls
  (list
   "https://mirror.sjtu.edu.cn/guix"
   ;;"https://mirror.c1r3u.xyz"
   ;;"https://guix-mirror.pengmeiyu.com"
   "https://mirror.guix.org.cn"
   "https://mirror.brielmaier.net"))

(define %z-fonts
  '(fira-code
    sarasa-gothic dejavu
    wqy-microhei
    gnu-unifont hack wqy-zenhei
    adobe-source-han-sans))

(define %z-keyboard
  (keyboard-layout
   "us" #:options
   '("ctrl:nocaps"
     "shift:both_capslock")))

(define %z-file-systems
  (list (file-system
          (device (file-system-label "my-root"))
          (mount-point "/")
          (type "ext4"))
        (file-system
          (device
           (uuid "8e6e73cf-4af1-464c-a3a4-f8292a04e3f1" 'ext4))
          (mount-point "/home")
          (type "ext4"))
        (file-system
          (device
           (uuid "9196-0CB1" 'fat))
          (mount-point "/boot/efi")
          (type "vfat"))))

(define z-system
  (operating-system
    (host-name "Z572")
    (hosts-file
     (file-append
      google-hosts
      "/hosts-files/hosts"))
    (timezone "Asia/Shanghai")
    (locale "en_US.UTF-8")
    (keyboard-layout %z-keyboard)
    (bootloader
     (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (target "/boot/efi")
      (keyboard-layout keyboard-layout)))
    (file-systems
     (append
      %z-file-systems
      %base-file-systems))
    (kernel
     (package
       (inherit linux)
       (version (package-version linux))
       (source
        (origin
          (inherit (package-source linux))
          (uri (string-append "https://mirrors.aliyun.com/linux-kernel/v"
                              (version-major version)".x/linux-" version ".tar.xz"))))))
    ;;(initrd microcode-initrd)
    (firmware (cons* (package
                       (inherit linux-firmware)
                       (version "20201118")
                       (source
                        (origin
                          (method url-fetch)
                          (uri (string-append "https://mirrors.tuna.tsinghua.edu.cn/kernel/firmware/"
                                              "linux-firmware-" version ".tar.gz"))
                          (sha256
                           (base32
                            "1dkyw5fa4mg053q0iy4yfjzrf8mkhjcsqw9ai6wcb5nn3prycbvh")))))
                     %base-firmware))
    (kernel-arguments
     (append '("modprobe.blacklist=ideapad_laptop")
             %default-kernel-arguments))
    (label (format #f "~s ~s包"
                   (package-full-name (operating-system-kernel this-operating-system))
                   (length (operating-system-packages this-operating-system))))
    (users
     (cons*
      (user-account
       (name "x")
       (comment "x")
       (group "users")
       (supplementary-groups
        (list "wheel" "netdev"
              "input" ;; "libvirt"
              "kvm"
              "audio" "video")))
      %base-user-accounts))
    (packages
     (append
      (map (package-input-rewriting/spec `(("emacs" . ,(const %z-emacs))))
           (list emacs-guix
                 emacs-rime
                 emacs-exwm
                 %z-emacs
                 emacs-cmake-mode
                 emacs-restart-emacs
                 emacs-pdf-tools
                 emacs-go-mode
                 emacs-web-mode
                 emacs-which-key
                 emacs-nov-el
                 emacs-yasnippet-snippets
                 emacs-yasnippet
                 emacs-prescient
                 emacs-symbol-overlay
                 emacs-company-box
                 emacs-all-the-icons
                 emacs-highlight-defined
                 emacs-aggressive-indent
                 emacs-emojify
                 emacs-selectrum
                 emacs-no-littering
                 emacs-page-break-lines
                 emacs-hl-todo
                 chili-sddm-theme
                 emacs-doom-modeline
                 emacs-debbugs
                 emacs-blackout
                 emacs-flycheck-guile
                 emacs-flycheck
                 emacs-macrostep
                 emacs-nix-mode
                 emacs-magit
                 emacs-magit-todos
                 emacs-diredfl
                 emacs-rust-mode
                 emacs-cider
                 emacs-telega
                 emacs-vterm))
      (map (compose specification->package+output
                    symbol->string
                    (cut symbol-append 'font- <>))
           %z-fonts)
      (map (compose specification->package+output
                    symbol->string)
           '(icecat
             picom
             ;;alacritty
             aria2
             iftop
             you-get
             fontmanager
             inkscape
             libreoffice
             ccls
             curl
             blender
             gcc-toolchain
             git
             go-github-com-junegunn-fzf
             imv
             isync
             okular
             fd
             jq
             iotop
             cmake
             shepherd
             krita
             make

             gvfs
             mpv
             ncdu
             notmuch
             ripgrep
             hsetroot
             tmux
             ;;screen
             file
             htop
             zsh
             ;;rofi
             unzip
             ;; virt-manager
             ;; virt-viewer
             xkill
             ;;guix-simplyblack-sddm-theme
             progress
             qemu
             nss-certs))
      %base-packages))
    (services
     (cons*
      (service tlp-service-type)
      ;; (service qemu-binfmt-service-type
      ;;          (qemu-binfmt-configuration
      ;;           (platforms (lookup-qemu-platforms "arm" "aarch64"))
      ;;           (guix-support? #t)))
      (service thermald-service-type)
      (simple-service 'powertop shepherd-root-service-type
                      (list (shepherd-service
                             (provision '(powertop))
                             (requirement '())
                             (start #~(lambda ()
                                        (invoke
                                         #$(file-append powertop "/sbin/powertop")
                                         "--auto-tune")))
                             (one-shot? #t)
                             (respawn? #f))))
      (service earlyoom-service-type)
      (service zram-device-service-type)
      (set-xorg-configuration
       (xorg-configuration
        (keyboard-layout keyboard-layout)) sddm-service-type)
      (service sddm-service-type
               (sddm-configuration
                (theme "chili")))
      (delete (service gdm-service-type)
              (modify-services %desktop-services
                (guix-service-type config =>
                                   (guix-configuration
                                    (inherit config)
                                    (substitute-urls
                                     (append %z-substitute-urls
                                             %default-substitute-urls))
                                    (authorized-keys (append
                                                      (list (plain-file "bricelmaier-key"
                                                                        "(public-key
 (ecc
  (curve Ed25519)
  (q #7514F8D729DB1935470A581CE3851ED9FD6F1F9BAFE1D8BEC77A931ADB7A4337#)))
")
                                                            (plain-file "guix-china-key"
                                                                        "(public-key
 (ecc
  (curve Ed25519)
  (q #A2E559D78D512DAEDFBBC772136F4BB89F1E57C7B147E15E0784A11664379796#)))
"))
                                                      %default-authorized-guix-keys))
                                    (extra-options
                                     (list "-M 4" "-c 0"))))))))
    (name-service-switch %mdns-host-lookup-nss)))

z-system
