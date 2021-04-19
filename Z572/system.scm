(define-module (Z572 system)
  #:autoload (guix build-system copy) (copy-build-system)
  #:autoload (srfi srfi-26) (cut)
  #:autoload (nongnu packages linux) (linux linux-firmware)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (srfi srfi-1)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages)
  #:use-module (gnu services admin)
  #:use-module (gnu services nix)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services audio)
  #:use-module (gnu services desktop)
  #:use-module (gnu services dns)
  #:use-module (gnu services linux)
  #:use-module (gnu services mcron)
  #:use-module (gnu services networking)
  #:use-module (gnu services pm)
  #:use-module (gnu services sddm)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services sysctl)
  #:use-module (gnu services sound)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services xorg)
  #:use-module (gnu system nss)
  #:use-module (gnu)
  #:use-module (guix channels)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix inferior)
  #:use-module (guix licenses)
  #:use-module (guix memoization)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module (guix transformations)
  #:use-module (guix utils))

(define-public my-xmonad
  (package/inherit
   xmonad
   (name "my-xmonad")
   (inputs
    `(,@(package-inputs xmonad)
      ("ghc" ,ghc)))
   (arguments
    (substitute-keyword-arguments (package-arguments xmonad)
      ((#:phases phases)
       `(modify-phases ,phases
          (add-after 'install 'wrap-executable
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (wrap-program (string-append (assoc-ref outputs "out") "/bin/xmonad")
                `("PATH" ":" prefix
                  (,(string-append (assoc-ref inputs "ghc") "/bin"))))))))))))

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

(define %z-substitute-urls
  (list
   "https://mirror.sjtu.edu.cn/guix"
   ;;"https://mirror.c1r3u.xyz"
   ;;"https://guix-mirror.pengmeiyu.com"
   "https://mirror.guix.org.cn"))

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
          (device
           (uuid "67F9-D714" 'fat))
          (mount-point "/boot/efi")
          (type "vfat"))
        (file-system
          (device (file-system-label "my-root"))
          (mount-point "/")
          (type "btrfs")
          (flags '(no-atime))
          (options "compress=zstd:7"))
        (file-system
          (device
           (uuid "8e6e73cf-4af1-464c-a3a4-f8292a04e3f1" 'ext4))
          (mount-point "/home")
          (type "ext4"))))

(define-public z-system
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
    (swap-devices
     (list "/dev/nvme0n1p3"))
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
                       (version (package-version linux-firmware))
                       (source
                        (origin
                          (inherit (package-source linux-firmware))
                          (uri (string-append "https://mirrors.tuna.tsinghua.edu.cn/kernel/firmware/"
                                              "linux-firmware-" version ".tar.gz")))))
                     %base-firmware))
    (kernel-arguments
     (append '("modprobe.blacklist=ideapad_laptop"
               "intel_iommu=on")
             %default-kernel-arguments))
    (label (format #f "~s ~s"
                   (package-full-name (operating-system-kernel this-operating-system))
                   (length (operating-system-packages this-operating-system))))
    (users
     (cons*
      (user-account
       (name "x")
       (comment "x")
       (group "users")
       (supplementary-groups
        (list "wheel"
              "netdev"
              "input"
              ;;"libvirt"
              "kvm"
              ;;"network"
              "audio"
              "video")))
      %base-user-accounts))
    (packages
     (append
      (map (compose specification->package+output
                    symbol->string
                    (cut symbol-append 'font- <>))
           %z-fonts)
      (list my-xmonad)
      (map (compose specification->package+output
                    symbol->string)
           '(ibus
             ibus-rime
             dconf

             icecat
             chili-sddm-theme

             ;;alacritty
             aria2
             btrfs-progs
             bspwm
             iftop
             pamixer
             alsa-utils
             gnupg
             you-get
             ;;fontmanager
             curl
             ;; blender
             gcc-toolchain
             git
             fzf
             ;;ungoogled-chromium
             imv
             xdg-utils
             fd
             ;;jq
             ;;iotop
             cmake
             shepherd

             make

             gvfs
             mpv
             ncdu
             ripgrep

             light
             tmux
             ;;screen
             file
             ppp
             htop
             zsh
             ;;rofi
             unzip
             virt-manager
             ghc-xmonad-contrib
             bridge-utils
             ghc-rio
             xkill
             progress
             qemu
             nss-certs))
      %base-packages))
    (services
     (cons*
      (service tlp-service-type)
      (service libvirt-service-type)
      (service virtlog-service-type)
      ;; (service qemu-binfmt-service-type
      ;;          (qemu-binfmt-configuration
      ;;           (platforms (lookup-qemu-platforms "arm" "aarch64"))
      ;;           (guix-support? #t)))
      (bluetooth-service)
      (service thermald-service-type)
      (udev-rules-service
       'backlight
       (file->udev-rule
        "90-backlight.rules"
        (file-append
         light
         "/lib/udev/rules.d/90-backlight.rules")))
      ;; (simple-service 'hnsd shepherd-root-service-type
      ;;                 (list (shepherd-service
      ;;                        (provision '(hnsd))
      ;;                        (requirement '(networking))
      ;;                        (start #~(make-forkexec-constructor
      ;;                                  (list (string-append #$hnsd-release "/bin/hnsd")
      ;;                                        "-p" "4"
      ;;                                        "-r" "127.0.0.1:53")
      ;;                                  #:pid-file "/var/run/hnsd.pid"))
      ;;                        (stop #~(make-kill-destructor)))))
      ;; (simple-service 'powertop shepherd-root-service-type
      ;;                 (list (shepherd-service
      ;;                        (provision '(powertop))
      ;;                        (one-shot? #t)
      ;;                        (start #~(make-forkexec-constructor
      ;;                                  (list
      ;;                                   #$(file-append powertop "/sbin/powertop") "--auto-tune")
      ;;                                  #:pid-file "/var/run/powertop.pid")))))
      ;; (simple-service 'powertop shepherd-root-service-type
      ;;                 (list ))
      (service nix-service-type
               (nix-configuration
                (extra-config
                 '("substituters = https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store/ https://cache.nixos.org/"))))
      ;;(service dnsmasq-service-type)
      (service earlyoom-service-type
               (earlyoom-configuration
                (avoid-regexp "emacs")
                (prefer-regexp "icecat|chromium")))
      (service zram-device-service-type)
      (service ipfs-service-type)
      (set-xorg-configuration
       (xorg-configuration
        (keyboard-layout keyboard-layout)) sddm-service-type)
      (service sddm-service-type
               (sddm-configuration
                (theme "chili")))
      (delete (service gdm-service-type)
              (modify-services %desktop-services
                ;; (network-manager-service-type config =>
                ;;                               (network-manager-configuration
                ;;                                (inherit config)
                ;;                                (dns "none")))
                (sysctl-service-type config =>
                                     (sysctl-configuration
                                      (inherit config)
                                      (settings (cons* '("net.ipv4.ip_forward" . "1")
                                                       (sysctl-configuration-settings config)))))
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
