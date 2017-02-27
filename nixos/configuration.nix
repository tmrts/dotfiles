# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ];

  boot = {
    tmpOnTmpfs = true;

    kernelPackages = pkgs.linuxPackages_latest;

    loader.grub = {
      enable = true;
      version = 2;
      device = "/dev/sda";
    };
  };

  networking = {
    hostName = "forge";

    wireless.enable = false;
    networkmanager.enable = true;

    firewall = {
      enable = true;
      allowPing = false;
    };

    # Uses the ad blocking DNS proxy
    nameservers = [ "127.0.0.1" "8.8.8.8" "4.4.4.4" ];
  };

  # Internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  fonts = {
    enableCoreFonts = true;
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      inconsolata
      ubuntu_font_family
      powerline-fonts
      font-awesome-ttf
      source-code-pro
    ];
    fontconfig = {
      dpi = 96;
    };
  };

  # Set your time zone.
  time.timeZone = "Europe/Amsterdam";

  nixpkgs.config = {
    # Allow unfree packages
    allowUnfree = true;

    chromium = {
      enablePepperFlash = true;
      enablePepperPDF = true;
    };
  };

  environment.systemPackages = with pkgs; [
    avfs bc gcc_multi debootstrap gnupg sqlite

    google-cloud-sdk

    crawlTiles tome4
    (dwarf-fortress.override {
      theme = dwarf-fortress-packages.phoebus-theme;
    })

    socat curl jq
    bind file tree
    fzf which fish

    lsof htop iotop iftop

    mercurialFull gitAndTools.gitFull
    nix-prefetch-git nix-prefetch-scripts

    rkt acbuild etcd docker
    gcc openssl glibc.out glibc.static autoreconfHook gnupg1 trousers cpio systemd wget acl squashfsTools automake autoconf cmake gnumake

    go_1_7 hugo glide
    python36 python36Packages.pytest
    R
    nodejs

    imagemagick
    rubber ghostscript
    texlive.combined.scheme-full graphviz pandoc octave libsndfile

    emacs tmux neovim

    zathura epdfview

    spotify
    electron chromium qutebrowser

    xsel
    libnotify
    xorg.xprop
    xorg.xmodmap
    xorg.xdpyinfo
    xorg.xwininfo

    feh gimp scrot ffmpeg screenfetch

    playerctl mpv

    weechat ranger transmission

    compton-git
    conky bar-xft dzen2
    dmenu rofi
    termite rxvt_unicode

    xmonad-with-packages
    haskellPackages.xmobar

    stow ag
    unrar zip unzip p7zip

    gnome3.gtk numix-gtk-theme

    # Mouse hiding
    unclutter
  ];

  systemd = {
    services = {
      dnsBlackholeDaemon = {
        description = "Blackhole DNS proxy that filters advertisement servers";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        serviceConfig = {
          Restart = "always";
          WorkingDirectory = "/root/grim";
          ExecStart = ''/root/grim/grimd -update'';
          PIDFile = "/var/run/grimd/grimd.pid";
          LimitNOFILE = "4096";
        };
      };
    };

    user.services = {
      offlineimap = {
        description = "Offlineimap: a software to sync mailboxes as local Maildirs";
        serviceConfig = {
          #Type = "forking";
          ExecStart = "${pkgs.offlineimap}/bin/offlineimap -u syslog -o";
        };
      };
      tmuxDaemon = {
        wantedBy = [ "default.target" ];
        serviceConfig = {
          Type = "forking";
          Restart = "always";
          ExecStart = ''${pkgs.tmux}/bin/tmux new-session --daemon'';
          ExecStop = ''${pkgs.tmux}/bin/tmux kill-server'';
        };
      };
    };
    user.timers = {
      offlineimap.timerConfig = {
        Unit = "offlineimap.service";
        OnCalendar = "*:0/10";
      };
    };
  };

  services = {
    # Enable the OpenSSH daemon.
    openssh.enable = true;

    # Refresh locatedb
    locate = {
      enable = true;
      interval = "12:12";
    };

    # NixOS/nixpkgs#16327
    emacs = {
      enable = true;
      install = true;
      defaultEditor = true;
    };

    dovecot2 = {
      enable = true;
      enablePop3 = false;
      enableImap = true;
      mailLocation = "maildir:/home/%u/Mail:LAYOUT=fs";
    };

    xserver = {
      enable = true;
      layout = "us";
      xkbOptions = "eurosign:e";

      videoDrivers = [ "ati" ];
      xrandrHeads = [ "DVI-0" "DVI-1" ];
      resolutions = [ { x = 1920; y = 1080; } ];

      windowManager = {
        xmonad.enable = true;
        xmonad.enableContribAndExtras = true;
      };

      desktopManager = {
        default = "";
        xterm.enable = false;
      };

      displayManager = {
        slim = {
          enable = true;
          autoNumlock = true;
          defaultUser = "tmrts";
        };
      };
    };
  };

  users.extraUsers.tmrts = {
    isNormalUser = true;
    home = "/home/tmrts";
    extraGroups = [ "audio" "disk" "wheel" "networkmanager" ];
  };

  swapDevices = [ { device = "/dev/nixos/swap"; } ];

  programs = {
    bash.enableCompletion = true;
  };

  security = {
    sudo.enable = true;
  };

  # Explicit NixOS release version to be compatible with stateful apps such as
  # databases.
  system.stateVersion = "17.03";

  nixpkgs.system = "x86_64-linux";
}
