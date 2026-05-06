{ inputs, pkgs, ... }:
{

  imports = [
    ./services
    ./programs
  ];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  home = {
    username = "kristian";
    homeDirectory = "/home/kristian";
  };
services.udiskie = {
    enable = true;
    settings = {
        # workaround for
        # https://github.com/nix-community/home-manager/issues/632
        program_options = {
            # replace with your favorite file manager
            file_manager = "${pkgs.nemo-with-extensions}/bin/nemo";
        };
    };
};
  home.packages = [
    pkgs.nemo-with-extensions
    pkgs.nixfmt-tree
    pkgs.ladybird
    pkgs.virt-manager
    pkgs.dconf
    pkgs.wdisplays
    pkgs.wev
    pkgs.swaylock-effects
    pkgs.digikam
    pkgs.wf-recorder
    pkgs.slurp
    pkgs.mpv
    pkgs.sway-contrib.grimshot
    pkgs.texliveFull
    pkgs.brightnessctl
    pkgs.pandoc
    pkgs.any-nix-shell
    pkgs.cmatrix
    pkgs.proton-pass
    pkgs.hunspell
    pkgs.hunspellDicts.en_GB-ise
    pkgs.hunspellDicts.nb_NO
    pkgs.hunspellDicts.nn_NO
    pkgs.playerctl
    inputs.hell.packages.${pkgs.system}.default
    pkgs.jq
    pkgs.teams-for-linux
  ];

  programs.home-manager.enable = true;

  home.sessionVariables.EDITOR = "emacsclient";

  programs.gh = {
    enable = true;
  };

  programs.neovim = {
    enable = true;
    extraConfig = ''
      set path+=**
      set wildignorecase
    '';
  };
  
  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  services.swaync.enable = true;

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "26.05";

}
