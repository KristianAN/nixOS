{ config, lib, ... }:
lib.mkIf config.programs.ewm.enable {
  programs.ewm.emacsPackage =
    config.home-manager.users.kristian.programs.emacs.finalPackage;
}
