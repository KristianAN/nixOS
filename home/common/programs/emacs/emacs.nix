{ ... }:
let

in
{
  programs.emacs = {
    enable = true;
    extraPackages =
      epkgs: with epkgs; [
        # Evil 
        evil
        evil-collection
        undo-fu
        undo-fu-session
        vim-tab-bar
        evil-visualstar
        evil-surround

        # Native compilation
        # compile-angel
        # Terminal
        vterm
        # Completion et.al.
        orderless
        vertico
        marginalia
        embark
        embark-consult
        consult
        company
        yasnippet

        # Misc
        nerd-icons
        cyberpunk-theme
        ef-themes
        indent-bars

        # Direnv
        envrc

        # Git
        magit

        # Project management
        projectile

        # Treesitter
        treesit-grammars.with-all-grammars
        scala-ts-mode

        # LSP
        lsp-metals
        lsp-mode
        lsp-ui
        dap-mode
      ];

    extraConfig = builtins.readFile ./init.el;
  };

  home.packages =
    [
    ];

  home.file = {
    ".emacs.d" = {
      source = ./.;
      recursive = true;
    };
  };

  services.emacs.enable = true;

}
