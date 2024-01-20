{ ... }:
{
  programs.nushell = {
    enable = true;
    extraConfig = ''
      $env.config = {
        show_banner: false
        edit_mode: vi

        cursor_shape: {
          vi_insert: line
          vi_normal: block
        }

        hooks: {
          pre_prompt: [{ ||
            if (which direnv | is-empty) {
              return
            }
            direnv export json | from json | default {} | load-env
          }]
        }
      }

      def gtree [] { git ls-tree -r --name-only HEAD | tree --fromfile }

    '';
  };
}
