{ ... }:
{
  programs.jujutsu = {
    enable = true;

    settings = {
      user = {
        name = "Kristian Nedrevold-Hansen";
        email = "kristiananh@proton.me";
      };

      ui = {
        color = "auto";
        editor = "emacsclient";
        paginate = "never";
        diff-formatter = ":git";
        conflict-marker-style = "git";
      };
    };
  };
}
