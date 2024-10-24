{ ... }:
{
  programs.jujutsu = {
    enable = true;

    settings = {
      user = {
        name = "Kristian Nedrevold-Hansen";
        email = "kristian@krined.no";
      };

      ui = {
        color = "auto";
        editor = "nvim";
      };
    };
  };
}
