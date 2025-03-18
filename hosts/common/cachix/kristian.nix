{
  nix = {
    settings.substituters = [
      "https://kristian.cachix.org"
    ];
    settings.trusted-public-keys = [
      "kristian.cachix.org-1:I9DC4coD583Us0HFeCW6KiiXbk6uhlWfB2MPxjJiiUI="
    ];
  };
}
