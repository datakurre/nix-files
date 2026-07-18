{ ... }:
{
  programs.git = {
    enable = true;
    signing = {
      key = "5A9D4532";
      signByDefault = true;
    };
    settings = {
      user = {
        email = "asko.soukka@iki.fi";
        name = "Asko Soukka";
      };
      github.user = "datakurre";
      push.default = "current";
      apply.whitespace = "nowarn";
      core.autocrlf = "input";
      gpg.program = "gpg";
      init.defaultBranch = "main";
      pull.rebase = true;
      color = {
        ui = true;
        branch = {
          current = "yellow reverse";
          local = "yellow";
          remote = "green";
        };
        diff = {
          meta = "yellow bold";
          frag = "magenta bold";
          old = "red bold";
          new = "green bold";
        };
        status = {
          added = "yellow";
          changed = "green";
          untracked = "cyan";
        };
      };
    };
  };
}
