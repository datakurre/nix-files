{
  xdg.desktopEntries = {
    lock-session = {
      name = "Lock Session";
      genericName = "Screen Locker";
      exec = "loginctl lock-session";
      terminal = false;
      categories = [
        "System"
        "Utility"
      ];
      icon = "system-lock-screen";
    };
  };
}
