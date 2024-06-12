with (import <nixpkgs> {}); let
  unstable = import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/50eb7ecf4cd0a5756d7275c8ba36790e5bd53e33.tar.gz");
  zig-overlay = fetchTarball "https://github.com/mitchellh/zig-overlay/archive/master.tar.gz";
in
  pkgs.mkShell {
    nativeBuildInputs = with pkgs.buildPackages; [
      gdb
      valgrind
      zls
      (import zig-overlay {}).master
    ];
  }
