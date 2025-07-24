{
  description = "Raylib development environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs =
    { self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
      };
      pakages = with pkgs; [
        gcc
        odin
        raylib
        glfw
        xorg.libX11
        xorg.libXrandr
        xorg.libXinerama
        xorg.libXcursor
        xorg.libXi
        xorg.xeyes
        mesa
        libglvnd
      ];
    in
    {
      devShells."${system}".default = pkgs.mkShell {
        nativeBuildInputs = pakages;

        shellHook = ''
          export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath pakages}:$LD_LIBRARY_PATH
          export LIBGL_ALWAYS_SOFTWARE=1
          export DISPLAY=:0
          export XDG_SESSION_TYPE=x11
          export GDK_BACKEND=wayland
          export SDL_VIDEODRIVER=wayland
          echo "Odin environment running"
          fish
        '';
      };

    };
}
