{ stdenv
, maven
, jdk
, src
}:
stdenv.mkDerivation {
  inherit src;
  name = "java-debug-repo";

  dontConfigure = true;
  buildInputs = [ maven jdk ];
  buildPhase = "${maven}/bin/mvn -Dmaven.repo.local=$out package";

  installPhase = ''
    echo $out

    find $out -type f -name \*.lastUpdated -delete
    find $out -type f -name resolver-status.properties -delete
    find $out -type f -name _remote.repositories -delete
  '';

  dontFixup = true;
  outputHashAlgo = "sha256";
  outputHashMode = "recursive";
  outputHash = "sha256-mllbHNL0eZ8Oa3+xlyuT2+ikROjyiVnF6CC2bBE6uHU=";
}
