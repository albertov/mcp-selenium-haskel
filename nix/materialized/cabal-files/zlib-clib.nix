{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "2.4";
      identifier = { name = "zlib-clib"; version = "1.3.1"; };
      license = "Zlib";
      copyright = "1995-2024 Jean-loup Gailly and Mark Adler";
      maintainer = "Duncan Coutts <duncan@community.haskell.org>, Andrew Lelechenko <andrew.lelechenko@gmail.com>, Emily Pillmore <emilypi@cohomolo.gy>, Herbert Valerio Riedel <hvr@gnu.org>";
      author = "";
      homepage = "";
      url = "";
      synopsis = "zlib C library bits";
      description = "This package provides the zlib C library bits that Haskell\nbindings can use to link against.";
      buildType = "Simple";
    };
    components = { "library" = { buildable = true; }; };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/zlib-clib-1.3.1.tar.gz";
      sha256 = "76ba8fa213dd451134419e3809cbd66f64dda7f37fd979fee7c6e38d78a36a94";
    });
  }) // {
    package-description-override = "cabal-version:   2.4\nname:            zlib-clib\nversion:         1.3.1\n\ncopyright:       1995-2024 Jean-loup Gailly and Mark Adler\nlicense:         Zlib\nlicense-file:    LICENSE\nmaintainer:      Duncan Coutts <duncan@community.haskell.org>, Andrew Lelechenko <andrew.lelechenko@gmail.com>, Emily Pillmore <emilypi@cohomolo.gy>, Herbert Valerio Riedel <hvr@gnu.org>\nbug-reports:     https://github.com/haskell/zlib/issues\ncategory:        Codec\nsynopsis:        zlib C library bits\ndescription:     This package provides the zlib C library bits that Haskell\n                 bindings can use to link against.\nbuild-type:      Simple\nextra-doc-files: changelog.md\n                 README.md\n\ntested-with:     GHC == 8.0.2\n               , GHC == 8.2.2\n               , GHC == 8.4.4\n               , GHC == 8.6.5\n               , GHC == 8.8.4\n               , GHC == 8.10.7\n               , GHC == 9.0.2\n               , GHC == 9.2.8\n               , GHC == 9.4.8\n               , GHC == 9.6.5\n               , GHC == 9.8.2\n               , GHC == 9.10.1\n\nsource-repository head\n  type: git\n  location: https://github.com/haskell/zlib.git\n\nlibrary\n  default-language: Haskell2010\n\n  include-dirs: cbits\n  includes: zlib.h\n\n  c-sources: cbits/adler32.c\n             cbits/compress.c\n             cbits/crc32.c\n             cbits/deflate.c\n             cbits/infback.c\n             cbits/inffast.c\n             cbits/inflate.c\n             cbits/inftrees.c\n             cbits/trees.c\n             cbits/uncompr.c\n             cbits/zutil.c\n\n  install-includes:\n     crc32.h\n     inffast.h\n     inflate.h\n     trees.h\n     deflate.h\n     inffixed.h\n     inftrees.h\n     zutil.h\n     gzguts.h\n     zlib.h\n     zconf.h\n\n";
  }