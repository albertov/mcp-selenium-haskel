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
      specVersion = "1.12";
      identifier = { name = "webdriver"; version = "0.12.0.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Tom McLaughlin <tom@codedown.io>";
      author = "Adam Curtis <kallisti.dev@gmail.com>";
      homepage = "https://github.com/haskell-webdriver/haskell-webdriver#readme";
      url = "";
      synopsis = "a Haskell client for the Selenium WebDriver protocol";
      description = "A Selenium WebDriver client for Haskell.\nYou can use it to automate browser sessions\nfor testing, system administration, etc.\n\nFor more information about Selenium itself, see\n<http://seleniumhq.org/>\n\nTo find out what's been changed in this version and others,\nsee the change log at\n<https://github.com/haskell-webdriver/haskell-webdriver/blob/main/CHANGELOG.md>";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."attoparsec-aeson" or (errorHandler.buildDepError "attoparsec-aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."call-stack" or (errorHandler.buildDepError "call-stack"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."directory-tree" or (errorHandler.buildDepError "directory-tree"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."zip-archive" or (errorHandler.buildDepError "zip-archive"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/webdriver-0.12.0.1.tar.gz";
      sha256 = "8112ce9719d2b097d6e987b03c623ac6c441e315e9b12abac5174b1a5c6da49e";
    });
  }) // {
    package-description-override = "cabal-version: 1.12\n\n-- This file has been generated from package.yaml by hpack version 0.37.0.\n--\n-- see: https://github.com/sol/hpack\n\nname:           webdriver\nversion:        0.12.0.1\nsynopsis:       a Haskell client for the Selenium WebDriver protocol\ndescription:    A Selenium WebDriver client for Haskell.\n                You can use it to automate browser sessions\n                for testing, system administration, etc.\n                .\n                For more information about Selenium itself, see\n                <http://seleniumhq.org/>\n                .\n                To find out what's been changed in this version and others,\n                see the change log at\n                <https://github.com/haskell-webdriver/haskell-webdriver/blob/main/CHANGELOG.md>\ncategory:       Web, Browser, Testing, WebDriver, Selenium\nhomepage:       https://github.com/haskell-webdriver/haskell-webdriver#readme\nbug-reports:    https://github.com/haskell-webdriver/haskell-webdriver/issues\nauthor:         Adam Curtis <kallisti.dev@gmail.com>\nmaintainer:     Tom McLaughlin <tom@codedown.io>\nlicense:        BSD3\nlicense-file:   LICENSE\nbuild-type:     Simple\ntested-with:\n    GHC == 8.6.5\n  , GHC == 8.8.4\n  , GHC == 8.10.7\n  , GHC == 9.0.2\n  , GHC == 9.2.7\n  , GHC == 9.4.4\nextra-source-files:\n    README.md\n    CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: https://github.com/haskell-webdriver/haskell-webdriver\n\nlibrary\n  exposed-modules:\n      Test.WebDriver\n      Test.WebDriver.Capabilities\n      Test.WebDriver.Chrome.Extension\n      Test.WebDriver.Class\n      Test.WebDriver.Commands\n      Test.WebDriver.Commands.Internal\n      Test.WebDriver.Commands.Wait\n      Test.WebDriver.Common.Keys\n      Test.WebDriver.Common.Profile\n      Test.WebDriver.Config\n      Test.WebDriver.Cookies\n      Test.WebDriver.Exceptions\n      Test.WebDriver.Exceptions.Internal\n      Test.WebDriver.Firefox.Profile\n      Test.WebDriver.Internal\n      Test.WebDriver.JSON\n      Test.WebDriver.Monad\n      Test.WebDriver.Session\n      Test.WebDriver.Session.History\n      Test.WebDriver.Types\n      Test.WebDriver.Utils\n  other-modules:\n      Test.WebDriver.IO\n  hs-source-dirs:\n      src\n  default-extensions:\n      ScopedTypeVariables\n      OverloadedStrings\n      FlexibleContexts\n      FlexibleInstances\n      RecordWildCards\n      NamedFieldPuns\n  ghc-options: -Wall\n  build-depends:\n      aeson >=0.6.2.0\n    , attoparsec >=0.10\n    , attoparsec-aeson >=2\n    , base ==4.*\n    , base64-bytestring >=1.0\n    , bytestring >=0.9\n    , call-stack\n    , data-default\n    , directory >1.0\n    , directory-tree >=0.11\n    , exceptions >=0.4\n    , filepath >1.0\n    , http-client >=0.3\n    , http-types >=0.8\n    , lifted-base >=0.1\n    , monad-control >=0.3\n    , network >=2.6\n    , network-uri >=2.6\n    , scientific >=0.2\n    , temporary >=1.0\n    , text >=0.11.3\n    , time >1.0\n    , transformers >=0.4\n    , transformers-base >=0.1\n    , unordered-containers >=0.1.3\n    , vector >=0.3\n    , zip-archive >=0.1.1.8\n  default-language: Haskell2010\n";
  }