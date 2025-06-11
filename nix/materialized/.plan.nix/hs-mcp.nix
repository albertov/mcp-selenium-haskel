{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "2.4";
      identifier = { name = "hs-mcp"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "Public Domain";
      maintainer = "\"drop 8 $ reverse<moc.liamg@gnikceub+maps_on>\"";
      author = "Bryan Buecking";
      homepage = "https://github.com/buecking/hs-mcp";
      url = "";
      synopsis = "Haskell implementation of Model Context Protocol (MCP)";
      description = "A Haskell implementation of the Model Context Protocol (MCP) for connecting AI models to tools and data sources.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
        ];
        buildable = true;
        modules = [
          "Network/MCP/Types"
          "Network/MCP/Transport/Types"
          "Network/MCP/Transport/StdIO"
          "Network/MCP/Client"
          "Network/MCP/Client/Request"
          "Network/MCP/Client/Types"
          "Network/MCP/Server"
          "Network/MCP/Server/Types"
          "Network/MCP/Server/StdIO"
        ];
        hsSourceDirs = [ "src" ];
      };
      exes = {
        "mcp-echo-server" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hs-mcp" or (errorHandler.buildDepError "hs-mcp"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
          buildable = true;
          hsSourceDirs = [ "." ];
          mainPath = [ "app/Main.hs" ];
        };
        "mcp-client" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hs-mcp" or (errorHandler.buildDepError "hs-mcp"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
          buildable = true;
          hsSourceDirs = [ "." ];
          mainPath = [ "app/Client.hs" ];
        };
      };
      tests = {
        "mcp-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hs-mcp" or (errorHandler.buildDepError "hs-mcp"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
          buildable = true;
          hsSourceDirs = [ "." ];
          mainPath = [ "Test/Main.hs" ];
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchgit {
      url = "0";
      rev = "minimal";
      sha256 = "";
    }) // {
      url = "0";
      rev = "minimal";
      sha256 = "";
    };
  }