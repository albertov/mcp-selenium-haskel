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
      specVersion = "3.0";
      identifier = { name = "mcp-selenium"; version = "0.1.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "alberto@toscat.net";
      author = "Alberto Valverde";
      homepage = "https://github.com/albertov/mcp-selenium-haskell";
      url = "";
      synopsis = "A Haskell implementation of MCP Selenium Server using WebDriver";
      description = "A Model Context Protocol (MCP) server implementation for Selenium WebDriver in Haskell,\nenabling browser automation through standardized MCP clients like Claude.\n.\nFeatures include:\n- Start browser sessions with customizable options\n- Navigate to URLs and interact with page elements\n- Find elements using various locator strategies\n- Click, type, and interact with elements\n- Perform mouse actions (hover, drag and drop)\n- Take screenshots\n- Support for headless mode\n- Console logs functionality\n- Support for Chrome and Firefox browsers\n\nThis project is heavily inspired by https://github.com/angiejones/mcp-selenium";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.md" "CHANGELOG.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-qq" or (errorHandler.buildDepError "aeson-qq"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
          (hsPkgs."webdriver" or (errorHandler.buildDepError "webdriver"))
          (hsPkgs."hs-mcp" or (errorHandler.buildDepError "hs-mcp"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."raw-strings-qq" or (errorHandler.buildDepError "raw-strings-qq"))
        ];
        buildable = true;
        modules = [
          "MCP/Selenium/Server"
          "MCP/Selenium/Tools"
          "MCP/Selenium/Utils"
          "MCP/Selenium/WebDriver"
        ];
        hsSourceDirs = [ "src" ];
      };
      exes = {
        "mcp-selenium-hs" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."mcp-selenium" or (errorHandler.buildDepError "mcp-selenium"))
          ];
          buildable = true;
          hsSourceDirs = [ "app" ];
          mainPath = [ "Main.hs" ];
        };
      };
      tests = {
        "mcp-selenium-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."mcp-selenium" or (errorHandler.buildDepError "mcp-selenium"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ];
          buildable = true;
          modules = [ "SessionTest" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Test.hs" ];
        };
      };
    };
  } // rec { src = pkgs.lib.mkDefault .././.; }