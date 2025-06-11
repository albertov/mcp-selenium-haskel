{
  pkgs = hackage:
    {
      packages = {
        filepath.revision = hackage.filepath."1.5.4.0".revisions.default;
        attoparsec.revision = import ./cabal-files/attoparsec.nix;
        attoparsec.flags.developer = false;
        text-iso8601.revision = import ./cabal-files/text-iso8601.nix;
        generically.revision = import ./cabal-files/generically.nix;
        base64-bytestring.revision = import ./cabal-files/base64-bytestring.nix;
        iproute.revision = import ./cabal-files/iproute.nix;
        unordered-containers.revision = import ./cabal-files/unordered-containers.nix;
        unordered-containers.flags.debug = false;
        case-insensitive.revision = import ./cabal-files/case-insensitive.nix;
        transformers-base.revision = import ./cabal-files/transformers-base.nix;
        transformers-base.flags.orphaninstances = true;
        OneTuple.revision = import ./cabal-files/OneTuple.nix;
        tasty.revision = import ./cabal-files/tasty.nix;
        tasty.flags.unix = true;
        data-fix.revision = import ./cabal-files/data-fix.nix;
        HUnit.revision = import ./cabal-files/HUnit.nix;
        appar.revision = import ./cabal-files/appar.nix;
        haskell-lexer.revision = import ./cabal-files/haskell-lexer.nix;
        uuid-types.revision = import ./cabal-files/uuid-types.nix;
        quickcheck-io.revision = import ./cabal-files/quickcheck-io.nix;
        vector-stream.revision = import ./cabal-files/vector-stream.nix;
        comonad.revision = import ./cabal-files/comonad.nix;
        comonad.flags.containers = true;
        comonad.flags.indexed-traversable = true;
        comonad.flags.distributive = true;
        ghc-bignum.revision = hackage.ghc-bignum."1.3".revisions.default;
        stm.revision = hackage.stm."2.5.3.1".revisions.default;
        blaze-builder.revision = import ./cabal-files/blaze-builder.nix;
        integer-conversion.revision = import ./cabal-files/integer-conversion.nix;
        transformers.revision = hackage.transformers."0.6.1.1".revisions.default;
        distributive.revision = import ./cabal-files/distributive.nix;
        distributive.flags.semigroups = true;
        distributive.flags.tagged = true;
        deepseq.revision = hackage.deepseq."1.5.0.0".revisions.default;
        optparse-applicative.revision = import ./cabal-files/optparse-applicative.nix;
        optparse-applicative.flags.process = true;
        webdriver.revision = import ./cabal-files/webdriver.nix;
        streaming-commons.revision = import ./cabal-files/streaming-commons.nix;
        streaming-commons.flags.use-bytestring-builder = false;
        directory.revision = hackage.directory."1.3.8.5".revisions.default;
        lifted-base.revision = import ./cabal-files/lifted-base.nix;
        parsec.revision = hackage.parsec."3.1.18.0".revisions.default;
        Cabal.revision = hackage.Cabal."3.12.1.0".revisions.default;
        strict.revision = import ./cabal-files/strict.nix;
        cryptohash-sha1.revision = import ./cabal-files/cryptohash-sha1.nix;
        th-abstraction.revision = import ./cabal-files/th-abstraction.nix;
        cookie.revision = import ./cabal-files/cookie.nix;
        uuid.revision = import ./cabal-files/uuid.nix;
        mtl.revision = hackage.mtl."2.3.1".revisions.default;
        ansi-terminal-types.revision = import ./cabal-files/ansi-terminal-types.nix;
        process.revision = hackage.process."1.6.25.0".revisions.default;
        base.revision = hackage.base."4.20.1.0".revisions.default;
        haskell-src-exts.revision = import ./cabal-files/haskell-src-exts.nix;
        call-stack.revision = import ./cabal-files/call-stack.nix;
        aeson-qq.revision = import ./cabal-files/aeson-qq.nix;
        base-orphans.revision = import ./cabal-files/base-orphans.nix;
        QuickCheck.revision = import ./cabal-files/QuickCheck.nix;
        QuickCheck.flags.old-random = false;
        QuickCheck.flags.templatehaskell = true;
        hspec.revision = import ./cabal-files/hspec.nix;
        Cabal-syntax.revision = hackage.Cabal-syntax."3.12.1.0".revisions.default;
        haskell-src-meta.revision = import ./cabal-files/haskell-src-meta.nix;
        monad-control.revision = import ./cabal-files/monad-control.nix;
        network-uri.revision = import ./cabal-files/network-uri.nix;
        indexed-traversable-instances.revision = import ./cabal-files/indexed-traversable-instances.nix;
        raw-strings-qq.revision = import ./cabal-files/raw-strings-qq.nix;
        these.revision = import ./cabal-files/these.nix;
        random.revision = import ./cabal-files/random.nix;
        async.revision = import ./cabal-files/async.nix;
        async.flags.bench = false;
        bifunctors.revision = import ./cabal-files/bifunctors.nix;
        bifunctors.flags.tagged = true;
        text.revision = hackage.text."2.1.2".revisions.default;
        time.revision = hackage.time."1.12.2".revisions.default;
        array.revision = hackage.array."0.5.8.0".revisions.default;
        data-default-class.revision = import ./cabal-files/data-default-class.nix;
        tf-random.revision = import ./cabal-files/tf-random.nix;
        semialign.revision = import ./cabal-files/semialign.nix;
        semialign.flags.semigroupoids = true;
        hspec-discover.revision = import ./cabal-files/hspec-discover.nix;
        text-short.revision = import ./cabal-files/text-short.nix;
        text-short.flags.asserts = false;
        semigroupoids.revision = import ./cabal-files/semigroupoids.nix;
        semigroupoids.flags.unordered-containers = true;
        semigroupoids.flags.containers = true;
        semigroupoids.flags.comonad = true;
        semigroupoids.flags.tagged = true;
        semigroupoids.flags.contravariant = true;
        semigroupoids.flags.distributive = true;
        byteorder.revision = import ./cabal-files/byteorder.nix;
        entropy.revision = import ./cabal-files/entropy.nix;
        entropy.flags.donotgetentropy = false;
        contravariant.revision = import ./cabal-files/contravariant.nix;
        contravariant.flags.semigroups = true;
        contravariant.flags.statevar = true;
        contravariant.flags.tagged = true;
        StateVar.revision = import ./cabal-files/StateVar.nix;
        network.revision = import ./cabal-files/network.nix;
        network.flags.devel = false;
        hspec-core.revision = import ./cabal-files/hspec-core.nix;
        happy.revision = import ./cabal-files/happy.nix;
        indexed-traversable.revision = import ./cabal-files/indexed-traversable.nix;
        hashable.revision = import ./cabal-files/hashable.nix;
        hashable.flags.random-initial-seed = false;
        hashable.flags.arch-native = false;
        zip-archive.revision = import ./cabal-files/zip-archive.nix;
        zip-archive.flags.executable = false;
        character-ps.revision = import ./cabal-files/character-ps.nix;
        th-compat.revision = import ./cabal-files/th-compat.nix;
        http-client.revision = import ./cabal-files/http-client.nix;
        http-client.flags.network-uri = true;
        syb.revision = import ./cabal-files/syb.nix;
        mime-types.revision = import ./cabal-files/mime-types.nix;
        ghc-internal.revision = hackage.ghc-internal."9.1002.0".revisions.default;
        digest.revision = import ./cabal-files/digest.nix;
        digest.flags.have_weak_getauxval = false;
        digest.flags.have_mm_prefetch = false;
        digest.flags.have_arm64_crc32c = false;
        digest.flags.have_sse42 = false;
        digest.flags.pkg-config = true;
        digest.flags.have_strong_getauxval = false;
        digest.flags.have_builtin_prefetch = false;
        binary.revision = hackage.binary."0.8.9.3".revisions.default;
        template-haskell.revision = hackage.template-haskell."2.22.0.0".revisions.default;
        th-reify-many.revision = import ./cabal-files/th-reify-many.nix;
        unix.revision = hackage.unix."2.8.6.0".revisions.default;
        primitive.revision = import ./cabal-files/primitive.nix;
        aeson.revision = import ./cabal-files/aeson.nix;
        aeson.flags.ordered-keymap = true;
        ansi-terminal.revision = import ./cabal-files/ansi-terminal.nix;
        ansi-terminal.flags.example = false;
        hsc2hs.revision = import ./cabal-files/hsc2hs.nix;
        hsc2hs.flags.in-ghc-tree = false;
        colour.revision = import ./cabal-files/colour.nix;
        exceptions.revision = hackage.exceptions."0.10.9".revisions.default;
        bytestring.revision = hackage.bytestring."0.12.2.0".revisions.default;
        witherable.revision = import ./cabal-files/witherable.nix;
        attoparsec-aeson.revision = import ./cabal-files/attoparsec-aeson.nix;
        base-compat.revision = import ./cabal-files/base-compat.nix;
        network-info.revision = import ./cabal-files/network-info.nix;
        integer-logarithms.revision = import ./cabal-files/integer-logarithms.nix;
        integer-logarithms.flags.integer-gmp = true;
        integer-logarithms.flags.check-bounds = false;
        time-compat.revision = import ./cabal-files/time-compat.nix;
        zlib-clib.revision = import ./cabal-files/zlib-clib.nix;
        tagged.revision = import ./cabal-files/tagged.nix;
        tagged.flags.transformers = true;
        tagged.flags.deepseq = true;
        ghc-boot-th.revision = hackage.ghc-boot-th."9.10.2".revisions.default;
        safe.revision = import ./cabal-files/safe.nix;
        os-string.revision = hackage.os-string."2.0.4".revisions.default;
        data-default.revision = import ./cabal-files/data-default.nix;
        transformers-compat.revision = import ./cabal-files/transformers-compat.nix;
        transformers-compat.flags.three = false;
        transformers-compat.flags.four = false;
        transformers-compat.flags.five-three = true;
        transformers-compat.flags.mtl = true;
        transformers-compat.flags.generic-deriving = true;
        transformers-compat.flags.two = false;
        transformers-compat.flags.five = false;
        temporary.revision = import ./cabal-files/temporary.nix;
        prettyprinter.revision = import ./cabal-files/prettyprinter.nix;
        prettyprinter.flags.buildreadme = false;
        prettyprinter.flags.text = true;
        assoc.revision = import ./cabal-files/assoc.nix;
        assoc.flags.tagged = false;
        ghc-prim.revision = hackage.ghc-prim."0.12.0".revisions.default;
        happy-lib.revision = import ./cabal-files/happy-lib.nix;
        th-expand-syns.revision = import ./cabal-files/th-expand-syns.nix;
        hspec-expectations.revision = import ./cabal-files/hspec-expectations.nix;
        pretty.revision = hackage.pretty."1.1.3.6".revisions.default;
        zlib.revision = import ./cabal-files/zlib.nix;
        zlib.flags.bundled-c-zlib = true;
        zlib.flags.non-blocking-ffi = true;
        zlib.flags.pkg-config = true;
        splitmix.revision = import ./cabal-files/splitmix.nix;
        splitmix.flags.optimised-mixer = false;
        containers.revision = hackage.containers."0.7".revisions.default;
        th-orphans.revision = import ./cabal-files/th-orphans.nix;
        directory-tree.revision = import ./cabal-files/directory-tree.nix;
        prettyprinter-ansi-terminal.revision = import ./cabal-files/prettyprinter-ansi-terminal.nix;
        scientific.revision = import ./cabal-files/scientific.nix;
        scientific.flags.integer-simple = false;
        vector.revision = import ./cabal-files/vector.nix;
        vector.flags.internalchecks = false;
        vector.flags.boundschecks = true;
        vector.flags.wall = false;
        vector.flags.unsafechecks = false;
        http-types.revision = import ./cabal-files/http-types.nix;
        dlist.revision = import ./cabal-files/dlist.nix;
        dlist.flags.werror = false;
        cryptohash-md5.revision = import ./cabal-files/cryptohash-md5.nix;
        th-lift.revision = import ./cabal-files/th-lift.nix;
      };
      compiler = {
        version = "9.10.2";
        nix-name = "ghc9102";
        packages = {
          "ghc-boot-th" = "9.10.2";
          "binary" = "0.8.9.3";
          "pretty" = "1.1.3.6";
          "array" = "0.5.8.0";
          "time" = "1.12.2";
          "ghc-prim" = "0.12.0";
          "bytestring" = "0.12.2.0";
          "process" = "1.6.25.0";
          "mtl" = "2.3.1";
          "text" = "2.1.2";
          "template-haskell" = "2.22.0.0";
          "parsec" = "3.1.18.0";
          "ghc-bignum" = "1.3";
          "stm" = "2.5.3.1";
          "Cabal" = "3.12.1.0";
          "filepath" = "1.5.4.0";
          "os-string" = "2.0.4";
          "unix" = "2.8.6.0";
          "exceptions" = "0.10.9";
          "deepseq" = "1.5.0.0";
          "Cabal-syntax" = "3.12.1.0";
          "transformers" = "0.6.1.1";
          "containers" = "0.7";
          "ghc-internal" = "9.1002.0";
          "base" = "4.20.1.0";
          "directory" = "1.3.8.5";
        };
      };
    };
  extras = hackage:
    {
      packages = {
        hs-mcp = ./.plan.nix/hs-mcp.nix;
        mcp-selenium = ./.plan.nix/mcp-selenium.nix;
      };
    };
  modules = [
    {
      preExistingPkgs = [
        "filepath"
        "ghc-bignum"
        "stm"
        "transformers"
        "deepseq"
        "directory"
        "parsec"
        "Cabal"
        "mtl"
        "process"
        "base"
        "Cabal-syntax"
        "text"
        "time"
        "array"
        "ghc-internal"
        "binary"
        "template-haskell"
        "unix"
        "exceptions"
        "bytestring"
        "ghc-boot-th"
        "os-string"
        "ghc-prim"
        "pretty"
        "containers"
      ];
    }
    ({ lib, ... }:
      {
        packages = {
          "hs-mcp" = { flags = {}; };
          "mcp-selenium" = { flags = {}; };
        };
      })
    ({ lib, ... }:
      {
        packages = {
          "data-fix".components.library.planned = lib.mkOverride 900 true;
          "Cabal-syntax".components.library.planned = lib.mkOverride 900 true;
          "tf-random".components.library.planned = lib.mkOverride 900 true;
          "integer-conversion".components.library.planned = lib.mkOverride 900 true;
          "quickcheck-io".components.library.planned = lib.mkOverride 900 true;
          "base64-bytestring".components.library.planned = lib.mkOverride 900 true;
          "attoparsec".components.sublibs."attoparsec-internal".planned = lib.mkOverride 900 true;
          "http-types".components.library.planned = lib.mkOverride 900 true;
          "iproute".components.library.planned = lib.mkOverride 900 true;
          "aeson-qq".components.library.planned = lib.mkOverride 900 true;
          "transformers-base".components.library.planned = lib.mkOverride 900 true;
          "hspec-discover".components.library.planned = lib.mkOverride 900 true;
          "hashable".components.library.planned = lib.mkOverride 900 true;
          "template-haskell".components.library.planned = lib.mkOverride 900 true;
          "text-short".components.library.planned = lib.mkOverride 900 true;
          "network-uri".components.library.planned = lib.mkOverride 900 true;
          "hs-mcp".components.exes."mcp-echo-server".planned = lib.mkOverride 900 true;
          "unordered-containers".components.library.planned = lib.mkOverride 900 true;
          "these".components.library.planned = lib.mkOverride 900 true;
          "QuickCheck".components.library.planned = lib.mkOverride 900 true;
          "distributive".components.library.planned = lib.mkOverride 900 true;
          "transformers".components.library.planned = lib.mkOverride 900 true;
          "hspec-core".components.library.planned = lib.mkOverride 900 true;
          "case-insensitive".components.library.planned = lib.mkOverride 900 true;
          "happy-lib".components.sublibs."tabular".planned = lib.mkOverride 900 true;
          "th-expand-syns".components.library.planned = lib.mkOverride 900 true;
          "scientific".components.library.planned = lib.mkOverride 900 true;
          "blaze-builder".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "happy-lib".components.sublibs."backend-glr".planned = lib.mkOverride 900 true;
          "unix".components.library.planned = lib.mkOverride 900 true;
          "base-orphans".components.library.planned = lib.mkOverride 900 true;
          "call-stack".components.library.planned = lib.mkOverride 900 true;
          "exceptions".components.library.planned = lib.mkOverride 900 true;
          "data-default-class".components.library.planned = lib.mkOverride 900 true;
          "directory".components.library.planned = lib.mkOverride 900 true;
          "HUnit".components.library.planned = lib.mkOverride 900 true;
          "syb".components.library.planned = lib.mkOverride 900 true;
          "semigroupoids".components.library.planned = lib.mkOverride 900 true;
          "indexed-traversable-instances".components.library.planned = lib.mkOverride 900 true;
          "happy-lib".components.sublibs."frontend".planned = lib.mkOverride 900 true;
          "lifted-base".components.library.planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          "parsec".components.library.planned = lib.mkOverride 900 true;
          "monad-control".components.library.planned = lib.mkOverride 900 true;
          "semialign".components.library.planned = lib.mkOverride 900 true;
          "random".components.library.planned = lib.mkOverride 900 true;
          "directory-tree".components.library.planned = lib.mkOverride 900 true;
          "raw-strings-qq".components.library.planned = lib.mkOverride 900 true;
          "th-reify-many".components.library.planned = lib.mkOverride 900 true;
          "hs-mcp".components.library.planned = lib.mkOverride 900 true;
          "dlist".components.library.planned = lib.mkOverride 900 true;
          "cryptohash-md5".components.library.planned = lib.mkOverride 900 true;
          "zlib-clib".components.library.planned = lib.mkOverride 900 true;
          "aeson".components.library.planned = lib.mkOverride 900 true;
          "attoparsec-aeson".components.library.planned = lib.mkOverride 900 true;
          "Cabal".components.library.planned = lib.mkOverride 900 true;
          "prettyprinter".components.library.planned = lib.mkOverride 900 true;
          "http-client".components.library.planned = lib.mkOverride 900 true;
          "happy-lib".components.library.planned = lib.mkOverride 900 true;
          "hspec-discover".components.exes."hspec-discover".planned = lib.mkOverride 900 true;
          "transformers-compat".components.library.planned = lib.mkOverride 900 true;
          "entropy".components.library.planned = lib.mkOverride 900 true;
          "cookie".components.library.planned = lib.mkOverride 900 true;
          "time-compat".components.library.planned = lib.mkOverride 900 true;
          "hspec".components.library.planned = lib.mkOverride 900 true;
          "StateVar".components.library.planned = lib.mkOverride 900 true;
          "stm".components.library.planned = lib.mkOverride 900 true;
          "haskell-src-exts".components.library.planned = lib.mkOverride 900 true;
          "network-info".components.library.planned = lib.mkOverride 900 true;
          "strict".components.library.planned = lib.mkOverride 900 true;
          "happy-lib".components.sublibs."grammar".planned = lib.mkOverride 900 true;
          "network".components.library.planned = lib.mkOverride 900 true;
          "comonad".components.library.planned = lib.mkOverride 900 true;
          "bytestring".components.library.planned = lib.mkOverride 900 true;
          "streaming-commons".components.library.planned = lib.mkOverride 900 true;
          "attoparsec".components.library.planned = lib.mkOverride 900 true;
          "colour".components.library.planned = lib.mkOverride 900 true;
          "integer-logarithms".components.library.planned = lib.mkOverride 900 true;
          "base-compat".components.library.planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "haskell-src-meta".components.library.planned = lib.mkOverride 900 true;
          "entropy".components.setup.planned = lib.mkOverride 900 true;
          "uuid".components.library.planned = lib.mkOverride 900 true;
          "optparse-applicative".components.library.planned = lib.mkOverride 900 true;
          "tagged".components.library.planned = lib.mkOverride 900 true;
          "zlib".components.library.planned = lib.mkOverride 900 true;
          "filepath".components.library.planned = lib.mkOverride 900 true;
          "assoc".components.library.planned = lib.mkOverride 900 true;
          "OneTuple".components.library.planned = lib.mkOverride 900 true;
          "hs-mcp".components.exes."mcp-client".planned = lib.mkOverride 900 true;
          "witherable".components.library.planned = lib.mkOverride 900 true;
          "happy".components.exes."happy".planned = lib.mkOverride 900 true;
          "cryptohash-sha1".components.library.planned = lib.mkOverride 900 true;
          "ghc-internal".components.library.planned = lib.mkOverride 900 true;
          "time".components.library.planned = lib.mkOverride 900 true;
          "primitive".components.library.planned = lib.mkOverride 900 true;
          "ghc-bignum".components.library.planned = lib.mkOverride 900 true;
          "pretty".components.library.planned = lib.mkOverride 900 true;
          "th-compat".components.library.planned = lib.mkOverride 900 true;
          "zip-archive".components.library.planned = lib.mkOverride 900 true;
          "haskell-lexer".components.library.planned = lib.mkOverride 900 true;
          "th-orphans".components.library.planned = lib.mkOverride 900 true;
          "character-ps".components.library.planned = lib.mkOverride 900 true;
          "os-string".components.library.planned = lib.mkOverride 900 true;
          "mcp-selenium".components.tests."mcp-selenium-test".planned = lib.mkOverride 900 true;
          "prettyprinter-ansi-terminal".components.library.planned = lib.mkOverride 900 true;
          "th-abstraction".components.library.planned = lib.mkOverride 900 true;
          "appar".components.library.planned = lib.mkOverride 900 true;
          "mcp-selenium".components.library.planned = lib.mkOverride 900 true;
          "safe".components.library.planned = lib.mkOverride 900 true;
          "th-lift".components.library.planned = lib.mkOverride 900 true;
          "mtl".components.library.planned = lib.mkOverride 900 true;
          "bifunctors".components.library.planned = lib.mkOverride 900 true;
          "uuid-types".components.library.planned = lib.mkOverride 900 true;
          "vector".components.sublibs."benchmarks-O2".planned = lib.mkOverride 900 true;
          "mime-types".components.library.planned = lib.mkOverride 900 true;
          "text-iso8601".components.library.planned = lib.mkOverride 900 true;
          "binary".components.library.planned = lib.mkOverride 900 true;
          "ansi-terminal-types".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot-th".components.library.planned = lib.mkOverride 900 true;
          "data-default".components.library.planned = lib.mkOverride 900 true;
          "mcp-selenium".components.exes."mcp-selenium-hs".planned = lib.mkOverride 900 true;
          "digest".components.library.planned = lib.mkOverride 900 true;
          "ansi-terminal".components.library.planned = lib.mkOverride 900 true;
          "hspec-expectations".components.library.planned = lib.mkOverride 900 true;
          "happy-lib".components.sublibs."backend-lalr".planned = lib.mkOverride 900 true;
          "webdriver".components.library.planned = lib.mkOverride 900 true;
          "hsc2hs".components.exes."hsc2hs".planned = lib.mkOverride 900 true;
          "vector-stream".components.library.planned = lib.mkOverride 900 true;
          "temporary".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "process".components.library.planned = lib.mkOverride 900 true;
          "vector".components.library.planned = lib.mkOverride 900 true;
          "contravariant".components.library.planned = lib.mkOverride 900 true;
          "tasty".components.library.planned = lib.mkOverride 900 true;
          "byteorder".components.library.planned = lib.mkOverride 900 true;
          "text".components.library.planned = lib.mkOverride 900 true;
          "indexed-traversable".components.library.planned = lib.mkOverride 900 true;
          "generically".components.library.planned = lib.mkOverride 900 true;
          "splitmix".components.library.planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "async".components.library.planned = lib.mkOverride 900 true;
        };
      })
  ];
}