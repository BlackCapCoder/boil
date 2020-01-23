let make-package-yaml
  =
    λ ( settings : ./settings.dhall ) →

{ default-extensions
    = ./extensions.dhall

, dependencies
    = ./dependencies.dhall

, name
    = settings.name

, verbatim
    = { cabal-version = 2.4
      }

, library
    = { source-dirs = "src"
      }
}

    in
      make-package-yaml
