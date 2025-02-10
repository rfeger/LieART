(* ::Package:: *)

PacletObject[
  <|
    "Name" -> "LieART",
    "Description" -> "Tools for Lie Algebras and Representation Theory",
    "Creator" -> "Robert Feger",
    "License" -> "LGPL-3.0-only",
    "PublisherID" -> "RobertFeger",
    "Version" -> "2.1.1",
    "WolframVersion" -> "11.3+",
    "PrimaryContext" -> "LieART`",
    "DocumentationURL" -> "https://resources.wolframcloud.com/PacletRepository/resources",
    "Extensions" -> {
      {
        "Kernel",
        "Root" -> "Kernel",
        "Context" -> {"LieART`", "BranchingRules`", "Tables`"}
      },
      {
        "Documentation",
        "Root" -> "Documentation",
        "Language" -> "English",
        "MainPage" -> "Guides/LieART"
      },
      {
        "Asset",
        "Root" -> "Latex",
        "Assets" -> {{"latex_package", "lieart.sty"}}
      },
      {
        "Asset",
        "Root" -> ".",
        "Assets" -> {
          {"changelog", "CHANGELOG.md"},
          {"readme", "README.md"},
          {"gpl", "COPYING"},
          {"lgpl", "COPYING.LESSER"}
        }
      },
      {
        "Asset",
        "Root" -> "Documentation/English/Tables",
        "Assets" -> {
          {"IrrepProperties", "IrrepProperties.nb"},
          {"TensorProducts", "TensorProducts.nb"},
          {"BranchingRules", "BranchingRules.nb"}
        }
      }
    }
  |>
]
