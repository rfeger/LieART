(* ::Package:: *)

PacletObject[
  <|
    "Name" -> "LieARTTeam/LieART",
    "Description" -> "Tools for Lie Algebras and Representation Theory",
    "Creator" -> "Robert Feger, Thomas Kephart and Robert Saskowski",
    "License" -> "LGPL-3.0-only",
    "PublisherID" -> "LieARTTeam",
    "Version" -> "2.1.1",
    "WolframVersion" -> "11.3+",
    "PrimaryContext" -> "LieARTTeam`LieART`",
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
