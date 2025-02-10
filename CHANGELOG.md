# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [2.1.1] - 2025-02-06
### Fixed
- Fix related bugs in irrep conjugacy, which treated some real irreps as complex, especially in SO(8). (reported and fixed by Matt Mitchell)
- Fix centering in SpindleShape for Mathematica version 14.

## [2.1.0] - 2022-03
### Added
- New commands DynkinDiagram and ExtendedDynkinDiagram.
### Changed
- Revise command WeightDiagram.

## [2.0.3] - 2022-02
### Fixed
- Fix command U1Charge[].

## [2.0.2] - 2020-08
### Fixed
- Fix normalization of U(1) charges for SO(2n)->SU(n)xU(1) to be consistent with Slansky.

## [2.0.1] - 2020-06
### Removed
- Remove several nonmaximal branchings.
### Added
- Add several missing branching rules, including Sp(4N+2)->SO(2N+1)xSU(2), Sp(14)->Sp(6), SO(27)->Sp(8), and SO(28)->SO(8).
- New option BasicIrrepsOnly for TensorProductsTable to consider the tensor product of basic irreps only.
- New predefined algebra short forms, especially SO32.
### Changed
- Change default of TraditionalForm to apply to current notebook only.

## [2.0.0] - 2019-06
### Fixed
- Compatibility with Mathematica version 12.
### Added
- New branching rules for regular and special maximal subalgebras.

## [1.1.7] - 2015-04
### Added
- New branching rule for Sp(N)->SU(N/2)xU(1) (requested by Kevin Ferreira).

## [1.1.6] - 2015-01
### Added
- New commands IrrepMultiplicity and CasimirInvariant (suggestion of Daniel Boer).

## [1.1.5] - 2014-08
### Fixed
- Fix bug in E7 and E8 branching rules due to different behavior of Permute in Mathematica 8 compared to Mathematica 9 and 10. All earlier E7 and E8 branching-rules bugs had the same origin.

## [1.1.4] - 2014-08
### Fixed
- Fix bug in the branching rules for E7->A5xA2.

## [1.1.3] - 2014-07
### Fixed
- Fix bug in DecomposeProduct using Klimyk's formula.

## [1.1.2] - 2014-07
### Fixed
- Fix bug in the branching rules for E8->A4xA4.

## [1.1.1] - 2014-02
### Fixed
- Fix bug in the branching rules for E7->A5xA2.

## [1.1.0] - 2014-01
### Changed
- Cosmetic changes.

## [1.0.9] - 2012-08
### Changed
- New algorithm for DecomposeProduct based on Klimyk's formula.

## [1.0.1] - 2012-07
### Fixed
- Fix bug in DecomposeProduct for ProductIrreps.

## [1.0.0] - 2012-06
### Added
- Initial Release.