# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Sign-extended operations of the `SGNJ` family
- Input operand NaN-boxing checks according to RISC-V ISA v2.2

### Fixed

- Comparisons for negative values as well as zeroes
- Comparisons for equality of NaNs now properly return false
- Updated integer result handling for conversions to reflect changes in RISC-V ISA v2.3draft
- Conversion status flags for rounded-to-zero results
- Updated `FMIN_MAX` instructions to reflect changes in RISC-V ISA v2.3draft

## [0.1.0] - 2018-04-13

### Added

- FPnew with support for all standard operations
