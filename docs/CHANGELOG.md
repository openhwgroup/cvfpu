# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

In this sense, we interpret the "Public API" of a hardware module as its port/parameter list.
Versions of the IP in the same major relase are "pin-compatible" with each other. Minor relases are permitted to add new parameters as long as their default bindings ensure backwards compatibility.


## [Unreleased]

### Added
### Changed
### Fixed


## [0.5.5] - 2019-06-02

### Fixed
- UF flag handling according to IEEE754-2008 (#11)


## [0.5.4] - 2019-06-02

### Added
- Documentation about multi-format operations
- Extended pipelining description slightly
- Extended semantic versioning declaration in changelog

### Changed
- Updated diagrams in architecture documentation

### Fixed
- [common_cells] Bumped to fix src_files.yml bugs
- [fpu_div_sqrt_mvp] Bumped to fix linter warnings


## [0.5.3] - 2019-05-31

### Fixed
- ips_list.yml entry for updated common_cells


## [0.5.2] - 2019-05-31

### Fixed
- Internal pipeline bypass in cast unit


## [0.5.1] - 2019-05-27

### Fixed
- Include path for `common_cells` in `src_files.yml`


## [0.5.0] - 2019-05-27

### Added
- The FPU :)
- Initial Documentation

### Changed
- "Restarted" the changelog as the old one was stale

### Fixed
- Handling of exception flags for infinity operands
