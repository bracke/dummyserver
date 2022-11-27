# Change Log

All notable changes to this project will be documented in this file.
The format is based on [Keep a Changelog](https://keepachangelog.com/) and this project adheres to [Semantic Versioning](https://semver.org/).

## [1.1.0] - 2022-11-26

### Added

- Added tool base64 to convert binary data into a string that can be used in a configuration file.
- Added tool stringify which prepares a formatted string for inclusion in a configuration file by removing whitespace and escaping certain characters.
- Added "contribute" and  "issues" topics.
- Added example config file for json:api.
- Added -v option to display version and exit.
- Added exception handlers for nicer error messages.

### Changed

- Moved default action into "serve" command.
- Made text output in terminal nicer with coloring and other text effects.
- Changed command line parsing logic  - now uses CLIC extensions.

## [1.0.0] - 2022-11-22

- Initial release.
