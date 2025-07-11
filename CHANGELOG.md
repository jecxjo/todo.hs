# Change Log
All notable changes to thsi project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## Unreleased

## [0.5.0] - 2025-07-10
### Added
- New `burndown` command that is terminal aware

### Fixed
- Fixed `burndown` command to include the archive file

## [0.4.9] - 2025-07-08
### Fixed
- Upgraded the stack version to the latest LTS
- Some cleanup of the code to have more strict compile rules
- Some cleanup of the unit tests to remove `test-fixture`

## [0.4.8] - 2022-02-16
### Added
- Added minimum priority optional argument to "standup" command
- Added numbering to standup
- Added "yesterday" command to mark completed tasks you forgot about
- Scripts get access to called todo

### Fixed
- Indexing fails when no addon directory set
- Standup should honor thresholds

## [0.4.7] - 2022-01-23
### Added
- Added scripting support

### Changed
- Updated README

## [0.4.6] - 2022-01-19
### Added
- Support for colored output
- Added debug command

### Changed
- Prompt's display of task supports color output

## [0.4.5] - 2021-01-13
### Changed
- Lint fixes
- Bumped Stackage release to LTS-16.29

## [0.4.3] - 2018-10-28
### Added
- Added "license" and "changelog" commands, prints relative files
- Added threshold support
- Added "standup" command, prints what you did yesterday and what is due today
- Added "today" command, printing tasks due today sorted by "at" time
- Added support for "at" time

### Changed
- Cleaned up compile time warnings
- Fixed bug (temporarily) in Control.Monad.TestFixture, pointing at changes until upstream publishes
- Requires stack version >= 1.7
- Moved 'todo.cabal' to ignore list, new stack uses yaml and autogenerates todo.cabal
- Fixed Key/Value pair to support non alpha-numeric characters in value

## [0.4.2] - 2018-05-02
### Added
- Added "help" with search terms
- Added "repeat" command to repeat a task
- Added description of todo.txt in "help todo.txt"

### Changed
- 'addx' command supports '-p' prompt

## [0.4.1] - 2018-04-29
### Added
- '-p' flag to prompt on all task changing commands
- Added "addx" command to add completed tasks

### Changed
- Contexts support GTD "@@" and "@\_" names

## [0.4.0] - 2018-04-18
### Changed
- Moved to an mtl style design

## [0.3.1] - 2018-01-30
### Added
- List an individual item using its number
- Projects command does pretty printing based on projects of pending

## [0.3.0] - 2017-06-14
### Added
- Regular expression based searches
- Regular expression based swaps

## [0.2.1] - 2017-04-30
### Changed
- Fixed Version Numbers
- Cleaned up all Compile Warnings

## [0.2.0] - 2017-04-27
### Added
- Due day abbreviations supported
- Added archive command
- Added report command

### Changed
- List command now case insensitive

## [0.1.0] - 2017-04-08
### Added
- Prepend and Replace commands
- List command now searches strings
- Added better error messages

### Changed
- Fixed indexing for priority command
- Handles empty todo.txt file (lots of newlines and no text)

## [0.0.2] - 2017-04-05
### Added
- Auto timestamping
- Modify priorities
- Smart Due Dates
- Sort based on due date

### Changed
- Ordering of tasks fixed
- Completed tasks stored in todo.txt and not deleted

## [0.0.1] - 2017-03-01
### Added
- Basic Parser and Command Line Interface
- Unit Tests
