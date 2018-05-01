# Change Log
All notable changes to thsi project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]
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
- Due day abbriviations supported
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
