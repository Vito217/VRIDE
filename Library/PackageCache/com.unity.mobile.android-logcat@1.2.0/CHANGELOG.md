# Changelog
All notable changes to this package will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [1.2.0] - 2020-08-18

### Fix & Improvements.
 - Correctly resolve top activity when device is locked.
 - Improved Stacktrace Utility, it's easier to set symbol paths and regexes.
 - Consola font is now selectable in Logcat settings.
 - Android Logcat per project settings are saved in ProjectSettings directory.
 - Fix issue where sometimes Android Logcat would stop working if USB cable is unplugged and replugged.
 - Properly save/restore Android Logcat settings, previously settings like tags were being lost during domain reload or Editor restart.
 - Added Capture button in Capture Screen window, also capturing screen no longer will lock Unity thread.
 - Fix issue where incorrect date format in incoming log message would break whole log parsing.
 - Moved Stacktrace Utility, Capture Screen, Open Terminal under Tools menu.
 - The package list will automatically clean itself, if there's more than 5 exited packages in the list;
 - Improved documentation.
 - Added Clear button in internal log window.
 - Minimum Unity version was raised to 2019.2. The reason was to drop .NET 3.5 support.
 - 'Enter IP' window got renamed to 'Other connection options.
 - In the device selection list, you'll also able to see disconnected and unauthorized devices for informational purposes.
 - 'Other connection options' window has a Disconnect button for devices connected via Network.
 - Added device selection in Screen Capture window
### Memory Window
 - Introduced a window for viewing application memory in real time, more information in the docs.

## [1.1.1] - 2020-03-12

### Fix & Improvements.
 - Fix warnings in scripts when active Editor platform is not Android.
 - Fix regex issues with logcat messages.

## [1.1.0] - 2020-02-14

### Fix & Improvements.
 - Added feature 'Filter by process id'
 - Fixed addr2line functionality, when we try to resolve stacktrace
 - Correctly open Terminal on macOS Catalina
 - Fix Open Terminal button not working on Windows sometimes.
 - Reworked Connect to IP window, it's now multithreaded, thus it will not lock Editor. It's now easier to connect to Android device via IP.
 - Added icons for messages
 - Added disconnect button, you can stop logcat messages this way.
 - Improved mouse right click behavior to be consistent with the rest of Unity
 - Correctly identify Android 9 version
 - Android Settings will have color settings separated between Free skin and Pro skin.
 - Moved Android Settingsunder Preferences->Analysis
 - Provide proper windows title for Stacktrace Utility window
 - Right clicking log lines behavior will be consistent with other Unity windows.
 - Ctrl/CMD + C will copy log lines correctly.
 - Column seperators will be drawn correctly
### Android Logcat Settings 
 - Introducing settings, accessible from Preferences. For ex., on Windows Edit->Preferences
 
## [1.0.0] - 2019-07-18

### Fix & Improvements.
- Added Open Terminal button
- Use monospace font for displaying log messages, this makes text align properly when displaying addresses
- Fix performance issues whene there's no Android device attached, the device querying will happen on worker thread.
- Fix Regex filter functionality, on newer devices it wasn't working as intended.
- Fix appearance of Delele button in Tag control window.
### Stacktrace Utility
- Add a custom way of resolving stacktraces, read more about in the documentation

## [0.2.7-preview] - 2019-04-24

### Minor fixes.
- Fixed the issues found during package validation.

## [0.2.6-preview] - 2019-04-15

### Minor fixes.
- Fixed some issues about tag window.
- Restored the states including current selected device, current selected package, tags, priority after closing and launching Unity Editor.

## [0.2.5-preview] - 2019-04-01

### Fixes & Improvements.
- UI improvements:
  - Add borders for columns.
  - Display odd/even background for message entries.
  - Fixed search bar issue.
  - Fix the issue that screenshot is cropped.
  - Add a simple tag control window to manipulate tags.
- Fixed the issue that search filter doesn't work on Android 6 or lower.
- Fixed the issue that tag filter doesn't work with some corner cases like "SSRM:k".
- Restored the states including current selected device, current selected package, tags, priority after closing and reopening the window.
- Fixed some issues in tags, like not allowing adding empty tag, setting tag length limitation to 23, etc.

## [0.2.1-preview] - 2019-01-22

### Fixes & Improvements.
- Made package compatible with .NET 3.5.
- Fixed the wrong year issue in the log message.
- Use "Cmd" instead of "Ctrl" for shortcuts on macOS.
- Fixed the issue that some log messages can't be parsed correctly.

## [0.2.0-preview] - 2018-12-18

### Fixes & Improvements.
- Show proper messages if the scripting runtime version is not .Net 4.x or the active platform is not Android.
- Added some shortcuts to copy, save logs etc.
- Fixed the "grep not fonud" issue on some old 4.x devices.

## [0.1.5-preview] - 2018-11-26

### Minor fixes.
- Fixed the issue during publishing the package.

## [0.1.4-preview] - 2018-11-26

### Minor fixes.
- Fixed the issue that "shift" selecting log messages doesn't work correctly.

## [0.1.3-preview] - 2018-11-16

### Fixes & Improvements.
- Fixed the issue that package filter doesn't work correctly on devices below Android 7.
- Fixed some issues at UI side.

## [0.1.2-preview] - 2018-10-30

### Minor fixes & Improvements.
- Added documentation.
- Fixed the issue that Android Logcat Package doesn't work with devices below Android 7.

## [0.1.1-preview] - 2018-10-22

### Minor fixes.

## [0.1.0-preview] - 2018-10-15

### This is the first release of *Android Logcat Package*.
