# Changelog for TIE

## Version 2.1.1

- Bugfix ([#35](https://github.com/notquiteamonad/tie/issues/35)): TIE no longer generates
  definitions for `Maybe` values including `void` as an option, as Elm would reject such values.

## Version 2.1.0

- ([#34](https://github.com/notquiteamonad/tie/issues/34)) When in watch mode,
  TIE will now regenerate definition if .TIE.toml changes
  (the previous behaviour was to just regenerate when .elm files changed).

## Version 2.0.0

- **BREAKING**: The `--colour` or `--color` flag must now be passed to get coloured output. This flag is not available for Windows.
- Add support for Windows

## Version 1.1.1

- Bugfix: TIE no longer incorrectly reads non-record type aliases split over multiple lines

## Version 1.1.0

- Add the ability to [override generated types](https://github.com/notquiteamonad/tie/blob/main/vault/Configuration.md)

## Version 1.0.5

- Minor documentation changes

## Version 1.0.4

- Manually link GitHub autolinks in README

## Version 1.0.3

- Fixed README (properly this time!)

## Version 1.0.2

- Fixed README

## Version 1.0.1

- Added README

## Version 1.0.0

- Initial release
