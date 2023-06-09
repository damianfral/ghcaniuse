# GHCanIUse

## Overview

GHCanIUse is a tool that generates an HTML page with a table of supported
language extensions for different versions of GHC (Glasgow Haskell Compiler). It
helps Haskell developers explore the language extensions available in various
GHC versions and provides quick access to the corresponding documentation.

## Usage

```bash
xdg-open $(nix build --print-out-paths)/index.html
```

## How It Works

GHCanIUse leverages Nix to fetch specific GHC versions. By using the GHC
`--supported-languages` flag, it retrieves the list of supported language
extensions for each GHC version.

After obtaining the supported language extensions, GHCanIUse scrapes the web
user guide for each GHC version to extract the documentation links associated
with the language extensions.

The gathered information is then used to generate an HTML page that presents a
table with the supported language extensions for each GHC version. The table
includes links to the corresponding documentation pages.
