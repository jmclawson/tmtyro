# Download a file once

`download_once()` checks for a local copy of a file found online. If a
copy doesn't exist, it downloads a copy for local use.

## Usage

``` r
download_once(url, filename = NULL, destdir = "data")
```

## Arguments

- url:

  The URL of an online document.

- filename:

  The file name to be saved locally. In many cases this parameter isn't
  necessary, since the file name can automatically be parsed from the
  URL, but some web addresses will obscure it.

- destdir:

  The destination directory to save the file. By default, this is the
  "data/" folder, which will be created if it doesn't yet exist.

## Value

Path to the local file, returned invisibly

## Examples

``` r
if (FALSE) { # \dontrun{
download_once("example.com/sample.csv")
} # }
```
