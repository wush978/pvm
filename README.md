# Package Version Manager for R

## Status

- Linux and OS X: [![Travis-ci Status](https://travis-ci.org/wush978/pvm.svg?branch=master)](https://travis-ci.org/wush978/pvm)
- Windows : [![Build status](https://ci.appveyor.com/api/projects/status/it85uq9rlya3f07t/branch/master?svg=true)](https://ci.appveyor.com/project/wush978/pvm/branch/master)


[![Coverage Status](https://img.shields.io/coveralls/wush978/FeatureHashing.svg)](https://coveralls.io/r/wush978/FeatureHashing?branch=master)

## Feature

- Export the version of installed packages to a simple [YAML](https://en.wikipedia.org/wiki/YAML) file via `export.packages`.
- Install the exported packages with given version and their dependencies from CRAN(latest or source) or [MRAN](https://mran.revolutionanalytics.com/)(outdated binary)
  according to the YAML file via `import.packages`.

## Install

```r
install.packages("remotes")
remotes::install_github("wush978/pvm")
```

## Usage

In the original workspace:

```r
pvm::export.packages()
```

- It will check the dependencies and export to `pvm.yml`.
- The exported yaml, the `pvm.yml`, file should be like this:
```
__version__: 0.2
1:
  proto: 1.0.0
  rstudioapi: 0.5
```
- Only version is specified if you want to install the package from CRAN/MRAN
- If you have non-CRAN packages, please modify the content of `pvm.yml`.
    - Change the repository from CRAN to the appropriate specification. Please see [#non-cran-repositories] for examples.
    - Some `DESCRIPTION` files of the local package will include the non-CRAN info. `export.packages` will automatically export
them as non-CRAN.



Then go to another workspace:

```r
pvm::import.packages()
```

- Install the packages via the followign rules
    - Search the binary/source package from CRAN/MRAN if the version is specified in the YAML. The users could specify if they want to install them from `source` or `both`.
    - List all non-listed dependencies and install the latest version of them from CRAN
    - Install the specified packages in YAML with the specified version from CRAN/MRAN/non-CRAN repositories

## Non-CRAN Repositories

Here are examples of specifications of this package:

- github: `github::repo=wush978/slidifyLibraries@422a92cd51a15a224d4be3f9d239012f4ffce874`

That is to say, you should see the following paragraph in `pvm.yml` if you export a non-CRAN `pvm` package:

```yml
  slidifyLibraries: github::repo=wush978/slidifyLibraries@422a92cd51a15a224d4be3f9d239012f4ffce874
```

During `import.packages`, `pvm` will install the package from github via `remotes::install_github("wush978/pvm@master")`.
