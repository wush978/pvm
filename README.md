# Package Version Manager for R

## Feature

- Record the version of installed packages (listed by `installed.packages`) to an [YAML](https://en.wikipedia.org/wiki/YAML) file.
    - The repository property instructs how R should install the specific package. The possible values are:
        - `CRAN`: R will install the package (with the specific version) from CRAN or MRAN (if the binary package is missing on CRAN)
        - non-CRAN repository specification. Package [remotes](https://github.com/mangothecat/remotes) will be used.
            
- Install the package of the specific version from CRAN(source) or MRAN(out dated binaries)
  according to the YAML file. It will search MRAN automatically according to the data from <https://github.com/wush978/metamran>

## Install

```r
install.packages("remotes") # Only required if you want to use pvm with non-CRAN packages
remotes::install_github("wush978/pvm")
```

## Usage

In the original workspace:

```r
pvm::export.packages()
```

It will check the dependencies and export to `pvm.yml`.
Modify the content if you have non-CRAN packages. 
Change the repository from CRAN to `<username>/<reponame>#<commit-ish>` if it is in github.
See <https://github.com/r-pkgs/remotes#dependencies> for more specifications of non-CRAN repositories.

Then go to another workspace:

```r
pvm::import.packages()
```

All packages shall be install from CRAN or other place
