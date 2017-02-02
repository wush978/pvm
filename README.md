# R Package Version Manager

## Feature

- Record the version of installed packages (listed by `installed.packages`) to an [YAML](https://en.wikipedia.org/wiki/YAML) file.
    - The repository property instructs how R should install the specific package. The possible values are:
        - `CRAN`: R will install the package (with the specific version) from CRAN or MRAN (if the binary package is missing on CRAN)
        - non-CRAN repository specification. Package [remotes](https://github.com/mangothecat/remotes) will be used.
            
- Install the package of the specific version from CRAN(source) or MRAN(binary)
  according to the JSON file

## Install

```r
install.packages("remotes")
remotes::install_github("wush978/rpvm")
```
