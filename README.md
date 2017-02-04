# Package Version Manager for R

## Feature

- Export the version of installed packages to an [YAML](https://en.wikipedia.org/wiki/YAML) file via `export.packages`.
- Install the exported packages with given version from CRAN(latest or source) or [MRAN](https://mran.revolutionanalytics.com/)(outdated binary)
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
- If you have non-CRAN packages, please modify the content of `pvm.yml`.
    - Change the repository from CRAN to the appropriate specification. Please see [#non-cran-repositories] for examples.

Then go to another workspace:

```r
pvm::import.packages()
```

- All latest packages will be installed from CRAN
- On Windows and OS X, `pvm` will search MRAN for binaries of outdated packages and try to install them.
- If there is no appropriate binary package on MRAN, then `pvm` will install the source package from CRAN. Then the appropriate build tools might be required.
- Non-CRAN packages will be installed via corresponding `remotes::install_?` function.

## Non-CRAN Repositories

Here are examples of specifications of this package:

- github: `github::wush978/pvm`
- bitbucket: `bitbucket::wush978/pvm`
- url: `url::https://github.com/wush978/pvm/archive/master.zip`
- svn: `svn::svn://github.com/wush978/pvm/trunk`
- git: `git::git://github.com/wush978/pvm.git`

That is to say, you should see the following paragraph in `pvm.yml`:

```yml
pvm:
  repository: github::wush978/pvm
  parent: yaml
  priority: .na
  name: pvm
  deps:
  - name: yaml
  version: 0.1.0
```
