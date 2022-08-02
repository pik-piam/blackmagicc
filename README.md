# MAgPIE-MAGICC Integration

R package **blackmagicc**, version **0.1.0**

[![CRAN status](https://www.r-pkg.org/badges/version/blackmagicc)](https://cran.r-project.org/package=blackmagicc)    

## Purpose and Functionality

Enables MAgPIE runs to project global surface area temperatures with MAGICC, using reference REMIND scenarios to represent emissions from the energy sector.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("blackmagicc")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Michael Crawford <crawford@pik-potsdam.de>.

## Citation

To cite package **blackmagicc** in publications use:

Crawford M (2022). _blackmagicc: MAgPIE-MAGICC Integration_. R package version 0.1.0.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {blackmagicc: MAgPIE-MAGICC Integration},
  author = {Michael Crawford},
  year = {2022},
  note = {R package version 0.1.0},
}
```
