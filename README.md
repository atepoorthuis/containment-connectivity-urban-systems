
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Compendium Package for Containment and Connectivity in Dutch Urban Systems

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/atepoorthuis/containment-connectivity-urban-systems/master?urlpath=rstudio)
[![DOI](https://zenodo.org/badge/220612499.svg)](https://zenodo.org/badge/latestdoi/220612499)

This repository contains the data and code for our paper:

> Poorthuis, A and M. van Meeteren, (2019). *Containment and
> connectivity in Dutch urban systems: A network-analytical
> operationalization of the three-systems model*. Tijdschrift voor
> Economische en Sociale Geografie. Online at
> <https://doi.org/10.1111/tesg.12391>

Our pre-print is online here:

> Poorthuis, A and M. van Meeteren, (2019). *Containment and
> connectivity in Dutch urban systems: A network-analytical
> operationalization of the three-systems model*. Tijdschrift voor
> Economische en Sociale Geografie, Accessed 20 Nov 2019. Online at
> <https://doi.org/10.31235/osf.io/y7dxf>

This repository contains all the data and code needed to reproduce the
results and figures in our
    paper.

  - [analysis/01-prepare-network-data.Rmd](analysis/01-prepare-network-data.md):
    Prepare network data and perform community detection
  - [analysis/02-analysis-figures.Rmd](analysis/02-analysis-figures.md):
    Process the community detection results for the different urban
    systems and create the figures included in the paper.

All necessary data can be found in `analysis/data/raw_data/`.

### How to download or install

You can download the compendium as a zip from from [this
URL](https://github.com/atepoorthuis/containment-connectivity-urban-systems/archive/master.zip)

Or you can install this compendium as an R package,
tesgcontainmentconnectivity, from GitHub with:

``` r
# install.packages("devtools")
remotes::install_github("atepoorthuis/containment-connectivity-urban-systems")
```

### Licenses

Text + figures and data:
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

Code: See the [DESCRIPTION](DESCRIPTION) file
