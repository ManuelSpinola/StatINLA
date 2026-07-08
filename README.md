# StatINLA <img src="inst/app/www/hexsticker_StatINLA.png" align="right" height="138"/>

<!-- badges: start -->
![Version](https://img.shields.io/badge/version-0.1.0-blue)
![License](https://img.shields.io/badge/license-MIT-green)
![StatSuite](https://img.shields.io/badge/StatSuite-ICOMVIS-1170AA)
<!-- badges: end -->

**StatINLA** is an interactive and didactic Shiny application for applied
Bayesian spatial and spatio-temporal modeling using Integrated Nested
Laplace Approximation (INLA), developed at
[ICOMVIS](https://icomvis.una.ac.cr/), Universidad Nacional de Costa Rica.
Part of the **StatSuite** ecosystem.

The app interface is in Spanish, targeting Spanish-speaking students and
researchers in ecology and conservation.

---

## Modules

<!-- BORRADOR: confirmar nombres y alcance final de cada módulo -->

| Module | Description |
|---|---|
| 🧮 **Introduction to INLA** | Laplace approximation, why it's much faster than MCMC |
| 📍 **Point process models (LGCP)** | Log-Gaussian Cox processes for species occurrence data |
| 🕸️ **SPDE spatial fields** | Matérn covariance, mesh construction, spatial random effects |
| ⏳ **Spatio-temporal models** | Extending SPDE random fields over time |
| 🔗 **Integrated species distribution models** | Combining presence-only (GBIF) and presence-absence (eBird) data in a shared spatial field |

---

## R ecosystem

StatINLA is built on:

- [`INLA`](https://www.r-inla.org/) — Integrated Nested Laplace Approximation
- [`inlabru`](https://inlabru-org.github.io/inlabru/) — user-friendly interface to INLA for spatial modeling
- [`fmesher`](https://inlabru-org.github.io/fmesher/) — mesh construction for SPDE models
- [`sf`](https://r-spatial.github.io/sf/) — spatial vector data
- [`terra`](https://rspatial.github.io/terra/) — spatial raster data

---

## Installation

> `INLA` is not on CRAN and must be installed from its own repository
> before installing StatINLA.

```r
# Install INLA
install.packages("INLA",
  repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"),
  dep = TRUE)

# Install StatINLA from GitHub
# install.packages("remotes")
remotes::install_github("ManuelSpinola/StatINLA")
```

## Usage

```r
library(StatINLA)
run_app()
```

---

## Author

**Manuel Spinola** · ICOMVIS, Universidad Nacional de Costa Rica  
✉️ mspinola10@gmail.com

Developed with assistance from **Claude (Anthropic)**.
