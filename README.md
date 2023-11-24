
<!-- README.md is generated from README.Rmd. Please edit that file -->

# predipreda <img src="man/figures/package-sticker.png" align="right" style="float:right; height:120px;"/>

<!-- badges: start -->
<!-- badges: end -->

## Table of contents

<p align="left">
• <a href="#overview">Overview</a><br> •
<a href="#features">Features</a><br> •
<a href="#installation">Installation</a><br> •
<a href="#get-started">Get started</a><br> •
<a href="#citation">Citation</a><br> •
<a href="#contributing">Contributing</a><br> •
<a href="#acknowledgments">Acknowledgments</a><br> •
<a href="#references">References</a>
</p>

## Overview

The R package `predipreda` is a machine learning-based package made to
learn **predation** interactions within ecosystems at various scale and
then infer predation interactions in a new environment. It use the
**Random Forest** algorithm, implemented with the R `ranger` package.
The package implement an **optimization function**, which return as many
model metric indicators as you want to test Random Forest option
combination. It help then to select the best Random Forest option
combination to infer new predation interactions. The cost function used
to assess the best Random Forest option combination is the **root mean
squared error**.

## Features

The main purpose of `predipreda` is to test many option combination of
the Random Forest algorithm. It includes various objects the user can
modify to choose the option she/he wants to test :

- the **weight** you give to the predation interaction (e.g. give more
  weight to prey than non-prey (no interactions recorded)).

- the **eigen vector** data derived from phylogenetic trees.

- an **hyperparameter** dataframe which includes classical Random Forest
  parameter in addition to the weight and eigen vector.

Finally, the package return a dataframe with all Random Forest option
combination associated to a value of root rean squared error.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
## Install < remotes > package (if not already installed) ----
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

## Install < predipreda > from GitHub ----
# remotes::install_github("MartPhilip/predipreda")
```

Then you can attach the package `predipreda`:

``` r
library("predipreda")
```

## Get started

For an overview of the main features of `predipreda`, please read the
[Get
started](https://MartPhilip.github.io/predipreda/articles/predipreda.html)
vignette.

## Citation

Please cite `predipreda` as:

> PHILIPPE-LESAFFRE Martin (2023) predipreda: An R package to **{{ TITLE
> }}**. R package version 0.0.0.9000.
> <https://github.com/MartPhilip/predipreda/>

## Contributing

All types of contributions are encouraged and valued. For more
information, check out our [Contributor
Guidelines](https://github.com/CyrilHaute/predipreda/blob/main/CONTRIBUTING.md).

Please note that the `predipreda` project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## Acknowledgments

**{{ OPTIONAL SECTION }}**

## References

**{{ OPTIONAL SECTION }}**
