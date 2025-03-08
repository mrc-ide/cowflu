# cowflu <img src='R/figures/logo.png' align="right" height="159" />

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![R build status](https://github.com/mrc-ide/cowflu/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/cowflu/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This software package accompanies the manuscript "A mathematical model of H5N1 influenza transmission in US dairy cattle" (Rawson et al., 2025).

The package provides a flexible interface for building SEIR meta-population models of infectious disease dynamics, which we primarily use for modelling HPAI H5N1 in US dairy cattle, as seen in our associated vignettes.

The underlying model code can be found at [`inst/dust/cows.cpp`].

Fitting the model, as seen in our vignettes, can make use of traditional MCMC or particle MCMC methods via the `dust2` and `monty` packages.



## Installation

To install `cowflu`:

```r
remotes::install_github("mrc-ide/cowflu", upgrade = FALSE)
```

Typical install time is < 30 seconds.

## Development

You will need `cpp11`, `decor`, and `brio` installed too.

1. Make changes to the model at [`inst/dust/cows.cpp`]
2. Run `dust2::dust_package(".")`
3. Run the tests with `devtools::test()` (once they exist!)

If you are set up with a command line, you can `make src/cows.cpp` instead of the second step.

## License

MIT © Imperial College of Science, Technology and Medicine
