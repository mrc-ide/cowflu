# cowflu <img src='R/figures/logo.png' align="right" height="159" />

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![R build status](https://github.com/mrc-ide/cowflu/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/cowflu/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

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
