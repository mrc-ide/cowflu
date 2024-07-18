# cowflu

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![R build status](https://github.com/mrc-ide/cowflu/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/cowflu/actions/workflows/R-CMD-check.yaml)
[![codecov.io](https://codecov.io/github/mrc-ide/cowflu/coverage.svg?branch=main)](https://codecov.io/github/mrc-ide/cowflu?branch=main)
<!-- badges: end -->

## Installation

To install `cowflu`:

```r
remotes::install_github("mrc-ide/cowflu", upgrade = FALSE)
```

## Development

You will need `cpp11`, `decor`, and `brio` installed too.

1. Make changes to the model at [`inst/dust/cows.cpp`]
2. Run `dust2::dust_package(".")`
3. Run the tests with `devtools::test()` (once they exist!)

If you are set up with a command line, you can `make src/cows.cpp` instead of the second step.

## License

MIT © Imperial College of Science, Technology and Medicine
