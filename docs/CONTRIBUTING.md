# Contributing to rendehR

Bug reports, questions and pull requests are welcome.

- **Patient data.** Never put MIMIC or eICU rows, identifiers or record
  screenshots in issues, pull requests, tests or examples; the PhysioNet
  data use agreements forbid it. Tests and examples use the open demo
  datasets.
- **Issues.** Include a small example that reproduces the problem, the
  data source and version, and
  [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html).
- **Pull requests.** Open them against `main`. Keep one change per pull
  request, add a test for new behaviour (`tests/testthat`), update the
  roxygen documentation (`devtools::document()`) and add a line to
  `NEWS.md`. `devtools::check()` should pass without errors or warnings.
- **Style.** Follow the style of the surrounding code (tidyverse style
  guide, two-space indent, `snake_case`).

By contributing you agree that your contribution is licensed under the
GPL-3, the licence of the package. Participation follows the [Code of
Conduct](https://ate-driven-lab.github.io/rendehR/CODE_OF_CONDUCT.md).
