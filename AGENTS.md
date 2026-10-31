# AGENTS.md — ParmOff

R package (checkmate-based) for matching arguments to functions (`ParmOff`), applying limits (`ParmLim`) and log transforms (`ParmLog`).

## Layout
- `/home/runner/work/ParmOff/ParmOff/R/` — `ParmOff.R`, `ParmLim.R`, `ParmLog.R` (all exported via `exportPattern` in `NAMESPACE`).
- `/home/runner/work/ParmOff/ParmOff/man/` — Rd files (hand-maintained; keep in sync with function signatures).
- `/home/runner/work/ParmOff/ParmOff/tests/testthat/` — testthat 3e tests, one file per R source file.
- `/home/runner/work/ParmOff/ParmOff/vignettes/` — knitr vignette.
- `.github/workflows/` — `R-CMD-check.yaml` (multi-OS check), `test-coverage.yaml` (covr, fails under 90%).

## Commands
- Test: `Rscript -e 'devtools::test()'`
- Check: `R CMD build . && R CMD check --as-cran ParmOff_*.tar.gz`
- Coverage: `Rscript -e 'covr::package_coverage()'`

## Conventions
- Validate inputs with `checkmate`; error messages are asserted in tests, so change them deliberately.
- Add a test for every new argument/branch (including error paths); keep tests in the matching `test-<Name>.R`.
- Bump `Version`/`Date` in `DESCRIPTION` for user-visible changes.
- Only `checkmate` is imported; don't add dependencies without need.
