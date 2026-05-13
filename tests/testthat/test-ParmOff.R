# Helper functions used across tests
f_xyz  <- function(x, y, z) x * y + z
f_xy   <- function(x, y) x + y
f_dots <- function(x, ...) x + sum(unlist(list(...)))
f_none <- function() 42L
f_only_dots <- function(...) sum(unlist(list(...)))

# ---------------------------------------------------------------------------
# Input validation (checkmate) -----------------------------------------------
# ---------------------------------------------------------------------------

test_that(".func must be a function", {
  expect_error(ParmOff(42, list(x = 1)),
               regexp = "Assertion")
  expect_error(ParmOff("not_a_func", list(x = 1)),
               regexp = "Assertion")
  expect_error(ParmOff(NULL, list(x = 1)),
               regexp = "Assertion")
})

test_that(".args must be NULL, a list, or an atomic vector", {
  # valid inputs don't error
  expect_silent(ParmOff(f_none, .args = NULL))
  expect_silent(ParmOff(f_none, .args = list()))
  expect_silent(ParmOff(f_none, .args = c(x = 1, y = 2)))
  expect_silent(ParmOff(f_none, .args = list(x = 1, y = 2)))
})

test_that(".use_args must be NULL or character vector", {
  expect_error(ParmOff(f_xyz, list(x=1,y=2,z=3), .use_args = 1L),
               regexp = "Assertion")
  expect_error(ParmOff(f_xyz, list(x=1,y=2,z=3), .use_args = TRUE),
               regexp = "Assertion")
})

test_that(".rem_args must be NULL or character vector", {
  expect_error(ParmOff(f_xyz, list(x=1,y=2,z=3), .rem_args = 1L),
               regexp = "Assertion")
  expect_error(ParmOff(f_xyz, list(x=1,y=2,z=3), .rem_args = list("x")),
               regexp = "Assertion")
})

test_that(".logged must be NULL or character vector", {
  expect_error(ParmOff(f_xyz, list(x=1,y=2,z=3), .logged = 1L),
               regexp = "Assertion")
  expect_error(ParmOff(f_xyz, list(x=1,y=2,z=3), .logged = TRUE),
               regexp = "Assertion")
})

test_that(".lower must be NULL, named numeric, or named list", {
  expect_error(ParmOff(f_xyz, list(x=1,y=2,z=3), .lower = c(0, 0, 0)),  # unnamed
               regexp = "Assertion")
  expect_error(ParmOff(f_xyz, list(x=1,y=2,z=3), .lower = "x"),
               regexp = "Assertion")
  expect_error(ParmOff(f_xyz, list(x=1,y=2,z=3), .lower = TRUE),
               regexp = "Assertion")
  # valid forms should not error
  expect_silent(ParmOff(f_xyz, list(x=1,y=2,z=3), .lower = NULL))
  expect_silent(ParmOff(f_xyz, list(x=1,y=2,z=3), .lower = c(x=0)))
  expect_silent(ParmOff(f_xyz, list(x=1,y=2,z=3), .lower = list(x=0)))
})

test_that(".upper must be NULL, named numeric, or named list", {
  expect_error(ParmOff(f_xyz, list(x=1,y=2,z=3), .upper = c(5, 5, 5)),  # unnamed
               regexp = "Assertion")
  expect_error(ParmOff(f_xyz, list(x=1,y=2,z=3), .upper = "x"),
               regexp = "Assertion")
  expect_silent(ParmOff(f_xyz, list(x=1,y=2,z=3), .upper = NULL))
  expect_silent(ParmOff(f_xyz, list(x=1,y=2,z=3), .upper = c(x=10)))
  expect_silent(ParmOff(f_xyz, list(x=1,y=2,z=3), .upper = list(x=10)))
})

test_that(".strip must be NULL or a single string", {
  expect_error(ParmOff(f_xyz, list(px=1,py=2,pz=3), .strip = c("p", "q")),
               regexp = "Assertion")
  expect_error(ParmOff(f_xyz, list(px=1,py=2,pz=3), .strip = 42),
               regexp = "Assertion")
  expect_silent(ParmOff(f_xyz, list(px=1,py=2,pz=3), .strip = "p"))
})

test_that(".quote must be a single logical flag", {
  expect_error(ParmOff(f_xyz, list(x=1,y=2,z=3), .quote = "yes"),
               regexp = "Assertion")
  expect_error(ParmOff(f_xyz, list(x=1,y=2,z=3), .quote = 1L),
               regexp = "Assertion")
  expect_error(ParmOff(f_xyz, list(x=1,y=2,z=3), .quote = c(TRUE, FALSE)),
               regexp = "Assertion")
})

test_that(".pass_dots must be a single logical flag", {
  expect_error(ParmOff(f_xyz, list(x=1,y=2,z=3), .pass_dots = "yes"),
               regexp = "Assertion")
  expect_error(ParmOff(f_xyz, list(x=1,y=2,z=3), .pass_dots = 1),
               regexp = "Assertion")
})

test_that(".return must be 'function' or 'args'", {
  expect_error(ParmOff(f_xyz, list(x=1,y=2,z=3), .return = "both"),
               regexp = "Assertion")
  expect_error(ParmOff(f_xyz, list(x=1,y=2,z=3), .return = ""),
               regexp = "Assertion")
  expect_error(ParmOff(f_xyz, list(x=1,y=2,z=3), .return = 1),
               regexp = "Assertion")
})

test_that(".envir must be an environment", {
  expect_error(ParmOff(f_xyz, list(x=1,y=2,z=3), .envir = "global"),
               regexp = "Assertion")
  expect_error(ParmOff(f_xyz, list(x=1,y=2,z=3), .envir = list()),
               regexp = "Assertion")
})

test_that(".bound_raw must be a single logical flag", {
  expect_error(ParmOff(f_xyz, list(x=1,y=2,z=3), .bound_raw = "yes"),
               regexp = "Assertion")
  expect_error(ParmOff(f_xyz, list(x=1,y=2,z=3), .bound_raw = 1),
               regexp = "Assertion")
  expect_error(ParmOff(f_xyz, list(x=1,y=2,z=3), .bound_raw = c(TRUE, FALSE)),
               regexp = "Assertion")
})

test_that(".check=FALSE bypasses checkmate validation", {
  # With .check=TRUE (default), invalid .use_args type throws Assertion error
  expect_error(ParmOff(f_none, list(), .use_args = 1L, .check = TRUE),
               regexp = "Assertion")
  # With .check=FALSE, the same invalid type is accepted; the call proceeds
  # (.use_args=1L effectively acts as an empty selector since 0 names match)
  expect_equal(ParmOff(f_none, list(), .use_args = 1L, .check = FALSE), 42L)
  # Disabling checks is faster but unsafe -- valid inputs still work correctly
  expect_equal(ParmOff(f_xyz, list(x=1, y=2, z=3), .check = FALSE), 5)
})

# ---------------------------------------------------------------------------
# Basic functionality --------------------------------------------------------
# ---------------------------------------------------------------------------

test_that("basic call returns correct result", {
  expect_equal(ParmOff(f_xyz, list(x=2, y=3, z=1)), 7)
})

test_that("named vector .args is coerced to list", {
  expect_equal(ParmOff(f_xyz, c(x=2, y=3, z=1)), 7)
})

test_that("extra .args names not in formals are silently dropped", {
  expect_equal(ParmOff(f_xyz, list(x=2, y=3, z=1, t=99)), 7)
})

test_that(".args = NULL works (function with no required args)", {
  expect_equal(ParmOff(f_none, .args = NULL), 42L)
})

test_that(".args = empty list works", {
  expect_equal(ParmOff(f_none, .args = list()), 42L)
})

# ---------------------------------------------------------------------------
# .use_args ------------------------------------------------------------------
# ---------------------------------------------------------------------------

test_that(".use_args restricts passed arguments", {
  # remove z: f_xyz(x=2, y=3) would be missing z - use a function that can handle
  expect_equal(ParmOff(f_xy, list(x=2, y=3, z=99), .use_args = c("x","y")), 5)
})

test_that(".use_args with single arg (was broken with > 1 guard)", {
  expect_equal(ParmOff(f_none, list(only=1), .use_args = "other"), 42L)
})

test_that(".use_args with non-existent names yields empty args", {
  # f_none takes no args so this should still work
  expect_equal(ParmOff(f_none, list(x=1,y=2), .use_args = "nonexistent"), 42L)
})

test_that(".use_args with empty character vector yields empty args", {
  expect_equal(ParmOff(f_none, list(x=1), .use_args = character(0)), 42L)
})

# ---------------------------------------------------------------------------
# .rem_args ------------------------------------------------------------------
# ---------------------------------------------------------------------------

test_that(".rem_args removes named arguments", {
  expect_equal(ParmOff(f_xy, list(x=2, y=3, z=99), .rem_args = "z"), 5)
})

test_that(".rem_args with single arg (single-arg fix)", {
  expect_equal(ParmOff(f_none, list(only=1), .rem_args = "only"), 42L)
})

test_that(".rem_args with non-existent name is a no-op", {
  expect_equal(ParmOff(f_xy, list(x=2, y=3), .rem_args = "zzz"), 5)
})

test_that(".rem_args removes all args leaving empty list", {
  expect_equal(ParmOff(f_none, list(x=1,y=2), .rem_args = c("x","y")), 42L)
})

test_that(".use_args and .rem_args combined: use first, then remove", {
  # keep x, y, z; then remove z -> x*y + <missing z> would fail for f_xyz
  # use a simpler target
  expect_equal(
    ParmOff(f_xy, list(x=2, y=3, z=99), .use_args=c("x","y","z"), .rem_args="z"),
    5
  )
})

# ---------------------------------------------------------------------------
# .strip ---------------------------------------------------------------------
# ---------------------------------------------------------------------------

test_that(".strip removes prefix from arg names", {
  expect_equal(ParmOff(f_xyz, list(p.x=2, p.y=3, p.z=1), .strip="p\\."), 7)
})

test_that(".strip removes suffix from arg names", {
  expect_equal(ParmOff(f_xyz, list(x_v=2, y_v=3, z_v=1), .strip="_v"), 7)
})

test_that(".strip that matches nothing is a no-op", {
  expect_equal(ParmOff(f_xyz, list(x=2, y=3, z=1), .strip="PREFIX"), 7)
})

test_that(".strip interacts with .use_args on post-stripped names", {
  expect_equal(
    ParmOff(f_xy, list(p.x=2, p.y=3, p.z=99), .strip="p\\.", .use_args=c("x","y")),
    5
  )
})

# ---------------------------------------------------------------------------
# .logged --------------------------------------------------------------------
# ---------------------------------------------------------------------------

test_that(".logged de-logs specified argument (base 10)", {
  # x=1 in log10 -> 10^1=10; y=2, z=3: f(10,2,3) = 10*2+3 = 23
  expect_equal(ParmOff(f_xyz, list(x=1, y=2, z=3), .logged="x"), 23)
})

test_that(".logged de-logs multiple arguments", {
  # x=1->10, y=1->10: f(10,10,3) = 103
  expect_equal(ParmOff(f_xyz, list(x=1, y=1, z=3), .logged=c("x","y")), 103)
})

test_that(".logged with name NOT in .args does not create new entries", {
  args_before <- list(x=1, y=2, z=3)
  result <- ParmOff(f_xyz, args_before, .logged=c("x","phantom"), .return="args")
  expect_equal(sort(names(result$current_args)), sort(c("x","y","z")))
})

test_that(".logged applied before .use_args filtering", {
  # x in log10=1 -> 10; then only keep x, y: f_xy(10, 2) = 12
  expect_equal(
    ParmOff(f_xy, list(x=1, y=2, z=3), .logged="x", .use_args=c("x","y")),
    12
  )
})

# ---------------------------------------------------------------------------
# .lower / .upper clamping ---------------------------------------------------
# ---------------------------------------------------------------------------

test_that(".lower clamps argument upward", {
  # x=0.5, lower x=1 -> x clamped to 1; f(1,2,3)=5
  expect_equal(ParmOff(f_xyz, list(x=0.5, y=2, z=3), .lower=c(x=1)), 5)
})

test_that(".upper clamps argument downward", {
  # y=10, upper y=2 -> y clamped to 2; f(1,2,3)=5
  expect_equal(ParmOff(f_xyz, list(x=1, y=10, z=3), .upper=c(y=2)), 5)
})

test_that(".lower and .upper both applied in order", {
  # x=0 lower=1 -> 1; y=10 upper=2 -> 2; z=3 unchanged; f(1,2,3)=5
  expect_equal(
    ParmOff(f_xyz, list(x=0, y=10, z=3), .lower=c(x=1), .upper=c(y=2)),
    5
  )
})

test_that(".lower with names NOT in .args is a no-op", {
  expect_equal(ParmOff(f_xyz, list(x=1, y=2, z=3), .lower=c(phantom=99)), 5)
})

test_that(".upper with names NOT in .args is a no-op", {
  expect_equal(ParmOff(f_xyz, list(x=1, y=2, z=3), .upper=c(phantom=-99)), 5)
})

test_that(".lower already satisfied (value >= bound) is a no-op", {
  expect_equal(ParmOff(f_xyz, list(x=5, y=2, z=3), .lower=c(x=1)), 13)
})

test_that(".upper already satisfied (value <= bound) is a no-op", {
  expect_equal(ParmOff(f_xyz, list(x=1, y=1, z=3), .upper=c(y=2)), 4)
})

test_that(".lower applied to ... argument", {
  # z supplied via ...; lower z=5 -> z clamped to 5; f(1,2,5)=7
  expect_equal(ParmOff(f_xyz, list(x=1, y=2), .lower=c(z=5), z=3), 7)
})

test_that(".upper applied to ... argument", {
  # z supplied via ...; upper z=2 -> z clamped to 2; f(1,2,2)=4
  expect_equal(ParmOff(f_xyz, list(x=1, y=2), .upper=c(z=2), z=3), 4)
})

test_that(".lower clamping happens before .logged de-logging", {
  # y=0 (log10 space), lower y=1 (log10 space) -> y clamped to 1 -> 10^1=10
  # f_xy(1, 10) = 11
  expect_equal(
    ParmOff(f_xy, list(x=1, y=0), .logged="y", .lower=c(y=1)),
    11
  )
})

test_that(".upper clamping happens before .logged de-logging", {
  # y=3 (log10), upper y=1 -> clamped to 1 -> 10^1=10; f_xy(1,10)=11
  expect_equal(
    ParmOff(f_xy, list(x=1, y=3), .logged="y", .upper=c(y=1)),
    11
  )
})

test_that(".lower > value AND .upper < value: .upper wins (pmin after pmax)", {
  # x=5, lower x=2, upper x=3: pmax(5,2)=5 then pmin(5,3)=3; f(3,2,3)=9
  expect_equal(
    ParmOff(f_xyz, list(x=5, y=2, z=3), .lower=c(x=2), .upper=c(x=3)),
    9
  )
})

test_that(".lower == .upper clamps to exact value", {
  # x=5, lower=upper=2: pmax(5,2)=5, pmin(5,2)=2; f(2,2,3)=7
  expect_equal(
    ParmOff(f_xyz, list(x=5, y=2, z=3), .lower=c(x=2), .upper=c(x=2)),
    7
  )
})

# .bound_raw=FALSE: bounds applied AFTER de-logging (bounds in real-number space)

test_that(".bound_raw=FALSE applies .lower in real space after de-logging", {
  # y=0 (log10) -> 10^0=1; lower y=2 (real) -> pmax(1,2)=2; f_xy(1,2)=3
  expect_equal(
    ParmOff(f_xy, list(x=1, y=0), .logged="y", .lower=c(y=2), .bound_raw=FALSE),
    3
  )
})

test_that(".bound_raw=FALSE applies .upper in real space after de-logging", {
  # y=2 (log10) -> 10^2=100; upper y=5 (real) -> pmin(100,5)=5; f_xy(1,5)=6
  expect_equal(
    ParmOff(f_xy, list(x=1, y=2), .logged="y", .upper=c(y=5), .bound_raw=FALSE),
    6
  )
})

test_that(".bound_raw=FALSE: satisfied bound in real space is a no-op", {
  # y=1 (log10) -> 10^1=10; lower y=5 (real) -> pmax(10,5)=10; f_xy(1,10)=11
  expect_equal(
    ParmOff(f_xy, list(x=1, y=1), .logged="y", .lower=c(y=5), .bound_raw=FALSE),
    11
  )
})

test_that(".bound_raw=FALSE and .bound_raw=TRUE differ when bound crosses log scale", {
  # y=0 (log10), lower=1
  # .bound_raw=TRUE (bound in log space): pmax(0,1)=1 -> 10^1=10; f_xy(1,10)=11
  # .bound_raw=FALSE (bound in real space): 10^0=1 -> pmax(1,1)=1; f_xy(1,1)=2
  expect_equal(
    ParmOff(f_xy, list(x=1, y=0), .logged="y", .lower=c(y=1), .bound_raw=TRUE),
    11
  )
  expect_equal(
    ParmOff(f_xy, list(x=1, y=0), .logged="y", .lower=c(y=1), .bound_raw=FALSE),
    2
  )
})

test_that(".bound_raw=FALSE without .logged behaves identically to TRUE", {
  # No de-logging, so bound_raw has no effect on the outcome
  expect_equal(
    ParmOff(f_xyz, list(x=0, y=10, z=3), .lower=c(x=1), .upper=c(y=2), .bound_raw=FALSE),
    5
  )
  expect_equal(
    ParmOff(f_xyz, list(x=0, y=10, z=3), .lower=c(x=1), .upper=c(y=2), .bound_raw=TRUE),
    5
  )
})

test_that(".bound_raw=FALSE applied to ... argument (real-space lower)", {
  # z via ...; z=2 (log10) -> 10^2=100; lower z=5 (real) -> pmax(100,5)=100; f(1,2,100)=102
  expect_equal(
    ParmOff(f_xyz, list(x=1, y=2), .logged="z", .lower=c(z=5), .bound_raw=FALSE, z=2),
    102
  )
})

# ---------------------------------------------------------------------------
# ... (dots) merging ---------------------------------------------------------
# ---------------------------------------------------------------------------

test_that("... args are merged into .args", {
  expect_equal(ParmOff(f_xyz, list(x=1, y=2), z=3), 5)
})

test_that("... duplicate names are dropped (.args takes precedence)", {
  # y supplied in both .args and ...; .args value (y=2) must win
  expect_equal(ParmOff(f_xyz, list(x=1, y=2, z=3), y=99), 5)
})

test_that("... can supply all arguments", {
  expect_equal(ParmOff(f_xyz, NULL, x=1, y=2, z=3), 5)
})

test_that("no ... supplied works fine (.pass_dots not mutated)", {
  expect_equal(ParmOff(f_xyz, list(x=1, y=2, z=3)), 5)
})

# ---------------------------------------------------------------------------
# .pass_dots -----------------------------------------------------------------
# ---------------------------------------------------------------------------

test_that(".pass_dots=TRUE passes unmatched args through ... in .func", {
  # f_dots(x, ...) sums x + any extras
  expect_equal(ParmOff(f_dots, list(x=1, y=2, z=3), .pass_dots=TRUE), 6)
})

test_that(".pass_dots=FALSE strips args not in .func formals", {
  expect_equal(ParmOff(f_dots, list(x=1, y=2, z=3), .pass_dots=FALSE), 1)
})

test_that(".pass_dots=TRUE is NOT reset to FALSE when no ... supplied", {
  # Even without ... in the call, .pass_dots should remain TRUE.
  # f_dots with only x=1 in .args (no extras) should still equal 1.
  expect_equal(ParmOff(f_dots, list(x=1), .pass_dots=TRUE), 1)
})

test_that(".func with no ... and .pass_dots=TRUE still filters unmatched args", {
  # f_xyz has no ..., so unmatched 't' must be dropped even with .pass_dots=TRUE
  expect_equal(ParmOff(f_xyz, list(x=1, y=2, z=3, t=99), .pass_dots=TRUE), 5)
})

test_that(".func with only ... accepts all args when .pass_dots=TRUE", {
  expect_equal(ParmOff(f_only_dots, list(a=1, b=2, c=3), .pass_dots=TRUE), 6)
})

test_that(".func with only ... and .pass_dots=FALSE yields empty call -> 0", {
  expect_equal(ParmOff(f_only_dots, list(a=1, b=2, c=3), .pass_dots=FALSE), 0)
})

# ---------------------------------------------------------------------------
# .return = 'args' -----------------------------------------------------------
# ---------------------------------------------------------------------------

test_that(".return='args' returns list with current_args and ignore_args", {
  result <- ParmOff(f_xyz, list(x=1, y=2, z=3, t=99), .return="args")
  expect_type(result, "list")
  expect_named(result, c("current_args", "ignore_args"))
})

test_that(".return='args' current_args contains only formals-matched args", {
  result <- ParmOff(f_xyz, list(x=1, y=2, z=3, t=99), .return="args")
  expect_equal(sort(names(result$current_args)), c("x","y","z"))
})

test_that(".return='args' ignore_args contains dropped args", {
  result <- ParmOff(f_xyz, list(x=1, y=2, z=3, t=99), .return="args")
  expect_named(result$ignore_args, "t")
})

test_that(".return='args' with .rem_args: removed arg in ignore_args", {
  result <- ParmOff(f_xyz, list(x=1, y=2, z=3), .rem_args="z", .return="args")
  expect_true("z" %in% names(result$ignore_args))
  expect_false("z" %in% names(result$current_args))
})

test_that(".return='args' with empty .args returns empty lists", {
  result <- ParmOff(f_none, list(), .return="args")
  expect_length(result$current_args, 0)
  expect_length(result$ignore_args, 0)
})

# ---------------------------------------------------------------------------
# Edge cases and corner cases ------------------------------------------------
# ---------------------------------------------------------------------------

test_that("function with no formals accepts no args", {
  expect_equal(ParmOff(f_none, list(x=1, y=2)), 42L)
})

test_that(".args with a single element and .use_args (single-arg fix)", {
  # With the old > 1 guard, a single-element .args bypassed filtering.
  # Now: list(x=5) filtered by .use_args="other" drops x, f_none still returns 42L.
  expect_equal(ParmOff(f_none, list(x=5), .use_args="other"), 42L)
})

test_that(".args with a single element and .rem_args (single-arg fix)", {
  # remove x, only y=3 remains; f_none ignores all
  expect_equal(ParmOff(f_none, list(x=5), .rem_args="x"), 42L)
})

test_that("single-element .args passes formals filter", {
  expect_equal(ParmOff(f_xy, list(x=5), y=3), 8)
})

test_that("NULL .args with ... only", {
  expect_equal(ParmOff(f_xy, NULL, x=2, y=3), 5)
})

test_that(".args containing NA values are passed through", {
  f_na <- function(x, y) is.na(x)
  expect_true(ParmOff(f_na, list(x=NA, y=1)))
})

test_that(".args containing negative values work with .lower", {
  # x=-5, lower x=-1 -> x clamped to -1; f_xy(-1, 2)=1
  expect_equal(ParmOff(f_xy, list(x=-5, y=2), .lower=c(x=-1)), 1)
})

test_that(".args as integer vector is coerced to list", {
  expect_equal(ParmOff(f_xyz, c(x=1L, y=2L, z=3L)), 5L)
})

test_that(".args as character vector (passed to matching char function)", {
  f_paste <- function(a, b) paste(a, b)
  expect_equal(ParmOff(f_paste, c(a="hello", b="world")), "hello world")
})

test_that("all .args filtered out by .use_args, still calls .func with no args", {
  expect_equal(ParmOff(f_none, list(x=1, y=2), .use_args="nonexistent"), 42L)
})

test_that(".strip empty string strips nothing meaningful", {
  # sub("", ...) inserts "" before every char and after -- just pass through
  # The call should not error
  expect_equal(ParmOff(f_xyz, list(x=1, y=2, z=3), .strip=""), 5)
})

test_that(".logged on zero produces 1 (10^0)", {
  # x=0 in log10 -> 10^0=1; f_xy(1, 2)=3
  expect_equal(ParmOff(f_xy, list(x=0, y=2), .logged="x"), 3)
})

test_that(".logged on negative log value produces fractional result", {
  # x=-1 in log10 -> 10^(-1)=0.1; f_xy(0.1, 2)=2.1
  expect_equal(ParmOff(f_xy, list(x=-1, y=2), .logged="x"), 2.1)
})

test_that("multiple feature interaction: .strip + .logged + .lower + .use_args", {
  # Names: p.x=1, p.y=0, p.z=3 -> strip "p\\." -> x=1, y=0, z=3
  # .logged="y": y=0 -> 10^0=1 (but .lower applied before .logged)
  # .lower=c(y=1): y clamped from 0 to 1 (in log space) -> 10^1=10
  # .use_args=c("x","y"): drop z -> f_xy(1, 10) = 11
  expect_equal(
    ParmOff(f_xy, list(p.x=1, p.y=0, p.z=3),
            .strip="p\\.", .logged="y", .lower=c(y=1), .use_args=c("x","y")),
    11
  )
})

test_that("multiple feature interaction: .strip + .logged + .lower + .use_args + .bound_raw=FALSE", {
  # Names: p.x=1, p.y=0, p.z=3 -> strip "p\\." -> x=1, y=0, z=3
  # .logged="y" first: y=0 -> 10^0=1
  # .lower=c(y=1) in real space: pmax(1,1)=1 (no change since 10^0=1=lower)
  # .use_args=c("x","y"): drop z -> f_xy(1, 1) = 2
  expect_equal(
    ParmOff(f_xy, list(p.x=1, p.y=0, p.z=3),
            .strip="p\\.", .logged="y", .lower=c(y=1), .use_args=c("x","y"),
            .bound_raw=FALSE),
    2
  )
})

test_that("... arg name collision with .args after .strip: .args wins", {
  # After stripping "p." from p.x -> x; then x=99 in dots is dropped
  expect_equal(ParmOff(f_xy, list(p.x=1, p.y=2), .strip="p\\.", x=99), 3)
})

test_that(".lower as named list (not numeric vector) works", {
  expect_equal(ParmOff(f_xy, list(x=0, y=2), .lower=list(x=1)), 3)
})

test_that(".upper as named list (not numeric vector) works", {
  expect_equal(ParmOff(f_xy, list(x=10, y=2), .upper=list(x=5)), 7)
})

test_that(".lower with Inf bound is a no-op", {
  expect_equal(ParmOff(f_xyz, list(x=1,y=2,z=3), .lower=c(x=-Inf)), 5)
})

test_that(".upper with Inf bound is a no-op", {
  expect_equal(ParmOff(f_xyz, list(x=1,y=2,z=3), .upper=c(x=Inf)), 5)
})

test_that(".lower with -Inf bound has no effect on finite values", {
  # pmax(1, -Inf) = 1 -> no change actually
  expect_equal(ParmOff(f_xy, list(x=1, y=2), .lower=c(x=-Inf)), 3)
})

test_that("passing same arg in both .use_args and .rem_args removes it (rem after use)", {
  # .use_args keeps only 'x'; then .rem_args removes 'x' -> empty -> f_none
  expect_equal(ParmOff(f_none, list(x=1,y=2), .use_args="x", .rem_args="x"), 42L)
})
