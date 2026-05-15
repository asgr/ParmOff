# ---------------------------------------------------------------------------
# ParmLog / ParmUnLog tests
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# ParmLog – character-vector selection (named inputs) -----------------------
# ---------------------------------------------------------------------------

test_that("ParmLog base-10: named selection logs correct elements", {
  x <- list(a = 100, b = 10, c = 5)
  out <- ParmLog(x, logged = c("a", "b"))
  expect_equal(out$a, log10(100))
  expect_equal(out$b, log10(10))
  expect_equal(out$c, 5)   # untouched
})

test_that("ParmLog base-10: name not in list is a no-op", {
  x <- list(a = 100, b = 10)
  out <- ParmLog(x, logged = "phantom")
  expect_equal(out, x)
})

test_that("ParmLog ln: named selection uses natural log", {
  x <- list(a = exp(2), b = exp(1), c = 3)
  out <- ParmLog(x, logged = c("a", "b"), log_type = 'ln')
  expect_equal(out$a, 2)
  expect_equal(out$b, 1)
  expect_equal(out$c, 3)
})

test_that("ParmLog: single character name works", {
  x <- list(a = 1000, b = 5)
  out <- ParmLog(x, logged = "a")
  expect_equal(out$a, 3)
  expect_equal(out$b, 5)
})

test_that("ParmLog: character selector on unnamed list is a no-op", {
  x <- list(1, 10, 100)
  out <- ParmLog(x, logged = "a")
  expect_equal(out, x)
})

# ---------------------------------------------------------------------------
# ParmLog – logical-vector selection ----------------------------------------
# ---------------------------------------------------------------------------

test_that("ParmLog base-10: logical selection logs flagged elements", {
  x <- list(a = 100, b = 10, c = 5)
  out <- ParmLog(x, logged = c(TRUE, TRUE, FALSE))
  expect_equal(out$a, log10(100))
  expect_equal(out$b, log10(10))
  expect_equal(out$c, 5)
})

test_that("ParmLog ln: logical selection uses natural log", {
  x <- list(a = exp(3), b = 7)
  out <- ParmLog(x, logged = c(TRUE, FALSE), log_type = 'ln')
  expect_equal(out$a, 3)
  expect_equal(out$b, 7)
})

test_that("ParmLog: all-FALSE logical is a no-op", {
  x <- list(a = 100, b = 10)
  out <- ParmLog(x, logged = c(FALSE, FALSE))
  expect_equal(out, x)
})

test_that("ParmLog: all-TRUE logical logs every element", {
  x <- list(a = 10, b = 100)
  out <- ParmLog(x, logged = c(TRUE, TRUE))
  expect_equal(out$a, 1)
  expect_equal(out$b, 2)
})

# ---------------------------------------------------------------------------
# ParmUnLog – character-vector selection (named inputs) ---------------------
# ---------------------------------------------------------------------------

test_that("ParmUnLog base-10: named selection unlogs correct elements", {
  x <- list(a = 2, b = 1, c = 5)
  out <- ParmUnLog(x, logged = c("a", "b"))
  expect_equal(out$a, 100)
  expect_equal(out$b, 10)
  expect_equal(out$c, 5)
})

test_that("ParmUnLog base-10: name not in list is a no-op", {
  x <- list(a = 2, b = 1)
  out <- ParmUnLog(x, logged = "phantom")
  expect_equal(out, x)
})

test_that("ParmUnLog ln: named selection uses exp", {
  x <- list(a = 2, b = 1, c = 3)
  out <- ParmUnLog(x, logged = c("a", "b"), log_type = 'ln')
  expect_equal(out$a, exp(2))
  expect_equal(out$b, exp(1))
  expect_equal(out$c, 3)
})

test_that("ParmUnLog: single character name works", {
  x <- list(a = 3, b = 5)
  out <- ParmUnLog(x, logged = "a")
  expect_equal(out$a, 1000)
  expect_equal(out$b, 5)
})

# ---------------------------------------------------------------------------
# ParmUnLog – logical-vector selection --------------------------------------
# ---------------------------------------------------------------------------

test_that("ParmUnLog base-10: logical selection unlogs flagged elements", {
  x <- list(a = 2, b = 1, c = 5)
  out <- ParmUnLog(x, logged = c(TRUE, TRUE, FALSE))
  expect_equal(out$a, 100)
  expect_equal(out$b, 10)
  expect_equal(out$c, 5)
})

test_that("ParmUnLog ln: logical selection uses exp", {
  x <- list(a = 2, b = 7)
  out <- ParmUnLog(x, logged = c(TRUE, FALSE), log_type = 'ln')
  expect_equal(out$a, exp(2))
  expect_equal(out$b, 7)
})

test_that("ParmUnLog: all-FALSE logical is a no-op", {
  x <- list(a = 2, b = 1)
  out <- ParmUnLog(x, logged = c(FALSE, FALSE))
  expect_equal(out, x)
})

# ---------------------------------------------------------------------------
# NULL logged is always a no-op ---------------------------------------------
# ---------------------------------------------------------------------------

test_that("ParmLog with NULL logged returns x unchanged", {
  x <- list(a = 100, b = 10)
  expect_equal(ParmLog(x, logged = NULL), x)
})

test_that("ParmUnLog with NULL logged returns x unchanged", {
  x <- list(a = 2, b = 1)
  expect_equal(ParmUnLog(x, logged = NULL), x)
})

# ---------------------------------------------------------------------------
# Round-trip: ParmLog followed by ParmUnLog ---------------------------------
# ---------------------------------------------------------------------------

test_that("ParmLog -> ParmUnLog round-trip base-10", {
  x <- list(a = 100, b = 0.5, c = 7)
  flags <- c("a", "b")
  expect_equal(ParmUnLog(ParmLog(x, flags), flags), x)
})

test_that("ParmLog -> ParmUnLog round-trip ln", {
  x <- list(a = exp(3), b = exp(0.5), c = 7)
  flags <- c("a", "b")
  expect_equal(
    ParmUnLog(ParmLog(x, flags, log_type = 'ln'), flags, log_type = 'ln'),
    x
  )
})

test_that("ParmLog -> ParmUnLog round-trip via logical vector", {
  x <- list(a = 1000, b = 10, c = 5)
  sel <- c(TRUE, TRUE, FALSE)
  expect_equal(ParmUnLog(ParmLog(x, sel), sel), x)
})

test_that("ParmLog log2: named selection uses log2", {
  x <- list(a = 8, b = 4, c = 5)
  out <- ParmLog(x, logged = c("a", "b"), log_type = 'log2')
  expect_equal(out$a, log2(8))
  expect_equal(out$b, log2(4))
  expect_equal(out$c, 5)
})

test_that("ParmUnLog log2: named selection applies inverse transformation", {
  x <- list(a = 3, b = 2, c = 5)
  out <- ParmUnLog(x, logged = c("a", "b"), log_type = 'log2')
  expect_equal(out$a, 2^3)
  expect_equal(out$b, 2^2)
  expect_equal(out$c, 5)
})

test_that("ParmLog -> ParmUnLog round-trip log2", {
  x <- list(a = 8, b = 4, c = 5)
  flags <- c("a", "b")
  expect_equal(
    ParmUnLog(ParmLog(x, flags, log_type = 'log2'), flags, log_type = 'log2'),
    x
  )
})

# ---------------------------------------------------------------------------
# Structure preservation (matrix) -------------------------------------------
# ---------------------------------------------------------------------------

test_that("ParmLog preserves matrix dimensions", {
  m <- matrix(c(10, 100, 1000, 10000), nrow = 2)
  x <- list(cov = m, mu = 5)
  out <- ParmLog(x, logged = "cov")
  expect_true(is.matrix(out$cov))
  expect_equal(dim(out$cov), c(2L, 2L))
  expect_equal(out$cov, log10(m))
  expect_equal(out$mu, 5)
})

test_that("ParmUnLog preserves matrix dimensions", {
  m <- matrix(c(2, 3, 4, 5), nrow = 2)
  x <- list(cov = m, mu = 5)
  out <- ParmUnLog(x, logged = "cov")
  expect_true(is.matrix(out$cov))
  expect_equal(dim(out$cov), c(2L, 2L))
  expect_equal(out$cov, 10^m)
  expect_equal(out$mu, 5)
})

test_that("ParmLog preserves vector length and names", {
  v <- c(x = 10, y = 100, z = 1000)
  params <- list(scale = v, offset = 3)
  out <- ParmLog(params, logged = "scale")
  expect_equal(length(out$scale), 3L)
  expect_equal(names(out$scale), c("x", "y", "z"))
  expect_equal(out$scale, log10(v))
})

# ---------------------------------------------------------------------------
# invalid log_type raises an error ------------------------------------------
# ---------------------------------------------------------------------------

test_that("ParmLog errors on invalid log_type", {
  expect_error(ParmLog(list(a = 10), "a", log_type = "log4"), regexp = "log_type")
})

test_that("ParmUnLog errors on invalid log_type", {
  expect_error(ParmUnLog(list(a = 1), "a", log_type = "log4"), regexp = "log_type")
})

# ---------------------------------------------------------------------------
# ParmOff still works via ParmUnLog refactor --------------------------------
# ---------------------------------------------------------------------------

test_that("ParmOff .logged still de-logs base-10 after refactor", {
  model <- function(x, y, z) x * y + z
  expect_equal(ParmOff(model, list(x = 1, y = 1, z = 3), .logged = "y"), 13)
})

test_that("ParmOff .logged on multiple args after refactor", {
  model <- function(x, y, z) x * y + z
  expect_equal(ParmOff(model, list(x = 1, y = 1, z = 3), .logged = c("x", "y")), 103)
})

test_that("ParmOff .log_type='ln' is forwarded to ParmUnLog", {
  # y stored as log(10) ~ 2.302585; de-logged with ln gives exp(log(10)) = 10
  f <- function(x, y) x + y
  expect_equal(ParmOff(f, list(x = 0, y = log(10)), .logged = "y", .log_type = 'ln'), 10)
})

test_that("ParmOff .log_type='log2' is forwarded to ParmUnLog", {
  # y stored as log2(8) = 3; de-logged with log2 gives 2^3 = 8
  f <- function(x, y) x + y
  expect_equal(ParmOff(f, list(x = 0, y = 3), .logged = "y", .log_type = 'log2'), 8)
})

# ---------------------------------------------------------------------------
# verbose argument -----------------------------------------------------------
# ---------------------------------------------------------------------------

test_that("ParmLog verbose emits message for each transformed element (character)", {
  expect_message(
    ParmLog(list(a = 100, b = 10, c = 5), logged = c("a", "b"), verbose = TRUE),
    regexp = "ParmLog.*a"
  )
})

test_that("ParmUnLog verbose emits message for each transformed element (character)", {
  expect_message(
    ParmUnLog(list(a = 2, b = 1, c = 5), logged = c("a", "b"), verbose = TRUE),
    regexp = "ParmUnLog.*a"
  )
})

test_that("ParmLog verbose emits message for each transformed element (logical)", {
  expect_message(
    ParmLog(list(a = 100, b = 10), logged = c(TRUE, FALSE), verbose = TRUE),
    regexp = "ParmLog.*a"
  )
})

test_that("ParmUnLog verbose emits message for each transformed element (logical)", {
  expect_message(
    ParmUnLog(list(a = 2, b = 1), logged = c(TRUE, FALSE), verbose = TRUE),
    regexp = "ParmUnLog.*a"
  )
})

test_that("ParmLog verbose message includes before and after values", {
  msg <- ""
  withCallingHandlers(
    ParmLog(list(a = 100), logged = "a", verbose = TRUE),
    message = function(m) { msg <<- conditionMessage(m); invokeRestart("muffleMessage") }
  )
  expect_match(msg, "100")
  expect_match(msg, "2")
})

test_that("ParmUnLog verbose message includes before and after values", {
  msg <- ""
  withCallingHandlers(
    ParmUnLog(list(a = 2), logged = "a", verbose = TRUE),
    message = function(m) { msg <<- conditionMessage(m); invokeRestart("muffleMessage") }
  )
  expect_match(msg, "2")
  expect_match(msg, "100")
})

test_that("ParmLog verbose=FALSE (default) is silent", {
  expect_silent(ParmLog(list(a = 100, b = 10), logged = "a"))
})

test_that("ParmUnLog verbose=FALSE (default) is silent", {
  expect_silent(ParmUnLog(list(a = 2, b = 1), logged = "a"))
})

test_that("ParmLog verbose is silent for name not in list (no-op)", {
  expect_silent(ParmLog(list(a = 100), logged = "phantom", verbose = TRUE))
})

test_that("ParmLog verbose reports log_type in message", {
  msg <- ""
  withCallingHandlers(
    ParmLog(list(a = 100), logged = "a", log_type = 'ln', verbose = TRUE),
    message = function(m) { msg <<- conditionMessage(m); invokeRestart("muffleMessage") }
  )
  expect_match(msg, "ln")
})

test_that("ParmOff .verbose passes to ParmUnLog", {
  model <- function(x, y, z) x * y + z
  expect_message(
    ParmOff(model, list(x = 1, y = 1, z = 3), .logged = "y", .verbose = TRUE),
    regexp = "ParmUnLog.*y"
  )
})

test_that("ParmLog verbose preserves return value", {
  result <- suppressMessages(
    ParmLog(list(a = 100, b = 10), logged = "a", verbose = TRUE)
  )
  expect_equal(result$a, log10(100))
  expect_equal(result$b, 10)
})

test_that("ParmUnLog verbose preserves return value", {
  result <- suppressMessages(
    ParmUnLog(list(a = 2, b = 1), logged = "a", verbose = TRUE)
  )
  expect_equal(result$a, 100)
  expect_equal(result$b, 1)
})

test_that("ParmLog verbose suppresses output for large vectors", {
  x_long <- list(scale = seq_len(25))  # length 25 > 20
  expect_silent(ParmLog(x_long, logged = "scale", verbose = TRUE))
})
