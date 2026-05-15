test_that("ParmLimLo clamps vectors at lower bound", {
  expect_equal(ParmLimLo(c(a = -1, b = 2), lower = 0), c(a = 0, b = 2))
})

test_that("ParmLimHi clamps vectors at upper bound", {
  expect_equal(ParmLimHi(c(a = -1, b = 2), upper = 1), c(a = -1, b = 1))
})

test_that("ParmLimBoth clamps vectors at both bounds", {
  expect_equal(
    ParmLimBoth(c(a = -1, b = 2), lower = 0, upper = 1),
    c(a = 0, b = 1)
  )
})

test_that("ParmLim helpers work on simple named lists", {
  x <- list(a = -1, b = 5)
  expect_equal(ParmLimLo(x, list(a = 0, b = 3)), list(a = 0, b = 5))
  expect_equal(ParmLimHi(x, list(a = 1, b = 4)), list(a = -1, b = 4))
  expect_equal(
    ParmLimBoth(x, list(a = 0, b = 3), list(a = 1, b = 4)),
    list(a = 0, b = 4)
  )
})

test_that("single named bound only affects matching child", {
  x <- list(a = -1, b = 5)
  expect_equal(ParmLimLo(x, lower = list(a = 0)), list(a = 0, b = 5))
})

test_that("ParmLim supports nested lists with partial named bounds", {
  x <- list(a = -1, b = list(c = 5, d = -3), e = 9)

  expect_equal(
    ParmLimLo(x, lower = list(a = 0, b = list(d = -2))),
    list(a = 0, b = list(c = 5, d = -2), e = 9)
  )

  expect_equal(
    ParmLimHi(x, upper = list(b = list(c = 4), e = 8)),
    list(a = -1, b = list(c = 4, d = -3), e = 8)
  )
})

test_that("ParmLim supports single bound broadcast across list leaves", {
  x <- list(a = -1, b = list(c = 2, d = -5))
  expect_equal(ParmLimLo(x, lower = 0), list(a = 0, b = list(c = 2, d = 0)))
  expect_equal(ParmLimHi(x, upper = 1), list(a = -1, b = list(c = 1, d = -5)))
})

test_that("ParmOff applies ParmLim correctly for list-valued arguments", {
  f_list_sum <- function(x) sum(unlist(x))
  input <- list(x = list(a = -1, b = list(c = 3, d = -5)))

  out <- ParmOff(
    f_list_sum,
    input,
    .lower = list(x = list(a = 0, b = list(d = -1))),
    .upper = list(x = list(b = list(c = 2)))
  )

  expect_equal(out, 1)
})

# ---------------------------------------------------------------------------
# Pure vectors (unnamed) -----------------------------------------------------
# ---------------------------------------------------------------------------

test_that("ParmLimLo clamps unnamed vector at scalar lower bound", {
  expect_equal(ParmLimLo(c(-3, 0, 5), lower = 0), c(0, 0, 5))
})

test_that("ParmLimHi clamps unnamed vector at scalar upper bound", {
  expect_equal(ParmLimHi(c(-3, 0, 5), upper = 2), c(-3, 0, 2))
})

test_that("ParmLimBoth clamps unnamed vector at both bounds", {
  expect_equal(ParmLimBoth(c(-3, 0, 5), lower = 0, upper = 2), c(0, 0, 2))
})

test_that("ParmLimLo with vector already above lower is a no-op", {
  x <- c(1, 2, 3)
  expect_equal(ParmLimLo(x, lower = 0), x)
})

test_that("ParmLimHi with vector already below upper is a no-op", {
  x <- c(1, 2, 3)
  expect_equal(ParmLimHi(x, upper = 10), x)
})

test_that("ParmLimLo aligns named bound by name on named vector", {
  x <- c(a = -5, b = 10, c = 0)
  lower <- c(a = 0, c = -1)
  out <- ParmLimLo(x, lower = lower)
  expect_equal(out[["a"]], 0)    # clamped up
  expect_equal(out[["b"]], 10)   # no matching bound → unchanged
  expect_equal(out[["c"]], 0)    # already above -1 → unchanged
})

test_that("ParmLimHi aligns named bound by name on named vector", {
  x <- c(a = -5, b = 10, c = 3)
  upper <- c(b = 5, c = 4)
  out <- ParmLimHi(x, upper = upper)
  expect_equal(out[["a"]], -5)   # no matching bound → unchanged
  expect_equal(out[["b"]], 5)    # clamped down
  expect_equal(out[["c"]], 3)    # already below 4 → unchanged
})

test_that("ParmLimBoth aligns named lower and upper on named vector", {
  x <- c(a = -5, b = 10, c = 3)
  out <- ParmLimBoth(x, lower = c(a = 0), upper = c(b = 7))
  expect_equal(out[["a"]], 0)    # clamped up by lower
  expect_equal(out[["b"]], 7)    # clamped down by upper
  expect_equal(out[["c"]], 3)    # no bound on c → unchanged
})

test_that("ParmLimLo with unnamed bound vector clamps element-wise", {
  x <- c(-1, 2, -3)
  lower <- c(0, 0, 0)
  expect_equal(ParmLimLo(x, lower = lower), c(0, 2, 0))
})

test_that("ParmLimHi with unnamed bound vector clamps element-wise", {
  x <- c(5, 2, 8)
  upper <- c(4, 3, 10)
  expect_equal(ParmLimHi(x, upper = upper), c(4, 2, 8))
})

# ---------------------------------------------------------------------------
# Deeply nested lists (3+ levels) -------------------------------------------
# ---------------------------------------------------------------------------

test_that("ParmLimLo descends 3 levels of named nesting", {
  x <- list(a = list(b = list(c = -10, d = 5), e = 2), f = 3)
  lower <- list(a = list(b = list(c = 0), e = 1))
  out <- ParmLimLo(x, lower = lower)
  expect_equal(out$a$b$c, 0)   # clamped
  expect_equal(out$a$b$d, 5)   # no bound → unchanged
  expect_equal(out$a$e, 2)     # already >= 1 → unchanged
  expect_equal(out$f, 3)       # no bound → unchanged
})

test_that("ParmLimHi descends 3 levels of named nesting", {
  x <- list(a = list(b = list(c = 10, d = 5), e = 8), f = 3)
  upper <- list(a = list(b = list(c = 4), e = 6), f = 2)
  out <- ParmLimHi(x, upper = upper)
  expect_equal(out$a$b$c, 4)   # clamped
  expect_equal(out$a$b$d, 5)   # no bound → unchanged
  expect_equal(out$a$e, 6)     # clamped
  expect_equal(out$f, 2)       # clamped
})

test_that("ParmLimBoth descends 3 levels and applies both bounds", {
  x <- list(p = list(q = list(r = -5, s = 20), t = 3), u = -1)
  lower <- list(p = list(q = list(r = 0), t = 0), u = 0)
  upper <- list(p = list(q = list(s = 10), t = 5))
  out <- ParmLimBoth(x, lower = lower, upper = upper)
  expect_equal(out$p$q$r, 0)   # clamped by lower
  expect_equal(out$p$q$s, 10)  # clamped by upper
  expect_equal(out$p$t, 3)     # above lower(0), below upper(5) → unchanged
  expect_equal(out$u, 0)       # clamped by lower
})

test_that("scalar lower bound broadcasts to all leaves of 3-level nesting", {
  x <- list(a = list(b = list(c = -10, d = -20), e = -5), f = -3)
  out <- ParmLimLo(x, lower = 0)
  expect_equal(out$a$b$c, 0)
  expect_equal(out$a$b$d, 0)
  expect_equal(out$a$e, 0)
  expect_equal(out$f, 0)
})

test_that("scalar upper bound broadcasts to all leaves of 3-level nesting", {
  x <- list(a = list(b = list(c = 100, d = 50), e = 200), f = 10)
  out <- ParmLimHi(x, upper = 10)
  expect_equal(out$a$b$c, 10)
  expect_equal(out$a$b$d, 10)
  expect_equal(out$a$e, 10)
  expect_equal(out$f, 10)
})

test_that("ParmLimBoth with scalar bounds broadcasts through 3 levels", {
  x <- list(a = list(b = list(c = -5, d = 20), e = 15), f = -2)
  out <- ParmLimBoth(x, lower = 0, upper = 10)
  expect_equal(out$a$b$c, 0)   # clamped by lower
  expect_equal(out$a$b$d, 10)  # clamped by upper
  expect_equal(out$a$e, 10)    # clamped by upper
  expect_equal(out$f, 0)       # clamped by lower
})

test_that("ParmLimLo leaves list unchanged when all values already above lower", {
  x <- list(a = list(b = list(c = 5, d = 3)), e = 10)
  out <- ParmLimLo(x, lower = 0)
  expect_equal(out, x)
})

test_that("ParmLimHi leaves list unchanged when all values already below upper", {
  x <- list(a = list(b = list(c = 1, d = 2)), e = 3)
  out <- ParmLimHi(x, upper = 100)
  expect_equal(out, x)
})

# ---------------------------------------------------------------------------
# Mixed leaf types: vectors inside nested lists -----------------------------
# ---------------------------------------------------------------------------

test_that("ParmLimLo clamps a named vector element nested inside a list", {
  x <- list(a = c(x = -1, y = 5), b = 3)
  # scalar broadcast clamps both elements of the vector
  out <- ParmLimLo(x, lower = 0)
  expect_equal(out$a, c(x = 0, y = 5))
  expect_equal(out$b, 3)
})

test_that("ParmLimHi clamps a matrix element nested inside a list", {
  m <- matrix(c(1, 10, 3, 20), nrow = 2)
  x <- list(mat = m, scalar = 5)
  out <- ParmLimHi(x, upper = 5)
  expect_equal(out$mat, matrix(c(1, 5, 3, 5), nrow = 2))
  expect_equal(out$scalar, 5)
})

# ---------------------------------------------------------------------------
# ParmOff integration: .lower / .upper / both with vector .args -------------
# ---------------------------------------------------------------------------

test_that("ParmOff .lower clamps a single vector argument", {
  f <- function(x) x
  out <- ParmOff(f, list(x = -5), .lower = list(x = 0))
  expect_equal(out, 0)
})

test_that("ParmOff .upper clamps a single vector argument", {
  f <- function(x) x
  out <- ParmOff(f, list(x = 100), .upper = list(x = 10))
  expect_equal(out, 10)
})

test_that("ParmOff .lower and .upper clamp simultaneously", {
  f <- function(x, y) x + y
  out <- ParmOff(f, list(x = -5, y = 20),
                 .lower = list(x = 0, y = 0),
                 .upper = list(x = 10, y = 10))
  expect_equal(out, 10)  # 0 + 10
})

test_that("ParmOff .lower/.upper with named list bounds clamps per-arg", {
  f <- function(a, b, c) a + b + c
  out <- ParmOff(f, list(a = -1, b = -2, c = 3),
                 .lower = list(a = 0, b = 0, c = 0),
                 .upper = list(a = 2, b = 2, c = 2))
  expect_equal(out, 2)  # 0 + 0 + 2
})

test_that("ParmOff .lower/.upper with deeply nested list arg", {
  f <- function(x) sum(unlist(x))
  input <- list(x = list(a = list(b = -10, c = 50), d = -3))
  out <- ParmOff(f, input,
                 .lower = list(x = list(a = list(b = 0), d = 0)),
                 .upper = list(x = list(a = list(c = 10))))
  expect_equal(out, 10)  # 0 + 10 + 0
})

test_that("ParmOff .bound_raw=FALSE applies bounds after de-logging", {
  # x is passed as log10(100)=2, .logged de-logs to 100, then upper=50 clamps to 50
  f <- function(x) x
  out <- ParmOff(f, list(x = 2), .logged = "x", .upper = list(x = 50), .bound_raw = FALSE)
  expect_equal(out, 50)
})

test_that("ParmOff .bound_raw=TRUE applies bounds before de-logging", {
  # x=2 clamped to upper=1 in log space first → then 10^1 = 10
  f <- function(x) x
  out <- ParmOff(f, list(x = 2), .logged = "x", .upper = list(x = 1), .bound_raw = TRUE)
  expect_equal(out, 10)
})

test_that("ParmOff .bound_raw=FALSE with logical .logged applies bounds after de-logging", {
  # x is passed as log10(100)=2, logical .logged de-logs to 100, then upper=50 clamps to 50
  f <- function(x) x
  out <- ParmOff(f, list(x = 2), .logged = TRUE, .upper = list(x = 50), .bound_raw = FALSE)
  expect_equal(out, 50)
})

test_that("ParmOff .bound_raw=TRUE with logical .logged applies bounds before de-logging", {
  # x=2 clamped to upper=1 in log space → then 10^1 = 10
  f <- function(x) x
  out <- ParmOff(f, list(x = 2), .logged = TRUE, .upper = list(x = 1), .bound_raw = TRUE)
  expect_equal(out, 10)
})

# ---------------------------------------------------------------------------
# verbose argument -----------------------------------------------------------
# ---------------------------------------------------------------------------

test_that("ParmLimLo verbose emits message when clamping occurs", {
  expect_message(
    ParmLimLo(list(a = -1, b = 5), lower = list(a = 0), verbose = TRUE),
    regexp = "Lower limit.*a"
  )
})

test_that("ParmLimHi verbose emits message when clamping occurs", {
  expect_message(
    ParmLimHi(list(a = -1, b = 5), upper = list(b = 3), verbose = TRUE),
    regexp = "Upper limit.*b"
  )
})

test_that("ParmLimBoth verbose emits messages for each clamped element", {
  msgs <- character(0)
  withCallingHandlers(
    ParmLimBoth(list(x = -2, y = 15), lower = list(x = 0), upper = list(y = 10),
                verbose = TRUE),
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") }
  )
  expect_true(any(grepl("Lower limit.*x", msgs)))
  expect_true(any(grepl("Upper limit.*y", msgs)))
})

test_that("ParmLimLo verbose is silent when no clamping occurs", {
  expect_silent(
    ParmLimLo(list(a = 1, b = 5), lower = list(a = 0), verbose = TRUE)
  )
})

test_that("ParmLimHi verbose is silent when no clamping occurs", {
  expect_silent(
    ParmLimHi(list(a = -1, b = 5), upper = list(a = 0), verbose = TRUE)
  )
})

test_that("ParmLimLo verbose=FALSE (default) is always silent", {
  expect_silent(ParmLimLo(list(a = -1), lower = list(a = 0)))
})

test_that("ParmLimLo verbose message includes before and after values", {
  msg <- ""
  withCallingHandlers(
    ParmLimLo(list(a = -3), lower = list(a = 0), verbose = TRUE),
    message = function(m) { msg <<- conditionMessage(m); invokeRestart("muffleMessage") }
  )
  expect_match(msg, "-3")
  expect_match(msg, "0")
})

test_that("ParmLimHi verbose message includes before and after values", {
  msg <- ""
  withCallingHandlers(
    ParmLimHi(list(y = 20), upper = list(y = 10), verbose = TRUE),
    message = function(m) { msg <<- conditionMessage(m); invokeRestart("muffleMessage") }
  )
  expect_match(msg, "20")
  expect_match(msg, "10")
})

test_that("ParmLimLo verbose works on named atomic vector", {
  expect_message(
    ParmLimLo(c(a = -5, b = 2), lower = 0, verbose = TRUE),
    regexp = "Lower limit"
  )
})

test_that("ParmLimLo verbose is silent for large vectors (length > 20)", {
  x_long <- as.list(setNames(rep(-1, 25), paste0("p", seq_len(25))))
  # no message expected because elements are scalars inside list — each is printed individually
  # but a plain atomic vector longer than 20 should be suppressed
  x_vec <- setNames(rep(-1, 25), paste0("p", seq_len(25)))
  expect_silent(ParmLimLo(x_vec, lower = 0, verbose = TRUE))
})

test_that("ParmOff .verbose passes to ParmLimLo", {
  f <- function(x) x
  expect_message(
    ParmOff(f, list(x = -5), .lower = list(x = 0), .verbose = TRUE),
    regexp = "Lower limit.*x"
  )
})

test_that("ParmOff .verbose passes to ParmLimHi", {
  f <- function(x) x
  expect_message(
    ParmOff(f, list(x = 20), .upper = list(x = 10), .verbose = TRUE),
    regexp = "Upper limit.*x"
  )
})
