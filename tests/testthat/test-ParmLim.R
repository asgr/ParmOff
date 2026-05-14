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
