context("ulong tests")

test_that("can read ulong", {
  con <- rawConnection(c(as.raw(0xff), raw(3)))
  expect_equal(read.ulong(con, "little"), 0x000000ff)
  con <- rawConnection(c(as.raw(0xff), raw(3)))
  expect_equal(read.ulong(con, "big"), 0xff000000)
})
