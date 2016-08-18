context("TOA5 reading")

Sys.setenv(TZ='GMT')
fpath <- system.file("extdata", "Station_Daily.dat", package="csdf")
obj <- read.toa5(fpath)

test_that("valid csdf object created", {
  expect_s4_class(obj, "csdf")
  expect_true(validObject(obj))
  expect_true(validObject.csdf(obj))
})

test_that("coercion to data.frame works", {
  expect_s3_class(as.data.frame(obj), "data.frame")
})

test_that("summary works", {
  s <- summary(obj)
  expect_s3_class(s, c("list", "summary.csdf"))
  expect_output(print(s), "Data coverage is from")
})

test_that("Can read from connection", {
  f <- file(fpath)
  expect_s4_class( read.toa5(f), "csdf" )
})
