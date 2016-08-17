context("TOB1")

Sys.setenv(TZ='GMT')
fpath <- system.file("extdata", "TOB1_Station_Daily.dat", package="csdf")
fpath.toa5 <- system.file("extdata", "Station_Daily.dat", package="csdf")
obj <- read.tob1(fpath)

test_that("valid csdf object created", {
  expect_s4_class(obj, "csdf")
  expect_true(validObject(obj))
  expect_true(validObject.csdf(obj))
})

test_that("idential to source TOA5", {
  expect_output_file(write.toa5(obj), fpath.toa5)
})
