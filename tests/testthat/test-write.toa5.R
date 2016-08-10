context("TOA5 writing")

Sys.setenv(TZ='GMT')
fpath <- system.file("extdata", "Station_Daily.dat", package="csdf")
obj <- read.toa5(fpath)

test_that("written file is identical to original", {
  expect_output_file(write.toa5(obj), fpath)
})
