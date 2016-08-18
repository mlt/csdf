context("TOB1")

Sys.setenv(TZ='GMT')
fpath <- system.file("extdata", "TOB1_Station_Daily.dat", package="csdf")
fpath.toa5 <- system.file("extdata", "Station_Daily.dat", package="csdf")

test_that("ULONG warning works", {
  expect_warning(
    assign("obj",
           read.tob1(fpath),
           envir=parent.frame(4)),
    "ULONG" )
#  expect_warning( obj <<- read.tob1(fpath), "ULONG" )
})

test_that("valid csdf object created", {
  expect_s4_class(obj, "csdf")
  expect_true(validObject(obj))
  expect_true(validObject.csdf(obj))
})

test_that("identical to source TOA5", {
  expect_output_file(write.toa5(obj), fpath.toa5)
})
