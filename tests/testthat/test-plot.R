context("TOA5 plotting")

Sys.setenv(TZ='GMT')
fpath <- system.file("extdata", "Station_Daily.dat", package="csdf")
obj <- read.toa5(fpath)
pdf(NULL)
# on.exit(dev.off())
p1 <- plot(obj)
p2 <- plot(obj, meta=FALSE)

test_that("we got correct classes", {
  expect_s3_class(p1, c("gtable", "gTree", "grob", "gDesc"))
  expect_s3_class(p2, c("gtable", "gTree", "grob", "gDesc"))
})

test_that("plot has panes", {
  expect_equal(max(p1$grobs[[2]]$layout$t), 4)
  expect_equal(max(p2$layout$t), 4)
})

test_that("plot succeeds", {
  expect_null( grid::grid.draw(p1) )
})

dev.off()
