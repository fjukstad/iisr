library(iisr)
test_that("read reads an example log file", {

  base_path = system.file(package="iisr")

  folder = file.path(base_path, "testdata")

  log =  read.iis(folder, extension="txt")

  expect_is(log, "data.frame") # is a data frame
  expect_equal(dim(log), c(142,21)) # correct dimensions
  expect_equal(unique(log$server), "server1") # server name is correct

})
