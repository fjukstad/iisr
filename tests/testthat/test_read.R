library(iisr)

test_that("read reads an example log file in a directory", {

  base_path = system.file(package="iisr")

  folder = file.path(base_path, "testdata")

  log =  read.iis(folder, extension="txt")

  expect_is(log, "data.frame") # is a data frame
  expect_equal(dim(log), c(142,22)) # correct dimensions
  expect_equal(unique(log$server), "server1") # server name is correct

})

test_that("read reads an example log file from a directory with a filename filter", {

  base_path = system.file(package="iisr")

  folder = file.path(base_path, "testdata")

  filenames = c("u_ex171118-sample")

  log =  read.iis(folder, filenames=filenames, extension="txt")

  expect_is(log, "data.frame") # is a data frame
  expect_equal(dim(log), c(142,22)) # correct dimensions
  expect_equal(unique(log$server), "server1") # server name is correct

})
test_that("read reads an example log file with a service filter", {

  base_path = system.file(package="iisr")

  folder = file.path(base_path, "testdata")

  services = c("site")

  log =  read.iis(folder, cs_uri_stem=services, extension="txt")

  expect_is(log, "data.frame") # is a data frame
  expect_equal(dim(log), c(1,22)) # correct dimensions
  expect_equal(unique(log$server), "server1") # server name is correct

})

test_that("reads a single example log file", {

  base_path = system.file(package="iisr")

  filename = file.path(base_path, "testdata", "server1", "u_ex171118-sample.txt")

  log =  read.single_iis_logfile(filename)

  expect_is(log, "data.frame") # is a data frame
  expect_equal(dim(log), c(142,20)) # correct dimensions

})
