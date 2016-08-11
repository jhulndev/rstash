library(rstash)
library(magrittr)
context('file_path')


## Stash Directories -----------------------------------------------------------
local <- local_stash(path = 'local/test')
ftp <- ftp_stash(server = '123.45.6.78', path = 'ftp/test', user = 'testuser',
    password = 'testpassword')
s3 <- s3_stash(bucket = 's3-test-bucket', path = 's3/test',
    access.key.id = 'testaccesskey', secret.access.key = 'testsecretkey',
    region = 'test-region')

all.stashes <- list(local, ftp, s3)

## Stash Files: ----------------------------------------------------------------
local.file <- local_stash(path = 'R/as.ftp_stash.R', is.file = TRUE)
local.base <- local_stash(path = 'data_dir', base.file = 'testing_data')
local.base.ext <- local_stash(path = '', base.file = 'testing_data',
    extension = 'csv', compression = 'gz', time.stamp = TRUE)
local.base.ext.file <- local.base.ext %>% as.stash_file()

all.files <- list(local.file, local.base, local.base.ext, local.base.ext.file)


## Start Tests: ----------------------------------------------------------------
test_that('Simple path', {
  paths <- file_path(all.stashes, 'archive')

  expect_identical(get_directory(paths),
    list('local/test/archive', 'ftp/test/archive', 's3/test/archive'))

})


test_that('1) file_path s3_stash dir and local_stash path where is.file = TRUE', {
  paths <- file_path(s3, local.file)
  expect_identical(get_directory(paths), 's3/test/R')
  expect_equal(paths[[1]],
    'http://s3-test-region.amazonaws.com/s3-test-bucket/s3/test/R/as.ftp_stash.R')
})


test_that('2) file_path s3_stash dir and local_stash file where is.file = FALSE', {
  paths <- file_path(s3, local.base.ext.file)
  expect_equal(get_directory(paths), 's3/test')
  expect_match(paths[[1]],
    '^http://s3-test-region.amazonaws.com/s3-test-bucket/s3/test/testing_data_D[0-9]{8}T[0-9]{8}.csv.gz$')
})


test_that('3) file_path s3_stash dir and local_stash base file', {
  paths <- file_path(s3, local.base.ext)
  expect_identical(get_directory(paths), 's3/test')
  expect_equal(paths[[1]],
    'http://s3-test-region.amazonaws.com/s3-test-bucket/s3/test')
})






