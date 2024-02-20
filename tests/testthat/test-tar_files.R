
test_that("tar_files works", {
  targets <- tar_files(test,paste0(tempdir(),'test.csv'))
  
  expect_identical(class(targets),'list')
})
