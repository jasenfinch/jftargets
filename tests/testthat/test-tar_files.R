
test_that("tar_files works", {
  
  file_path <- tempdir()
  
  targets::tar_dir({
    targets::tar_script({
      list(
        targets::tar_target(
          iris,
          write.csv(iris,paste0(!!file_path,'/iris.csv'))
        ),
        jftargets::tar_files(test,paste0(!!file_path,'/iris.csv'))
      )
    })
    
    targets::tar_make(callr_function = NULL)
  })
  
  expect_true(file.exists(paste0(!!file_path,'/iris.csv')))
})
