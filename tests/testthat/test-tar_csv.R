test_that("tar_csv works", {
  export_path <- paste0(tempdir(),'/test')
  
  targets::tar_dir({
    targets::tar_script({
      list(
        targets::tar_target(
          iris,
          iris
        ),
        jftargets::tar_csv(
          iris,
          export_path = export_path
        )
      )
    })
    
    targets::tar_make(callr_function = NULL)
  })
  
  expect_true(file.exists(paste0(export_path,'/iris.csv')))
})
