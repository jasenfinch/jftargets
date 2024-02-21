test_that("tar_xlsx works", {
  export_path <- paste0(tempdir(),'/xlsx')
  
  targets::tar_dir({
    targets::tar_script({
      list(
        targets::tar_target(
          iris,
          iris
        ),
        jftargets::tar_xlsx(
          iris,
          export_path = export_path
        )
      )
    })
    
    targets::tar_make(callr_function = NULL)
  })
  
  expect_true(file.exists(paste0(export_path,'/iris.xlsx')))
})
