test_that("tar_png works", {
  
  export_path <- paste0(tempdir(),'/test')
  
  targets::tar_dir({
    targets::tar_script({
      list(
        targets::tar_target(
          iris_plot,
          ggplot2::ggplot(
            iris,
            ggplot2::aes(
              x = Sepal.Length,
              y = Sepal.Width
            ) 
          ) +
            ggplot2::geom_point()
        ),
        jftargets::tar_png(
          iris_plot,
          export_path = export_path
        )
      )
    })
    
    targets::tar_make(callr_function = NULL)
  })
  
  expect_true(file.exists(paste0(export_path,'/iris_plot.png')))
})
