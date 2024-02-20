library(targets)
list(targets::tar_target(iris_plot, ggplot2::ggplot(iris, ggplot2::aes(x = Sepal.Length, 
    y = Sepal.Width)) + ggplot2::geom_point()), jftargets::tar_png(iris_plot, 
    export_path = export_path))
