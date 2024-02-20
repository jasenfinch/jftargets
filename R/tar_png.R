#' Save a plot as a png
#' @description
#' A target for exporting a plot target as a png.
#' @inheritParams ggplot2::ggsave
#' @param plot_target the name of the target that will generate the plot to export
#' @param export_path the destination path of the exported plot file
#' @return A target object for exporting a plot as a png.
#' @details
#' The name of the returned target will be the name of the plot target with the prefix `export_`.
#' The file name of the exported plot will be that of the argment provided to `plot_target`. 
#' 
#' @examples
#' # example code
#' 
#' @export

tar_png <- function(
    plot_target,
    width = 9,
    height = 5,
    dpi = 300,
    units = c("in", "cm", "mm", "px"),
    export_path = 'exports/figures') {

    plot_target <- rlang::enexpr(plot_target)

    target_name <- paste0(
        'export_',
        targets::tar_deparse_language(plot_target)
    )

    if (!dir.exists(export_path)) {
      dir.create(export_path, recursive = TRUE)
    }
    
    file_name <- paste0(export_path, '/', rlang::expr_text(plot_target), '.png')

    targets::tar_target_raw(
        target_name,
        rlang::expr(
            {
                ggplot2::ggsave(
                    !!file_name,
                    plot = !!plot_target,
                    width = !!width,
                    height = !!height,
                    dpi = !!dpi,
                    units = !!units
                )
                !!file_name
            }
        ),
        format = 'file'
    )
}
