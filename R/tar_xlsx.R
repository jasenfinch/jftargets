#' Save a data frame as an xlsx
#' @description
#' A target for exporting a data frame as a xlsx file.
#' @param dataframe_target the name of the target that will generate the data frame for export
#' @param export_path the destination path of the exported xlsx file
#' @return A target object for exporting a plot as a png.
#' @details
#' The name of the returned target will be the name of the data frame target with the prefix `export_`.
#' The file name of the exported xlsx file will be that of the argument provided to `dataframe_target`. 
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#'   targets::tar_dir({
#'     targets::tar_script({
#'       list(
#'         targets::tar_target(
#'           iris,
#'           iris
#'         ),
#'         jftargets::tar_xlsx(iris)
#'       )
#'     })
#'     
#'     targets::tar_make()
#'     tar_read(export_iris)
#'   })
#' }
#' @export

tar_xlsx <- function(dataframe_target,export_path = 'exports'){
  dataframe_target <- rlang::enexpr(dataframe_target)
  
  target_name <- paste0(
    'export_',
    targets::tar_deparse_language(dataframe_target)
  )
  
  if (!dir.exists(export_path)) {
    dir.create(export_path, recursive = TRUE)
  }
  
  file_name <- paste0(export_path, '/', rlang::expr_text(dataframe_target), '.xlsx')
  
  targets::tar_target_raw(
    target_name,
    rlang::expr(
      jfmisc::exportXLSX(!!dataframe_target,!!file_name)
    ),
    format = 'file'
  )
}