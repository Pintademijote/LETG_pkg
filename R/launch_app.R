#' @import shiny raster dplyr shinyFiles shinyWidgets shinycssloaders parallel doSNOW leaflet ggplot2 plotly 
#' @importFrom data.table fread
#' @importFrom data.table fwrite
#' @export

launch_app <- function() {
  appdir <- system.file("shiny_app", package = "LETGmod")
  if (appdir == "") {
    stop("It appears that the app is not install, re-install `mypackage`.", call. = FALSE)
  }
  shiny::runApp(appdir, display.mode = "normal")
}