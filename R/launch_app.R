#' @import shiny raster dplyr shinyFiles shinyWidgets shinycssloaders parallel doSNOW leaflet ggplot2 plotly 
#' @importFrom data.table fread
#' @importFrom data.table fwrite
#' @export

launch_app <- function() {
  appDir <- system.file("shiny_app", package = "LETGmod")
  if (appDir == "") {
    stop("Could not find myapp. Try re-installing `mypackage`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}