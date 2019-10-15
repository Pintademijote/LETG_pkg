#' @export

launch_app <- function() {
  appdir <- system.file("shiny_app", package = "LETGmod")
  if (appdir == "") {
    stop("It appears that the app is not install, re-install `mypackage`.", call. = FALSE)
  }
  shiny::runApp(appdir, display.mode = "normal")
}