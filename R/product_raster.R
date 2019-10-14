#' @import raster
#' @export

product_raster <- function(model, predictor){
  T0 <- Sys.time()
  # to <- model
  raster_reference <- predictor
  predic <- raster::as.matrix(predictor)
  predic[is.na(predic)] <- 0
  pred_rast <- predict(model, predic)
  temp_svm <- raster::subset(raster_reference, 1)
  test <- raster::setValues(temp_svm, 0)
  temp_svm <- raster::setValues(test, pred_rast)
  # nom<-model$nom
  T1 <- Sys.time()
  print(T1 - T0)
  # writeRaster(temp_svm,paste(out_folder,nom,".tiff",sep=""))
  return(temp_svm)
}