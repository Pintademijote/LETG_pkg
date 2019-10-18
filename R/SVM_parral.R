#' @export

SVM_parral <- function(Yvar, variable, k=5, 
                       cost=1, epsilon=0.1, cross=5, 
                       kernel=c("radial", "linear")){
  T0 <- Sys.time()
  data_test <- variable
  data_test$Y <- Yvar
  data_test <- data_test[!is.na(data_test$Y),]
  ### SPLIT DATA INTO K FOLDS ###
  df1 <- data_test
  df1$fold <- caret::createFolds(1:nrow(data_test), k = k, list = FALSE)
  ### K-FOLD VALIDATION ###
  tc <- e1071::tune.control(nrepeat = 10, sampling = "cross", cross = cross)
  mdl <- e1071::tune.svm(Y ~ ., data = data_test, cost = cost, epsilon = epsilon, 
                         kernel = kernel, tune.control = tc)
  mdl <- mdl$best.model
  CO <- mdl$cost
  EPS <- mdl$epsilon
  out <- lapply(sort(unique(df1$fold)), FUN = function(x){
    deve <- df1[df1$fold != x, ]
    test <- df1[df1$fold == x, ]
    MODpred <- e1071::svm(Y ~ ., data = deve, scale = T, 
                          cost = CO, epsilon = EPS, decision.values = TRUE,
                          probability = TRUE)
    pred <- predict(MODpred, test, decision.values = TRUE, probability = TRUE)
    DON <- data.frame(train = test$Y, pred = pred)
    DON$Capt <- rownames(DON) #?
    SS <- list(DON)
    return(DON)
  })
  DON <- do.call("rbind", out)
  DON$OP <- DON$train - DON$pred
  RMSE <- sqrt(mean( (DON$OP ^ 2), na.rm = T))
  result <- list(RES = DON, model = mdl, RMSE = RMSE)
  ### CALCULATE SVM PERFORMANCE ###
  bestMod <- result
  names(bestMod) <- c("DON", "svm_model", "rmse")
  T1 <- Sys.time()
  print(T1 - T0)
  return(bestMod)
}