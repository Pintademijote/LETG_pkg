#' @import caret e1071
#' @export

SVM_parral<-function(Yvar,variable,k=5,cost=1,epsilon=0.1,cross,kernel=c("radial","linear")){
  T0<-Sys.time()
  kernel<-kernel
  bob<-Yvar
  print(colnames(Yvar))
  print(names(Yvar))
  xl<-which(!is.na(bob)==T)
  coord_capt_var2012<-variable
  data_test<-cbind(bob,coord_capt_var2012)
  data_test<-data_test[xl,]
  data_test<-as.data.frame(data_test)
  colnames(data_test)[1]<-"Y"
  
  df1<-data_test
  colnames(df1)[1]<-"Y"
  ### SPLIT DATA INTO K FOLDS ###
  
  df1$fold <- caret::createFolds(1:nrow(df1), k = k, list = FALSE)
  ### PARAMETER LIST ###
  cost <- cost
  
  eps<-epsilon
  
  ### LOOP THROUGH PARAMETER VALUES ###
  
  ### K-FOLD VALIDATION ###
  out<-list()
  tc<-tune.control(nrepeat = 10,sampling = "cross",cross=cross)
  mdl<-tune.svm(Y~.,data=data_test,cost=cost,epsilon=epsilon,kernel=kernel,tune.control=tc)
  mdl<-mdl$best.model
  CO<-mdl$cost
  EPS<-mdl$epsilon
  for(j in 1:max(df1$fold) ){
    
    N<-ncol(df1)
    deve <- df1[df1$fold != j, ]
    test <- df1[df1$fold == j, ]
    
    MODpred <- svm(Y~.,data = deve ,scale = T,cost=CO,epsilon=EPS,decision.values = TRUE, probability = TRUE)
    pred<-predict(MODpred,test,decision.values = TRUE, probability = TRUE)
    DON<-data.frame(train = test$Y, pred = pred)
    DON$Capt<-rownames(DON) #?
    SS<-list(DON,mdl)
    out[[j]]<-SS
  }
  DATA<-do.call(args = out,what = "rbind")
  DON<-do.call(args=DATA[,1],what = "rbind")
  # rownames(DON)<-DON$Capt
  OP<-(DON$train-DON$pred)
  OP<-as.data.frame(OP)
  rownames(OP)<-rownames(data_test)
  DON$OP<-unlist(OP)
  mdl<-out[[1]][2]
  RMSE<-sqrt(mean((unlist(OP)^2),na.rm=T))
  result<-list(RES=DON,model=mdl,RMSE=RMSE)
  
  ### CALCULATE SVM PERFORMANCE ###
  # roc <- pROC::roc(as.factor(out$y), out$pred)
  
  
  
  ### CALCULATE SVM PERFORMANCE ###
  # roc <- pROC::roc(as.factor(out$y), out$pred)
  bestMod<-result
  names(bestMod)<-c("DON","svm_model","rmse")
  T1<-Sys.time()
  print(T1-T0)
  return(bestMod)
}