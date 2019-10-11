server <- function(input, output,session) {
  
  options(shiny.maxRequestSize=60*1024^2)
  
  volumes <- getVolumes()
  
  # shinyDirChoose(input, 'directory', roots=volumes, session=session)
  # dir <- reactive({
  #   req(input$directory_rast_pred)
  #   paste(as.character(parseDirPath(volumes,input$directory_rast_pred)),"/",sep = "")
  # })
  
  MODEL<- reactiveValues(data = NULL,pred= NULL,pred_Bolean=F)
  MOD_PRED<- reactiveValues(data = NULL)

  
  x=reactive({
    if(is.null(input$tmin$datapath)){
      read.csv("./data/temp_test.csv")[-1]
    }else{read.csv(input$tmin$datapath)[,-1]}
  })
  
  variable=reactive({
    if(is.null(input$tmin$datapath)){
      read.csv("./data/var_test.csv")[-1]
    }else{read.csv(input$variables$datapath)[,-1]}
  })
  
  mergeX_VAR=reactive({
    cbind(x(),variable())
  })
  
  epsilon=reactive({
    seq(input$seq_epsilon[1],input$seq_epsilon[2],by=input$nb_seq_epsilon)
  })
  
  cost=reactive({
    c(2^(-input$seq_cost[1]:input$seq_cost[2]))
  })
  
  
  shinyDirChoose(input, 'directory_rast_pred', roots=volumes, session=session)
  rasters_pred=reactive({
    # req(input$directory_rast_pred)
    ref=list.files(paste(as.character(parseDirPath(volumes,input$directory_rast_pred)),"/",sep = ""),full.names = T)
    print(ref)
    print(volumes)
    print(input$directory_rast_pred)
    # ref=list.files(paste("C:/Users/pglem/Documents/Master/LETG/MNT",sep = ""),full.names = T)
    raster::stack(ref)
  })
  
  # observeEvent(input$directory_rast_pred,{
  #   updateSelectInput(session,"date_pred",choices = names(MOD_PRED()))
  # })
  
  # observeEvent(input$slider_tmin[1],{
  #   updateSelectInput(session,"date_pred",
  #                     choices = colnames(x())[which(input$slider_date[1]==colnames(x())):which(input$slider_date[2]==colnames(x()))])
  # })
  
  output$slider_tmin <- renderUI({
    sliderTextInput(
      inputId = "slider_date",
      label = "Your choice:",
      grid = TRUE,
      force_edges = TRUE,
      choices = colnames(x()),
      selected = c(colnames(x())[1],colnames(x())[2])
    )
    
  })
  
  output$date_pred <- renderUI({
    selectInput("date_pred",  h5("Date"), choices = 
                  colnames(x())[which(input$slider_date[1]==colnames(x())):which(input$slider_date[2]==colnames(x()))])
    
  })
  
  output$variable_pred <- renderUI({
    selectInput("variable_pred",  h5("Variable"), choices = colnames(variable()))
    
  })
  
  output$plot_var<-renderPlotly({
    ggplot(mergeX_VAR(),aes_string(x=input$date_pred,y=input$variable_pred))+geom_point()
  })
  
  
  output$hist_x<-renderPlotly({
    ggplot(x(),aes_string(x=input$date_pred))+geom_histogram()
    })
  
  
  # output$test <- renderPrint({
  #   which(input$slider_date[1]==colnames(x())):which(input$slider_date[2]==colnames(x()))
  # })
  
  observeEvent(input$run,{
    print(input$kernel)
    
    pred_df=data.table::fread(paste0(config$path_save,"pred_df.csv"))
    perf_df=data.table::fread(paste0(config$path_save,"perf_df.csv"))
    
    if(input$parallelize==F){
      
      MODELS<-lapply(x()[,which(input$slider_date[1]==colnames(x())):which(input$slider_date[2]==colnames(x()))],FUN= function(x){
        poulou<-SVM_parral(Yvar = x,variable = variable(),cross = input$nb_cross,k = input$k,cost = cost(),kernel = input$kernel,epsilon = epsilon())
      })
      names(MODELS)=colnames(x()[,which(input$slider_date[1]==colnames(x())):which(input$slider_date[2]==colnames(x()))])
      
    }
    if(input$parallelize==T){
      
      temperatures<-x()[,which(input$slider_date[1]==colnames(x())):which(input$slider_date[2]==colnames(x()))]
      variable<-variable()
      cross<-input$nb_cross
      kernel<-input$kernel
      epsilon<-epsilon()
      cost<-cost()
      k=input$k
      
      cl=makeCluster(input$nb_core, type = "SOCK")
      registerDoSNOW(cl)
      
      clusterExport(cl, list("SVM_parral","variable","cross","kernel","epsilon","k","cost","tune.control","tune.svm",
                             "svm"),envir=environment())
      
      MODELS<-parLapply(cl,temperatures,fun = function(x){
        poulou<-SVM_parral(Yvar = x,variable = variable,cross = cross,k = k,cost = cost,kernel = kernel,epsilon = epsilon)
        return(poulou)
      })
      
      
      stopCluster(cl)
      names(MODELS)=colnames(temperatures)
      print(MODELS[[2]][[3]])
    }
    
    
    MODEL$data=MODELS
    
    predictor=rasters_pred()
    print(summary(predictor))
    MODELS_temp=MODEL$data
    
    # MOD_SVM<-lapply(MODELS_temp, function(x){x$svm_model})
    
    MOD_SVM<-list()
    for ( i in 1:length(MODELS_temp)){
      MOD_SVM[i]<-MODELS_temp[[i]]$svm_model
    }
    names(MOD_SVM)=names(MODELS_temp)
    
    MOD_PRED<-lapply(X=MOD_SVM,FUN=function(y){
      product_raster(model = y,predictor = predictor)
    })
    
    MOD_PRED<-raster::brick(MOD_PRED)
    names(MOD_PRED)<-names(MODELS_temp)
    
    MODEL$pred=MOD_PRED
    MODEL$pred_Bolean=T
    
    date=gsub("X","",names(MODELS_temp))
    
    temp_DON=do.call(rbind,lapply(MODELS_temp, function(x){
      x$DON
    }))
    temp_DON$date=do.call(c,lapply(date,function(x){rep(x,nrow(MODELS_temp[[1]]$DON))}))
    pred_df=rbind(temp_DON[!(temp_DON$date %in% pred_df$date),],as.data.frame(pred_df))
    
    temp_perf=data.frame(rmse=sapply(MODELS_temp, function(x){
      x$rmse
    }),
    date= date,row.names = NULL)
    perf_df=rbind(temp_perf[!(temp_perf$date %in% perf_df$date),],as.data.frame(perf_df))
    
    data.table::fwrite(pred_df,file = paste0(config$path_save,"pred_df.csv"))
    data.table::fwrite(perf_df,file = paste0(config$path_save,"perf_df.csv"))
    
    showModal(modalDialog(
      title = "Models are done",
      easyClose = TRUE,
      footer = NULL))
    
    
    
  })
  
  output$test=renderText({
    cat("OK MODS")
    summary(MODELS())
    names(MODELS()[[1]])
  })
  
  output$test3=renderText({
    cat("OK PRED")
    summary(rasters_pred())
    names(rasters_pred())
  })
  
  ############
  #######Predictions
  ############
  
  
  # MOD_PRED=reactive({
  #   # req(rasters_pred())
  #
  #   output$test2=renderText({
  #   print("1")
  #   predictor=rasters_pred()
  #   print("2")
  #   MOD_SVM<-lapply(MODELS(), function(x){x$svm_model})
  #   print("3")
  #   MOD_PRED<-lapply(MOD_SVM,FUN=function(x){
  #     product_raster(model = x,predictor = predictor)
  #   })
  #   print("4")
  #   MOD_PRED<-raster::brick(MOD_PRED)
  #   names(MOD_PRED)<-names(MODELS())
  #
  #   })
  #
  #   MOD_PRED
  # })
  
  # output$test2=renderText({
  #   cat("OK PRED")
  #   summary(MOD_PRED())
  # })
  

    output$pred_map <- renderLeaflet({
      if(MODEL$pred_Bolean==F) return()
  
      rast_to_plot=MODEL$pred
      rast_to_plot=rast_to_plot[[input$date_pred]]


      colfunc <- colorRampPalette(c("yellow", "red"))
      pal <- colorNumeric(c("#ffffff", colfunc(15)), values(rast_to_plot),
                          na.color = "transparent")

      leaflet() %>%
        addTiles() %>%
        addProviderTiles("Esri.WorldImagery") %>%
        addRasterImage(rast_to_plot,opacity = 0.5, group ="Predictions",colors=pal)%>%
        addLayersControl(
          overlayGroups = c("Predictions"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        addLegend(pal=pal,values = values(rast_to_plot),
                  title = "Predictions")

    })
  
  output$report <- downloadHandler(
    
    filename = "Report.html",
    content = function(file) {
      
      param <- list(x = isolate(x()[,which(input$slider_date[1]==colnames(x())):which(input$slider_date[2]==colnames(x()))]),
                     date_pred=isolate(input$date_pred))
      print(param)
      tempReport <- file.path(tempdir(), "Report.Rmd")
      file.copy("Report.Rmd", tempReport, overwrite = TRUE)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = param,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  

  
}
