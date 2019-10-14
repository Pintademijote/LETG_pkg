server <- function(input, output, session) {
  options(shiny.maxRequestSize = 60 * 1024 ^ 2)
  volumes <- shinyFiles::getVolumes()
  ################
  ######Input
  ###############
  ###############
  ##Data
  ###############
  model_save <- reactiveValues(data = NULL, pred = NULL, pred_Bolean = F)

  x <- reactive({
    if (is.null(input$tmin$datapath)){
      read.csv("./data/temp_test.csv")[, -1]
    }else{
      read.csv(input$tmin$datapath)[, -1]
      }
  })
  
  variable <- reactive({
    if (is.null(input$tmin$datapath)){
      read.csv("./data/var_test.csv")[, -1]
    }else{
      read.csv(input$variables$datapath)[, -1]
      }
  })
  
  shinyFiles::shinyDirChoose(
    input, "directory_rast_pred", roots = volumes, session = session)
  
  rasters_pred <- reactive({
    ref  <- list.files(
      paste(as.character(shinyFiles::parseDirPath(volumes, input$directory_rast_pred)),
            "/", sep = ""), full.names = T)
    raster::stack(ref)
  })
  
  merge_x_var <- reactive({
    cbind(x(), variable())
  })
  
  ###############
  ##Parameters
  ###############
  
  epsilon <- reactive({
    seq(input$seq_epsilon[1], input$seq_epsilon[2], by = input$nb_seq_epsilon)
  })
  
  cost <- reactive({
    c(2 ^ (input$seq_cost[1]:input$seq_cost[2]))
  })
  
  output$slider_tmin <- renderUI({
    shinyWidgets::sliderTextInput(
      inputId = "slider_date",
      label = "Your choice:",
      grid = TRUE,
      force_edges = TRUE,
      choices = colnames(x()),
      selected = c(colnames(x())[1], colnames(x())[2])
    )
  })
  
  output$date_pred <- renderUI({
    selectInput("date_pred",  h5("Date"), choices = 
                  colnames(x())[which(input$slider_date[1] == colnames(x())):
                                  which(input$slider_date[2] == colnames(x()))])
  })
  
  output$variable_pred <- renderUI({
    selectInput("variable_pred",  
                h5("Variable"), 
                choices = colnames(variable()))
  })
  
  ################
  ######Output 
  ###############
  ###############
  ##Model
  ###############
  
  observeEvent(input$run, {
    pred_df <- data.table::fread(paste0(config$path_save, "pred_df.csv"))
    perf_df <- data.table::fread(paste0(config$path_save, "perf_df.csv"))
    
    if (input$parallelize == F){
      models <- lapply(x()[, which(input$slider_date[1] == colnames(x())):
                             which(input$slider_date[2] == colnames(x()))],
                       FUN = function(x){
                         SVM_parral(Yvar = x, 
                                    variable = variable(), 
                                    cross = input$nb_cross,
                                    k = input$k,
                                    cost = cost(),
                                    kernel = input$kernel,
                                    epsilon = epsilon())
                       })
      names(models) <- colnames(x()[, which(input$slider_date[1] == colnames(x())):
                                      which(input$slider_date[2] == colnames(x()))])
    }
    if (input$parallelize == T){
      temperatures <- x()[, which(input$slider_date[1] == colnames(x())):
                            which(input$slider_date[2] == colnames(x()))]
      variable <- variable()
      cross <- input$nb_cross
      kernel <- input$kernel
      epsilon <- epsilon()
      cost <- cost()
      k <- input$k
      #Cluster
      cl <- parallel::makeCluster(input$nb_core, type = "SOCK")
      doSNOW::registerDoSNOW(cl)
      parallel::clusterExport(cl,
                    list("SVM_parral", "variable", "cross", 
                         "kernel", "epsilon", "k",
                         "cost", "tune.control", "tune.svm",
                         "svm"), 
                    envir = environment())
      #parLapply
      models <- parallel::parLapply(cl, temperatures, fun = function(x){
        poulou <- SVM_parral(Yvar = x, variable = variable, cross = cross, 
                             k = k, cost = cost, kernel = kernel, 
                             epsilon = epsilon)
        return(poulou)
      })
      parallel::stopCluster(cl)
      names(models) <- colnames(temperatures)
    }
    #Output models
    model_save$data <- models
    #Predictions
    mode_svm <- list()
    for (i in 1:length(model_save$data)){
      mode_svm[i] <- model_save$data[[i]]$svm_model
    }
    names(mode_svm) <- names(model_save$data)
    mod_pred <- lapply(X = mode_svm, FUN = function(y){
      product_raster(model = y, predictor = rasters_pred())
    })
    mod_pred <- raster::brick(mod_pred)
    names(mod_pred) <- names(model_save$data)
    #Output predictions
    model_save$pred <- mod_pred
    model_save$pred_Bolean <- T
    #Saving output
    date <- gsub("X", "", names(model_save$data))
    temp_don <- do.call(rbind, lapply(model_save$data, function(x){
      x$DON
    }))
    temp_don$date <- do.call(c, lapply(date, function(x){
      rep(x, nrow(model_save$data[[1]]$DON))
      }))
    pred_df <- rbind(
      temp_don[!(temp_don$date %in% pred_df$date), ], 
      as.data.frame(pred_df))
    temp_perf <- data.frame(rmse = sapply(model_save$data, function(x){
      x$rmse
    })
    , date <- date, row.names = NULL)
    perf_df <- rbind(
      temp_perf[!(temp_perf$date %in% perf_df$date), ], 
      as.data.frame(perf_df))
    #Writing results
    data.table::fwrite(pred_df, file = paste0(config$path_save, "pred_df.csv"))
    data.table::fwrite(perf_df, file = paste0(config$path_save, "perf_df.csv"))
    #Completion message
    showModal(modalDialog(
      title = "Models are done",
      easyClose = TRUE,
      footer = NULL))
  })
  
  ###############
  ##Plots
  ###############
  
  output$plot_var <- plotly::renderPlotly({
    ggplot(merge_x_var(), 
           aes_string(x = input$date_pred, y = input$variable_pred))+ 
      geom_point()
  })
  
  output$hist_x <- plotly::renderPlotly({
    ggplot(x(), aes_string(x = input$date_pred))+ 
      geom_histogram()
    })
  
  output$pred_map <- renderLeaflet({
    if (model_save$pred_Bolean == F) return()
    colfunc <- colorRampPalette(c("yellow", "red"))
    pal <- colorNumeric(c("#ffffff", colfunc(15)), 
                        values(model_save$pred[[input$date_pred]]),
                          na.color = "transparent")
    leaflet() %>%
      addTiles() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addRasterImage(model_save$pred[[input$date_pred]], 
                     opacity = 0.5, group = "Predictions", colors = pal)%>%
      addLayersControl(
        overlayGroups = c("Predictions"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(pal = pal, values = values(model_save$pred[[input$date_pred]]),
                  title = "Predictions")
    })
    
    ###############
    ##Download
    ###############
  
  output$report <- downloadHandler(
    filename = "Report.html",
    content = function(file) {
      param <- list(x = isolate(x()[, which(input$slider_date[1] == colnames(x())):
                                      which(input$slider_date[2] == colnames(x()))]),
                     date_pred=isolate(input$date_pred))
      temp_report <- file.path(tempdir(), "Report.Rmd")
      file.copy("Report.Rmd", temp_report, overwrite = TRUE)
      rmarkdown::render(temp_report, output_file = file,
                        params = param,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  

  
}