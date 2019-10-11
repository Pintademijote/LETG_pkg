ui <- fluidPage(
  tabsetPanel(
    tabPanel("SVM", Fluid=TRUE,
             titlePanel("SVM"),
             sidebarLayout(
               sidebarPanel(
                 h3("Add your the files"),
                 fileInput("tmin", h5("tmin")),
                 uiOutput("slider_tmin"),
                 fileInput("variables", h5("variables")),
                 shinyDirButton("directory_rast_pred",
                                "Patch to the folder of your rasters", "Please select a folder",
                                FALSE),
                 h3("Tune the parameters"),
                 # shinyDirButton("directory_rast_pred",
                 #                "Add your path to the folder where ascii are stored", "Please select a folder",
                 #                FALSE),
                 selectInput("kernel",  h5("Type of kernel"), choices = c("radial","linear")),
                 numericInput("nb_cross", "Number of cross:", 5, min = 1, max = Inf),
                 numericInput("k", "Number of k:", 5, min = 1, max = Inf),
                 numericInput("nb_seq_epsilon", "Observations:", 0.05, min = 0, max = 1, step = 0.01),
                 sliderInput(inputId = "seq_epsilon",
                             label = "Epsilon:",
                             min = 0,
                             max = 1,
                             value = c(0,1)),
                 # numericInput("nb_seq_cost", "Observations:", 10, min = 1, max = 100),
                 sliderInput(inputId = "seq_cost",
                             label = "Epsilon:",
                             min = -3,
                             max = 8,
                             value = c(-3,8)),
                 numericInput("nb_core", "Observations:", 1, min = 1, max = parallel::detectCores(), 1),
                 shinyWidgets::switchInput(inputId = "parallelize",value=FALSE),
                 actionButton("run", "Run Analysis"),
                 downloadButton("report", "Generate report")

               ),
               mainPanel(
                 # selectInput("date_pred",  h5("Date"), choices = NULL),
                 uiOutput("date_pred") %>% withSpinner(color="#0dc5c1"),
                 uiOutput("variable_pred") %>% withSpinner(color="#0dc5c1"),
                 plotlyOutput("hist_x") %>% withSpinner(color="#0dc5c1"),
                 plotlyOutput("plot_var") %>% withSpinner(color="#0dc5c1"),
                 leafletOutput("pred_map",width = "100%", height = 600) %>% withSpinner(color="#0dc5c1")
               )
             )

    )
    # tabPanel("Predictions", Fluid=TRUE,
    #          titlePanel("Predictions"),
    #          sidebarLayout(
    #            sidebarPanel(
    #              shinyDirButton("directory_rast_pred",
    #                             "Patch to the folder of your rasters", "Please select a folder",
    #                             FALSE),
    #              selectInput("date_pred",  h5("Date"), choices = NULL)
    #            ),
    #            mainPanel(
    #              leafletOutput("pred_map",width = "100%", height = 600) %>% withSpinner(color="#0dc5c1")
    #            )
    #          )
    # )
  )
)