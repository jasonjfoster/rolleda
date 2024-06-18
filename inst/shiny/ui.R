ui <- shiny::fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "yeti"),
  
  # library(magick)
  # logo <- image_read("logo.png")
  # logo <- image_scale(logo, "x50")
  # image_write(logo, "logo.png")
  
  shiny::titlePanel(title = shiny::div(shiny::img(src = "www/logo.png"),
                                       shiny::HTML("<b>Rolling exploratory data analysis</b>")),
                    window = "Rolling exploratory data analysis"),
  
  shiny::sidebarLayout(
    
    shiny::sidebarPanel(

      shiny::uiOutput("input_files"),
      shiny::uiOutput("select_file"),
      shiny::uiOutput("select_dep"),
      shiny::uiOutput("select_indep"),
      shiny::uiOutput("statistic"),
      shiny::numericInput("width", "Rolling window", 100, min = 1),
      shiny::uiOutput("plot_variable"),
      shiny::downloadButton("download", "Download"),
      shiny::actionButton("reset_files", "Reset"),
      shiny::br(), shiny::br(),
      "For questions, please email ", shiny::a("Jason Foster", href = "mailto:jason.j.foster@gmail.com"),
      width = 3

    ),
    
    shiny::mainPanel(
      dygraphs::dygraphOutput("result_plot", height = 700)
    ),
    
    position = "right"
    
  )
  
)