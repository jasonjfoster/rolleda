##' Rolling Exploratory Data Analysis
##'
##' A web application for rolling exploratory data analysis of time-series data.
##' 
##' @param ... Additional arguments passed to \code{\link[shiny:shinyApp]{shiny::shinyApp}}.
##' @examples
##' \dontrun{
##' # rolling exploratory data analysis
##' roll_eda()
##' }
##' @export
roll_eda <- function(...) {
  
  shiny::addResourcePath("www", system.file("www", package = "rolleda"))
  
  ui <- shiny::fluidPage(
    
    # shinythemes::themeSelector(),
    
    theme = shinythemes::shinytheme("yeti"),
    
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
        shiny::numericInput("width", "Rolling window", 100),
        shiny::uiOutput("plot_variable"),
        shiny::downloadButton("download", "Download"),
        shiny::actionButton("reset_files", "Reset"),
        shiny::br(), shiny::br(),
        "For questions, please email ", shiny::a("Jason Foster", href = "mailto:jason.j.foster@gmail.com"),
        width = 3
        
      ),
      
      shiny::mainPanel(
        dygraphs::dygraphOutput("result_plot", height = 700)
      )
      
    )
    
  )
  
  server <- function(input, output) {
    
    values <- shiny::reactiveValues(files_ls = list("nycflights13.csv" = nycflights13::weather),
                                    select_file = "nycflights13.csv",
                                    file_df = nycflights13::weather,
                                    file_idx = "time_hour",
                                    select_dep = "temp",
                                    select_indep = c("dewp", "humid"),
                                    subset_df = nycflights13::weather,
                                    result_xts = nycflights13::weather)
    
    shiny::observeEvent(input$input_files, {
      
      files_ls <- lapply(input$input_files$datapath, fread_idx)
      names(files_ls) <- input$input_files$name
      
      if (length(values$files_ls) == 0) {
        values$files_ls <- files_ls
      } else {
        
        duplicate <- names(files_ls)[names(files_ls) %in% names(values$files_ls)]
        values$files_ls[duplicate] <- files_ls[duplicate]
        values$files_ls <- c(values$files_ls, files_ls[!names(files_ls) %in% duplicate])
        
      }
      
    })
    
    output$input_files <- shiny::renderUI({
      
      input$input_files # reset the input
      
      shiny::fileInput("input_files", "Choose data file(s)",
                       multiple = TRUE)
      
    })
    
    shiny::observeEvent(input$select_file, {
      
      values$select_file <- input$select_file
      values$file_df <- values$files_ls[[values$select_file]]
      values$file_idx <- colnames(values$file_df)[vapply(values$file_df, is_date, logical(1))][1]
      values$select_dep <- colnames(values$file_df)[vapply(values$file_df, is.numeric, logical(1))]
      values$select_indep <- colnames(values$file_df)[vapply(values$file_df, is.numeric, logical(1))]
      values$subset_df <- values$files_ls[[values$select_file]]
      values$result_xts <- values$files_ls[[values$select_file]]
      
    })
    
    output$select_file <- shiny::renderUI({
      
      shiny::selectizeInput("select_file", "Select data file",
                            choices = names(values$files_ls),
                            selected = values$select_file)
      
    })
    
    output$select_dep <- shiny::renderUI({
      
      shiny::selectizeInput("select_dep", "Dependent variable(s)",
                            choices = values$select_dep,
                            selected = "temp",
                            multiple = TRUE)
      
    })
    
    output$select_indep <- shiny::renderUI({
      
      shiny::selectizeInput("select_indep", "Independent variable(s)",
                            choices = values$select_indep,
                            selected = c("dewp", "humid"),
                            multiple = TRUE)
      
    })
    
    output$statistic <- shiny::renderUI({
      
      # see fBasics::basicStats
      shiny::selectInput("statistic", "Rolling statistic",
                         choices = c("Level",
                                     "Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Maximum",
                                     "Variance", "Stdev", "Z-Score",
                                     "Covariance", "Correlation", "Beta", "R-squared"),
                         selected = "Beta")
      
    })
    
    shiny::observe({
      
      shiny::validate(shiny::need(c(input$select_dep, input$select_indep) %in% colnames(values$file_df), ""))
      
      cols <- c(values$file_idx, input$select_dep, input$select_indep)
      
      # values$subset_df <- data.table::as.data.table(values$file_df)[order(get(values$file_idx)), cols, with = FALSE]
      values$subset_df <- data.table::as.data.table(values$file_df)[ , cols, with = FALSE]
      subset_xts <- data.table::as.xts.data.table(values$subset_df)
      
      roll_fn <- function(fn, ...) {
        fn(subset_xts[, c(input$select_dep, input$select_indep)], width = input$width, ...)
      }
      
      roll_cov_fn <- function(fn) {
        
        shiny::req(input$select_dep)
        shiny::req(input$select_indep)
        
        result_xts <- fn(
          x = subset_xts[, input$select_indep],
          y = subset_xts[, input$select_dep],
          width = input$width
        )
        
        dimnames(result_xts)[[3]] <- as.character(zoo::index(subset_xts))
        result_dt <- data.table::as.data.table(result_xts)
        result_dt[ , variable := paste(get("V2"), get("V1"), sep = "_")]
        result_dt <- data.table::dcast(result_dt, V3 ~ variable, fun.aggregate = mean)
        result_dt[ , V3 := parse_dates(get("V3"))]
        data.table::setcolorder(result_dt, c("V3", paste0(rep(input$select_dep, each = length(input$select_indep)),
                                                          "_", input$select_indep)))
        
        result_xts <- data.table::as.xts.data.table(result_dt)
        
        return(result_xts)
        
      }
      
      roll_lm_fn <- function(statistic) {
        
        shiny::req(input$select_dep)
        shiny:: req(input$select_indep)
        
        result_xts <- roll::roll_lm(
          x = subset_xts[, input$select_indep],
          y = subset_xts[, input$select_dep],
          width = input$width
        )
        
        if (statistic == "R-squared") {
          
          result_xts <- result_xts$r.squared
          
          if (is.list(result_xts)) {
            result_xts <- do.call(merge, result_xts)
          }
          
          colnames(result_xts) <- input$select_dep
          
        } else if (statistic == "Beta") {
          
          result_xts <- result_xts$coefficients
          
          if (is.list(result_xts)) {
            result_xts <- do.call(merge, result_xts)
          }
          
          colnames(result_xts) <- paste0(rep(input$select_dep, each = length(input$select_indep) + 1),
                                         "_", c("(Intercept)", input$select_indep))
          
        }
        
        return(result_xts)
        
      }
      
      statistic_map <- list(
        "Level" = function() subset_xts[, c(input$select_dep, input$select_indep)],
        "Minimum" = function() roll_fn(roll::roll_min),
        "1st Quartile" = function() roll_fn(roll::roll_quantile, p = 0.25),
        "Median" = function() roll_fn(roll::roll_median),
        "Mean" = function() roll_fn(roll::roll_mean),
        "3rd Quartile" = function() roll_fn(roll::roll_quantile, p = 0.75),
        "Maximum" = function() roll_fn(roll::roll_max),
        "Variance" = function() roll_fn(roll::roll_var),
        "Stdev" = function() roll_fn(roll::roll_sd),
        "Z-Score" = function() roll_fn(roll::roll_scale),
        "Covariance" = function() roll_cov_fn(roll::roll_cov),
        "Correlation" = function() roll_cov_fn(roll::roll_cor),
        "R-squared" = function() roll_lm_fn("R-squared"),
        "Beta" = function() roll_lm_fn("Beta")
      )
      
      values$result_xts <- statistic_map[[input$statistic]]()
      
    })
    
    output$plot_variable <- shiny::renderUI({
      
      shiny::req(input$select_dep)
      shiny::req(input$select_indep)
      
      shiny::selectizeInput("plot_variable", "Plot variable(s)",
                            choices = colnames(values$result_xts),
                            selected = colnames(values$result_xts),
                            multiple = TRUE)
      
    })
    
    output$result_plot <- dygraphs::renderDygraph({
      
      shiny::validate(shiny::need(inherits(values$result_xts, "xts"), ""))
      
      n_dep <- length(input$select_dep)
      n_indep <- length(input$select_indep)
      
      if (input$statistic %in% c("Level", "Minimum", "1st Quartile", "Median", "Mean",
                                 "3rd Quartile", "Maximum", "Variance", "Stdev", "Z-Score")) {
        
        colors <- palette_jjf(n_dep + n_indep)
        
      } else if (input$statistic %in% c("Covariance", "Correlation")) {
        
        if (n_dep > 1) {
          colors <- palette_jjf(n_dep, n_indep)
        } else {
          colors <- palette_jjf(n_indep)
        }
        
      } else if (input$statistic == "R-squared") {
        
        shiny::req(input$select_dep)
        
        colors <- palette_jjf(n_dep)
        
      } else if (input$statistic == "Beta") {
        
        shiny::req(input$select_dep)
        shiny::req(input$select_indep)
        
        if (n_dep > 1) {
          colors <- palette_jjf(n_dep, n_indep + 1)
        } else {
          colors <- palette_jjf(n_indep + 1)
        }
        
      }
      
      names(colors) <- colnames(values$result_xts)
      
      colors <- colors[names(colors) %in% input$plot_variable]
      names(colors) <- NULL
      
      plot_xts <- values$result_xts[ , colnames(values$result_xts) %in% input$plot_variable]
      
      ext <- paste0(".", file_ext(input$select_file))
      
      dygraphs::dygraph(plot_xts,
                        main = paste0(gsub(ext, "", input$select_file), " - ", input$statistic)) |>
        dygraphs::dyAxis("y", axisLabelWidth = 60) |>
        dygraphs::dyAxis("y2", axisLabelWidth = 0) |>
        dygraphs::dyRangeSelector(fillColor = grDevices::rgb(238, 238, 238, maxColorValue = 255),
                                  strokeColor	= grDevices::rgb(204, 204, 204, maxColorValue = 255)) |>
        dygraphs::dyOptions(colors = colors) |>
        dygraphs::dyCSS("inst/www/dygraph.css")
      
    })
    
    output$download <- shiny::downloadHandler(
      
      filename = function() {
        
        ext <- paste0(".", file_ext(input$select_file))
        paste0(gsub(ext, "", input$select_file), " - ", input$statistic, ext)
        
      },
      
      content = function(file) {
        data.table::fwrite(data.table::as.data.table(values$result_xts), file)
      }
      
    )
    
    shiny::observeEvent(input$reset_files, {
      
      values$files_ls <- list()
      values$select_file <- NULL
      values$file_df <- NULL
      values$file_idx <- NULL
      values$select_dep <- NULL
      values$select_indep <- NULL
      values$subset_df <- NULL
      values$result_xts <- NULL
      
    })
    
  }
  
  shiny::shinyApp(ui, server, ...)
  
}
