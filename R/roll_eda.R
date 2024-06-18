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
  
  shiny_path <- system.file("shiny", package = "rolleda")
  
  source(file.path(shiny_path, "utils.R"))
  ui <- source(file.path(shiny_path, "ui.R"))$value
  server <- source(file.path(shiny_path, "server.R"))$value
  
  # add resource path for logo
  shiny::addResourcePath("www", file.path(shiny_path, "www"))
  
  shiny::shinyApp(ui, server, ...)
  
}