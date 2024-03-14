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
  
  shiny::shinyApp(ui, server, ...)
  
}