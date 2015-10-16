#' ui_dictionary
#'
#' returns a re-usable user interface element
#'
#' @author Reinhard Simon
#' @param type of ui Element; default is a tab in a shiny dashboard
#' @param title display title
#' @param name a reference name
#' @param output name of output element
#' @export
ui_dictionary <- function(type = "tab", title = "Dictionary configuration",
                             name = "resource_dictionary",
                             output = "hot_dictionary"){
  shinydashboard::tabItem(tabName = name,
                          shiny::fluidRow(
                            shinydashboard::box( height = 600, width = 1200,
                                                 title = title,
                                                 rhandsontable::rHandsontableOutput(output, 
                                                                                    height = 600, width = 1200)
                            )
                          )
  )
}
