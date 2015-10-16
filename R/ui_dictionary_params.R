#' ui_dictionary_params
#'
#' An interface to parameters
#'
#' @param name character
#' @author Reinhard Simon
#' @export
ui_dictionary_params <- function(name = "resource_dictionary"){
  shiny::conditionalPanel(
    paste0("input.menu == '",name,"'"),
    shiny::HTML("<center>"),
    shiny::uiOutput("dictionary_crop"),
    shiny::actionButton("butSaveDictionary", "Save", inline = TRUE),
    shiny::HTML("</center>")
  )
}
