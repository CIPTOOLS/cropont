
#' server_dictionary
#'
#' Constructs table
#'
#' @param input shinyserver input
#' @param output shinyserver output
#' @param session shinyserver session
#' @param dom target dom element name
#' @param values reactive values
#' @author Reinhard Simon
#' @export
server_dictionary <- function(input, output, session, dom="hot_dictionary", values){

  setHot_dictionary = function(x) values[[dom]] = x
  setDictionary_crop = function(x) values[["dictionary_crop"]] = x
  
  # shiny::observe({
  #   input$saveBtn
  #   if (!is.null(values[[dom]]) && !is.null(values[["dictionary_crop"]])) {
  #     crop <- values[["dictionary_crop"]]
  #     cropont::post_dictionary_table(crop, values[[dom]])
  #   }
  #   
  # }, suspended = TRUE)
  # 
  
  output$dictionary_crop <- shiny::renderUI({
    crops <- fbcrops::get_crop_table()$crop_name
    shiny::selectInput("dictionary_crop", NULL, choices = crops, width = '50%')
    
  } )
  
 
  output$hot_dictionary = rhandsontable::renderRHandsontable({
    shiny::withProgress(message = 'Loading table', {
      crop <- input$dictionary_crop
      DF <- cropont::get_dictionary_table(crop)
      if(!is.null(DF)){
        setDictionary_crop(crop)
        setHot_dictionary(DF)
        rh <- rhandsontable::rhandsontable(DF,   stretchH = "all", height = 400)
        rhandsontable::hot_table(rh, highlightCol = TRUE, highlightRow = TRUE)
      }
      # crop <- input$dictionary_crop
      # #print(crop)
      # if (is.null(crop)) return(NULL)
      # if (!is.null(values[["dictionary_crop"]]) && crop != values[["dictionary_crop"]]) {
      #   DF = rhandsontable::hot_to_r(cropont::get_dictionary_table(crop))
      # }
      # setDictionary_crop(crop)
      # 
      # 
      # if (!is.null(input[[dom]])) {
      #   DF = rhandsontable::hot_to_r(input[[dom]])
      # } else {
      #   DF = cropont::get_dictionary_table(crop)
      # }
      # 
      # setHot_dictionary(DF)
      # 
      # rh <- rhandsontable::rhandsontable(DF,   stretchH = "all", height = 400)
      # rhandsontable::hot_table(rh, highlightCol = TRUE, highlightRow = TRUE)
    })
  })
  
  shiny::observeEvent(input$butSaveDictionary, ({
    #print("saving ?")
    if (!is.null(input[[dom]])) {
      #print(input[[dom]])
      table_dictionary = rhandsontable::hot_to_r(input[[dom]])
      #print(table_dictionary[, 1:5])
      cropont::post_dictionary_table(input$dictionary_crop, table_dictionary)
    }
  })
  )
  

}
