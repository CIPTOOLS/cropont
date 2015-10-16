library(shiny)
library(shinydashboard)
library(rhandsontable)
library(fbglobal)
library(fbcrops)
library(cropont)

tabNameC  <- "resource_dictionary"

server <- function(input, output, session) {
  values = shiny::reactiveValues()
  cropont::server_dictionary(input, output, session, values = values)
}

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Demo Dictionary"),
                    dashboardSidebar(width = 300,
                      menuItem("Resources",
                               sidebarMenu(id = "menu",
                                           menuSubItem("Dictionary", icon = icon("crop"),
                                                       tabName = tabNameC)
                                           ,
                                           cropont::ui_dictionary_params()
                               )
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        cropont::ui_dictionary(name = tabNameC)
                      )
                    )
)

shinyApp(ui = ui, server = server)
