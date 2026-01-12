library(shiny)
source("module.R")

ui <- fluidPage(
  mod_diabetes_ui("diabetes1")
)

server <- function(input, output, session) {
  mod_diabetes_server(
    id = "diabetes1",
    model_path = file.path(getwd(), "models", "diabetes_tidymodels.rds")
  )
}

shinyApp(ui, server)



