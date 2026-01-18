# modules/mod_diabetes_predict.R

library(shiny)

# ---------------- UI ----------------
# mod_diabetes_ui <- function(id) {
#   ns <- NS(id)
#   
#   tagList(
#     h3("Diabetes Risk Prediction (Pima Model)"),
#     
#     numericInput(ns("pregnancies"), "Pregnancies", value = 0, min = 0),
#     numericInput(ns("glucose"), "Glucose (mg/dL)", value = 120, min = 0),
#     numericInput(ns("blood_pressure"), "Blood Pressure (mm Hg)", value = 70, min = 0),
#     numericInput(ns("skin_thickness"), "Skin Thickness (mm)", value = 20, min = 0),
#     numericInput(ns("insulin"), "Insulin (µU/mL)", value = 80, min = 0),
#     numericInput(ns("bmi"), "BMI (kg/m²)", value = 25, min = 0),
#     numericInput(ns("diabetes_pedigree"), "Diabetes Pedigree Function", value = 0.5, min = 0),
#     numericInput(ns("age"), "Age (years)", value = 30, min = 1),
#     
#     actionButton(ns("predict"), "Predict Diabetes Status",
#                  class = "btn-primary"),
#     
#     hr(),
#     
#     verbatimTextOutput(ns("prediction")),
#     verbatimTextOutput(ns("probability"))
#   )
# }
# 
# # ---------------- Server ----------------
# mod_diabetes_server <- function(id, model_path) {
#   moduleServer(id, function(input, output, session) {
#     
#     # Load pretrained model
#     model <- readRDS(model_path)
#     
#     new_data <- eventReactive(input$predict, {
#       data.frame(
#         pregnant = input$pregnancies,
#         glucose = input$glucose,
#         blood_pressure = input$blood_pressure,
#         skin_thickness = input$skin_thickness,
#         insulin = input$insulin,
#         bmi = input$bmi,
#         diabetes_pedigree = input$diabetes_pedigree,
#         age = input$age
#       )
#     })
#     
#     prediction <- eventReactive(input$predict, {
#       
#       # Works for glm, randomForest, xgboost, tidymodels, etc.
#       prob <- predict(model, new_data(), type = "prob")
#       
#       status <- ifelse(prob >= 0.5, "Diabetic", "Not Diabetic")
#       
#       list(
#         status = status,
#         prob = prob
#       )
#     })
#     
#     output$prediction <- renderText({
#       req(prediction())
#       paste("Predicted Status:", prediction()$status)
#     })
#     
#     output$probability <- renderText({
#       req(prediction())
#       paste("Predicted Probability:", round(prediction()$prob, 3))
#     })
#     
#   })
# }

mod_diabetes_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Diabetes Risk Prediction (Pima Model)"),
    
    numericInput(ns("pregnancies"), "Pregnancies", value = 0, min = 0),
    numericInput(ns("glucose"), "Glucose (mg/dL)", value = 120, min = 0),
    numericInput(ns("blood_pressure"), "Blood Pressure (mm Hg)", value = 70, min = 0),
    numericInput(ns("skin_thickness"), "Skin Thickness (mm)", value = 20, min = 0),
    numericInput(ns("insulin"), "Insulin (µU/mL)", value = 80, min = 0),
    numericInput(ns("bmi"), "BMI (kg/m²)", value = 25, min = 0),
    numericInput(ns("diabetes_pedigree"), "Diabetes Pedigree Function", value = 0.5, min = 0),
    numericInput(ns("age"), "Age (years)", value = 30, min = 1),
    
    sliderInput(
      ns("threshold"),
      "Decision Threshold",
      min = 0,
      max = 1,
      value = 0.5,
      step = 0.01
    ),
    
    actionButton(
      ns("predict"),
      "Predict Diabetes Status",
      class = "btn-primary"
    ),
    
    hr(),
    
    tableOutput(ns("prediction_table"))
  )
}

mod_diabetes_server <- function(id, model_path) {
  moduleServer(id, function(input, output, session) {
    
    # Load pretrained model
    model <- readRDS(model_path)
    
    new_data <- eventReactive(input$predict, {
      data.frame(
        pregnant = input$pregnancies,
        glucose = input$glucose,
        blood_pressure = input$blood_pressure,
        skin_thickness = input$skin_thickness,
        insulin = input$insulin,
        bmi = input$bmi,
        diabetes_pedigree = input$diabetes_pedigree,
        age = input$age
      )
    })
    
    prediction_table <- reactive({
      req(new_data())
      
      probs <- predict(model, new_data(), type = "prob")
      
      # If model returns two-class probs
      if (is.matrix(probs) || is.data.frame(probs)) {
        diabetes_risk <- probs[, 2]
        non_diabetes_prob <- probs[, 1]
      } else {
        # If model returns single probability for diabetes
        diabetes_risk <- probs
        non_diabetes_prob <- 1 - probs
      }
      
      status <- ifelse(
        diabetes_risk >= input$threshold,
        "Diabetic",
        "Non-Diabetic"
      )
      
      data.frame(
        `Diabetes risk` = round(diabetes_risk, 3),
        `Non-Diabetes probability` = round(non_diabetes_prob, 3),
        `Diabetic status` = status,
        check.names = FALSE
      ) 
    })
    
    output$prediction_table <- renderTable({
      prediction_table()
    })
    
  })
}


