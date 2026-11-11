
# ============================================================
# BONE FRACTURE CLASSIFICATION - SHINY APPLICATION
# ============================================================

library(shiny)
library(ggplot2)

# ============================================================
# 1. LOAD MODEL EXPORT FILE
# ============================================================

project_root <- if (dir.exists(file.path(getwd(), "data"))) {
  getwd()
} else if (dir.exists(file.path(getwd(), "..", "data"))) {
  file.path(getwd(), "..")
} else {
  stop("Set the working directory to the repository root or code directory.")
}
project_root <- normalizePath(project_root, winslash = "/")
source(file.path(project_root, "code", "Model Saver.R"))
saved_models <- load_fracture_models(project_root)
model <- saved_models$classification
fracture_classes <- saved_models$classes
if (is.null(fracture_classes)) {
  fracture_classes <- c("Comminuted", "Greenstick", "Healthy", "Linear",
    "Oblique Displaced", "Oblique", "Segmental", "Spiral",
    "Transverse Displaced", "Transverse")
}


# ============================================================
# 2. USER INTERFACE
# ============================================================

ui <- fluidPage(
  
  # ----------------------------------------------------------
  # CSS STYLING
  # ----------------------------------------------------------
  
  tags$head(
    
    tags$style(HTML("

      body {
        background-color: #f4f6f9;
        font-family: Arial, sans-serif;
      }

      .main-heading {
        text-align: center;
        color: #243b53;
        font-weight: bold;
        padding: 15px;
      }

      .subtitle {
        text-align: center;
        color: #627d98;
        margin-bottom: 25px;
      }

      .panel-box {
        background-color: white;
        padding: 22px;
        border-radius: 10px;
        box-shadow: 0px 2px 8px rgba(0,0,0,0.08);
        margin-bottom: 20px;
      }

      .image-container {
        width: 100%;
        height: 450px;
        border: 2px solid #cccccc;
        border-radius: 8px;
        display: flex;
        justify-content: center;
        align-items: center;
        background-color: #f5f5f5;
        overflow: hidden;
      }

      .image-container img {
        max-width: 100%;
        max-height: 100%;
        width: auto;
        height: auto;
        object-fit: contain;
      }

      .section-heading {
        color: #243b53;
        font-weight: bold;
        margin-bottom: 15px;
      }

      .result-box {
        background-color: #eaf3f8;
        border-left: 5px solid #2878a5;
        padding: 18px;
        border-radius: 6px;
        margin-top: 15px;
      }

      .confidence-text {
        font-size: 22px;
        color: #2878a5;
        font-weight: bold;
      }

      .btn-primary {
        background-color: #2878a5;
        border-color: #2878a5;
        width: 100%;
        font-weight: bold;
        padding: 12px;
      }

      .btn-primary:hover {
        background-color: #1b5c80;
      }

      .disclaimer {
        color: #7b8794;
        font-size: 12px;
        margin-top: 20px;
      }

    "))
    
  ),
  
  
  # ----------------------------------------------------------
  # APPLICATION HEADING
  # ----------------------------------------------------------
  
  div(
    
    h2(
      "Bone Fracture Classification",
      class = "main-heading"
    ),
    
    p(
      "CNN-Based X-ray Image Classification System",
      class = "subtitle"
    )
    
  ),
  
  
  # ----------------------------------------------------------
  # MAIN LAYOUT
  # ----------------------------------------------------------
  
  sidebarLayout(
    
    # ========================================================
    # SIDEBAR - IMAGE UPLOAD
    # ========================================================
    
    sidebarPanel(
      
      h4(
        "Upload an X-ray",
        class = "section-heading"
      ),
      
      fileInput(
        
        inputId = "xray",
        
        label = "Choose an image:",
        
        accept = c(
          ".jpg",
          ".jpeg",
          ".png"
        )
        
      ),
      
      p(
        "Supported formats: JPG, JPEG and PNG",
        style = "color: #627d98; font-size: 12px;"
      ),
      
      hr(),
      
      actionButton(
        
        inputId = "classify",
        
        label = "Classify Image",
        
        class = "btn-primary"
        
      ),
      
      br(),
      br(),
      
      actionButton(
        
        inputId = "reset",
        
        label = "Reset",
        
        class = "btn-default",
        
        style = "width: 100%;"
        
      )
      
    ),
    
    
    # ========================================================
    # MAIN PANEL - IMAGE AND CLASSIFICATION RESULTS
    # ========================================================
    
    mainPanel(
      
      # ------------------------------------------------------
      # IMAGE PREVIEW
      # ------------------------------------------------------
      
      div(
        
        class = "panel-box",
        
        h3(
          "Image Preview",
          class = "section-heading"
        ),
        
        div(
          
          class = "image-container",
          
          imageOutput(
            "preview",
            height = "440px"
          )
          
        )
        
      ),
      
      
      # ------------------------------------------------------
      # CLASSIFICATION RESULT
      # ------------------------------------------------------
      
      div(
        
        class = "panel-box",
        
        h3(
          "Classification Result",
          class = "section-heading"
        ),
        
        uiOutput("result")
        
      ),
      
      
      # ------------------------------------------------------
      # PREDICTION PROBABILITY GRAPH
      # ------------------------------------------------------
      
      div(
        
        class = "panel-box",
        
        h3(
          "Classification Probabilities",
          class = "section-heading"
        ),
        
        plotOutput(
          "probability_plot",
          height = "350px"
        )
        
      )
      
    )
    
  ),
  
  
  # ----------------------------------------------------------
  # DISCLAIMER
  # ----------------------------------------------------------
  
  fluidRow(
    
    column(
      
      width = 12,
      
      p(
        
        "Disclaimer: This application is an experimental
        machine learning project. Predictions are not a
        medical diagnosis and should not be used to make
        clinical decisions.",
        
        class = "disclaimer"
        
      )
      
    )
    
  )
  
)


# ============================================================
# 3. SERVER
# ============================================================

server <- function(input, output, session) {
  
  # Store prediction results
  prediction_result <- reactiveVal(NULL)
  
  # Store prediction errors
  prediction_error <- reactiveVal(NULL)
  
  
  # ----------------------------------------------------------
  # DISPLAY UPLOADED IMAGE
  # ----------------------------------------------------------
  
  output$preview <- renderImage({
    
    req(input$xray)
    
    list(
      
      src = input$xray$datapath,
      
      contentType = input$xray$type,
      
      alt = "Uploaded X-ray image"
      
    )
    
  }, deleteFile = FALSE)
  
  
  # ----------------------------------------------------------
  # RESET RESULTS WHEN A NEW IMAGE IS UPLOADED
  # ----------------------------------------------------------
  
  observeEvent(input$xray, {
    
    prediction_result(NULL)
    
    prediction_error(NULL)
    
  }, ignoreInit = TRUE)
  
  
  # ----------------------------------------------------------
  # CLASSIFY IMAGE USING THE SAVED MODEL
  # ----------------------------------------------------------
  
  observeEvent(input$classify, {
    
    req(input$xray)
    
    # Clear previous results
    prediction_result(NULL)
    
    prediction_error(NULL)
    
    # Run existing prediction function
    tryCatch({
      
      result <- predict_fracture(
        input$xray$datapath,
        model,
        fracture_classes
      )
      
      # Store prediction
      prediction_result(result)
      
    },
    
    error = function(e) {
      
      prediction_error(
        conditionMessage(e)
      )
      
    })
    
  })
  
  
  # ----------------------------------------------------------
  # DISPLAY CLASSIFICATION RESULT
  # ----------------------------------------------------------
  
  output$result <- renderUI({
    
    # Display error if prediction fails
    if (!is.null(prediction_error())) {
      
      return(
        
        div(
          
          style = "color: #b42318;",
          
          h4("Classification Error"),
          
          p(prediction_error())
          
        )
        
      )
      
    }
    
    # Display instructions before classification
    if (is.null(prediction_result())) {
      
      return(
        
        p(
          "Upload an X-ray image and click Classify Image
          to generate a prediction.",
          style = "color: #627d98;"
        )
        
      )
      
    }
    
    result <- prediction_result()
    
    # Display classification
    div(
      
      class = "result-box",
      
      h4("Predicted Fracture Type"),
      
      h2(
        
        result$predicted_class,
        
        style = "
          color: #2878a5;
          font-weight: bold;
        "
        
      ),
      
      hr(),
      
      h4("Prediction Confidence"),
      
      p(
        
        paste0(
          round(result$confidence * 100, 2),
          "%"
        ),
        
        class = "confidence-text"
        
      )
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # DISPLAY CLASSIFICATION PROBABILITIES
  # ----------------------------------------------------------
  
  output$probability_plot <- renderPlot({
    
    req(prediction_result())
    
    result <- prediction_result()
    
    # Obtain probabilities returned by the saved model
    probabilities <- result$probabilities
    
    # Create dataframe
    probability_data <- data.frame(
      
      Class = fracture_classes,
      
      Probability = as.numeric(probabilities) * 100
      
    )
    
    # Order classes by probability
    probability_data$Class <- factor(
      
      probability_data$Class,
      
      levels = probability_data$Class[
        order(probability_data$Probability)
      ]
      
    )
    
    # Generate probability graph
    ggplot(
      
      probability_data,
      
      aes(
        x = Class,
        y = Probability
      )
      
    ) +
      
      geom_col(
        
        fill = "steelblue",
        
        width = 0.7
        
      ) +
      
      geom_text(
        
        aes(
          label = paste0(
            round(Probability, 2),
            "%"
          )
        ),
        
        hjust = -0.15,
        
        size = 3.5
        
      ) +
      
      coord_flip() +
      
      scale_y_continuous(
        
        limits = c(
          0,
          max(105, max(probability_data$Probability) * 1.15)
        )
        
      ) +
      
      labs(
        
        x = "Fracture Classification",
        
        y = "Prediction Probability (%)",
        
        title = "CNN Prediction Probabilities"
        
      ) +
      
      theme_minimal()
    
  })
  
  
  # ----------------------------------------------------------
  # RESET APPLICATION
  # ----------------------------------------------------------
  
  observeEvent(input$reset, {
    
    prediction_result(NULL)
    
    prediction_error(NULL)
    
    updateFileInput(
      session,
      "xray",
      label = "Choose an image:"
    )
    
  })
  
}


# ============================================================
# 4. RUN SHINY APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
