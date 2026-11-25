
# ============================================================
# BONE FRACTURE CLASSIFICATION - SHINY APPLICATION
# ============================================================

# ============================================================
# 1. LOAD REQUIRED PACKAGES AND PYTHON ENVIRONMENT
# ============================================================

library(shiny)
library(keras3)
library(EBImage)
library(reticulate)


# ============================================================
# 2. LOAD TRAINED MODEL AND FRACTURE CLASSES
# ============================================================

# Locate repository root

project_root <- if (dir.exists(file.path(getwd(), "data"))) {
  getwd()
} else if (dir.exists(file.path(getwd(), "..", "data"))) {
  file.path(getwd(), "..")
} else {
  stop(
    "Set the working directory to the repository root or code directory."
  )
}

project_root <- normalizePath(
  project_root,
  winslash = "/"
)

# Locate model files

model_path <- file.path(
  project_root,
  "models"
)

model_file <- file.path(
  model_path,
  "classification_model.keras"
)

classes_file <- file.path(
  model_path,
  "class_names.rds"
)

# Check that model files exist

if (!file.exists(model_file)) {
  stop("Trained model not found: ", model_file)
}

if (!file.exists(classes_file)) {
  stop("Fracture classes not found: ", classes_file)
}

# Load trained CNN model

model <- load_model(model_file)

# Load fracture class mapping

fracture_classes <- readRDS(classes_file)

# Map numerical class identifiers to fracture labels

fracture_labels <- c(
  "0" = "Comminuted",
  "1" = "Greenstick",
  "2" = "Healthy",
  "3" = "Linear",
  "4" = "Oblique Displaced",
  "5" = "Oblique",
  "6" = "Segmental",
  "7" = "Spiral",
  "8" = "Transverse Displaced",
  "9" = "Transverse"
)


# ============================================================
# 3. USER INTERFACE
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
        padding: 5px;
        margin-top: 5px;
        margin-bottom: 0px;
      }

      .subtitle {
        text-align: center;
        color: #627d98;
        margin-bottom: 10px;
      }

      .panel-box {
        background-color: white;
        padding: 12px;
        border-radius: 10px;
        box-shadow: 0px 2px 8px rgba(0,0,0,0.08);
        margin-bottom: 10px;
      }

      .image-container {
        width: 100%;
        height: 300px;
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
        margin-bottom: 8px;
        margin-top: 3px;
        font-size: 20px;
      }

      .result-box {
        background-color: #eaf3f8;
        border-left: 5px solid #2878a5;
        padding: 10px;
        border-radius: 6px;
        margin-top: 5px;
      }

      .btn-primary {
        background-color: #2878a5;
        border-color: #2878a5;
        width: 100%;
        font-weight: bold;
        padding: 8px;
      }

      .btn-primary:hover {
        background-color: #1b5c80;
      }

      .disclaimer {
        color: #7b8794;
        font-size: 11px;
        margin-top: 5px;
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
        inputId = "image",
        label = "Choose an image:",
        accept = c(".jpg", ".jpeg", ".png")
      ),
      
      p(
        "Supported formats: JPG, JPEG and PNG",
        style = "color: #627d98; font-size: 12px;"
      ),
      
      hr(),
      
      actionButton(
        inputId = "predict",
        label = "Classify Image",
        class = "btn-primary"
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
            "uploaded_image",
            height = "250px"
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
        
        div(
          
          class = "result-box",
          
          h4("Predicted Fracture Type"),
          
          textOutput("prediction"),
          
          hr(),
          
          h4("Prediction Confidence"),
          
          textOutput("confidence")
          
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
# 4. SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # DISPLAY UPLOADED IMAGE
  # ----------------------------------------------------------
  
  output$uploaded_image <- renderImage({
    
    req(input$image)
    
    list(
      src = input$image$datapath,
      contentType = input$image$type,
      width = "100%",
      alt = "Uploaded X-ray"
    )
    
  }, deleteFile = FALSE)
  
  
  # ----------------------------------------------------------
  # IMAGE PREPROCESSING FUNCTION
  # ----------------------------------------------------------
  
  preprocess_image <- function(image_path) {
    
    # Read uploaded image using EBImage
    
    img <- EBImage::readImage(image_path)
    
    # Convert grayscale image to RGB if necessary
    
    if (length(dim(img)) == 2) {
      
      img <- EBImage::rgbImage(
        img, img, img
      )
      
    } else if (length(dim(img)) == 3 && dim(img)[3] == 1) {
      
      img <- EBImage::rgbImage(
        img[, , 1],
        img[, , 1],
        img[, , 1]
      )
      
    }
    
    # Ensure image has three colour channels
    
    if (length(dim(img)) != 3 || dim(img)[3] != 3) {
      stop("Image must have three colour channels.")
    }
    
    # Resize image to match CNN training dimensions
    
    img <- EBImage::resize(
      img,
      w = 150,
      h = 150
    )
    
    # Convert image into an array
    
    img_array <- as.array(img)
    
    # Add batch dimension
    
    img_array <- array(
      img_array,
      dim = c(1, dim(img_array))
    )
    
    return(img_array)
    
  }
  
  
  # ----------------------------------------------------------
  # IMAGE CLASSIFICATION
  # ----------------------------------------------------------
  
  observeEvent(input$predict, {
    
    req(input$image)
    
    tryCatch({
      
      # Process uploaded image
      
      uploaded_image_array <- preprocess_image(
        input$image$datapath
      )
      
      # Obtain prediction probabilities
      
      prediction_prob <- predict(
        model,
        uploaded_image_array
      )
      
      # Extract prediction probabilities
      
      probabilities <- as.numeric(
        prediction_prob[1, ]
      )
      
      # Check prediction output matches the class mapping
      
      if (length(probabilities) != length(fracture_classes)) {
        stop("Prediction output does not match the fracture class mapping.")
      }
      
      # Identify predicted fracture class
      
      predicted_index <- which.max(probabilities)
      
      predicted_class <- as.character(
        fracture_classes[predicted_index]
      )
      
      # Obtain corresponding fracture label
      
      if (predicted_class %in% names(fracture_labels)) {
        
        predicted_fracture <- unname(
          fracture_labels[predicted_class]
        )
        
      } else {
        
        predicted_fracture <- predicted_class
        
      }
      
      # Obtain prediction confidence
      
      confidence <- max(probabilities) * 100
      
      # Display predicted class
      
      output$prediction <- renderText({
        
        paste(
          "Predicted Fracture Class:",
          predicted_fracture
        )
        
      })
      
      # Display prediction probability
      
      output$confidence <- renderText({
        
        paste(
          "Prediction Confidence:",
          round(confidence, 2),
          "%"
        )
        
      })
      
    }, error = function(e) {
      
      # Display classification error
      
      output$prediction <- renderText({
        
        paste("Error:", e$message)
        
      })
      
      output$confidence <- renderText({
        
        ""
        
      })
      
    })
    
  })
  
}


# ============================================================
# 5. RUN SHINY APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
