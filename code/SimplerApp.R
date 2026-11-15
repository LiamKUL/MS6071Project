Sys.setenv(
  RETICULATE_PYTHON =
    "C://Users//Kallen//anaconda3//envs//fracture_env//python.exe"
)

library(shiny)
library(keras3)
library(EBImage)
library(reticulate)

model_path <- "C:/Users/Kallen/Documents/MS6071 Project"

model <- load_model(
  file.path(model_path, "bone_fracture_model.keras")
)

fracture_classes <- readRDS(
  file.path(model_path, "fracture_classes.rds")
)

fracture_labels <- c(
  "0"="Comminuted",
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


## Build the layout for the User Interface

ui <- fluidPage(
  
  
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
  
  # APPLICATION HEADING
  
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
  
  # MAIN LAYOUT
  
  sidebarLayout(
    
    # SIDEBAR - IMAGE UPLOAD
    
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
    
    
    
    mainPanel(
      
      # IMAGE PREVIEW
      
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
      
      # CLASSIFICATION RESULT
      
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
  
  # DISCLAIMER
  
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


## Use the server function to connect the prediction functions
## to the app
server <- function(input, output, session) {
  
  # Display uploaded image
  
  output$uploaded_image <- renderImage({
    
    req(input$image)
    
    list(
      src = input$image$datapath,
      contentType = input$image$type,
      width = "100%",
      alt = "Uploaded X-ray"
    )
    
  }, deleteFile = FALSE)
  
  
## Image preprocessing function
  preprocess_image <- function(image_path) {
    
    # Read uploaded image using EBImage package
    img <- EBImage::readImage(image_path)
    
    # Convert grayscale image to RGB if necessary
    if (length(dim(img)) == 2) {
      
      img <- EBImage::rgbImage(
        img, img, img
      )
      
    }
    
    # Ensure image has three colour channels matching 150x150x3 
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
  
  ## Classification
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
      
      # Identify predicted fracture class
      predicted_index <- which.max(
        prediction_prob[1, ]
      )
      
      predicted_class <- fracture_classes[predicted_index]
      
      predicted_fracture <- fracture_labels[
        as.character(predicted_class)
      ]
      
      # Obtain prediction confidence
      confidence <- max(
        prediction_prob[1, ]
      ) * 100
      
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
      
      output$prediction <- renderText({
        paste("Error:", e$message)
      })
      
      output$confidence <- renderText({
        ""
      })
      
    })
    
  })
  
}


shinyApp(ui = ui, server = server)
