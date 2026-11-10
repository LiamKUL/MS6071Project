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

## Build the layout for the User Interface

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    .image-container {
      width: 600px;
      height: 450px;
      border: 2px solid #333333;
      border-radius: 8px;
      display: flex;
      justify-content: center;
      align-items: center;
      overflow: hidden;
      background-color: #f5f5f5;
      padding: 10px;
      box-sizing: border-box;
    }

    .image-container img {
      max-width: 100%;
      max-height: 100%;
      object-fit: contain;
    }
  "))
  ),
  
  titlePanel("Bone Fracture Classification"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Upload X-ray Image"),
      
      fileInput(
        inputId = "image",
        label = "Choose an X-ray image:",
        accept = c(".jpg", ".jpeg", ".png")
      ),
      
      actionButton(
        inputId = "predict",
        label = "Classify Fracture",
        class = "btn-primary"
      )
      
    ),
    
    mainPanel(
      
      h3("Uploaded X-ray Image"),
      
      div(
        class = "image-container",
        imageOutput("uploaded_image", height = "100%")
      ),
      
      hr(),
      
      h3("Classification Results"),
      
      textOutput("prediction"),
      
      textOutput("confidence")
      
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
      
      predicted_class <- fracture_classes[
        predicted_index
      ]
      
      # Obtain prediction confidence
      confidence <- max(
        prediction_prob[1, ]
      ) * 100
      
      # Display predicted class
      output$prediction <- renderText({
        
        paste(
          "Predicted Fracture Class:",
          predicted_class
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

# ============================================================
# 5. RUN APPLICATION
# ============================================================

shinyApp(ui = ui, server = server)
