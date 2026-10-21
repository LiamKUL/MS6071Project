
library(shiny)

ui <- fluidPage(
  
  titlePanel("Bone Fracture Classification"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Upload an X-ray"),
      
      fileInput(
        inputId = "xray",
        label = "Choose an image",
        accept = c(".jpg", ".jpeg", ".png")
      ),
      
      imageOutput("preview", height = "400px"),
      
      
      actionButton(
        inputId = "classify",
        label = "Classify Image",
        class = "btn-primary"
      )
    ),
    
    mainPanel(
      
      h3("Image Preview"),
      
      imageOutput("preview", height = "400px"),
      
      hr(),
      
      h3("Classification Result"),
      
      uiOutput("result"),
      
      hr(),
      
      p(
        "This application is a research prototype and is ",
        "not intended to diagnose or rule out a fracture."
      )
    )
  )
)


server <- function(input, output, session) {
  
  # Display the uploaded image
  output$preview <- renderImage({
    
    req(input$xray)
    
    list(
      src = input$xray$datapath,
      contentType = input$xray$type,
      alt = "Uploaded X-ray image"
    )
    
  }, deleteFile = FALSE)
  
  
  # Respond when the user clicks the classification button
  observeEvent(input$classify, {
    
    req(input$xray)
    
    output$result <- renderUI({
      
      div(
        style = "padding: 15px; background-color: #f2f2f2;",
        h4("Model not connected"),
        p(
          "The image has been uploaded successfully. ",
          "A trained classification model must be connected ",
          "before a fracture prediction can be produced."
        )
      )
      
    })
    
  })
  
}


shinyApp(ui = ui, server = server)
