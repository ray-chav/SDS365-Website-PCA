#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# STARTING MATERIALS ==========================
library(shiny)
library(bslib)

# Load in functions for compressing & plotting
source('image_compress.R')
source('plotting_functions.R')
source('convert_img_to_mat.R')

# format: <NAME> <- function(input, output){}
# The shinyApp function is run once, when you launch your app
# The server function is run once each time a user visits your app

# SIDEBAR UI ==================================
# Sidebar for uploading images and choosing k
ui <- page_sidebar(
  
  # Application title
  title = "PCA Image Compression - SDS365",
  
  # Background color
  bg = "#fffffa",
  
  # Collapsible sidebar w/ instructions & input
  sidebar = sidebar(
    position = "left",
    bg = "#fffffa",
    
    title = "Image Compressor",
    p("Compress an image by uploading your file
        and then choosing your", em("k.")),
    
    
    # UPLOADING IMAGE FILE ==================
    fileInput(
      "img_file", "Upload your image here",
      accept = c(".csv", ".jpg", ".jpeg", ".bmp", ".tiff")
    ),
    
    # CHOOSING USER K =======================
    sliderInput("bins", # K GOES HERE
                "Pick compression level",
                min = 1,
                max = 50, # MAX P HERE
                value = 30) # MAX P HERE
    
  ),
  
  # Card to contain original image
  card(
    card_header("Original image:"),
    plotOutput("original_image") # Show origiinal image file
  ),
  
  # Card to contain compressed image
  card(
    card_header("Edited image:"),
    plotOutput("edited_image") # Show edited compressed image
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # VALIDATE FILE TyPE ====================
  check_file_type <- reactive({
    
    # Ensure there was a user input first
    req(input$img_file)
    
    # Grab file extension
    file_ext <- tools::file_ext(input$img_file$name)
    
    # Make sure the file type is actually valid-
    # should be filtered from the ui but the tutorial says this so..
    validate(
      need(
        file_ext %in% c("csv", "jpg", "jpeg", "bmp", "tiff"),
        "Your file is not in the correct format."
      )
    )
    
    # Return the validity of this file
    file_ext
  })
  
  
  # READ THE FILE ==========================
  uploaded_data <- reactive({
    
    # Ensure there was a user input first
    req(input$img_file)
    
    # Grab the file and make sure it's valid
    file <- input$img_file
    file_ext <- check_file_type()
    
    # Read in the data into  th eoutput
    if (file_ext == "csv") {
      read.csv(file$datapath)
    } else {
      load.image(file$datapath)
    }
  })
  
  
  # CALCULATE MAX K =========================
  calculated_max <- reactive({
    
    # Grab our data
    data <- uploaded_data()
    file_ext <- check_file_type()
    
    # ROUTE 1: File is a csv--max k value == p
    if (file_ext == "csv") {
      max_val <- ncol(data)
      # ROUTE 2: File is an image--covert to RGB matrices first
    } else {
      channels <- imsplit(data, "c")
      max_val <- dim(channels[[1]])[2]
    }
    
    # Return our maximum value of k
    max_val
  })
  
  # UPDATE SLIDER MAX K DYNAMICALLY ==========
  observeEvent(calculated_max(), {
    # Ensure the value exists
    req(calculated_max())
    
    # Update the slider UI
    updateSliderInput(
      session = session,
      inputId = "bins",
      max = calculated_max(),
      value = calculated_max()
    )
  })
  
  # SHOW ORIGINAL IMAGE =======================
  output$original_image <- renderPlot({
    
    # Grab the data of user input
    data <- uploaded_data()
    
    # Check file extension
    # TODO: make this like less reptitive
    file_ext <- check_file_type()
    
    # ROUTE 1: If it's a csv, run image()
    if (file_ext == "csv") {
      image(as.matrix(data))
      # ROUTE 2: If it's already an image, just show it off
    } else {
      plot(data)
    }
  })
  
  # SHOW COMPRESSED IMAGE ====================
  output$edited_image <- renderPlot({
    
    # Grab the data of user input
    data <- uploaded_data()
    
    # Check file extension
    file_ext <- check_file_type()
    
    # ROUTE 1: csv -> straight PCA compression
    if (file_ext == "csv") {
      
      # Grab function output
      pca_k = compress_image(data, input$bins) # FIX PLACEHOLDER.. input$bins?
      # Create compressed image
      image(pca_k[[1]])
      
      # ROUTE 2: If it's already an image, just show it off
    } else {
      
      # FIX: UR USING THE FILE PATH BRUH
      pca_k = compress_image_from_imager(input$img_file$datapath, input$bins) # PLACEHODLER
      plot(pca_k$image)
      
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
