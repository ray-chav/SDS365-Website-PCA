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
      imageOutput("original_image") # Show origiinal image file
    ),
    
    # Card to contain compressed image
    card(
      card_header("Edited image:"),
      imageOutput("edited_image") # Show edited compressed image
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # VALIDATE FILE TYPE ============
  check_file_type <- reactive({
    
    # Run only when we have an uploaded file
    req(input$img_file)
    # Grab inputted file (if it exists)
    file <- input$img_file
    
    # Get file extension from uploaded file name
    file_ext <- tools::file_ext(file$datapath)
    
    # Validate file type
    validate(
      need(file_ext %in% c("csv", "jpg", "jpeg", "bmp", "tiff"),
           "Your file is not in the correct format.")
    )
    
    # Return whether or not this file is a csv
    is_csv = file_ext == "csv"
    return(is_csv)
  })

  # CALCULATE MAX K VALUE =====================
  calculated_max <- reactive({
    
    # Run only when we have an uploaded file
    req(input$img_file)
    # Grab inputted file (if it exists)
    file <- input$img_file
    
    is_csv = check_file_type(file$datapath)
    
    # ROUTE 1: CSV FILE =======================
    if (is_csv){
      csv_df <- read.csv(file$datapath)
      
      # Calculate max k value
      max_val <- ncol(csv_df)
    }
    
    # ROUTE 2: IMAGE FILE =====================
    else {
      
      # Read in image as values
      # Read image
      img <- load.image(file$datapath)
      
      # Split into RGB channels
      channels <- imsplit(img, "c")
      
      # Grab max k-value
      max_val <- dim(channels[[1]])[2]
    }
    return(max_val)
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
  output$original_image <- renderImage({
    
    
  })
  
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #          xlab = 'Waiting time to next eruption (in mins)',
    #          main = 'Histogram of waiting times')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
