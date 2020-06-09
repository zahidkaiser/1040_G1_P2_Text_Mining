fluidPage(
  # Application title
  titlePanel("Word Cloud"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      #actionButton("update", "Change"),  
      fileInput("selection", "Upload a text file for Word Cloud",
                multiple = TRUE,
                accept = c("text/plain")
      ),
      actionButton("update", "Create Word Cloud"),
      
      hr(),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 50, value = 15),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 300,  value = 100),
      checkboxInput("random",
                    "Random Order")
    ),
    
    # Show Word Cloud
    
    mainPanel(
      plotOutput("plot")
    )
  )
)