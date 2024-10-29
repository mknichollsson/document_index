library(shiny)
library(DT)
library(rio)

# data import
df <- import("clinical_document_index.csv")
colnames(df) <- c("source", "reference")



# Define UI
ui <- fluidPage(
  titlePanel("Source Lookup"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("inputText", "Select or Enter Reference Text:", 
                     choices = NULL,  # Initially set to NULL
                     options = list(create = TRUE, server = TRUE)),  # Enable server-side filtering
      actionButton("submit", "Submit")
    ),
    mainPanel(
      h3("Matched Sources:"),
      DTOutput("outputTable")  # Use DT table for output
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Update selectize input choices based on user input
  observe({
    # Create a reactive expression to filter unique references based on input
    filtered_references <- unique(df$reference[grepl(paste0("^", input$inputText), df$reference, ignore.case = TRUE)])
    
    # Update the choices in the selectizeInput
    updateSelectizeInput(session, "inputText", choices = filtered_references, selected = input$inputText)
  })
  
  observeEvent(input$submit, {
    req(input$inputText)  # Ensure input is not empty
    
    # Find matches in the reference column
    matched_sources <- df$source[df$reference == input$inputText]
    
    # Create a data frame for output
    output_df <- data.frame(Source = matched_sources, stringsAsFactors = FALSE)
    
    # Return matched sources as a data table
    output$outputTable <- renderDT({
      if (nrow(output_df) > 0) {
        datatable(output_df, options = list(dom = 't', paging = FALSE))
      } else {
        datatable(data.frame(Source = "No matches found."), options = list(dom = 't', paging = FALSE))
      }
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
