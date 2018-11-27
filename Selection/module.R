###################################################
##########          Selection Module       ########
###################################################

selection_path <- "Selection/"

# UI

selection_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        title = "Choose dataset",
        br(),
        fluidRow(
            box(class = "selection-button-column", shinyjs::disabled(actionButton(ns("button_run_model"), "Run model"))),
            box(class = "selection-picker-column", title = h5("Choose a CSV file"), fileInput(ns("input_csv_file"), NULL, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")))
        ),
        fluidRow(
            box(width = 12,
                title = h4("Summary"),
                verbatimTextOutput(ns("dataset.summary"))
            )
        ),
        fluidRow(
            box(class = "well m-0", title = h4("Header"),
                tableOutput(ns("dataset.head"))
            ),
            box(class = "well m-0", title = h4("Tail"),
                tableOutput(ns("dataset.tail"))
            )
        )
    )
}

# Server

selection_server <- function(input, output, session) {

    observe({
        req(input$input_csv_file) # Only when file is chosen

        # Read data
        data <- read.table(input$input_csv_file$datapath, header = TRUE, sep = ",", na.strings = "NONE", quote = "")
        data <- data[complete.cases(data),]
        data$Date <- as.Date(data$Date)

        # Save dataset
        values$dataset <<- data

        # Enable run model
        shinyjs::enable("button_run_model")
    })

    output$dataset.summary <- renderPrint({
        req(values$dataset)
        return(summary(values$dataset))
    })

    output$dataset.head <- renderTable({
        req(values$dataset)
        return(head(values$dataset))
    })

    output$dataset.tail <- renderTable({
        req(values$dataset)
        return(tail(values$dataset))
    })

    observeEvent(input$button_run_model, {
        values$navigateTo("Results")
        values$state <<- "processing"
    })
}
