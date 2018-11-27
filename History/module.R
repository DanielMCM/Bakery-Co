###################################################
##########          History Module         ############
###################################################

history_path <- "History/"

# UI

history_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        title = "History",
        fluidRow(
            box(width = 12, title = h5("Last model runs")),
            box(width = 12, renderTable(ns("table.history")))))
}

# Server

history_server <- function(input, output, session) {

    output$table.history <- renderTable({
        print("--- Render table")
        values$history()
    })
}
