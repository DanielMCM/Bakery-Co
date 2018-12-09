###################################################
##########           Libraries     ################
###################################################

library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(stringr)
library(arules)
library(dplyr)
library(gridExtra)
library(grid)
library(dplyr)
library(ggplot2)
library(lattice)
library(arulesViz)
library(utils)
library(lubridate)
library(rlist)
library(lubridate)

###################################################
##########           Sources       ################
###################################################

source("layout.R")
source("global.R")

###################################################
##########           UI            ################
###################################################

ui <- fluidPage(
    shinyjs::useShinyjs(debug = TRUE),
    theme = shinytheme("journal"),
    fluidRow(class = "main",
        div(class = "content-wrapper",
            header("Header"),
            content("Content")
        )
    ),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    tags$head(tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Dancing+Script:400,700"))
)

###################################################
##########           Server        ################
###################################################

server <- function(input, output, session) {

    # Save global session
    values$globalSession <- session

    # Call modules
    callModule(selection_server, "Selection")
    callModule(results_server, "Results")
    callModule(team_server, "Team")
}

shinyApp(ui, server)
