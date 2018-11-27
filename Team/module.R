###################################################
##########          Team Module       ########
###################################################

team_path <- "Team/"

# UI

team_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        title = "Team",
        fluidRow(
            box(title = "Daniel Minguez Camacho"),
            box(title = "Javier de la Rúa Martínez")
        )
    )
}

# Server

team_server <- function(input, output, session) {

}
