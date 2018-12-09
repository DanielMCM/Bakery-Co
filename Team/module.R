###################################################
##########          Team Module       ########
###################################################

team_path <- "Team/"

# UI

team_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        title = "Team",
        fluidRow(class = "team-content",
            box(class = "team-profile", img(src = 'Team member 1.png', align = "center", class = "team-profile-img"), h3("Daniel Minguez Camacho")),
            box(class = "team-profile", img(src = 'Team member 2.jpg', align = "center", class = "team-profile-img"), h3("Javier de la Rúa Martínez"))
        )
    )
}

# Server

team_server <- function(input, output, session) {

}
