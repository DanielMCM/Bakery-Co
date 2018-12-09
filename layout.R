###################################################
##########           Layout        ################
###################################################

source("Selection/module.R")
source("Results/module.R")
source("Team/module.R")

# Header

header <- function(id) {
    ns <- NS(id)
    div(
        h1("Bakery & Co.", class = "title"),
        hr(class = "title-underline")
    )
}

# Content

content <- function(id) {
    ns <- NS(id)
    tabsetPanel(id = "tabs", selection_ui("Selection"), results_ui("Results"), team_ui("Team"), type = "pills")
}