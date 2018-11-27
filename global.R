###################################################
##########           Global        ################
###################################################

values <- reactiveValues()

# Dataset
values$dataset <- NULL

# History
values$history <- list()

# Global session & navigation
values$globalSession <- NULL
values$navigateTo <- function(tabName) {
    showTab(inputId = "tabs", target = tabName, select = TRUE, session = values$globalSession)
}

# State
values$state <- "waiting"
