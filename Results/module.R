###################################################
##########          Results Module         ########
###################################################

results_path <- "Results/"

# UI

results_ui <- function(id) {
    ns <- NS(id)
    tabPanel(
        title = "Results",
        div(id = ns("div_processing"),
            div(class = "sk-cube-grid", div(class = "sk-cube sk-cube1"), div(class = "sk-cube sk-cube2"), div(class = "sk-cube sk-cube3"), div(class = "sk-cube sk-cube4"),
            div(class = "sk-cube sk-cube5"), div(class = "sk-cube sk-cube6"), div(class = "sk-cube sk-cube7"), div(class = "sk-cube sk-cube8"), div(class = "sk-cube sk-cube9"),
            p(class = "results-label-processing", "Processing..."))),
        div(id = ns("div_waiting"), h5("Choose a dataset and run the model")),
        div(id = ns("div_finished"),
            tabsetPanel(type = "pills",
                tabPanel(title = "Every day",
                    fluidRow(
                        box(width = 12, title = h5("Apriori algorithm with different support levels"), plotOutput(ns("trans.plot.linear")))),
                    fluidRow(
                        box(title = h5("Support level"), class = "full-width-container", sliderInput(ns("trans.level.support"), NULL, min = 0.005, max = 0.1, value = 0.05)),
                        box(title = h5("Confidence level"), class = "full-width-container", sliderInput(ns("trans.level.confidence"), NULL, min = 0.1, max = 0.9, value = 0.8, step = 0.1))),
                    fluidRow(
                        box(width = 12, h5("Rules")),
                        box(plotOutput(ns("trans.plot.rules_graph"))),
                        box(plotOutput(ns("trans.plot.rules_circle"))),
                        box(width = 12, class = "box-rules", verbatimTextOutput(ns("trans.rules"))),
                        box(width = 12, br()),
                        box(width = 12, plotOutput(ns("trans.plot.rules_scatter"))),
                        box(width = 12, br())),
                    fluidRow(
                        box(width = 12, h5("Summary")),
                        box(verbatimTextOutput(ns("trans.summary"))),
                        box(verbatimTextOutput(ns("trans.glimpse"))),
                        box(width = 12, plotOutput(ns("trans.frequency.plot")))),
                    fluidRow(
                        box(width = 12, h5("Original data")),
                        box(plotOutput(ns("data.plot.line"))),
                        box(plotOutput(ns("data.plot.bars")))
                    )),
                tabPanel(title = "Weekdays (lunch time)",
                    fluidRow(
                        box(width = 12, title = h5("Apriori algorithm with different support levels"), plotOutput(ns("trans1.plot.linear")))),
                    fluidRow(
                        box(title = h5("Support level"), class = "full-width-container", sliderInput(ns("trans1.level.support"), NULL, min = 0.005, max = 0.1, value = 0.05)),
                        box(title = h5("Confidence level"), class = "full-width-container", sliderInput(ns("trans1.level.confidence"), NULL, min = 0.1, max = 0.9, value = 0.8, step = 0.1))),
                    fluidRow(
                        box(width = 12, h5("Rules")),
                        box(plotOutput(ns("trans1.plot.rules_graph"))),
                        box(plotOutput(ns("trans1.plot.rules_circle"))),
                        box(width = 12, class = "box-rules", verbatimTextOutput(ns("trans1.rules"))),
                        box(width = 12, br()),
                        box(width = 12, plotOutput(ns("trans1.plot.rules_scatter"))),
                        box(width = 12, br())),
                    fluidRow(
                        box(width = 12, h5("Summary")),
                        box(verbatimTextOutput(ns("trans1.summary"))),
                        box(verbatimTextOutput(ns("trans1.glimpse"))),
                        box(width = 12, plotOutput(ns("trans1.frequency.plot"))))),
                tabPanel(title = "Weekends",
                    fluidRow(
                        box(width = 12, title = h5("Apriori algorithm with different support levels"), plotOutput(ns("trans2.plot.linear")))),
                    fluidRow(
                        box(title = h5("Support level"), class = "full-width-container", sliderInput(ns("trans2.level.support"), NULL, min = 0.005, max = 0.1, value = 0.05)),
                        box(title = h5("Confidence level"), class = "full-width-container", sliderInput(ns("trans2.level.confidence"), NULL, min = 0.1, max = 0.9, value = 0.8, step = 0.1))),
                    fluidRow(
                        box(width = 12, h5("Rules")),
                        box(plotOutput(ns("trans2.plot.rules_graph"))),
                        box(plotOutput(ns("trans2.plot.rules_circle"))),
                        box(width = 12, class = "box-rules", verbatimTextOutput(ns("trans2.rules"))),
                        box(width = 12, br()),
                        box(width = 12, plotOutput(ns("trans2.plot.rules_scatter"))),
                        box(width = 12, br())),
                    fluidRow(
                        box(width = 12, h5("Summary")),
                        box(verbatimTextOutput(ns("trans2.summary"))),
                        box(verbatimTextOutput(ns("trans2.glimpse"))),
                        box(width = 12, plotOutput(ns("trans2.frequency.plot"))))))))
}

# Server

results_server <- function(input, output, session) {

    # Load model

    source(str_c(results_path, "model.R"), local = TRUE)

    # Observers

    observe({
        req(values$state)
        shinyjs::toggle(id = "div_processing", condition = values$state == "processing")
        shinyjs::toggle(id = "div_waiting", condition = values$state == "waiting")
        shinyjs::toggle(id = "div_finished", condition = values$state == "finished")
    })

    observe({
        req(values$state)

        # Check processing state

        if (values$state != "processing") {
            return()
        }

        # Build model

        model.build(function() {
            values$state <- "finished" # Change state to finished
        });
    })

    # Trans

    trans.rules <- reactive({
        req(data.trans)
        rules <- apriori(data.trans(), parameter = list(sup = input$trans.level.support, conf = input$trans.level.confidence, target = "rules"))
    })

    output$trans.rules <- renderPrint({
        req(trans.rules)
        trans.rules.df <- data.frame(antecedent = labels(trans.rules()@lhs), consequent = labels(trans.rules()@rhs), trans.rules()@quality)
        return(trans.rules.df[order(-trans.rules.df$lift),])
    })

    output$trans.plot.linear <- renderPlot({
        req(data.trans)

        result <- model.calculate_num_rules(data.trans())

        # Number of rules found with a support level of 10%, 5%, 1% and 0.5%
        ggplot(data = result$num_rules, aes(x = result$levels.confidence)) +

        # Plot line and points (support level of 10%)
        geom_line(aes(y = result$num_rules$rules_sup10, colour = "Support level of 10%")) +
        geom_point(aes(y = result$num_rules$rules_sup10, colour = "Support level of 10%")) +

        # Plot line and points (support level of 5%)
        geom_line(aes(y = result$num_rules$rules_sup5, colour = "Support level of 5%")) +
        geom_point(aes(y = result$num_rules$rules_sup5, colour = "Support level of 5%")) +

        # Plot line and points (support level of 1%)
        geom_line(aes(y = result$num_rules$rules_sup1, colour = "Support level of 1%")) +
        geom_point(aes(y = result$num_rules$rules_sup1, colour = "Support level of 1%")) +

        # Plot line and points (support level of 0.5%)
        geom_line(aes(y = result$num_rules$rules_sup0.5, colour = "Support level of 0.5%")) +
        geom_point(aes(y = result$num_rules$rules_sup0.5, colour = "Support level of 0.5%")) +

        # Labs and theme
        labs(x = "Confidence levels", y = "Number of rules found") +
        theme_bw() + theme(legend.title = element_blank())
    })

    output$trans.summary <- renderPrint({
        req(data.trans)
        summary(data.trans())
    })

    output$trans.glimpse <- renderPrint({
        req(data.trans)
        glimpse(data.trans())
    })

    output$trans.frequency.plot <- renderPlot({
        req(data.trans)
        itemFrequencyPlot(data.trans(), topN = 15, type = "relative", col = "lightcyan2", xlab = "Item name",
                  ylab = "Frequency (relative)", main = "Relative Item Frequency Plot")
    })

    output$trans.plot.rules_graph <- renderPlot({
        req(trans.rules)
        plot(trans.rules(), method = "graph")
    })

    output$trans.plot.rules_circle <- renderPlot({
        req(trans.rules)
        plot(trans.rules(), method = "graph", control = list(layout = igraph::in_circle()))
    })

    output$trans.plot.rules_scatter <- renderPlot({
        req(trans.rules)
        plot(trans.rules(), measure = c("support", "lift"), shading = "confidence", jitter = 0)
    })

    # Trans 1

    trans1.rules <- reactive({
        req(data.trans1)
        rules <- apriori(data.trans1(), parameter = list(sup = input$trans1.level.support, conf = input$trans1.level.confidence, target = "rules"))
    })

    output$trans1.rules <- renderPrint({
        req(trans1.rules)
        trans1.rules.df <- data.frame(antecedent = labels(trans1.rules()@lhs), consequent = labels(trans1.rules()@rhs), trans1.rules()@quality)
        return(trans1.rules.df[order(-trans1.rules.df$lift),])
    })

    output$trans1.plot.linear <- renderPlot({
        req(data.trans1)

        result <- model.calculate_num_rules(data.trans1())

        # Number of rules found with a support level of 10%, 5%, 1% and 0.5%
        ggplot(data = result$num_rules, aes(x = result$levels.confidence)) +

        # Plot line and points (support level of 10%)
        geom_line(aes(y = result$num_rules$rules_sup10, colour = "Support level of 10%")) +
        geom_point(aes(y = result$num_rules$rules_sup10, colour = "Support level of 10%")) +

        # Plot line and points (support level of 5%)
        geom_line(aes(y = result$num_rules$rules_sup5, colour = "Support level of 5%")) +
        geom_point(aes(y = result$num_rules$rules_sup5, colour = "Support level of 5%")) +

        # Plot line and points (support level of 1%)
        geom_line(aes(y = result$num_rules$rules_sup1, colour = "Support level of 1%")) +
        geom_point(aes(y = result$num_rules$rules_sup1, colour = "Support level of 1%")) +

        # Plot line and points (support level of 0.5%)
        geom_line(aes(y = result$num_rules$rules_sup0.5, colour = "Support level of 0.5%")) +
        geom_point(aes(y = result$num_rules$rules_sup0.5, colour = "Support level of 0.5%")) +

        # Labs and theme
        labs(x = "Confidence levels", y = "Number of rules found") +
        theme_bw() + theme(legend.title = element_blank())
    })

    output$trans1.summary <- renderPrint({
        req(data.trans1)
        summary(data.trans1())
    })

    output$trans1.glimpse <- renderPrint({
        req(data.trans1)
        glimpse(data.trans1())
    })

    output$trans1.frequency.plot <- renderPlot({
        req(data.trans1)
        itemFrequencyPlot(data.trans1(), topN = 15, type = "relative", col = "lightcyan2", xlab = "Item name",
                  ylab = "Frequency (relative)", main = "Relative Item Frequency Plot")
    })

    output$trans1.plot.rules_graph <- renderPlot({
        req(trans1.rules)
        plot(trans1.rules(), method = "graph")
    })

    output$trans1.plot.rules_circle <- renderPlot({
        req(trans1.rules)
        plot(trans1.rules(), method = "graph", control = list(layout = igraph::in_circle()))
    })

    output$trans1.plot.rules_scatter <- renderPlot({
        req(trans1.rules)
        plot(trans1.rules(), measure = c("support", "lift"), shading = "confidence", jitter = 0)
    })

    # Trans 2

    trans2.rules <- reactive({
        req(data.trans2)
        rules <- apriori(data.trans2(), parameter = list(sup = input$trans2.level.support, conf = input$trans2.level.confidence, target = "rules"))
    })

    output$trans2.rules <- renderPrint({
        req(trans2.rules)
        trans2.rules.df <- data.frame(antecedent = labels(trans2.rules()@lhs), consequent = labels(trans2.rules()@rhs), trans2.rules()@quality)
        return(trans2.rules.df[order(-trans2.rules.df$lift),])
    })

    output$trans2.plot.linear <- renderPlot({
        req(data.trans2)

        result <- model.calculate_num_rules(data.trans2())

        # Number of rules found with a support level of 10%, 5%, 1% and 0.5%
        ggplot(data = result$num_rules, aes(x = result$levels.confidence)) +

        # Plot line and points (support level of 10%)
        geom_line(aes(y = result$num_rules$rules_sup10, colour = "Support level of 10%")) +
        geom_point(aes(y = result$num_rules$rules_sup10, colour = "Support level of 10%")) +

        # Plot line and points (support level of 5%)
        geom_line(aes(y = result$num_rules$rules_sup5, colour = "Support level of 5%")) +
        geom_point(aes(y = result$num_rules$rules_sup5, colour = "Support level of 5%")) +

        # Plot line and points (support level of 1%)
        geom_line(aes(y = result$num_rules$rules_sup1, colour = "Support level of 1%")) +
        geom_point(aes(y = result$num_rules$rules_sup1, colour = "Support level of 1%")) +

        # Plot line and points (support level of 0.5%)
        geom_line(aes(y = result$num_rules$rules_sup0.5, colour = "Support level of 0.5%")) +
        geom_point(aes(y = result$num_rules$rules_sup0.5, colour = "Support level of 0.5%")) +

        # Labs and theme
        labs(x = "Confidence levels", y = "Number of rules found") +
        theme_bw() + theme(legend.title = element_blank())
    })

    output$trans2.summary <- renderPrint({
        req(data.trans2)
        summary(data.trans2())
    })

    output$trans2.glimpse <- renderPrint({
        req(data.trans2)
        glimpse(data.trans2())
    })

    output$trans2.frequency.plot <- renderPlot({
        req(data.trans2)
        itemFrequencyPlot(data.trans2(), topN = 15, type = "relative", col = "lightcyan2", xlab = "Item name",
                      ylab = "Frequency (relative)", main = "Relative Item Frequency Plot")
    })

    output$trans2.plot.rules_graph <- renderPlot({
        req(trans2.rules)
        plot(trans2.rules(), method = "graph")
    })

    output$trans2.plot.rules_circle <- renderPlot({
        req(trans2.rules)
        plot(trans2.rules(), method = "graph", control = list(layout = igraph::in_circle()))
    })

    output$trans2.plot.rules_scatter <- renderPlot({
        req(trans2.rules)
        plot(trans2.rules(), measure = c("support", "lift"), shading = "confidence", jitter = 0)
    })

    # General data

    output$data.plot.line <- renderPlot({
        ggplot(data = data() %>%
            group_by(Date) %>%
            summarise(Transaction = n_distinct(Transaction)), aes(x = Date, y = Transaction)) + geom_line()
    })
    output$data.plot.bars <- renderPlot({
        ggplot(data = data() %>%
            group_by(Date = weekdays(Date)) %>%
            summarise(Transaction = n_distinct(Transaction)), aes(x = Date, y = Transaction)) + geom_bar(stat = "identity")
    })
}
