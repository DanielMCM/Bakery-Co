###################################################
##########            Model            ############
###################################################

# Data

data <- reactive({
    req(values$dataset)
    return (values$dataset)
})

data.test1 <- reactive({
    req(data)
    return(data()[which((weekdays(data()$Date) != "sábado")
                & (weekdays(data()$Date) != "domingo")
                & (hour(hms(data()$Time)) < 16)
                & (hour(hms(data()$Time)) > 12))
                ,])
})

data.test2 <- reactive({
    return(data()[which((weekdays(data()$Date) == "sábado")
                | (weekdays(data()$Date) == "domingo"))
                ,])
})

data.trans <- reactive({
    write.csv(data(), file = "Transactions/bread2.csv", quote = FALSE, row.names = FALSE)
    return(read.transactions("Transactions/Bread2.csv", format = "single", cols = c("Transaction", "Item"), sep = ",", rm.duplicates = FALSE, quote = ''))
})

data.trans1 <- reactive({
    write.csv(data.test1(), file = "Transactions/bread2_Goal.csv", quote = FALSE, row.names = FALSE)
    return(read.transactions("Transactions/Bread2_Goal.csv", format = "single", cols = c("Transaction", "Item"), sep = ",", rm.duplicates = FALSE, quote = ''))
})

data.trans2 <- reactive({
    write.csv(data.test2(), file = "Transactions/bread2_Goal2.csv", quote = FALSE, row.names = FALSE)
    return(read.transactions("Transactions/Bread2_Goal2.csv", format = "single", cols = c("Transaction", "Item"), sep = ",", rm.duplicates = FALSE, quote = ''))
})

# Calculate num rules

model.calculate_num_rules <- function(data) {

    levels.support <- c(0.1, 0.05, 0.01, 0.005) # percentage of total transactions
    levels.confidence <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1) # confidence levels for rules

    rules_sup10 <- integer(length = 9)
    rules_sup5 <- integer(length = 9)
    rules_sup1 <- integer(length = 9)
    rules_sup0.5 <- integer(length = 9)

    # Apriori algorithm with a support level of 10%
    for (i in 1:length(levels.confidence)) {
        rules_sup10[i] <- length(apriori(data, parameter = list(sup = levels.support[1], conf = levels.confidence[i], target = "rules")))
    }

    # Apriori algorithm with a support level of 5%
    for (i in 1:length(levels.confidence)) {
        rules_sup5[i] <- length(apriori(data, parameter = list(sup = levels.support[2], conf = levels.confidence[i], target = "rules")))
    }

    # Apriori algorithm with a support level of 1%
    for (i in 1:length(levels.confidence)) {
        rules_sup1[i] <- length(apriori(data, parameter = list(sup = levels.support[3], conf = levels.confidence[i], target = "rules")))
    }

    # Apriori algorithm with a support level of 0.5%
    for (i in 1:length(levels.confidence)) {
        rules_sup0.5[i] <- length(apriori(data, parameter = list(sup = levels.support[4], conf = levels.confidence[i], target = "rules")))
    }
    
    # Data frame
    num_rules <- data.frame(rules_sup10, rules_sup5, rules_sup1, rules_sup0.5, levels.confidence)

    return(list("num_rules" = num_rules, "levels.support" = levels.support, "levels.confidence" = levels.confidence))
}

# Start

model.build <- function(callback) {

    # Notify end
    callback();
}