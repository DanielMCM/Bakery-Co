###################################################
##########            Model            ############
###################################################

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

model.build <- function(callback) {

    # Notify end
    callback();
}