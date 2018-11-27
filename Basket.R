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

#Read the data and transform it to transactions object
# We write again in order to read duplicate items with read.transactions and remove nulls (NONE)

data <- read.table("BreadBasket_DMS.csv", header = TRUE, sep = ",", na.strings = "NONE", quote = "")
data <- data[complete.cases(data),]

data$Date <- as.Date(data$Date)

# Goal --> Transactions during weekdays at lunch time
datatest <- data[which((weekdays(data$Date) != "sábado")
                & (weekdays(data$Date) != "domingo")
                & (hour(hms(data$Time)) < 16)
                & (hour(hms(data$Time)) > 12))
                ,]

#Secondary example --> Transactions during weekends
datatest2 <- data[which((weekdays(data$Date) == "sábado")
                | (weekdays(data$Date) == "domingo"))
                ,]

write.csv(data, file = "bread2.csv", quote = FALSE, row.names = FALSE)
write.csv(datatest, file = "bread2_Goal.csv", quote = FALSE, row.names = FALSE)
write.csv(datatest2, file = "bread2_Goal2.csv", quote = FALSE, row.names = FALSE)

trans <- read.transactions("Bread2.csv", format = "single", cols = c("Transaction", "Item"), sep = ",", rm.duplicates = FALSE, quote = '')
trans2 <- read.transactions("Bread2_Goal.csv", format = "single", cols = c("Transaction", "Item"), sep = ",", rm.duplicates = FALSE, quote = '')
trans3 <- read.transactions("Bread2_Goal2.csv", format = "single", cols = c("Transaction", "Item"), sep = ",", rm.duplicates = FALSE, quote = '')

#Summary of the data

summary(trans)
glimpse(trans)

summary(trans2)
glimpse(trans2)

summary(trans3)
glimpse(trans3)

#Trans data
#The density is very low for trans, 0.0210037
#41% of the transaction include only 1 item
#The mean of unique items per transaction is 1.995
#The mean of items per transaction is 2.16 (Total Items / Total unique transaction)

#Goal Data
#The density is higher, 0.028155
#38% of the transaction include only 1 item
#The mean of unique items per transaction is 2.055
#The mean of items per transaction is 2,21 (Total Items / Total unique transaction)


# Plot of more frequent items

itemFrequencyPlot(trans, topN = 15, type = "relative", col = "lightcyan2", xlab = "Item name",
                  ylab = "Frequency (relative)", main = "Relative Item Frequency Plot")

itemFrequencyPlot(trans2, topN = 15, type = "relative", col = "lightcyan2", xlab = "Item name",
                  ylab = "Frequency (relative)", main = "Relative Item Frequency Plot")

itemFrequencyPlot(trans3, topN = 15, type = "relative", col = "lightcyan2", xlab = "Item name",
                  ylab = "Frequency (relative)", main = "Relative Item Frequency Plot")

# Analysis of original data by date and weekday
ggplot(data = data %>%
    group_by(Date) %>%
    summarise(Transaction = n_distinct(Transaction)), aes(x = Date, y = Transaction)) + geom_line()

ggplot(data = data %>%
    group_by(Date = weekdays(Date)) %>%
    summarise(Transaction = n_distinct(Transaction)), aes(x = Date, y = Transaction)) + geom_bar(stat = "identity")

#Analysis

#Support levels & Confident levels
#We only make it for our goal, for the total data or the 2nd goal the process is similar
supportLevels <- c(0.1, 0.05, 0.01, 0.005) #Percentage of total transactions
confidenceLevels <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1) #Confidence levels for rules

rules_sup10 <- integer(length = 9)
rules_sup5 <- integer(length = 9)
rules_sup1 <- integer(length = 9)
rules_sup0.5 <- integer(length = 9)

# Apriori algorithm with a support level of 10%
for (i in 1:length(confidenceLevels)) {
    rules_sup10[i] <- length(apriori(trans2, parameter = list(sup = supportLevels[1],
                                                         conf = confidenceLevels[i], target = "rules")))
}

# Apriori algorithm with a support level of 5%
for (i in 1:length(confidenceLevels)) {
    rules_sup5[i] <- length(apriori(trans2, parameter = list(sup = supportLevels[2],
                                                        conf = confidenceLevels[i], target = "rules")))
}

# Apriori algorithm with a support level of 1%
for (i in 1:length(confidenceLevels)) {

    rules_sup1[i] <- length(apriori(trans2, parameter = list(sup = supportLevels[3],
                                                        conf = confidenceLevels[i], target = "rules")))

}

# Apriori algorithm with a support level of 0.5%
for (i in 1:length(confidenceLevels)) {

    rules_sup0.5[i] <- length(apriori(trans2, parameter = list(sup = supportLevels[4],
                                                        conf = confidenceLevels[i], target = "rules")))

}

# Data frame
num_rules <- data.frame(rules_sup10, rules_sup5, rules_sup1, rules_sup0.5, confidenceLevels)

# Number of rules found with a support level of 10%, 5%, 1% and 0.5%
ggplot(data = num_rules, aes(x = confidenceLevels)) +

# Plot line and points (support level of 10%)
geom_line(aes(y = rules_sup10, colour = "Support level of 10%")) +
  geom_point(aes(y = rules_sup10, colour = "Support level of 10%")) +

# Plot line and points (support level of 5%)
geom_line(aes(y = rules_sup5, colour = "Support level of 5%")) +
  geom_point(aes(y = rules_sup5, colour = "Support level of 5%")) +

# Plot line and points (support level of 1%)
geom_line(aes(y = rules_sup1, colour = "Support level of 1%")) +
  geom_point(aes(y = rules_sup1, colour = "Support level of 1%")) +

# Plot line and points (support level of 0.5%)
geom_line(aes(y = rules_sup0.5, colour = "Support level of 0.5%")) +
  geom_point(aes(y = rules_sup0.5, colour = "Support level of 0.5%")) +

# Labs and theme
labs(x = "Confidence levels", y = "Number of rules found",
       title = "Apriori algorithm with different support levels") +
  theme_bw() +
  theme(legend.title = element_blank())

#We choose the support level at 1% and confidence at 50%
rules_sup1_conf50 <- apriori(trans2, parameter = list(sup = supportLevels[3],
                             conf = confidenceLevels[5], target = "rules"))

# Scatter plot
plot(rules_sup1_conf50, measure = c("support", "lift"), shading = "confidence")

# Graph
plot(rules_sup1_conf50, method = "graph")

plot(rules_sup1_conf50, method = "grouped")

# As a further exploration we use Apriori algorithm execution with a support level of 0.5% and a confidence level of 60%
rules_sup0.5_conf60 <- apriori(trans2, parameter = list(sup = supportLevels[4], conf = confidenceLevels[4], target = "rules"))

plot(rules_sup0.5_conf60, method = "graph")

plot(rules_sup0.5_conf60, method = "graph", control = list(layout = igraph::in_circle()))

plot(rules_sup0.5_conf60, measure = c("support", "lift"), shading = "confidence", jitter = 0)

# As a further exploration we use Apriori algorithm execution with a support level of 0.5% and a confidence level of 10%
rules_sup0.5_conf10 <- apriori(trans2, parameter = list(sup = supportLevels[4], conf = confidenceLevels[9], target = "rules"))

plot(rules_sup0.5_conf10, method = "graph")

plot(rules_sup0.5_conf10, method = "graph", control = list(layout = igraph::in_circle()))

plot(rules_sup0.5_conf10, measure = c("support", "lift"), shading = "confidence", jitter = 0)
