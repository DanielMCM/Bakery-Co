###################################################
##########          Reactives           ###########
###################################################

#dataset <- reactive({
#if (outliers()) {
#temp <- mpg
#temp$cty <- remove_outliers(temp$cty)
#temp$hwy <- remove_outliers(temp$hwy)
#temp$displ <- remove_outliers(temp$displ)
#temp <- temp[complete.cases(temp),]
#return(temp)
#}
#else {
#return(mpg)
#}
#})

#datasetM <- reactive({
#mmpg <- melt(dataset(), id = c("manufacturer", "model", "displ", "year", "cyl", "trans", "drv", "fl", "class", "tr"))
#mmpg$variable <- factor(mmpg$variable, levels = c("cty", "hwy"), labels = c("City Mileage", "Highway Mileage"))
#mmpg$fl <- factor(mmpg$fl, levels = c("p", "d", "e", "r"), labels = c("Petrol", "Diesel", "Ethanol", "Regular"))
#return(mmpg)
#})