###   assignment solution for Heidelberg Cement company
###
###   author:    Kapslik
###   created:   15.9.2018
###   purpose:   analyze data regarding Number of new houses approved in Australia. Further instructions are in the "Big Data Exercise.docx" file


rm(list=ls())
setwd("C:/Users/Vapno/Documents/a Jobs/HeidelbergCement/R files")



library(jsonlite)     # flexible, robust, high performance tools for working with JSON in R
# library(downloader)   # Download Files over HTTP and HTTPS  #  only if you want to download the data again


####################################
######   Declarations

data.url.source=paste0("http://stat.data.abs.gov.au/sdmx-json/data/ABS_BA_SA2_ASGS2016/1.9.1...0+1+102+1GSYD+10201.M/all?detail=Full&dimensionAtObservation=AllDimensions&startPeriod=2011-07&endPeriod=2017-07")
data.local.path = "data/Australian_data"


####################################
######   Data definition

# download(data.url.source,dest=data.local.path)
data.json = jsonlite::fromJSON(data.local.path)  #  can be loaded directly from data.url.source
data.main = data.json$dataSets
data.obs = data.json$dataSets$observations

ts.begin = min(data.json$structure$dimensions$observation$values[[8]][,1]) # beginning of time-series
ts.end = max(data.json$structure$dimensions$observation$values[[8]][,1])   # end of time-series


###   data description
names(data.json)
data.json$header
data.json$structure$dimensions$observation         #  predictors
data.json$structure$dimensions$observation$values  #  predictors' values
data.json$structure$attributes$observation         #  units


#####     data columns refactoring

###   transpose dataframe

data.obs.tr <- as.data.frame(t(data.obs))
data.obs.t <- cbind(rownames(data.obs.tr),data.obs.tr)
rm(data.obs.tr)
rownames(data.obs.t) <- NULL
colnames(data.obs.t) <- c('category', 'values')
head(data.obs.t)


###   separate values into columns and set names

clean.category <- data.frame(do.call('rbind',strsplit(as.character(data.obs.t$category), ':', fixed=TRUE)))
colnames(clean.category) <- data.json$structure$dimensions$observation$id

clean.values <- data.frame(do.call('rbind',strsplit(substr(as.character(data.obs.t$values)
                                                           , start=3
                                                           , stop=nchar(as.character(data.obs.t$values))-1)
                                                    , ', '
                                                    , fixed=TRUE)))

data.all.columns <- cbind(clean.category,clean.values)
head(data.all.columns)
rm(clean.category,clean.values, data.obs.t)


###   set correctly factors' values

# sapply(data.all.columns, levels)
# levels(data.all.columns$BUILDING_TYPE)
# data.json$structure$dimensions$observation$values[[4]]
# data.json$structure$dimensions$observation$values[[5]]
# data.json$structure$dimensions$observation$values[[6]]
levels(data.all.columns$BUILDING_TYPE) <- as.vector(data.json$structure$dimensions$observation$values[[4]]$name)
levels(data.all.columns$REGIONTYPE) <- as.vector(data.json$structure$dimensions$observation$values[[5]]$name)
levels(data.all.columns$ASGS_2016) <- as.vector(data.json$structure$dimensions$observation$values[[6]]$name)



###   remove redundant columns and create clean dataset  

# sapply(data.all.columns, levels)
colnames(data.all.columns)[9] <- "Value"
data.clean <- dplyr::select(data.all.columns, BUILDING_TYPE, REGIONTYPE, ASGS_2016, TIME_PERIOD, Value)
rm(data.all.columns)

###   change data types to numeric

data.clean$Value <- as.integer(as.character(data.clean$Value))
data.clean$TIME_PERIOD <- as.integer(as.character(data.clean$TIME_PERIOD))
# sapply(data.clean, class)
# sapply(data.clean, levels)


###   Extract one particular time series – Total number of new houses in New South Wales

selected.asgs.2016 = "New South Wales"
selected.building.type = "Houses"

data.Wales <- data.clean[data.clean$ASGS_2016 %in% selected.asgs.2016 
                         & data.clean$BUILDING_TYPE %in% selected.building.type,]
data.Wales


###   Check the hint from the instructions

data.clean[data.clean$Value == 1511,]
data.Wales[data.Wales$TIME_PERIOD == 0,]


###   convert data to a time-series

# library('forecast')
data.ts <- ts(data.Wales$Value, start=c(2011,7,1), end=c(2017,7,1), frequency = 12)

summary(data.ts)
plot(data.ts)
# matplot(time(data.ts), data.ts, type='l', lty=1)
abline(h=c(range(data.ts), mean(data.ts)), col='red', lty=2)
plot(log(data.ts))





#######################################################################################
######################             MODELING               ######################
#######################################################################################


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### Running all the models for comparison #####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

source('run_ts_models.R')

valid.sizes = c(14)
valid.sizes = c(21)
test.sizes = c(21)
test.sizes = c(1)

results <- run_ts_models(data.to.analyze = data.Wales$Value, 
                         valid.sizes = valid.sizes,
                         test.sizes = test.sizes,
                         ts.start = c(2011,7,1),
                         ts.end = c(2017,7,1),
                         ts.freq = 12)
# lapply(results,class)
# View(results)
# colnames(results)
# mena <- colnames(results)
colnames(results) <- c("t.s.", "v.s.", "method",
                       "resid.t", "resid.t.rel", "resid.v", "resid.v.rel",
                       "< 2%")
# results[with(results, order(results$resid.v)),]

## displaying ordered output
results <- results[,-c(4,6,8)]
table.of.results <- results[with(results, order(results$method, results$resid.v.rel)),]
View(table.of.results)


## saving and loading the data results
## table.of.results <- results[with(results, order(results$model, results$resid.v.rel)),]
save(table.of.results, file='table.of.results')
load('table.of.results')
View(table.of.results)
#








#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### fun and experiments with individual models #####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



selected.months.number = 36  #  selected number of months we want to predict
selected.window.type <- 'periodic'

##### method stl #####
dekomp.stl <- stl(data.ts,s.window = selected.window.type)
# resid.sum.train <- sum(abs(dekomp.stl$time.series[,'remainder']))
# resid.sum.train.relative <- resid.sum.train/sum(data.ts)

# library(forecast)
prediction <- forecast::forecast(dekomp.stl, h = selected.months.number)
# resid.sum.valid <- sum(abs(na.remove(prediction$mean - valid.model)))
# resid.sum.valid.relative <- resid.sum.valid / sum(valid.model)
# less.than.2percent <- resid.sum.valid.relative < 0.02

plot(dekomp.stl)
plot(prediction)


###  method stl for log()

dekomp.stl.log <- stl(log(data.ts), s.window = selected.window.type)
prediction <- forecast::forecast(dekomp.stl.log, h = selected.months.number)
plot(dekomp.stl.log)
plot(prediction)


##### method ets #####

model.ets <- ets(data.ts)
model.ets$fitted - data.ts

resid.sum.train <- sum(abs(model.ets$fitted - data.ts))
resid.sum.train.relative <- resid.sum.train/sum(data.ts)

prediction <- forecast(model.ets, h=selected.months.number)
# resid.sum.valid <- sum(abs(na.remove(prediction$mean - valid.model)))
# resid.sum.valid.relative <- resid.sum.valid / sum(valid.model)

plot(prediction)


##### method ARIMAfit #####

ARIMAfit <- auto.arima(data.ts, approximation=FALSE,trace=FALSE)
# XX more parameters can be added

resid.sum.train <- sum(abs(ARIMAfit$residuals))
resid.sum.train.relative <- resid.sum.train / sum(data.ts)

prediction <- forecast(ARIMAfit, h = selected.months.number)
# resid.sum.valid <- sum(abs(na.remove(prediction$mean - valid.model)))
# resid.sum.valid.relative <- resid.sum.valid / sum(valid.model)

plot(prediction)


##### method HoltWinters #####

HW <- HoltWinters(data.ts)
resid.sum.train <- sum(abs(residuals(HW)))
resid.sum.train.relative <- resid.sum.train / sum(data.ts)

prediction <- predict(HW, selected.months.number)
# resid.sum.valid <-sum(abs(na.remove(prediction - valid.model)))
# resid.sum.valid.relative <- resid.sum.valid / sum(valid.model)

plot(prediction)
