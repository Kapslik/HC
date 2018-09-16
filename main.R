###   assignment solution for HC company
###
###   author:    Kapslik (FA)
###   created:   15.9.2018
###   platform:  Windows 7
###   purpose:   analyze data regarding Number of new houses approved in Australia. 
###              Further instructions are in the "Big Data Exercise.docx" file (not in repository)
###   
###   structure: This is the main script in which:
###
###               > data manipulation:
###                  > data is downloaded and loaded
###                  > data is transformed from JSON format to data.frame
###                  > data is cleaned: attributes divided, meaningless attributes removed, transposed etc.
###                  > a specific subset of data is extracted
###
###               > model selection:
###                  > the "run_ts_models.R" script is sourced for comparison of selected time-series models
###                  > models considered: 
###                     > ARIMA
###                     > Holt-WInters
###                     > Seasonal Decomposition by Loess (STL)
###                     > Exponential smoothing state space model (ETS)
###                  > FInal model is selected, based on the comparison table
###
###               > prediction:
###                  > using the best selected model
###
###   

# 



rm(list=ls())
setwd("C:/Users/Vapno/Documents/a Jobs/HeidelbergCement/R files")  #  set your working directory here. Hint: getwd()



library(jsonlite)     # flexible, robust, high performance tools for working with JSON in R
# library(downloader)   # Download Files over HTTP and HTTPS  #  only if you want to download the data again
library(tseries)     # run_ts_models.R
library(forecast)    # run_ts_models.R


####################################
######   Declarations

data.url.source=paste0("http://stat.data.abs.gov.au/sdmx-json/data/ABS_BA_SA2_ASGS2016/1.9.1...0+1+102+1GSYD+10201.M/all?detail=Full&dimensionAtObservation=AllDimensions&startPeriod=2011-07&endPeriod=2017-07")
data.local.path = "data/Australian_data"

selected.asgs.2016 = "New South Wales"
selected.building.type = "Houses"
data.frequency <- 12

####################################
######   Data definition

# download(data.url.source,dest=data.local.path) # not neccessary
data.json = jsonlite::fromJSON(data.local.path)  #  can be loaded directly from data.url.source
data.obs = data.json$dataSets$observations

##  manuall assignment of numbers to attributes by their order. Based on the raw data file footer.
data.levels.order.number <- list("BUILDING_TYPE" = 4,
                                 "REGIONTYPE" = 5,
                                 "ASGS_2016" = 6,
                                 "TIME" = 8)

ts.begin = min(data.json$structure$dimensions$observation$values[[data.levels.order.number$TIME]][,1]) # beginning of time-series
ts.end = max(data.json$structure$dimensions$observation$values[[data.levels.order.number$TIME]][,1])   # end of time-series



###   data description
names(data.json)
str(data.json)
summary(data.json)
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
levels(data.all.columns$BUILDING_TYPE) <- 
   as.vector(data.json$structure$dimensions$observation$values[[data.levels.order.number$BUILDING_TYPE]]$name)
levels(data.all.columns$REGIONTYPE) <- 
   as.vector(data.json$structure$dimensions$observation$values[[data.levels.order.number$REGIONTYPE]]$name)
levels(data.all.columns$ASGS_2016) <- 
   as.vector(data.json$structure$dimensions$observation$values[[data.levels.order.number$ASGS_2016]]$name)



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


data.Wales <- data.clean[data.clean$ASGS_2016 %in% selected.asgs.2016 &
                         data.clean$BUILDING_TYPE %in% selected.building.type, ]


###   Check the hint from the instructions :)

data.clean[data.clean$Value == 1511,]
data.Wales[data.Wales$TIME_PERIOD == 0,]


###   convert data to a time-series

# library('forecast')
data.ts <- ts(data.Wales$Value, start=c(2011,7,1), end=c(2017,7,1), frequency = data.frequency)

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

valid.sizes = c(6,12,18,24)
test.sizes = c(12)

time.start <- Sys.time()
#  2 valid sizes and 2 test.sizes took approx. 7 seconds
system.time(results <- run_ts_models(data.to.analyze = data.Wales$Value, 
                                     valid.sizes = valid.sizes,
                                     test.sizes = test.sizes,
                                     ts.start = c(2011,7,1),
                                     ts.end = c(2017,7,1),
                                     ts.freq = 12))
time.end <- Sys.time()
time.taken <- time.end - time.start
time.taken

# lapply(results,class)
# View(results)
# colnames(results)
# mena <- colnames(results)

# results[with(results, order(results$resid.v)),]

## displaying ordered output
# results <- results[,-c(4,6,8)]
results <- results[,-c(8)]
results
# table.of.results <- results[with(results, order(results$method, results$resid.v.rel)),]
table.of.results <- results[with(results, order(results$`relative SSR validation`)),]
View(table.of.results)


## saving and loading the data results
## table.of.results <- results[with(results, order(results$model, results$resid.v.rel)),]
save(table.of.results, file='table.of.results')
load('table.of.results')
View(table.of.results)
#



#####     single best model
selected.test.set.size <- 12


train.set.size <- length(data.ts) - selected.test.set.size
train.ts <- window(data.ts, 
                   end = time(data.ts)[train.set.size])
test.ts  <- window(data.ts, 
                   start = time(data.ts)[train.set.size + 1])


ARIMAfit <- auto.arima(train.ts, approximation=FALSE,trace=FALSE)

resid.sum.train <- sum(abs(ARIMAfit$residuals))
resid.sum.train.relative <- resid.sum.train / sum(train.ts)

prediction <- forecast::forecast(ARIMAfit, h = selected.test.set.size)

plot(prediction)

###   compare final model with the test data
matlines(time(test.ts), 
         test.ts, 
         type='l', lty=1, lwd=2, col='red')
resid.sum.test <- sum(abs(na.remove(prediction$mean - test.ts)))
resid.sum.valid.relative <- resid.sum.test / sum(test.ts) # 12% in the optimistic direction.


###   PREDICT for the next 3 years

selected.months.number = 36  #  selected number of months we want to predict
img.hline.seq <- seq(1000,4000, by=500)

ARIMAfit <- auto.arima(train.ts, approximation=FALSE,trace=FALSE)
prediction <- forecast::forecast(ARIMAfit, h = selected.test.set.size + selected.months.number)

par(mfrow=c(1,2))
plot(prediction)
matlines(time(test.ts), 
         test.ts, 
         type='l', lty=1, lwd=2, col='red')
abline(h=img.hline.seq, col='red', lty=2)


###   using entire dataset for training
ARIMAfit.full.train <- auto.arima(data.ts, approximation=FALSE,trace=FALSE)
prediction.full.train  <- forecast::forecast(ARIMAfit.full.train, h = selected.months.number)

plot(prediction.full.train)
abline(h=img.hline.seq, col='red', lty=2)
par(mfrow=c(1,1))

