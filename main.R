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

# user's input about the time series analyzed
ts.input <- list('freq' = 12,
                 'begin' = c(2011,7,1),  # beginning of time-series
                 'end' = c(2017,7,1))    # end of time-series)

#  manual assignment of numbers to attributes by their order. Based on the raw data file footer.
data.levels.order.number <- list("BUILDING_TYPE" = 4,
                                 "REGIONTYPE" = 5,
                                 "ASGS_2016" = 6,
                                 "TIME" = 8)


####################################
######   Data definition

# downloader::download(data.url.source,dest=data.local.path) # not neccessary
data.json = jsonlite::fromJSON(data.local.path)  #  can be loaded directly from data.url.source
data.obs = data.json$dataSets$observations       #  obs stands for observations


###   basic data description
names(data.json)
str(data.json)
summary(data.json)
data.json$header
data.json$structure$dimensions$observation         #  predictors
data.json$structure$dimensions$observation$values  #  predictors' values
data.json$structure$attributes$observation         #  units


####################################
#####     data columns refactoring

###   transpose dataframe and name columns

data.obs.tr <- as.data.frame(t(data.obs))
data.obs.t <- cbind(rownames(data.obs.tr),data.obs.tr)
rm(data.obs.tr)
rownames(data.obs.t) <- NULL
colnames(data.obs.t) <- c('category', 'values')


###   separate values into columns and set names

clean.category <- data.frame(do.call('rbind',strsplit(as.character(data.obs.t$category), ':', fixed=TRUE)))
colnames(clean.category) <- data.json$structure$dimensions$observation$id

clean.values <- data.frame(do.call('rbind',strsplit(substr(as.character(data.obs.t$values)
                                                           , start=3
                                                           , stop=nchar(as.character(data.obs.t$values))-1)
                                                    , ', '
                                                    , fixed=TRUE)))

data.all.columns <- cbind(clean.category,clean.values)
rm(clean.category,clean.values, data.obs.t)


###   set correctly factors' values

# sapply(data.all.columns, levels)
levels(data.all.columns$BUILDING_TYPE) <- 
   as.vector(data.json$structure$dimensions$observation$values[[data.levels.order.number$BUILDING_TYPE]]$name)
levels(data.all.columns$REGIONTYPE) <- 
   as.vector(data.json$structure$dimensions$observation$values[[data.levels.order.number$REGIONTYPE]]$name)
levels(data.all.columns$ASGS_2016) <- 
   as.vector(data.json$structure$dimensions$observation$values[[data.levels.order.number$ASGS_2016]]$name)



###   remove redundant columns and create clean dataset  

colnames(data.all.columns)[9] <- "Value"
data.clean <- dplyr::select(data.all.columns, BUILDING_TYPE, REGIONTYPE, ASGS_2016, TIME_PERIOD, Value)
rm(data.all.columns)

###   change data types to numeric

data.clean$Value <- as.integer(as.character(data.clean$Value))              # as.character() is crucial, otherwise values change
data.clean$TIME_PERIOD <- as.integer(as.character(data.clean$TIME_PERIOD))  # as.character() is crucial, otherwise values change
# sapply(data.clean, class)
# sapply(data.clean, levels)


###   Extract one particular time series – Total number of new houses in New South Wales

data.Wales <- data.clean[data.clean$ASGS_2016 %in% selected.asgs.2016 &
                         data.clean$BUILDING_TYPE %in% selected.building.type, ]


###   Check the hint from the instructions :)

data.clean[data.clean$Value == 1511,]
data.Wales[data.Wales$TIME_PERIOD == 0,]



###   convert data to a time-series and have a look

# data.ts <- ts(data.Wales$Value, start=ts.input$begin, end=ts.input$end, frequency = ts.input$freq)
# summary(data.ts)
# plot(data.ts)
# abline(h=c(range(data.ts), mean(data.ts)), col='red', lty=2)









#######################################################################################
######################             MODELING               ######################
#######################################################################################


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### Running all the models for comparison #####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


###   user input
valid.sizes = c(6,12,18,24)
test.sizes = c(12)


###   cleaning the working table
rm(selected.asgs.2016, selected.building.type, data.ts, data.levels.order.number, data.url.source, data.local.path, data.clean)
source('run_ts_models.R')


###   calling heavy-lifting function that compares the methods

time.start <- Sys.time()  #  2 valid sizes and 2 test.sizes took approx. 7 seconds
system.time(results <- run_ts_models(data.to.analyze = data.Wales$Value, 
                                     valid.sizes = valid.sizes,
                                     test.sizes = test.sizes,
                                     ts.start = ts.input$begin,
                                     ts.end = ts.input$end,
                                     ts.freq = 12))
time.end <- Sys.time()
time.taken <- time.end - time.start
time.taken


###   displaying the results

results <- results[,-c(8)]  # remove the extra column
table.of.results <- results[with(results, order(results$`relative SSR validation`)),]
View(table.of.results)


###   saving and loading the data results

# caution, overwrites the file:  save(table.of.results, file='table.of.results')
load('table.of.results')         # must be in the working directory. hint: getwd()
View(table.of.results)
#



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### Checking the best model against testing dataset #####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

rm(time.start, time.end, time.taken, valid.sizes, test.sizes)


###   User's input

selected.test.set.size <- 12  #  the final test-set size


###   defining training and testing data

data.ts <- ts(data.Wales$Value, start=ts.input$begin, end=ts.input$end, frequency = ts.input$freq)
train.set.size <- length(data.ts) - selected.test.set.size
train.ts <- window(data.ts, 
                   end = time(data.ts)[train.set.size])
test.ts  <- window(data.ts, 
                   start = time(data.ts)[train.set.size + 1])


###   Applying the model

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



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### Applying the best model for prediction #####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


###   User's input

selected.months.number = 36  #  selected number of months we want to predict
img.hline.seq <- seq(1000,4000, by=500)
ylab.text <- "Number of new houses in Wales"

###   Predictions

##  with testing data in mind
ARIMAfit <- auto.arima(train.ts, approximation=FALSE,trace=FALSE)
prediction <- forecast::forecast(ARIMAfit, h = selected.test.set.size + selected.months.number)

##  using entire dataset for training
ARIMAfit.full.train <- auto.arima(data.ts, approximation=FALSE,trace=FALSE)
prediction.full.train  <- forecast::forecast(ARIMAfit.full.train, h = selected.months.number)


###   Visualisation

par(mfrow=c(1,2))
   # plot(prediction)
   plot(prediction, ylab = ylab.text, main="Prediction for 3 years with 1 year testing data")
   matlines(time(test.ts), 
            test.ts, 
            type='l', lty=1, lwd=2, col='red')
   abline(h=img.hline.seq, col='red', lty=2)
   
   # plot(prediction.full.train)
   plot(prediction.full.train, ylab = ylab.text, main="Prediction for 3 years using entire dataset")
   title(ylab = ylab.text)
   abline(h=img.hline.seq, col='red', lty=2)
par(mfrow=c(1,1))


