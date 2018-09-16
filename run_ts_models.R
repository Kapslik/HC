# this whole script is a single function run_ts_models created to try several models for a time-series prediction.
# Function fits in ets, stl, HoltWinters and ARIMAfit models for sequencies of parameters with a single data.frame as output, which should be basis for choosing the best performing method.
# 
# 




library(tseries)
library(forecast)



run_ts_models <- function(data.to.analyze, 
                          valid.sizes, 
                          test.sizes,
                          ts.start,
                          ts.end,
                          ts.freq) {
   # function returns comparison of different time-series analysis methods
   # function runs models for different sizes of training, validation and testing data and returns table of comparison
   # resulting table includes sums of squared errors (SSE) on validation data, that can be used for model selection.
   # testing dataset is not "looked at" in this function.
   #
   # input:
   #     data.to.analyze = ordered vector of values in time.
   #     valid.sizes = sequence of sizes of the validation set 
   #                    (last n observations that will be excluded from the training set
   #                    and on which the models will be evaluated)
   #                    e.g. c(12,24)
   #     test.sizes  = sequence of number of observations to be left out for test set.
   #                    Same as valid.sizes
   #                    e.g. c(12,24)
   #     ts.start    = start parameter passed to the ts() function, e.g. c(2017,7,1)
   #     ts.end      = end parameter passed to the ts() function, e.g. c(2017,7,1)
   #     ts.freq     = freq parameter passed to the ts() function, e.g. 12
   # 
   # output:
   #    Single data.frame with results of all the methods for comparison
  
  results.tab <- data.frame(train.set.size = integer(),
                                  valid.set.size = integer(),
                                  method = factor(levels=c('ets', 'stl', 
                                                           'HoltWinters', 
                                                           'ARIMAfit')),
                                  resid.sum.train = numeric(),
                                  resid.sum.train.relative = numeric(),
                                  resid.sum.valid = numeric(),
                                  resid.sum.valid.relative = numeric(),
                                  less.than.2percent = logical() )

  colnames(results.tab) <- c("train size", "validation size", "method",
                             "SSR training", "relative SSR training", "SSR validation", "relative SSR validation",
                             "< 2%")
  
     data.ts <- ts(data.to.analyze, start=ts.start, end=ts.end, frequency = ts.freq)
     
     ##### splitting the data #####
     
     for(valid.set.size in valid.sizes) {
        for(test.set.size in test.sizes) {
           
           train.set.size <- length(data.ts) - test.set.size - valid.set.size
           
           train.ts <- window(data.ts, 
                              end = time(data.ts)[train.set.size])
           valid.ts <- window(data.ts, 
                              start = time(data.ts)[train.set.size + 1],
                              end = time(data.ts)[train.set.size + valid.set.size])
           test.ts  <- window(data.ts, 
                              start = time(data.ts)[train.set.size + valid.set.size + 1])
           
           ###   checking the split
           
           check.split <- c(train.ts,valid.ts, test.ts)
           if(sum(check.split == data.to.analyze) != length(data.to.analyze)){
              stop('data splitting error - some data were changed.')
           }
           if(length(train.ts) + length(test.ts) + length(valid.ts) != length(data.ts)){
              stop('data splitting error - some data were lost.')
           }
           rm(check.split)
           
           
           ########    modelling    ########
           
           
           ##### method stl #####
           
           method <- 'stl'
           dekomp_stl <- stl(train.ts,s.window = 'periodic')
           resid.sum.train <- sum(abs(dekomp_stl$time.series[,'remainder']))
           resid.sum.train.relative <- resid.sum.train/sum(train.ts)
           
           prediction <- forecast::forecast(dekomp_stl, h = valid.set.size)
           resid.sum.valid <- sum(abs(na.remove(prediction$mean - valid.ts)))
           resid.sum.valid.relative <- resid.sum.valid / sum(valid.ts)
           
           results.tab <- add.to.results.tab(results.tab, train.set.size, valid.set.size, method, 
                                             resid.sum.train, resid.sum.train.relative,
                                             resid.sum.valid, resid.sum.valid.relative)
           
           rm(method, dekomp_stl, prediction, resid.sum.train, resid.sum.train.relative, 
              resid.sum.valid, resid.sum.valid.relative)
           
           
           ##### method ets #####
           
           method <- 'ets'
           model.ets <- ets(train.ts)
           model.ets$fitted - train.ts
           
           resid.sum.train <- sum(abs(model.ets$fitted - train.ts))
           resid.sum.train.relative <- resid.sum.train/sum(train.ts)
           
           prediction <- forecast::forecast(model.ets, h=valid.set.size)
           resid.sum.valid <- sum(abs(na.remove(prediction$mean - valid.ts)))
           resid.sum.valid.relative <- resid.sum.valid / sum(valid.ts)
           
           results.tab <- add.to.results.tab(results.tab, train.set.size, valid.set.size, method, 
                                             resid.sum.train, resid.sum.train.relative,
                                             resid.sum.valid, resid.sum.valid.relative)
           
           rm(method, resid.sum.train, resid.sum.train.relative, 
              resid.sum.valid, resid.sum.valid.relative)
           
           
           ##### method ARIMAfit #####
           
           method <- 'ARIMAfit'
           ARIMAfit <- auto.arima(train.ts, approximation=FALSE,trace=FALSE)
           # XX more parameters might be added
           
           resid.sum.train <- sum(abs(ARIMAfit$residuals))
           resid.sum.train.relative <- resid.sum.train / sum(train.ts)
           
           prediction <- forecast::forecast(ARIMAfit, h = valid.set.size)
           resid.sum.valid <- sum(abs(na.remove(prediction$mean - valid.ts)))
           resid.sum.valid.relative <- resid.sum.valid / sum(valid.ts)
           
           results.tab <- add.to.results.tab(results.tab, train.set.size, valid.set.size, method, 
                                             resid.sum.train, resid.sum.train.relative,
                                             resid.sum.valid, resid.sum.valid.relative)
           
           rm(method, ARIMAfit, resid.sum.train, resid.sum.train.relative, 
              resid.sum.valid, resid.sum.valid.relative, prediction)
           
           
           ##### method HoltWinters #####
           
           method <- 'HoltWinters'
           HW <- HoltWinters(train.ts)
           resid.sum.train <- sum(abs(residuals(HW)))
           resid.sum.train.relative <- resid.sum.train / sum(train.ts)
           
           prediction <- predict(HW, valid.set.size)
           resid.sum.valid <-sum(abs(na.remove(prediction - valid.ts)))
           resid.sum.valid.relative <- resid.sum.valid / sum(valid.ts)
           
           
           results.tab <- add.to.results.tab(results.tab, train.set.size, valid.set.size, method, 
                                             resid.sum.train, resid.sum.train.relative,
                                             resid.sum.valid, resid.sum.valid.relative)
           
           rm(method, HW, resid.sum.train, resid.sum.train.relative, 
              resid.sum.valid, resid.sum.valid.relative, prediction)
        } # end for test.set.size
     } # end for valid.set.size
  return(results.tab)
}



add.to.results.tab <- function(tab, train.set.size, valid.set.size, method, resid.sum.train, resid.sum.train.relative, 
                               resid.sum.valid, resid.sum.valid.relative) {
   ##  function adds results to the table of results
   
   new.row <- nrow(tab) + 1
   tab[new.row, 1] <- train.set.size
   tab[new.row, 2] <- valid.set.size
   tab[new.row, 3] <- method
   tab[new.row, 4] <- round(resid.sum.train, 2)
   tab[new.row, 5] <- round(resid.sum.train.relative, 4)
   tab[new.row, 6] <- round(resid.sum.valid, 2)
   tab[new.row, 7] <- round(resid.sum.valid.relative, 4)
   tab[new.row, 8] <- resid.sum.valid.relative < 0.02
   
   return(tab)
   
}
   
