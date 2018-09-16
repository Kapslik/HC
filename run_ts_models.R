# this whole script is a single function run_ts_models created to try several models for a time-series prediction.
# Function fits in ets, stl, HoltWinters and ARIMAfit models for sequencies of parameters with a single data.frame as output, which should be basis for choosing the best performing method.
# 
# input:
#    frequencies = sequence of frequencies for the time-series data
#    valid.sizes = sequence of sizes of the validation set 
#                   (last n observations that will be excluded from the training set
#                    and on which the models will be evaluated)
#    test.sizes = sequence of number of observations to be left out for test set.
#                 Same as valid.sizes
#    models.set = if input is multiple-series, each series must be specified
#                 in the original case, this is sequence c('AA','BB','CC','AA+BB+CC')
# 
# output:
#    Single data.frame with results of all the methods for comparison



library(tseries)
library(forecast)



## recommended input:
# frequencies = c(7,30)
# valid.sizes = c(14)
# test.sizes = c(21)
# models.set = c('AA','BB','CC','AA+BB+CC')


run_ts_models <- function(data.to.analyze, 
                          valid.sizes, 
                          test.sizes,
                          ts.start,
                          ts.end,
                          ts.freq) {
  
  
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
  
  
  
     data.ts <- ts(data.to.analyze, start=ts.start, end=ts.end, frequency = ts.freq)
     
     ##### splitting the data #####
     
     for(valid.set.size in valid.sizes) {# cycle ends at the end of script
        for(test.set.size in test.sizes) {# cycle ends at the end of script
           
           train.set.size <- length(data.ts) - test.set.size - valid.set.size
           
           train.ts <- window(data.ts, 
                              end = time(data.ts)[train.set.size])
           valid.ts <- window(data.ts, 
                              start = time(data.ts)[train.set.size + 1],
                              end = time(data.ts)[train.set.size + valid.set.size])
           test.ts  <- window(data.ts, 
                              start = time(data.ts)[train.set.size + valid.set.size + 1])
           
           ###   checking the split
           class(test.ts)
           dim(data.ts)
           check.split <- c(train.ts,valid.ts, test.ts)
           if(sum(check.split == data.to.analyze) != length(data.to.analyze)){
              stop('data splitting error - some data were changed.')
           }
           if(length(train.ts) + length(test.ts) + length(valid.ts) != length(data.ts)){
              stop('data splitting error - some data were lost.')
           }
           
           
           ########    modelling    ########
           
           
           ##### method stl #####
           
           method <- 'stl'
           dekomp_stl <- stl(train.ts,s.window = 'periodic')
           resid.sum.train <- sum(abs(dekomp_stl$time.series[,'remainder']))
           resid.sum.train.relative <- resid.sum.train/sum(train.ts)
           
           prediction <- forecast::forecast(dekomp_stl, h = valid.set.size)
           resid.sum.valid <- sum(abs(na.remove(prediction$mean - valid.ts)))
           resid.sum.valid.relative <- resid.sum.valid / sum(valid.ts)
           less.than.2percent <- resid.sum.valid.relative < 0.02
           
           results.tab <- add.to.results.tab(results.tab, train.set.size, valid.set.size, method, 
                                             resid.sum.train, resid.sum.train.relative,
                                             resid.sum.valid, resid.sum.valid.relative, 
                                             less.than.2percent)
           
           rm(method, dekomp_stl, prediction, resid.sum.train, resid.sum.train.relative, 
              resid.sum.valid, resid.sum.valid.relative, less.than.2percent)
           
           
           ##### method ets #####
           
           method <- 'ets'
           model.ets <- ets(train.ts)
           model.ets$fitted - train.ts
           
           resid.sum.train <- sum(abs(model.ets$fitted - train.ts))
           resid.sum.train.relative <- resid.sum.train/sum(train.ts)
           
           prediction <- forecast::forecast(model.ets, h=valid.set.size)
           resid.sum.valid <- sum(abs(na.remove(prediction$mean - valid.ts)))
           resid.sum.valid.relative <- resid.sum.valid / sum(valid.ts)
           less.than.2percent <- resid.sum.valid.relative < 0.02
           
           results.tab <- add.to.results.tab(results.tab, train.set.size, valid.set.size, method, 
                                             resid.sum.train, resid.sum.train.relative,
                                             resid.sum.valid, resid.sum.valid.relative, 
                                             less.than.2percent)
           
           rm(method, resid.sum.train, resid.sum.train.relative, 
              resid.sum.valid, resid.sum.valid.relative, less.than.2percent)
           
           
           ##### method ARIMAfit #####
           
           method <- 'ARIMAfit'
           ARIMAfit <- auto.arima(train.ts, approximation=FALSE,trace=FALSE)
           # XX tu dalsie parametre este pridat by sli
           
           resid.sum.train <- sum(abs(ARIMAfit$residuals))
           resid.sum.train.relative <- resid.sum.train / sum(train.ts)
           
           prediction <- forecast::forecast(ARIMAfit, h = valid.set.size)
           resid.sum.valid <- sum(abs(na.remove(prediction$mean - valid.ts)))
           resid.sum.valid.relative <- resid.sum.valid / sum(valid.ts)
           less.than.2percent <- resid.sum.valid.relative < 0.02
           
           results.tab <- add.to.results.tab(results.tab, train.set.size, valid.set.size, method, 
                                             resid.sum.train, resid.sum.train.relative,
                                             resid.sum.valid, resid.sum.valid.relative, 
                                             less.than.2percent)
           
           rm(method, ARIMAfit, resid.sum.train, resid.sum.train.relative, 
              resid.sum.valid, resid.sum.valid.relative, less.than.2percent, prediction)
           
           
           ##### method HoltWinters #####
           
           method <- 'HoltWinters'
           HW <- HoltWinters(train.ts)
           resid.sum.train <- sum(abs(residuals(HW)))
           resid.sum.train.relative <- resid.sum.train / sum(train.ts)
           
           prediction <- predict(HW, valid.set.size)
           resid.sum.valid <-sum(abs(na.remove(prediction - valid.ts)))
           resid.sum.valid.relative <- resid.sum.valid / sum(valid.ts)
           less.than.2percent <- resid.sum.valid.relative < 0.02
           
           
           results.tab <- add.to.results.tab(results.tab, train.set.size, valid.set.size, method, 
                                             resid.sum.train, resid.sum.train.relative,
                                             resid.sum.valid, resid.sum.valid.relative, 
                                             less.than.2percent)
           
           rm(method, HW, resid.sum.train, resid.sum.train.relative, 
              resid.sum.valid, resid.sum.valid.relative, less.than.2percent, prediction)
        }
     }
  return(results.tab)
}



add.to.results.tab <- function(tab, train.set.size, valid.set.size, method, resid.sum.train, resid.sum.train.relative, 
                               resid.sum.valid, resid.sum.valid.relative, less.than.2percent) {
   ##  function adds results to the table of results
   new.row <- nrow(tab) + 1
   tab[new.row, 1] <- train.set.size
   tab[new.row, 2] <- valid.set.size
   tab[new.row, 3] <- method
   tab[new.row, 4] <- round(resid.sum.train, 2)
   tab[new.row, 5] <- round(resid.sum.train.relative, 4)
   tab[new.row, 6] <- round(resid.sum.valid, 2)
   tab[new.row, 7] <- round(resid.sum.valid.relative, 4)
   tab[new.row, 8] <- less.than.2percent
   
   return(tab)
   
}
   
