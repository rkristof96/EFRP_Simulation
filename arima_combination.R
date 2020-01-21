setwd("C:/Users/Kristof/OneDrive/EFRP/ARIMA combination")

library(bazar)
library(forecast)
library(Metrics)
library(parallel)
library(foreach) # speeding up for cycle

s <- Sys.time()

set.seed(2020)

#parameters

samplesize <- 1000
times <- 3 #25 # number of repeating experiments

modelsnum <- 6 # number of models

std <- c(1,2)
#std <- c(0.01,0.05,0.1,0.15,0.25,0.5,0.75,1,2,5) # standard deviation of the noise-> the noise is assumed being normally distributed
samplespliratio <- 0.9

# rmse
modelnames <- c("arma0", "arma1","arma2","arma_original","arma3","arma4","combination")
results <- array(NA,dim = c((modelsnum+1), times, length(std)), dimnames = list(modelnames,1:times,paste0("sd=",std)))
# first dim model number
# second dim number of repeated experiments
# third dim # change of standard deviation

# data generation

for(i in 1:length(std)){
  std_i <- std[i]
for(j in 1: times){

# sample points generation from ARMA(2,2) with given parameters
  
samplepoints<-arima.sim(n=samplesize, list(ar=c(0.6,0.2), ma=c(0.6,0.2)),innov = rnorm(samplesize, mean = 0, sd=std_i))
# cheking proper samplesplit ratio; samplesize must be whole number
assertthat::assert_that(samplesize>1|samplesize<0, msg="Samplesplit ratio variable must be between 0 and 1!")

# sample splitting
trainlength <- samplespliratio*samplesize
assertthat::assert_that(is.wholenumber(trainlength)==TRUE, msg="The sample size is not whole number, please revise samplesplit!")

# in-sample
insample <- samplepoints[1:trainlength]
#out-of-sample
outofsample <- samplepoints[(trainlength+1):samplesize]

# temp matrix is necessary for rolling-window estimation; windowsize=insample size; lag=1 (which is the horizont of the forecast)
temp <- matrix(NA, nrow=(modelsnum+1), ncol = (length(outofsample)))

# model estimation with rolling window

for(l in 1:(length(outofsample))){


arma0 <- arima(if(l==1){insample}else{as.ts(c(insample,temp[1,1:(l-1)]))}, order = c(2,0,1), fixed = c(0.6,0.2,0.6,NA),transform.pars = FALSE)
arma1 <- arima(if(l==1){insample}else{as.ts(c(insample,temp[2,1:(l-1)]))}, order = c(1,0,2), fixed = c(0.6,0.6,0.2,NA),transform.pars = FALSE)
arma2 <- arima(if(l==1){insample}else{as.ts(c(insample,temp[3,1:(l-1)]))}, order = c(2,0,3), fixed = c(0.6,0.2,0.4,0.2,0.1,NA),transform.pars = FALSE)
arma_original <- arima(if(l==1){insample}else{as.ts(c(insample,temp[4,1:(l-1)]))}, order = c(2,0,2), fixed = c(0.6,0.2,0.6,0.2,NA),transform.pars = FALSE)
arma3 <- arima(if(l==1){insample}else{as.ts(c(insample,temp[5,1:(l-1)]))}, order = c(3,0,2), fixed = c(0.4,0.2,0.1,0.6,0.2,NA),transform.pars = FALSE)
arma4 <- arima(if(l==1){insample}else{as.ts(c(insample,temp[6,1:(l-1)]))}, order = c(3,0,3), fixed = c(0.4,0.2,0.1,0.4,0.2,0.1,NA),transform.pars = FALSE)

# doing forecast

forecast_arma0 <- forecast(arma0, h=1)
forecast_arma1 <- forecast(arma1, h=1)
forecast_arma2 <- forecast(arma2, h=1)
forecast_arma_original <- forecast(arma_original, h=1)
forecast_arma3 <- forecast(arma3, h=1)
forecast_arma4 <- forecast(arma4, h=1)


# for the combined model I need a forecast object, so I overwrite one model
forecast_combination <- forecast_arma4
# equally weighted estimation 
combination_mean <- 1/6*(forecast_arma0$mean + forecast_arma1$mean + forecast_arma2$mean + forecast_arma_original$mean + forecast_arma3$mean + forecast_arma4$mean)
#combination_mean <- 1/5*(forecast_arma0$mean + forecast_arma1$mean + forecast_arma2$mean + forecast_arma3$mean + forecast_arma4$mean)


forecast_combination$mean <-combination_mean

# temp matrix stores the rolling window estimations

temp[1,l] <- forecast_arma0$mean
temp[2,l] <- forecast_arma1$mean
temp[3,l] <- forecast_arma2$mean
temp[4,l] <- forecast_arma_original$mean
temp[5,l] <- forecast_arma0$mean
temp[6,l] <- forecast_arma0$mean
temp[7,l] <- forecast_combination$mean

}

# rmse calculation for all models
results[,j,i] <- apply(temp, 1,FUN=function(x) rmse(x,outofsample))

}
}


best_rmse_models<-matrix(NA, ncol=times, nrow=length(std))
# this matrix  includes the models with lowest RMSE
rownames(best_rmse_models) <- paste0("sd=", std)
colnames(best_rmse_models) <- paste0("exp=",1:times)


best_models_list<-list()

indexes <- c(4,7)
# index 4 means the original model and index 7 the combined

for(k in 1:length(std)){
best_rmse_models[k,]<-(apply(results[indexes,,k],2, FUN = which.min))
best_models_list[[k]] <- table(best_rmse_models[k,])
}

# legjobb modellek szórásonként
compared_models <- c("arma_original", "combination")

final <- compared_models[as.numeric(names(unlist(lapply(best_models_list, which.max))))]
# This step seems to be complicated, it is not!
# 1. I calculate the index of the most frequent model apperad in the sample (lapply)
# 2. I convert list to vector by unlisting
# 3. I check the names of the vector (names), which are the model indexes
# 4. I convert the characters to numbers as.numeric()
# 5. I select the best model from the compared_models list

names(final) <- paste0("sd=", std)
# solution; best models for every standard deviation
final

e <- Sys.time()

e-s
