run_experiments <- function(input, l){
  # l means index
arma0 <- arima(if(l==1){input[["insample"]]}else{as.ts(c(input[["insample"]],input[["rolling_window_estimation"]][1,1:(l-1)]))}, order = c(2,0,1), fixed = c(0.6,0.2,0.6,NA),transform.pars = FALSE)
arma1 <- arima(if(l==1){input[["insample"]]}else{as.ts(c(input[["insample"]],input[["rolling_window_estimation"]][2,1:(l-1)]))}, order = c(1,0,2), fixed = c(0.6,0.6,0.2,NA),transform.pars = FALSE)
arma2 <- arima(if(l==1){input[["insample"]]}else{as.ts(c(input[["insample"]],input[["rolling_window_estimation"]][3,1:(l-1)]))}, order = c(2,0,3), fixed = c(0.6,0.2,0.4,0.2,0.1,NA),transform.pars = FALSE)
arma_original <- arima(if(l==1){input[["insample"]]}else{as.ts(c(input[["insample"]],input[["rolling_window_estimation"]][4,1:(l-1)]))}, order = c(2,0,2), fixed = c(0.6,0.2,0.6,0.2,NA),transform.pars = FALSE)
arma3 <- arima(if(l==1){input[["insample"]]}else{as.ts(c(input[["insample"]],input[["rolling_window_estimation"]][5,1:(l-1)]))}, order = c(3,0,2), fixed = c(0.4,0.2,0.1,0.6,0.2,NA),transform.pars = FALSE)
arma4 <- arima(if(l==1){input[["insample"]]}else{as.ts(c(input[["insample"]],input[["rolling_window_estimation"]][6,1:(l-1)]))}, order = c(3,0,3), fixed = c(0.4,0.2,0.1,0.4,0.2,0.1,NA),transform.pars = FALSE)

# doing one step forecasts

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

# input[["rolling_window_estimation"]] matrix stores the rolling window estimations

input[["rolling_window_estimation"]][1,l] <- forecast_arma0$mean
input[["rolling_window_estimation"]][2,l] <- forecast_arma1$mean
input[["rolling_window_estimation"]][3,l] <- forecast_arma2$mean
input[["rolling_window_estimation"]][4,l] <- forecast_arma_original$mean
input[["rolling_window_estimation"]][5,l] <- forecast_arma0$mean
input[["rolling_window_estimation"]][6,l] <- forecast_arma0$mean
input[["rolling_window_estimation"]][7,l] <- forecast_combination$mean

# return(input[["rolling_window_estimation"]])
return(input)


}