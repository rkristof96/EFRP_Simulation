repeat_experiments <- function(input, u, std_i){
  
  
  # sample points generation from ARMA(2,2) with given parameters
  
  input[["samplepoints"]]<-arima.sim(n=input[["samplesize"]][u], list(ar=c(0.6,0.2), ma=c(0.6,0.2)),innov = rnorm(input[["samplesize"]][u], mean = 0, sd=std_i))
  # cheking proper samplesplit ratio; input[["samplesize"]] must be whole number
  assertthat::assert_that((input[["samplesplitratio"]]<1&input[["samplesplitratio"]]>0), msg="Samplesplit ratio variable must be between 0 and 1!")
  
  # sample splitting
  input[["trainlength"]] <- input[["samplesplitratio"]]*input[["samplesize"]][u]
  assertthat::assert_that(is.wholenumber(input[["trainlength"]])==TRUE, msg="The sample size is not whole number, please revise samplesplit!")
  
  # in-sample
  input[["insample"]] <- input[["samplepoints"]][1:input[["trainlength"]]]
  #out-of-sample
  input[["outofsample"]] <- input[["samplepoints"]][(input[["trainlength"]]+1):input[["samplesize"]][u]]
  
  # rolling_window_estimation matrix is necessary for rolling-window estimation; windowsize=insample size; lag=1 (which is the horizont of the forecast)
  input[["rolling_window_estimation"]] <- matrix(NA, nrow=(input[["modelsnum"]]+1), ncol = (length(input[["outofsample"]])))
  
  # model estimation with rolling window
  
  return(input)
  
}