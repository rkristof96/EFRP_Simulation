input<-function(short_or_long_experiment=TRUE){
  # if you want to check the code set short_or_long_experiment==False
  # if you want to run the real experiment please select True
  
  input<-list()
  
  if(short_or_long_experiment==TRUE){
    input[["samplesize"]] <- c(50, 100, 200, 500, 1000) # samplesize different cases
    input[["experiments_times"]] <- 100 # describes how many times will be repeated the experiments
    input[["standard_deviation_of_data_generating_process"]] <- c(0.01,0.05,0.1,0.15,0.25,0.5,0.75,1,2,5)
    input[["samplesplitratio"]] <- 0.9
    input[["modelnames"]] <- c("arma0", "arma1","arma2","arma_original","arma3","arma4","combination")
    input[["modelsnum"]] <- (length(input[["modelnames"]]) - 1) # 6
    # model names, only arma_original and combination are compared
    input[["compared_models"]] <- c("arma_original", "combination") # names of compared models
    input[["indexes"]]  <- c(4,7) # indexes of compared models from modelnames list
    #input[["rmse_resultslist"]] <- list()  # contains the RMSE data
    
  }else{
    
    input[["samplesize"]]<- c(50,100, 200)
    input[["experiments_times"]] <- 5
    input[["standard_deviation_of_data_generating_process"]] <- c(1,2)
    input[["samplesplitratio"]] <- 0.9
    input[["modelnames"]] <- c("arma0", "arma1","arma2","arma_original","arma3","arma4","combination")
    input[["modelsnum"]] <- (length(input[["modelnames"]]) - 1) # 6
    # model names, only arma_original and combination are compared
    input[["compared_models"]] <- c("arma_original", "combination") # names of compared models
    input[["indexes"]]  <- c(4,7) # indexes of compared models from modelnames list
    # input[["rmse_resultslist"]] <- list()  # contains the RMSE data
    
  }
  return(input)
}