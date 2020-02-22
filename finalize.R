finalize <- function(input, output){
  
  for(n in 1:length(input[["samplesize"]])){
    output[["modelsfrequency"]] <- matrix(NA, ncol = 2, nrow = length(input[["standard_deviation_of_data_generating_process"]]))
    for(m in 1:length(input[["standard_deviation_of_data_generating_process"]])){
      output[["modelsfrequency"]][m,] <- table(apply(output[["rmse_resultslist"]][[n]][,,m],2,FUN=which.min))
    }
    output[["modelsfrequencylist"]][[n]]<-output[["modelsfrequency"]]
  }
  
  # frequencytable presents count how many time overperformed one modell the other
  output[["frequencytable"]] <- array(unlist(output[["modelsfrequencylist"]]), dim = c(length(input[["standard_deviation_of_data_generating_process"]]), 2, length(input[["samplesize"]])), dimnames = list(paste0("sd=", input[["standard_deviation_of_data_generating_process"]]), input[["compared_models"]], paste0("T=", input[["samplesize"]])))
  
  
  # includes the solution of the simulation
  output[["final"]] <- matrix(input[["compared_models"]][sapply(output[["modelsfrequencylist"]], FUN=function(x)apply(x, 1, which.max))],ncol=length(input[["standard_deviation_of_data_generating_process"]]),nrow = length(input[["samplesize"]]), byrow = T)
  colnames(output[["final"]]) <- paste0("sd=", input[["standard_deviation_of_data_generating_process"]])
  rownames(output[["final"]]) <- paste0("T=", input[["samplesize"]])
  
  
  return(output)
  
}