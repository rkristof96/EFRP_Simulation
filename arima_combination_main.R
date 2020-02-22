
# loading packages
library(bazar)
library(forecast)
library(Metrics)
library(parallel)

# loading functions
source("input.R")
source("output.R")
source("repeat_experiments.R")
source("run_experiments.R")
source("finalize.R")


# starting clock
s <- Sys.time()

# fixing seed for reproduction purposes
set.seed(2020)

# input function reads input for the analysis
# the code is running slowly
# if you ONLY want to CHECK the code, please write FALSE as input in the input() function
# it will generate a small sample
# the whole analysis is presented on a larger sample
input <-input()
# output variable are imported
output <- output()

# data generation
for(u in 1:length(input[["samplesize"]])){

    output[["rmse_results"]] <- array(NA,dim = c(2, input[["experiments_times"]], length(input[["standard_deviation_of_data_generating_process"]])), dimnames = list(input[["compared_models"]],1:input[["experiments_times"]],paste0("sd=",input[["standard_deviation_of_data_generating_process"]])))
  # first dim model number
  # second dim number of repeated experiments
  # third dim # change of standard deviation
  for(i in 1:length(input[["standard_deviation_of_data_generating_process"]])){
    std_i <- input[["standard_deviation_of_data_generating_process"]][i]
     for(j in 1:input[["experiments_times"]]){
       # modifying inputs for repeated experiments
       input <- repeat_experiments(input, u, std_i)
       for(l in 1:(length(input[["outofsample"]]))){
         # running simulations
         input <- run_experiments(input, l)
       }
       # rmse calculation for all models
       output[["rmse_results"]][,j,i] <- apply(input[["rolling_window_estimation"]][input[["indexes"]],], 1,FUN=function(x) rmse(x,input[["outofsample"]]))
     }
   }
   output[["rmse_resultslist"]][[u]] <-output[["rmse_results"]]
}

results <- finalize(input, output)

e <- Sys.time()
# time needed for the simulation
e-s

# final table of best models
results[["final"]]

# frequency table of different models apperearing as best models
results[["frequencytable"]] 

# write output into a csv file (optional)
# write.csv(results[["final"]],"final.csv")
# write.csv(results[["frequencytable"]] ,"frequencytable.csv")
