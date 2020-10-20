#### Packages ####
library(nlrx)
library(future) # needed to run simulations in parallel in R with function plan(multisession)
library(furrr)  # to run simulations in parallel using function furrr::map

#### Functions #####

#' @description Runs simulations of the Netlogo model Logotraits recycling the burn-in period.
#' @param seed:          vector with the seed values. Each seed indicates the random seed of a replicate.
#' @param nl:            nlrx object, in which simdesign has already been set.
#' @param siminputrow:   Which row of siminput to run?
#' @param threats: matrix of threat values, cols = type of threat, rows = range of values
#' @param shout:         whether to report simulation time. Note: when this function is called by future::map, it will only report at the end of simulations.
#' @return An nl object with all parameter values, and all values for all chosen metrics.
sprout <- function(seed, nl, siminputrow, threats, shout = T){
  subexp <- list()
  
  # Change global parameters stored in nl so that simulation only runs burn-in.
  nl@simdesign@siminput$`only-run-burn-in?` = "true"
  nl@simdesign@siminput$`load-world?`       = "false"
  
  #create temporary file to store the burn-in period
  filename <- paste(paste(sample(LETTERS, 5), collapse =""), format(Sys.time(),'_%Y%m%d_%H%M%S'),sep ="")
  nl@simdesign@siminput$filename = paste("\"", filename, "\"", sep= "")
  
  if (shout) {start1 <- Sys.time()} #reports the time it took to simulate
  
  # run burn-in period. simulation stops after burn-in period ends.
  run_nl_one(nl, seed, siminputrow = siminputrow)
  
  if (shout){
    print("burn-in finished.")
    print(Sys.time() - start1)
  }
  
  # Now set next simulations to re-use burn-in population for several runs
  nl@simdesign@siminput$`only-run-burn-in?` =  "false"
  nl@simdesign@siminput$`load-world?`        = "true"
  
  # Now run once without threats
  o <- 1
  subexp[[o]] <- run_nl_one(nl,seed, siminputrow = siminputrow)
  o <- o + 1
  
  # outer loop selects threat.
  for (l in 1:ncol(threats)){
    threat1 <- names(threats)[l]
    # inner loop selects value for the threat
    for (m in 2:nrow(threats)){ #it will not run the first row because first row was already run (threat value 0).
      nl@simdesign@siminput[threat1] = threats[m, threat1]
      if(shout){start1 <- Sys.time()} # Timing
      subexp[[o]] <- run_nl_one(nl, seed = seed, siminputrow = siminputrow, silent = T)
      o <- o + 1
      if(shout){
        print("finished simulation.")
        print(Sys.time() - start1)
      }
    }
    nl@simdesign@siminput[threat1] = 0 #reset threats
  }
  unlink(filename)
  return (subexp)
}

#' This function recursively transforms lists of lists of dataframes into a single dataframe
#' by merging rows
#' @param x a list of lists of dataframes or tibles
#' @return a single dataframe
to_df <- function(x){
  df <- NULL
  
  if(class(x[[1]]) %in% c("spec_tbl_df", "tbl_df", "tbl", "data.frame")){
    for (i in 1:length(x)){
      df <- rbind(df, x[[i]])
    }
    return(df)
  }
  for (i in 1:length(x)){
    df <- rbind(df, to_df(x[[i]]))
  }
  return(df)
}


# The following code is better understood if the user knows how to use package nlrx.
# For a guide consult the following link:
# https://rdrr.io/cran/nlrx/man/nlrx-package.html

#### Step 1: Creating the nl object ####
nl <- nl(nlversion = nlversion,
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 8192)


#### Step 2: Attaching an experiment to nl object ####

nl@experiment <- experiment(expname=expname_SA,
                            outpath=outpath,
                            repetition=repetition,
                            tickmetrics=tickmetrics,
                            idsetup=idsetup,
                            idgo=idgo,
                            runtime=runtime,
                            metrics=metrics,
                            variables = list(),
                            constants = c(base_parameters, parameters_SA),
                            stopcond = stopcond)

if(RA){
  nl@experiment@expname   <- expname_RA
  nl@experiment@variables <- parameters_RA
  nl@experiment@constants <- base_parameters
}

#Creating a dataframe containing all threats and their intensities
threats <- as.data.frame(cbind(habitat_loss, habitat_fragmentation,
                               habitat_degradation, direct_killing,
                               invasives))

#### Step 3: Attaching a simulation design ####

nl@simdesign <- simdesign_simple(nl = nl, nseeds = SA_n)
if(RA)
  nl@simdesign <- simdesign_distinct(nl = nl, nseeds = RA_n)

#### Step 4: Running the simulations in parallel ####
# Package nlrx comes with a tool to run simulations in parallel, but it is
# too simple to be used with experimental designs which recycle simulations.
# The following code uses functionalities of the nlrx package, but parallelism is
# ensured in a different way. Each different core runs the function sprout() in parallel.
# sprout() runs one burn-in, then with each burned-in population we test each threat value.

# This chunk of code reports time
all_branches <- length(nl@simdesign@siminput[[1]])
branches_finished <- 0
start <- Sys.time()


all_results <- list()
# for testing purposes
#all_results[[1]]<- purrr::map(nl@simdesign@simseeds, sprout, nl, threats, siminputrow = 1)

#Outer loop: iterates over all sets of parameter values (when using SA, there is only one set)
for (i in 1:nrow(nl@simdesign@siminput)) {
  # create instances of R
  plan(multisession)
  #Run simulations!
  all_results[[i]] <- furrr::future_map(nl@simdesign@simseeds, sprout, nl, threats, siminputrow = i, .progress = T)
  
  #Time reporters
  branches_finished <- branches_finished + 1
  percent <- branches_finished / all_branches * 100
  print(paste("Finished set of parameter values ", i, 
              ". Progress: ", branches_finished, "/", all_branches, " - ", round(percent, 1), "%", sep = ""))
  print(Sys.time() - start)
}
end <- Sys.time()
end - start # Runtime

#### Step 5: Saving to disk in output directory ####
# Create file name to store results
exp_name<- paste(gsub(":", "", Sys.time()), "_", nl@experiment@expname, ".csv", sep = "")

# Convert to .csv
results_df <- to_df(all_results)
# Write results to file
write.csv(results_df, paste(outpath, exp_name, sep="/"))