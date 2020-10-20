# This file is supplementary information to the publication XXXXXX

# Instructions:
# 1. Change the working directories and filepaths accordingly under "3. Input/Output"
# 2. Under "2. SETTINGS", check that the parameter values are correct
# 3. Do you want to run the standard analysis (SA)?
# 4. Do you want to run the robustness analysis (RA)?
# 5. Source this file! It will take some time to run the simulations. It should create 
#  an output file .csv with the experiment's results.

#### 1. Terminology ####
# SS: Standard analysis
# RA: Robustness analysis

#### 2. SETTINGS ####
expname_SA <- "logotraits19_SS" # Name for the SA
expname_RA <- "logotraits19_RA" # Name for the RA

run_SA <- F  # whether to run SA
run_RA <- T  # whether to run RA

SA_n <- 64 # number of random seeds of the SA
RA_n <- 4  # number of random seeds of the RA

### Range of threat values ###

#***NOTE****
# Make sure the first element of each of the following vectors is 0 (threat value 0).
direct_killing        <- round(seq(from=0,   to = 10,   length.out = 20),1)
habitat_loss          <- round(seq(from=0,   to = 80,   length.out = 20),1)
habitat_fragmentation <- round(seq(from=0,   to = 70,   length.out = 20),1)
habitat_degradation   <- round(seq(from=0,   to = 50,   length.out = 20),1)
invaders             <- round(seq(from=0,   to = 70,   length.out = 20),1)

### Parameters that are the same for both SA and RA ###
base_parameters = list(
  #MAP
  "map-seed"    = 0,
  
  #STARTING ORGANISMS
  "starting-nr" =  500,
  
  ####THREATS                       # Leave at zero          
  "direct_killing"                 = 0, 
  "habitat_loss"                   = 0,
  "habitat_fragmentation"          = 0,
  "habitat_degradation"            = 0, 
  "invaders"                       = 0, 
  
  #OTHER PARAMETERS
  "stand-dev-to-body-size"         = 1,
  
  #VISUALS
  "turtle-aesthetics?"             = "false",
  
  #WHICH TRAITS MUTATE
  "body-size-mutation?"     = "true",
  "maturity-age-mutation?"  = "true",
  "fecundity-mutation?"     = "true", 
  "disp-ability-mutation?"  = "true",
  
  
  #STOPPING CONDITIONS
  "nr-of-means"                   = 5,
  "stopping-conditions-threshold" = 0.05,
  "stopping-conditions-interval"  = 50,
  
  #BURN-IN OPTIONS
  "only-run-burn-in?" = "false",
  "load-world?"       = "false",
  "filename"          = "\"filename\""
)

### Parameter values (SA) ###
parameters_SA <- list(
  #WORLD DIMENSIONS
  "w-width"                  =   33,
  "w-length"                 =   33,
  "nr-resource-clusters"     =   20,
  "resource-patch-fraction"  =   0.5,
  
  #ENERGETIC CONSTANTS
  "maturation-cost-coef"     = 0.1,
  "dispersal-cost-coef"      = 0.01,
  "maintenance-cost-coef"    = 0.1,
  "max-energy-intake-coef"   = 0.3,
  "energy-to-reproduce-coef" = 0.7,
  "energy-after-reprod-coef" = 0.3,
  
  #PATCHES
  "regen-rate"               = 0.2,
  
  #OTHER PARAMETERS
  "longevity-maturity-coef"        = 3,
  "metabolic-allometric-exponent"  = 0.75,
  "mutation-amplitude"             = 0.05
)

### Range of parameter values to test (RA) ###
parameters_RA <- list (
  #MAP PARAMETERS
  "w-width"                        = list(values=c(20,99, rep(33,21))),
  "w-length"                       = list(values=c(20,99, rep(33,21))),
  "nr-resource-clusters"           = list(values=c(rep(20,2),7,60,rep(20,19))),
  "resource-patch-fraction"        = list(values=c(rep(0.5,4),0.16,1,rep(0.5,17))),
  
  #ENERGETIC CONSTANTS
  "maturation-cost-coef"           = list(values=c(rep(0.1,6) , 0.03, 0.20, rep(0.1 ,15))),
  "dispersal-cost-coef"            = list(values=c(rep(0.01,8), 0.003, 0.025, rep(0.01,13))),
  "maintenance-cost-coef"          = list(values=c(rep(0.1,10), 0.03, 0.20, rep(0.1 ,11))),
  "max-energy-intake-coef"         = list(values=c(rep(0.3,12), 0.9, rep(0.3 ,10))),
  "energy-to-reproduce-coef"       = list(values=c(rep(0.7,13), 0.9, rep(0.7,9))),
  "energy-after-reprod-coef"       = list(values=c(rep(0.3,14), 0.1, rep(0.3,8))),
  
  #PATCHES
  "regen-rate"                     = list(values=c(rep(0.2,15), 0.07, 0.6, rep(0.2,6))),
  
  #OTHER PARAMETERS
  "longevity-maturity-coef"        = list(values=c(rep(3,17), 1, 9, rep(3,4))),
  "metabolic-allometric-exponent"  = list(values=c(rep(0.75,19), 0.66, 1, rep(0.75,2))),
  "mutation-amplitude"             = list(values=c(rep(0.05,21), 0.017, 0.15))
)

### metrics to extract from the model ###
metrics <- c("init-mean-body-size",
             "init-mean-maturity-age",
             "init-mean-disp-ability",
             "init-mean-fecundity",
             "init-nr-organisms",
             
             "final-mean-body-size",
             "final-mean-maturity-age",
             "final-mean-disp-ability",
             "final-mean-fecundity",
             "final-nr-organisms",
             
             "init-sd-body-size",
             "init-sd-maturity-age",
             "init-sd-disp-ability",
             "init-sd-fecundity",
             
             "final-sd-body-size",
             "final-sd-maturity-age",
             "final-sd-disp-ability",
             "final-sd-fecundity",
             
             "ticks",
             "timer")

### Other settings ### (do not change!)
idgo        <- "go"    # Name of "go" procedure
idsetup     <- "setup" # Name of "setup procedure
repetition  <- 1       # Number of replicates with the same random seed (leave unchanged)
runtime     <- 50000   # Maximum number of timesteps
tickmetrics <- "false" # Whether to report metrics every timestep (the model is not prepared for this)
stopcond    <- "not any? organisms or simulation-over?" # stopping conditions in netlogo syntax


#### 3. Input/output ####
# Set the working directory, netlogo path, model file path, output path and nl version,
#  which are required to run the nl package
model_filename <- "logotraits19.nlogo"
if (Sys.info()["nodename"] == "APHAENOGASTER") {
  setwd("C:/Users/filipe/OneDrive - University of Helsinki/PhD/PART 2_Models/")
  model_folder <- getwd() # folder in which the model file is located 
  netlogopath <- file.path("C:/Program Files/NetLogo 6.1.1")
  
} else if (Sys.info()["nodename"] == "DFMNH-01" ){
  setwd("C:/Users/hyadmin/OneDrive - University of Helsinki/PhD/PART 2_Models/")
  model_folder <- getwd() # folder in which the model file is located
  netlogopath <- file.path("C:/Program Files/NetLogo 6.1.1/")
  
} else{
  setwd("C:/LocalData/chicorr/OneDrive - University of Helsinki/PhD/PART 2_Models/")
  model_folder <- getwd() # folder in which the model file is located
  netlogopath <- file.path("C:/Program Files/NetLogo 6.1.1")
  
}
nlversion = "6.1.1"
modelpath   <- file.path(paste(model_folder, model_filename, sep="/"))
outpath     <- file.path(getwd())

#### 4. Run Standard analysis ####
if (run_SA){
  RA <- F
  source("Logotraits_interface.R")
}

#### 5. Run Robustness analysis ####
if(run_RA){
  RA <- T
  source("Logotraits_interface.R")
}