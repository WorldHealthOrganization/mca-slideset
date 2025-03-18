
# main.R
# code to run BMat using 2019 data base
# Leontine Alkema
#-----------------------
# make sure that your working directory equals the bmat2019_code project directory,
# with subfolders R and inputs
#-----------------------

#-----------------------
# output will be saved in folder output/runname
runname <- "test_bmat_test" # choose your runname

#-----------------------
# load libraries
library(rjags)
library(msm)
library(mvtnorm)
library(R2jags)
library(foreign)
library(tidyverse)
library(readxl)
library(R.utils)
library(truncnorm)

#-----------------------
# source scripts in R subfoler with functions
Rfiles <- list.files(file.path(paste0(getwd(),"/R/")), ".R")
Rfiles <- Rfiles[grepl(".R", Rfiles)]
sapply(paste0(paste0(getwd(),"/R/"), Rfiles), source)

#-----------------------
# create output folder and store relevant info in it
output.dir <- MakeDirs(runname)
dat <- read_csv("inputs/BMat2019_datainputs.csv", na = "NA",
                # use guess_max to avoid mmr_obs and SE column turning into logicals
                # (alternatively, specify all column types)
                guess_max = 4424) 
# rename for consistency with code
datall <- dat %>%
  rename(final_pm = pm_obs, final_env  = env,
         rhovr = crvs_completeness) %>%
  filter(modelinclude)
saveRDS(datall, paste0(output.dir, "/datall.rds"))
meta <- readRDS("inputs/meta.rds")
saveRDS(meta, paste0(output.dir, "/meta.rds"))
file.copy("inputs/jags_model_file.txt", paste0(output.dir, "/model.txt"), overwrite = TRUE)
GetJagsData(runname = runname) # adds object jagsdata to the outputdir with info to run the model

#-----------------------
# run model
RunMCMC(runname, runsettings = "test") # just to get test results
#RunMCMC(runname, runsettings = "quick") # 1 hour run
#RunMCMC(runname, runsettings = "long") # for actual results

#-----------------------
# process outputs
percentiles <- c(0.1, 0.5, 0.9)
percnames <- paste0("perc_", 100*percentiles, "%")
GetCountryResults(runname)

# access various outputs:
CIs <- readRDS(paste0(output.dir, "CIs.rds"))
names(CIs)
CIs$mmr.cqt
CIs$mmr.cqt

#-----------------------
# The End!
