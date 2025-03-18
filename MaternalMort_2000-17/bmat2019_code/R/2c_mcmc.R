
#---------------------
# domcmc.R
# Leontine Alkema and Sanqian Zhang, 2015
# Updated by LA 2019
#-------------------------------

RunMCMC <- function(
  runname  = runname,
  # mcmc SETTINGS
  runsettings,
  ## choose from ("test", "quick", "long")
  ## test is only for checking if the model runs (just a few iterations);
  # quick is for getting approximate results in an hour or so;
  # long is for geting final results
  run.on.server = FALSE # using doMC and foreach libraries
){
  if (!is.element(runsettings, c("test", "quick", "long"))){
    print("You provided a non-standard option for the MCMC settings, a test run will be started.")
    runsettings <- "test"
  }
  if (runsettings=="long") print("Warning: you are starting a longggg run, which is going to take several hours of parallel computing")
  if (runsettings=="test") print("Warning: you are starting a test run, to be used for checking the model code only, do NOT use the results (they are not reliable)")
  if (runsettings=="quick") print("Warning: you are starting a quick run, which is going to take an hour or so. Results are approximate")

  output.dir <- MakeDirs(runname)
  meta <- readRDS(paste0(output.dir, "meta.rds"))
  jagsdata <- readRDS(paste0(output.dir, "jagsdata.rds"))


  jags.params <- parnames <- c(
    "v.j",
    "sigma.alpha", "rsigma.alpha", "sensworld",
    "sens.ct",
    "arma.ct",
    "phi", "theta",
    "sqrtgamma0", "sigma.lambda","lambda.c","sigma.c",
    "logsigma.ar.ct","sigma.ar.ct", "mean.gamma", "sigma.gamma","phi.gamma",
    "mu.ct", "mustar.ct", "beta.h", "alpha.c", "alpha.r", "alpha.world", "sigma.country", "sigma.region",
    "nonsamplingdhs.se","nonsamplingnondhs.se",
    "oneminpi.c", "logphi.preg.jnew", "logphi.jnew",
    "tau.jnew",
    "tau.j",
    "p.wprior.k2")

  # Note: this is related to validation runs, not relevant (nor tested) in this public release code set
  SavePredPM <- FALSE
  if (SavePredPM){
    jags.params <- c(jags.params, "logpmpred.g", "logpmpred.ginq", "logpmpred.gnew")
  }

  inits <- function(AddARMA = TRUE,  meta){
    initslist <- list()
    if (AddARMA){
      initslist <- c(initslist, list("phi" = runif(1,0.6,0.8)))
    }
    # make sure we keep R below 1 for things to run
    initslist <- c(initslist, list(alpha.c = (runif(meta$C,-1,-0.2)+
        min(-
              # this is the est for Rtilde - alpha with min and max betas as defined above to get max outcome
              (- ifelse(c(meta$X.cth[,,1])<0, 1.1, 0.9)*0.4*c(meta$X.cth[,,1])
               + ifelse(c(meta$X.cth[,,2])<0,0.9, 1.1)*1.1*c(meta$X.cth[,,2])
               - ifelse(c(meta$X.cth[,,3])<0,1.1, 0.9)*0.8*c(meta$X.cth[,,3]))
      ))))
    return(list(initslist))
  } # end inits function

  # MCMC settings
  if (runsettings=="quick"){
    ChainNums <- seq(1,4)
    n.chains <- length(ChainNums)
    N.STEPS = 1
    n.iter.perstep = 1000
    n.thin = floor((n.iter.perstep*N.STEPS*n.chains)/3000)
    n.burnin = 2000
  }
  if (runsettings=="long"){
    ChainNums <- seq(1,10)
    n.chains <- length(ChainNums)
    N.STEPS = 15
    n.iter.perstep = 4000
    n.thin = floor((n.iter.perstep*N.STEPS*n.chains)/3000)
    n.burnin = 5000
  }
  if (runsettings=="test"){
    ChainNums <- c(1,2)
    n.chains <- length(ChainNums)
    N.STEPS = 1
    n.iter.perstep = 5
    n.thin = 1
    n.burnin = 2
  }

  rnorm(1)
  set.seed(1234)

  if (!run.on.server){
    #mod <- jags(jagsdata,  jags.params, model.file = paste0(output.dir, "model.txt"),
    mod <- jags.parallel(jagsdata,  jags.params, model.file = paste0(output.dir, "model.txt"),
                          inits = inits(meta =meta),
                            n.chains=n.chains,
    n.burnin = n.burnin, n.iter= n.burnin+n.iter.perstep*N.STEPS, n.thin = n.thin)
     mcmc.array = mod$BUGSoutput$sims.array
  } else {
    # start parallel runs, save results in steps
    library(foreach)
    library(doMC)
    registerDoMC()

    foreach(chainNum=ChainNums) %dopar% {
      set.seed(chainNum)
      temp <- rnorm(chainNum)
      mod <- jags(data = jagsdata,
                  inits = inits(meta = meta),
                  parameters.to.save = jags.params,
                  n.chains = 1,
                  n.iter = n.iter.perstep+n.burnin,
                  n.burnin = n.burnin, n.thin = n.thin,
                  model.file= paste0(output.dir, "model.txt"),
                  jags.seed = 123*chainNum,
                  working.directory= getwd())
      i = 1 # index for which update
      mod.upd <- mod
      save(mod.upd, file=paste(output.dir, "/temp.JAGSobjects/jags_mod", chainNum, "update_", i, ".Rdata", sep = "")
      )
      cat(paste("MCMC results step", 1, " for chain ", chainNum, " written to folder temp.JAGSobjects"), "\n")
      #--- update MCMC ----------
      if (N.STEPS > 1){
        for (i in 2:(N.STEPS)){
          mod.upd <-update(mod.upd, parameters.to.save=jags.params,
                           n.iter=n.iter.perstep,
                           n.thin=n.thin)
          save(mod.upd, file=paste(output.dir, "/temp.JAGSobjects/jags_mod", chainNum, "update_", i, ".Rdata", sep = ""))
          cat(paste("MCMC results step", i, " for chain ", chainNum, " written to folder temp.JAGSobjects"), "\n")
        }
      }
    } # end chains

    mcmc.array <- ReadJagsOutput(n.steps = N.STEPS, ChainNums = ChainNums, output.dir = output.dir)
  }
  saveRDS(mcmc.array, file = paste0(output.dir, "/mcmc.array.rds"))
  # bit unnecessary to save both objects but done here for convenience
  #mcmc.sp <- wrap(mcmc.array, map=list(NA,3))
  #save(mcmc.sp, file = paste0(output.dir, "/mcmc.sp.rda"))
  return(NULL)
} # end function

#-----

#---------------------
ReadJagsOutput <- function(n.steps, ChainNums = NULL, n.chains = NULL, output.dir = "output/"){
  if(is.null(ChainNums))  ChainNums<- seq(1, n.chains)
  if (is.null(n.chains)) n.chains <- length(ChainNums)
  chain =ChainNums[1]
  load(paste(output.dir, "temp.JAGSobjects/jags_mod", chain, "update_1.Rdata", sep = ""))
  n.iter.perstep <- dim(mod.upd$BUGSoutput$sims.array)[1]
  n.sim <- n.steps *n.iter.perstep
  n.par <- dim(mod.upd$BUGSoutput$sims.array)[3]
  mcmc.array <- array(NA, c(n.sim, n.chains, n.par))
  dimnames(mcmc.array) <- list(NULL, NULL, names(mod.upd$BUGSoutput$sims.array[1,1,]))
  for (chain in 1:n.chains){
    chain_saved <- ifelse(length(ChainNums)==1,1,ChainNums[chain])
    cat(paste("Reading in chain number ", chain_saved, " (step 1)", sep = ""), "\n")
    load(paste(output.dir,"temp.JAGSobjects/jags_mod", chain_saved, "update_1.Rdata", sep = ""))
    mcmc.array[1:n.iter.perstep,chain, ] = mod.upd$BUGSoutput$sims.array[,1,]
    if (n.steps>1){
      for (step in 2:n.steps){
        cat(paste("Reading in chain number ", chain_saved, " (step ", step, ")", sep = ""), "\n")
        load(paste(output.dir,"temp.JAGSobjects/jags_mod", chain_saved, "update_", step, ".Rdata", sep = ""))
        mcmc.array[((step-1)*n.iter.perstep+1):(step*n.iter.perstep),chain, ] = mod.upd$BUGSoutput$sims.array[,1,]
      }
    }
  }
  n.sample.max = 10000000
  if (n.sim > n.sample.max){
    mcmc.array <- mcmc.array[seq(1, n.sample.max, length.out = n.sample.max), , ]
  }
  saveRDS(mcmc.array, file = paste(output.dir, "mcmc.array.rds", sep = ""))
  return(NULL)
}

#-----------------
# The End!
