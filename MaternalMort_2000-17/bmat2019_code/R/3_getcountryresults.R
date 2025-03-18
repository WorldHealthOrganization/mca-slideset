
#---------------------
# Functions to obtain country results
# Leontine Alkema and Sanqian Zhang, 2015
#---------------------

#-----------------
GetCountryResults <- function(runname =runname, getarrs = T){
  print("Loading samples and calculating a whole bunch of things, this will take a little while")
  output.dir <- MakeDirs(runname)
  meta <- readRDS(paste0(output.dir, "meta.rds"))
  jagsdata <- readRDS(paste0(output.dir, "jagsdata.rds"))

  #load(paste0(output.dir, "mcmc.sp.rda"))
  mcmc.array <- readRDS(paste0(output.dir, "mcmc.array.rds"))
  mcmc.sp <- wrap(mcmc.array, map=list(NA,3))

  #load(paste0(output.dir, "mcmc.sp.rda"))
  S <- dim(mcmc.sp)[1]
  percnames <- paste0("q",percentiles)

  #------------------------
  # 1. MMR, PM etc
  #------------------------
  # Note: non-aids does not include unc in births and deaths
  # all other arrays are
  emptyarray.cqt <- array(NA, c(meta$C, length(percentiles), meta$nyears))
  dimnames(emptyarray.cqt) <- list(meta$name.c, percnames, meta$year.t)
  mmr.cts <- matdeaths.cts <- array(NA, c(meta$C, meta$nyears, S))

  mmr.cqt <- emptyarray.cqt
  mmraids.ct <- emptyarray.cqt[,1,]
  mmrmean.ct <- matrix(NA, meta$C, meta$nyears)
  for (c in 1:meta$C){
    for (t in 1:meta$nyears){
      mmr_nocrisis.s <- c(mcmc.sp[,paste0("mu.ct[", c, ",", t, "]")])
      if (meta$crisisdeaths.ct[c,t] > 0){
        mmr.cts[c,t,] <- add_crisis_unc(mmr_nocrisis.s, #deaths = meta$deaths.ct[c,t],
                                      births = meta$births.ct[c,t], crisis_deaths = meta$crisisdeaths.ct[c,t])
      } else {
        mmr.cts[c,t,] <- mmr_nocrisis.s
      }
      matdeaths.cts[c,t,] <- mmr.cts[c,t,]*meta$births.ct[c,t]
      mmr.cqt[c,,t] <- quantile(c(mmr.cts[c,t,]),percentiles)
      mmrmean.ct[c,t] <- mean(c(mmr.cts[c,t,]))
      mmraids.ct[c,t]<- jagsdata$muaids.ct[c,t]
    }
  }
  aidsdeaths.ct <- mmraids.ct*meta$births.ct

  ltr.cqt <- mmrate.cqt <- pm.cqt <- matdeaths.cqt <- emptyarray.cqt
  for(q in 1:length(percentiles)){
    mmrate.cqt[,q,] <- GetMMRate(mmr=mmr.cqt[,q,],births=meta$births.ct,
                                 women= meta$births.ct/meta$gfr.ct)
    ltr.cqt[,q,]<-GetLTR(mmrate=mmrate.cqt[,q,],t15t50=meta$t15t50.ct,l15=meta$l15.ct)
    pm.cqt[,q,] <- mmr.cqt[,q,]*meta$births.ct/meta$deaths.ct
    matdeaths.cqt[,q,] <- mmr.cqt[,q,]*meta$births.ct
  }
  pmmean.ct <- mmrmean.ct*meta$births.ct/meta$deaths.ct
  #------------------------
  # 2. ARRs
  #------------------------
  ## 5 year and more interval ARR - include the changes in rates of change too
  ## for continuous ARR
  arrscts.kx <- data.frame(name = character(), iso = character(), period = character(),
                           matrix(nrow=0,ncol=length(percentiles),dimnames=list(NULL,percnames)))
  if (getarrs){
    for (c in 1:meta$C){
      res <- GetARRs(mmr.tj = mmr.cts[c,,], addname = meta$name.c[c], year.t  = meta$year.t,
                     addiso = meta$iso.c[c],method="continuous")
      arrscts.kx <- rbind(arrscts.kx, res$arrs.kx)
    }
  }
  CIs <- list(mmr.cqt = mmr.cqt,  pm.cqt = pm.cqt,
              pmmean.ct = pmmean.ct, mmrmean.ct = mmrmean.ct,
              mmraids.ct = mmraids.ct,
              mmrate.cqt=mmrate.cqt,
              ltr.cqt=ltr.cqt,
              aidsdeaths.ct = aidsdeaths.ct, matdeaths.cqt = matdeaths.cqt,
              arrscts.kx = arrscts.kx
  )
  saveRDS(CIs, file = paste0(output.dir, "/CIs.rds"))
  saveRDS(matdeaths.cts, file = paste0(output.dir, "/matdeaths.cts.rds"))
  print(paste0("RDS-objects CIs and matdeaths.cts saved in ", output.dir))
  return(NULL)
}

GetARRs <- function(mmr.tj,
                    year.t, # the years that mmr.tj refers to, defaults to meta$year.t = 1990:2015
                    # optional: add point estimates (eg for aggregates) to get point estimates for ARR
                    # if NULL, the median mmr will be used for that t
                    mmrpoint.t  = NULL,

                    # arrperiods need to be a vector with periods, where periods are recorded as "1990-2010"
                    arrperiods.k = c(
                      paste(seq(1990, 2010, 5), seq(1990, 2010, 5)+5, sep = "-"),
                      paste(1990, 2017, sep = "-"),
                      paste(1990, 2000, sep = "-"),
                      paste(2000, 2017, sep = "-"),
                      paste(2007, 2012, sep = "-"),
                      paste(2012, 2017, sep = "-"),
                      paste(2000, 2010, sep = "-"),
                      paste(2010, 2017, sep = "-")),#percentiles = percentiles,
                    addname = NA, addiso = NA,method="discrete"){
  K <- length(arrperiods.k)
  J <- dim(mmr.tj)[[2]]
  tstart.k <- tend.k <- rep(NA, K)
  nyears <- length(year.t)
  for (k in 1:K){
    tstart.k[k] <- seq(1, nyears)[year.t == (strsplit(arrperiods.k[k], split = "-")[[1]][1])]
    tend.k[k] <- seq(1, nyears)[year.t == (strsplit(arrperiods.k[k], split = "-")[[1]][2])]
  }
  arr.jk <- matrix(NA, J, K)
  arrs.kx <- data.frame(name = rep(addname, K), iso = rep(addiso, K), period = arrperiods.k,
                        matrix(NA,nrow=K,ncol=length(percentiles),dimnames=list(NULL,percnames)),
                        probgrzero=rep(NA,K), probgrmdg5=rep(NA,K), catmdg= rep(NA, K))
  changeinarrs.kmin1x <- data.frame(name = rep(addname, K-1), iso = rep(addiso, K-1), period = rep(NA, K-1),
                                    matrix(NA,nrow=K-1,ncol=length(percentiles),dimnames=list(NULL,percnames)),
                                    probgrzero=rep(NA,K-1), probgrmdg5=rep(NA,K-1),
                                    catmdg= rep(NA, K-1)) #not used but left to have same size
  if (is.null(mmrpoint.t)){
    mmrpoint.t <- apply(mmr.tj,1,median)
  }
  for (k in 1:K){
    arrstart.j <- mmr.tj[tstart.k[k],]
    arrend.j <- mmr.tj[tend.k[k],]
    # for point estimates
    mmrstart <- mmrpoint.t[tstart.k[k]]
    mmrend <- mmrpoint.t[tend.k[k]]
    arr.jk[,k] <- GetTrend(startvalues = arrstart.j,
                           endvalues =  arrend.j,
                           lengths = tend.k[k] - tstart.k[k],method=method)
    arrs.kx[k,percnames] <- quantile(arr.jk[,k], percentiles)
    arrs.kx[k,names(arrs.kx)=="q0.5"] <- arrpoint <- GetTrend(startvalues = mmrstart,
                                                              endvalues =  mmrend,
                                                              lengths = tend.k[k] - tstart.k[k],method=method)

    arrs.kx[k,"probgrzero"] <- mean(arr.jk[,k]>0)
    if (arrperiods.k[k] == paste(1990, 2015, sep = "-")){
      # addition: add in classification for MDG5
      # note: only relevant for 1990-2015 output
      # HARDCODED
      # arrs for cut-offs
      arr1 <- GetTrend(startvalues=1,  endvalues=0.25, lengths=25, method="continuous")
      arr2 <- GetTrend(startvalues=1,  endvalues=0.5, lengths=25, method="continuous")
      arr3 <- GetTrend(startvalues=1,  endvalues=0.75, lengths=25, method="continuous")
      arr4 <- 0
      arrlower <- quantile(arr.jk[,k], 0.1)
      arrs.kx[k,"catmdg"] <- ifelse(arrpoint > arr1, 1,
                                    ifelse( (arrpoint > arr2 & arrlower > arr3), 2,
                                            ifelse( (arrpoint > arr3 & arrlower > arr4), 3, 4)))
      arrs.kx[k,"probgrmdg5"] <- mean(arr.jk[,k]> arr1)
    }
  }
  for (k in 1:(K-1)){
    change.j <- arr.jk[,k] - arr.jk[,k+1]
    changeinarrs.kmin1x[k,percnames] <- quantile(change.j, percentiles)
    changeinarrs.kmin1x[k,names(changeinarrs.kmin1x)=="q0.5"] <- (arrs.kx[k,names(arrs.kx)=="q0.5"] -
                                                                    arrs.kx[k+1,names(arrs.kx)=="q0.5"])
    changeinarrs.kmin1x[k,"period"] <- paste(arrperiods.k[k], arrperiods.k[k+1], sep = " (minus) ")
    changeinarrs.kmin1x[k,"probgrzero"] <- mean(change.j>0)

  }
  return(list(arrs.kx =  rbind(arrs.kx, changeinarrs.kmin1x)))
}

#----

add_crisis_unc <- function(mmr_nocrisis.s, #deaths,
                           births, crisis_deaths){
  matdeaths_nocrisis.s <- mmr_nocrisis.s*births
  set.seed(1234)
  # upper bound based on ratio max maternal deaths plus x% of crisis deaths to max maternal deahts
  ratio_max <- min(1.2, (max(matdeaths_nocrisis.s) + 0.5*crisis_deaths)/max(matdeaths_nocrisis.s))
  #print(paste("max is", ratio_max))
  mult <- sort(rtruncnorm(length(matdeaths_nocrisis.s), mean = 1, sd = 0.15, a = 1- (ratio_max - 1)/2, b = ratio_max))
  matdeaths.s <- matdeaths_nocrisis.s*(mult[rank(matdeaths_nocrisis.s)])
  if (any((matdeaths.s - matdeaths_nocrisis.s) > crisis_deaths)){
    print("additional deaths for crisis uncertainty greater than crisis deaths, no unc added")
    matdeaths.s <- matdeaths_nocrisis.s
  }
  mmr.s <- matdeaths.s/births
  return(mmr.s)
}


#--------------------------
# The End
