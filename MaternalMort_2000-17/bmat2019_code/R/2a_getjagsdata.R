
#---------------------
# Get data object to use in BMat
# Leontine Alkema and Sanqian Zhang, 2015
# Updated by LA, 2019
#---------------------

GetJagsData <- function(
  runname = runname,
  # model settings
  referenceyear = 1990,
  # default settings for time series parameters:
  sqrtgamma0max = 0.025,
  max.sigma.lambda = 2,
  # for sampling and stochastic errors
  imputeSElogPM = 0.25,
  ## warning: validationinfo is not yet fully tested in this version of the code!!
  validationinfo = list(RunValidation = FALSE, ValidateByTime = FALSE, cutoffyear = 2008, setseed = 0123)
){

  output.dir <- MakeDirs(runname)
  meta <- readRDS(paste0(output.dir, "meta.rds"))
  datall <- readRDS(paste0(output.dir, "datall.rds"))

  ## warning: validationinfo is not yet fully tested in this version of the code!!
  if (validationinfo$RunValidation){
    if(validationinfo$ValidateByTime==TRUE){
        datall$validate.exclude<-datall$year >= validationinfo$cutoffyear
    } else {
      set.seed(validationinfo$setseed)
      datall$validate.exclude<-rbinom(n=nrow(datall),1,prob=0.20)
    }
  } else {
    datall$validate.exclude <- rep(FALSE, dim(datall)[1])
  }

  #-------------------------------------
  set.seed(123456)
  logbeta <- rnorm(1000000, 0, 1)

  #--------------
  jagsdata <- list()
  jagsdata$referenceyear <- referenceyear
  jagsdata$imputeSElogPM <- imputeSElogPM

  #-------------------------------------
  # 1. model par and baselist

  ## Time series parameters
  jagsdata$sqrtgamma0max <- sqrtgamma0max
  jagsdata$max.sigma.lambda <- max.sigma.lambda

  ##aids parameters
  jagsdata$caids = meta$caids
  jagsdata$uaids = meta$uaids
  jagsdata$kaids = meta$kaids

  jagsdata$year.ref<-which(meta$year.t==referenceyear)

  X.cth <- meta$X.cth
  B.ct <- meta$births.ct
  isssa.c <- ifelse(meta$mdg.c =="Sub-Saharan Africa",1,0)

  baselist <- list(
    X.cth = X.cth,
    isssa.c=isssa.c,
    a.ct  = meta$a.ct,
    E.ct = meta$deaths.ct, B.ct = B.ct,
    R = meta$R, getr.c = meta$getr.c, nyears = meta$nyears, C = meta$C,
    pi.constant = pi,
    input0.ct = matrix(0, meta$C, meta$nyears),# included for restriction R <1
    v.ct = meta$v.ct,
    Raids.ct = meta$v.ct*meta$uaids,
    muaids.ct = meta$v.ct*meta$uaids/B.ct*meta$a.ct*meta$deaths.ct
  )

  #---------------
  # specialized studies with reported envelope, index is jinq
  isjinq_all.d <- (datall$type=="inq"
               & !is.na(datall$final_pm)
               &!is.na(datall$final_env)
               &datall$modelinclude)
  isjinq.d <- isjinq_all.d & datall$completeness_inq > 0.95
  isjinq_incomplete.d <- isjinq_all.d & !(datall$completeness_inq > 0.95)

  datainq <- datall[isjinq.d,]
  datainq_incomplete <- datall[isjinq_incomplete.d,]

  J <- dim(datainq)[1]
  getc.j = Getc.i(iso.i = paste(datainq$iso), iso.c = meta$iso.c)
  gett.j = Gett.i(years.i = floor(datainq$year), year.t = meta$year.t)
  # get the reference period
  start.j <- datainq$start
  end.j <- datainq$end
  gettstart.j <- Gett.i(years.i = floor(start.j), year.t = meta$year.t)
  gettend.j <- Gett.i(years.i = ceiling(end.j-1), year.t = meta$year.t)
  X.j <- gettend.j - gettstart.j +1
  partialtime.xj <- partialwhoenv.xj <- matrix(NA, max(X.j),J)
  for (j in 1:J){
    partialtime.xj[1:X.j[j],j] <- GetPartialTime(start = start.j[j], end = end.j[j], X = X.j[j])
    partialwhoenv.xj[1:X.j[j],j] <- partialtime.xj[1:X.j[j],j]*meta$deaths.ct[getc.j[j], gettstart.j[j]:gettend.j[j]]
  }

  datinq <-  list(
    isjinq.d = isjinq.d,
    Jinq = J,
    getc.jinq = getc.j, gett.jinq = gett.j,
    gettstart.jinq = gettstart.j,
    gettend.jinq = gettend.j,
    env.jinq = ceiling(datainq$final_env),
    mat.jinq  = floor(datainq$final_pm*datainq$final_env),
    X.jinq = X.j,
    partialtime.xjinq  = partialtime.xj,
    partialwhoenv.xjinq  = partialwhoenv.xj
  )

  # repeated coding, change outcome vars only in lsit that gets's outputted
  datainq <- datainq_incomplete


  J <- dim(datainq)[1]
  getc.j = Getc.i(iso.i = paste(datainq$iso), iso.c = meta$iso.c)
  gett.j = Gett.i(years.i = floor(datainq$year), year.t = meta$year.t)
  # get the reference period
  start.j <- datainq$start
  end.j <- datainq$end
  gettstart.j <- Gett.i(years.i = floor(start.j), year.t = meta$year.t)
  gettend.j <- Gett.i(years.i = ceiling(end.j-1), year.t = meta$year.t)
  X.j <- gettend.j - gettstart.j +1
  partialtime.xj <- partialwhoenv.xj <- matrix(NA, max(X.j),J)
  for (j in 1:J){
    partialtime.xj[1:X.j[j],j] <- GetPartialTime(start = start.j[j], end = end.j[j], X = X.j[j])
    partialwhoenv.xj[1:X.j[j],j] <- partialtime.xj[1:X.j[j],j]*meta$deaths.ct[getc.j[j], gettstart.j[j]:gettend.j[j]]
  }

  # add completeness
  varfrombeta.j <- rep(NA, J)
  for (j in 1:J){
    varfrombeta.j[j] <- var(1/(datainq$completeness_inq[j] +
                                 (1-datainq$completeness_inq[j])*exp(logbeta)))
  }



  datinq2 <-  list(
    isjinq_incomplete.d = isjinq.d,
    Jinq_incomplete = J,
    getc_incomplete.jinq = getc.j,
    gett_incomplete.jinq = gett.j,
    gettstart_incomplete.jinq = gettstart.j,
    gettend_incomplete.jinq = gettend.j,
    env_incomplete.jinq = ceiling(datainq$final_env),
    mat_incomplete.jinq  = floor(datainq$final_pm*datainq$final_env),
    X_incomplete.jinq = X.j,
    partialtime_incomplete.xjinq  = partialtime.xj,
    partialwhoenv_incomplete.xjinq  = partialwhoenv.xj,
    # add completeness
    varfrombeta.jinq = varfrombeta.j
  )
  # just reset to not cause problems later
  datainq <- datall[isjinq.d,]
  datainq_incomplete <- datall[isjinq_incomplete.d,]

  #-------------------------------------
  # for VR data, excl vr-like

  isj.d<-datall$type=="vr" & datall$modelinclude
  datavr <- datall[isj.d,]

  getj.k1 <- seq(1, dim(datavr)[1])
  K1 <- length(getj.k1)
  datvrmultiplier <- list(
      getj.k1 = getj.k1,
      K1 = K1
  )

  varfrombeta <- rep(NA, dim(datavr)[1])
  for (j in 1:dim(datavr)[1]){
    varfrombeta[j] <- var(1/(datavr$rhovr[j] + (1-datavr$rhovr[j])*exp(logbeta)))
  }
  getc.j <- Getc.i(iso.i = paste(datavr$iso), iso.c = meta$iso.c)
  gett.j <- Gett.i(years.i = floor(datavr$year), year.t = meta$year.t)
  datvr <- list(
    isj.d = isj.d, ##save it for later postprocessing
    J=dim(datavr)[1],
    getc.j = getc.j,
    gett.j = gett.j,
    pmobs.j = ifelse(datavr$final_pm > 0, datavr$final_pm, 0.0001),
    logpmobs.j = log(ifelse(datavr$final_pm > 0, datavr$final_pm, 0.0001)),
    mat.j  = round(datavr$final_pm*datavr$final_env),
    env.j  = ceiling(datavr$final_env),
    var_sens.j = datavr$var_sens,
    var_spec.j = datavr$var_spec,
    cov_sesp.j = datavr$cov_sesp,
    sens.j = datavr$sens,
    spec.j = datavr$spec,
    sens_sq.j = datavr$sens_sq,
    oneminspec_sq.j = datavr$oneminspec_sq,
    rhovr.j = datavr$rhovr,
    input0forRstar.ct = matrix(0, meta$C, meta$nyears),
    input0forVR.ct = matrix(0, meta$C, meta$nyears),
    varfrombeta.j = varfrombeta
  )



  #------------------------------------------------------------------------
  # other studies, index is jnew
  isjnew.d<- isj.d==FALSE & isjinq.d==FALSE & !is.na(datall$final_pm) & datall$modelinclude
  datanonvr <- datall[isjnew.d, ]
  J <- dim(datanonvr)[1]
  getc.j = Getc.i(iso.i = paste(datanonvr$iso), iso.c = meta$iso.c)
  gett.j = Gett.i(years.i = floor(datanonvr$year), year.t = meta$year.t)
  start.j <- datanonvr$start
  end.j <- datanonvr$end
  gettstart.j <- Gett.i(years.i = floor(start.j), year.t = meta$year.t)
  gettend.j <- Gett.i(years.i = ceiling(end.j-1), year.t = meta$year.t)
  X.j <- gettend.j - gettstart.j +1
  partialtime.xj <- partialwhoenv.xj <- matrix(NA, max(X.j),J)
  for (j in 1:J){
    partialtime.xj[1:X.j[j],j] <- GetPartialTime(start = start.j[j], end = end.j[j], X = X.j[j])
    partialwhoenv.xj[1:X.j[j],j] <- partialtime.xj[1:X.j[j],j]*meta$deaths.ct[getc.j[j], gettstart.j[j]:gettend.j[j]]
  }
  aids.jnew <- rep(NA, J)
  for (j in 1:J){
    aids.jnew[j] <- baselist$v.ct[getc.j[j],gett.j[j]]*meta$a.ct[getc.j[j],gett.j[j]]
  }
  selogpm.jnew <- datanonvr$obs_selogpm
  selogpm.jnew[is.na(selogpm.jnew)] <- imputeSElogPM

  # data types are misc, with subtype dhs
  ismisc.jnew  <- datanonvr$type!="inq"
  isinq.jnew <- datanonvr$type=="inq"
  isdhs.jnew <- datanonvr$type=="dhs"

  getjnew.m <- which(ismisc.jnew)
  M <- length(getjnew.m)

  datnonvr <-  list(
    ismisc.jnew = ismisc.jnew,
    isinq.jnew = isinq.jnew,
    isdhs.jnew = isdhs.jnew,
    isjnew.d = isjnew.d,
    tausamp.jnew  = 1/selogpm.jnew^2,
    logpm.jnew = log(datanonvr$final_pm),
    isssa.jnew = isssa.c[getc.j],
    Jnew = J,
    getc.jnew = getc.j, gett.jnew = gett.j,
    ispreg.jnew = ifelse(datanonvr$definition=="pregn",1,0),
    gettstart.jnew = gettstart.j,
    gettend.jnew = gettend.j,
    X.jnew = X.j,
    partialtime.xjnew  = partialtime.xj,
    partialwhoenv.xjnew  = partialwhoenv.xj,
    aidsva.jnew = aids.jnew)

  #-------------------------------------
  if (validationinfo$RunValidation){
    getj.g = which(datavr$validate.exclude==TRUE)
    getjinq.ginq = which(datainq$validate.exclude==TRUE)
    getjnew.gnew = which(datanonvr$validate.exclude==TRUE)
  } else {
    getj.g = 1:datvr$J
    getjinq.ginq = 1:datinq$Jinq
    getjinq_incomplete.ginq = 1:datinq2$Jinq_incomplete
    getjnew.gnew = 1:datnonvr$Jnew
  }

  datvalidation=list(
    Gtrain = datvr$J-sum(datavr$validate.exclude),
    Ginqtrain = datinq$Jinq-sum(datainq$validate.exclude),
    #to change for validation
    Ginqtrain_incomplete = datinq2$Jinq_incomplete - sum(datainq_incomplete$validate.exclude),
    Gnewtrain = datnonvr$Jnew-sum(datanonvr$validate.exclude),

    getj.gtrain = which(datavr$validate.exclude==FALSE),
    getjinq.ginqtrain = which(datainq$validate.exclude==FALSE),
    getjinq_incomplete.ginqtrain = which(datainq_incomplete$validate.exclude==FALSE),
    getjnew.gnewtrain = which(datanonvr$validate.exclude==FALSE),

    G= ifelse(validationinfo$RunValidation, sum(datavr$validate.exclude), datvr$J),
    Ginq= ifelse(validationinfo$RunValidation, sum(datainq$validate.exclude), datinq$Jinq),
    Ginq_incomplete = ifelse(validationinfo$RunValidation, sum(datainq_incomplete$validate.exclude), datinq2$Jinq_incomplete),
    Gnew=ifelse(validationinfo$RunValidation, sum(datanonvr$validate.exclude),datnonvr$Jnew),

    getj.g = getj.g,
    getjinq.ginq = getjinq.ginq,
    getjinq_incomplete.ginq = getjinq_incomplete.ginq,
    getjnew.gnew = getjnew.gnew

  )

  #-------------------------------------
  jagsdata <- c(jagsdata, baselist, datvr, datinq,datinq2,
                datnonvr
                 , datvrmultiplier
                ,datvalidation
  )

  # for constraints
  jagsdata$input1.ct <- matrix(1, meta$C, meta$nyears)
  jagsdata$input1again.ct <- jagsdata$input1.ct
  #-------------------------------------

  saveRDS(jagsdata, file = paste0(output.dir, "jagsdata.rds"))
  print(paste0("Object jagsdata written to ", output.dir, "jagsdata.rds"))
  return(NULL)
} # end function

#----------------------
# The End!
