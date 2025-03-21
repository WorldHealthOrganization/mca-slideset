#---------------------
# Miscellaneous functions
# Leontine Alkema and Sanqian Zhang, 2015
#---------------------

GetStartAndEnd <- function(startsall, endsall){
  # startsall and endsall are the start and end indices for (potentially overlapping) inquiries
  # WITH LENGTH > 1!!!
  # get unique starts and for each unique start, the last year that is observed within an inquiry,
  # before another unique start date
  yearsincluded <- startsall[1]:endsall[1]
  for (i in 2:length(startsall)){
    yearsincluded <- c(yearsincluded, startsall[i]:endsall[i])
  }
  starts <- sort(unique(startsall))
  ends <- NULL
  for (i in 1:(-1+length(starts))){
    ends <- c(ends,
              seq(starts[i],starts[i+1]-1)[sum(cumprod(is.element(seq(starts[i],starts[i+1]-1),yearsincluded)))])
  }
  ends[length(starts)] <- max(yearsincluded)
  return(list(starts = starts, ends = ends))
}
#GetStartAndEnd(startsall = c(1,10), endsall = c(12,11))
#GetStartAndEnd(startsall = c(1,10), endsall = c(3,11))
#GetStartAndEnd(startsall = c(1,8,10), endsall = c(12,10,11))
#GetStartAndEnd(startsall = c(1,1,10), endsall = c(12,10,11))
#GetStartAndEnd(startsall = c(1,2,3), endsall = c(1,2,3))
#GetStartAndEnd(startsall = rev(c(1,2,3)), endsall = rev(c(1,2,3)))

#----------
MakeDirs <- function(runname){
  output.dir <- outputdir <- paste0(getwd(), "/output/", runname, "/")
  dir.create("output/", showWarnings = FALSE)
  dir.create(outputdir, showWarnings = FALSE)
  dir.create(paste0(outputdir, "/temp.JAGSobjects/"), showWarnings = FALSE)
  return(output.dir)
}

#--------------------------
logit <- function(x){
  log(x/(1-x))
}
inverselogit <- function(y) 1/(1+exp(-y))

#----
LogitMinMax <- function(delta, mindelta = 1, maxdelta = 4){
  log((delta-mindelta)/(maxdelta-delta))
}
InverseLogitMinMax <- function(logtrdelta, mindelta = 1, maxdelta = 4){
  mindelta+(maxdelta - mindelta)/(1+exp(-logtrdelta))
}

#----
GetTrend <- function(startvalues, # vector of start values
                     endvalues, # vector of end values
                     lengths, #vector of length of intervals
                     method="discrete") {
  # Computes annual rate of decline by two methods
  #    "continuous" for average instantaneous rate of decline (annualized)
  #    "discrete"   for average annual rate of decline
  # Continuous method used for 2008 estimates, discrete method for 2010 estimates
  A1 <- startvalues
  A2 <- endvalues
  t.int <- lengths
  if (!method %in% c("continuous", "discrete"))
    if (length(A1)!=length(A2) | length(A1)!=length(t.int)) { print("Error: length of A1, A2, and t.int vectors must be the same"); break }
  r <- if (method=="continuous") { - log(A2/A1) / t.int } else if (method=="discrete"  ) { 1 - (A2/A1)^(1/t.int) }
  return(r)
}
#GetTrend(startvalues, endvalues, lengths)

#------------------------
GetNumbersPerYear <- function(start, # exact start time e.g 1991.1
                              end, # exact end time, e.g. 1999.0
                              tosum.t, # whatever needs to be (partially) added, in vector where years correspond to calyear.t
                              calyear.t, # cal year thus rounded to 0, e.g. 1990, 1991 etc
                              combiNAandnonNAok = FALSE
){
  years.j <- floor(start):ceiling(end-1)
  J <- length(years.j)
  if (!combiNAandnonNAok){
    # all NA if one is missing!!!
    if (prod(is.element(years.j, calyear.t))!=1) {
      number.j = rep(NA, J)
      return(number.j)
    }
  }
  number.j <- rep(NA, J)
  if (J==1) {
    number.j[1] <- (end - start)*tosum.t[calyear.t==years.j[1]]
    return(number.j)
  }
  # J>1:
  number.j[1] <- (1-(start - floor(start)))*tosum.t[calyear.t==years.j[1]]
  number.j[J] <- (end- ceiling(end-1))*tosum.t[calyear.t==years.j[J]]
  # note: end - floor(end) doesn't work!!
  if (J>2){
    # perhaps not sorted so don't use number.j[2:(J-1)] <- tosum.t[calyear.t==years.j[2:(J-1)]]
    for (j in 2:(J-1)){
      number.j[j] <- tosum.t[calyear.t==years.j[j]]
    }
  }
  return(number.j)
}
#GetNumbersPerYear(start,end, tosum.t, calyear.t)
# GetNumbersPerYear(1990.5, 1990.7, seq(1,11), seq(1990,2000))
# GetNumbersPerYear(1990, 1991, seq(1,11), seq(1990,2000))
# GetNumbersPerYear(1990.5, 1991, seq(1,11), seq(1990,2000))
# GetNumbersPerYear(1990.5, 1992, seq(1,11), seq(1990,2000))
# GetNumbersPerYear(1990.5, 1992, seq(1,11), seq(1990,2000))
# GetNumbersPerYear(1990, 1992.5, seq(1,11), seq(1990,2000))

#-------------------------------
GetSums <- function(start, # exact start time e.g 1991.1
                    end, # exact end time, e.g. 1999.0
                    tosum.t, # whatever needs to be added, in vector where years correspond to calyear.t
                    calyear.t, # cal year thus rounded to 0, e.g. 1990, 1991 etc
                    # I should update the f/t for this argument!
                    na.rm = FALSE # when TRUE, does give NA if all inputs are NA (instead of 0)
){
  # to avoid problems, check to make sure it's not all NAs when na.rm = T (then sum is 0 but should be NA)
  if (na.rm){
    if (sum(is.element(calyear.t, floor(start):ceiling(end-1)))==0){
      return(NA)
    }
  }

  # first add up all sum.t from start year to last calendar year, floor(start):ceiling(end-1)
  # use ceiling(end-1) for last calendar year because eg end = 1991 means 1990 was last calendar year
  # then subtract partial stuff
  sums <- sum(tosum.t[is.element(calyear.t, floor(start):ceiling(end-1))], na.rm = na.rm)
  if (start > floor(start)) sums <- sums  -   (start - floor(start))*tosum.t[calyear.t==floor(start)]
  if (end > floor(end)) sums <- sums - (ceiling(end) - end)*tosum.t[calyear.t==floor(end)]
  return(sums)
}
#GetSums(start,end, tosum.t, calyear.t)
#GetSums(1990, 1991, rep(1,11), seq(1990,2000))
#GetSums(1990.5, 1991, rep(1,11), seq(1990,2000))
#GetSums(1990.5, 1990.7, rep(1,11), seq(1990,2000))

#-----------------------
# functions related to SEs

#functions used to get stoch error for PM
# does not work (and is not used) for zero entries
GetSEPM <- function(pm, env){
  se <- sqrt(pm*(1-pm)/env)
  return(se)
}
GetSElogPM <- function(pm, env){
  seforlogpm <- sqrt((1-pm)/(pm*env))
  return(seforlogpm)
}

GetSElogPMFromSEforPM <- function(se, pm){
  return(se/pm)
}

#-----------------------
Gett.i <- function(years.i, year.t ){
  gett.i <- rep(NA, length(years.i))
  for (i in 1:length(years.i)){
    gett.i[i] <- which(year.t == years.i[i])
  }
  return(gett.i)
}


#-----------------------
Getc.i <- function(iso.i, iso.c){
  getc.i <- rep(NA, length(iso.i))
  for (i in 1:length(iso.i)){
    getc.i[i] <- which(iso.c == iso.i[i])
  }
  return(getc.i)
}


#-------------------------------------------
GetMMRate <- function(mmr=NULL,matdeath=NULL,births=NULL,women=NULL){
  if(is.null(matdeath)){
    return(mmr*births/women)
  }else{
    return(matdeaths/women)
  }

}
GetLTR <- function(mmrate,t15t50,l15){
  return(mmrate*t15t50/l15)
}

#--------
GetPartialTime <- function(start, end, X){
  partialtime.x <- rep(NA, X)
  if (X ==1){
    partialtime.x <- end-start
  } else { # end is at least one calendar year after start
    partialtime.x[1] <- 1-(start - floor(start))
    partialtime.x[X] <- end - ceiling(end-1)
    ### note to self: end.j[j] - floor(end.j[j]) is not correct: e.g. 1995.0-1997.0
    if (X>2) partialtime.x[2:(X-1)] <- 1
  }
  return(partialtime.x)
}

#-------------------------------------------------------------------------------------
WriteCountryCsvs <- function(
  CIs,
  csvdir = output.dir){
  names.Li <- lapply(strsplit(names(CIs), split = "[.]"), function(l) l[1])
  for (i in 1:length(CIs)){
    WriteCsv(res = CIs[[i]],
             filename = paste0(csvdir, "/", names.Li[[i]], ".csv"))
  }
}

#-----------------------
GetPMfromME<-function(mat,env){
  pm<-mat/env
  pm
}
#-------------
# The End!
