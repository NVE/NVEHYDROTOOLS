
#' Function for calculating number of values that are not NA in a vector
#'
#' @param xx The array of numbers and possible NA values
#' @return An integer with the number of values that are not NA
#' @export
#' @examples som_nona(x)
sum_nona<-function(xx){
  sum(!is.na(xx))
}

#' @title Calculate recession times for a set of stations
#' @description See 'get_recession_time' for details on how recession times are extracted
#' @param fraction quantile of the pdf of recession coefficients lamda used in calculation. A high value is recommended
#' @param stations_periods_file a file with list of stations and data periods to be used for estimationg recession
#' @param dailydata folder with daildata
#' @param outfile file for storing results
#'
#' @return a data object with recession times in days for all catchments
#' @export
#'
#' @examples extract_recessiontimes_allstations(fraction=0.995,
#' "inst/Example_data/Flooddata/Table_stations_periods.csv",
#' "inst/Dailydata", "inst/Example_data/Flooddata/recessiontimes.txt")
#'
extract_recessiontimes_allstations<-function(fraction=0.995,
  stations_periods_file="inst/Example_data/Flooddata/Table_stations_periods.csv",
  dailydata="inst/Example_data/Dailydata",
  outfile="inst/Example_data/Flooddata/recessiontimes.txt"){

  stations_ams <- read.table(stations_periods_file, sep=";",header = T)
  stations_ams<-stations_ams[!is.na(stations_ams[,1]),]
  myrec<-NA
  for(i in 1:nrow(stations_ams)){
    myrec_temp<-get_recession_time(fraction,stations_ams[i,1],dailydata, stations_ams[i,4],
                                   stations_ams[i,2],stations_ams[i,3],stations_ams[i,5])
    if(i==1|is.na(myrec)) myrec<-myrec_temp
    else if(!is.na(myrec_temp))myrec<-rbind(myrec,myrec_temp)
  }
  write.table(myrec,file=outfile,row.names=FALSE)
  return(myrec)
}

#' @title Calculate recession time for daily data
#' @description Calculates average recession times in days, based on method described in Skaugen & Onof, 2014.
#' Select a high quantile from the distribution of the observed recessions specified by 'fraction'. Then
#' estimate the recession time as the time needed to empty a linear resevoir to be 1% of its initial value
#' Skaugen, T. and Onof, C. (2014): A rainfall-runoff model parameterized from GIS and runoff data. Hydrol. Process., 28, 4529-4542.
#'
#' @param fraction Quantile of the pdf of recession coefficients lamda used in calculation. A high value is recommended
#' @param stationnumber Station number given as rrmmmmm
#' @param path_dd Path for daily data
#' @param active_station If active station is 1, ignore d_last and use data until end of data period
#' @param d_first First year for extracting recession data
#' @param d_last Last year for extracting recession data
#' @param d_exclude Years to be excluded
#'
#' @return recession time in days for the sepcified catchment
#' @export
#'
#' @examples get_recession_time(fraction=0.995,stationnumber=200011, path_dd='inst/Example_data/Dailydata',
#' active_station=0,d_first=1880,d_last=2015,d_exclude=NA)
get_recession_time<-function(fraction=0.995,stationnumber=200011, path_dd='inst/Example_data/Dailydata',active_station=0,
                      d_first=1880,d_last=2015,d_exclude=NA){

  #extract regine and main numbers for later matching
  reginenr <- floor(stationnumber/100000)
  mainnr <- stationnumber-reginenr*100000

  # list files
  myfiles_day <- list.files(path_dd)
  snumbers_day <- substr(myfiles_day,1,nchar(myfiles_day)-4)

  # find snumbers location for day and knekk data for loading discharge (vf) data in
  loc_day <- which(snumbers_day == stationnumber)
  if(length(loc_day) > 0){
    mydata <- read.table(paste(path_dd,'/',myfiles_day[loc_day], sep=""),sep=" ")
    colnames(mydata) <- c("datum","vf")
    #take negative values out
    mydata[mydata == -9999] <- NA
    mydata$date <- as.Date(mydata$datum, format = "%Y%m%d")
    mydata$year <-  as.numeric(format(as.Date(mydata$date), "%Y"))
    # extract year, start at full year

    # get the first and last year in the data file.
    last_year<-max(unique(mydata$year),na.rm=TRUE)
    first_year<-min(unique(mydata$year),na.rm=TRUE)

    if(active_station==1)d_last=last_year # if it is an active flood station,I use all data in the daily file. If not I do not extend the period forward in time
    if(last_year<d_last)d_last=last_year # I cann only use the period for which there are data!
    if(first_year>d_first)d_first=first_year # I cann only use the period for which there are data!

    # make a sequence from first to last year
    d_years<-seq(d_first,d_last,by=1)

    # Exclude some years
    excl<-as.character(d_exclude)
    excluded_years<-NA
    # Make a vector of ecluded years as specified in the input.
    if(nchar(excl)>0)excluded_years<-eval(parse(text=paste("c(",excl,")",sep="")))
    d_years<-d_years[!is.element(d_years,excluded_years)]

    # Limit the daily data to the selected years:
    mydata<-mydata[is.element(mydata$year,d_years),]

    # Check that each year has more than 363 observations
    length_OK<-(as.numeric(unlist(by(mydata$vf,mydata$year,sum_nona,simplify=TRUE)))>359)
    d_years<-d_years[length_OK]
    # Limit the subdaily data to the selected years:
    mydata<-mydata[is.element(mydata$year,d_years),]

    if (length(na.omit(mydata$vf)) > 0){
        Q = mydata$vf
        Q_d <- head(log(mydata$vf), -1) - tail(log(mydata$vf), -1)
        Q_d_positive <- Q_d[Q_d > 0]
        # calculate lambda (eq 11 in paper)
#        lamda <- data.frame(matrix(NA, length(Q_d_positive), ncol = 1))
#        colnames(lamda) = ("lamda")

        #calculate Empirical Cumulative Distribution Function
        emp_dist_l <- ecdf(Q_d_positive)

        # get the x corresponding to given y -> use linear interpolation
        inv_ecdf <- function(emp_dist_l){
          x <- environment(emp_dist_l)$x
          y <- environment(emp_dist_l)$y
          approxfun(y, x)
        }

        g <- inv_ecdf(emp_dist_l)
        value_lamda <- g(fraction)

        #calculate recessiontime t, stop when flow approximates 1% of initial flow
        # i.e. find dt so that Q(t+dt) = 0.01*Q(t)
        t = -log(0.01) / value_lamda
        t_rounded <- round(t, digits = 0)

        myout<-cbind(regine=reginenr,main=mainnr,lamda=value_lamda,recession_time=t_rounded)
    }
    else myout<-cbind(regine=reginenr,main=mainnr,lamda=NA,recession_time=NA)
  }
  else   myout<-cbind(regine=reginenr,main=mainnr,lamda=NA,recession_time=NA)
  return(myout)
}


