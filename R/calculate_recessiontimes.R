## This script calculates average recession times in days, based on method described in Skaugen & Onof, 2014.
# Skaugen, T. and Onof, C. (2014): A rainfall-runoff model parameterized from GIS and runoff data. Hydrol. Process., 28, 4529-4542.
#(c) Lena Schlichting, lesc@nve.no, 2015


#load required libraries
#library(compare)

#select lamda for fraction (worked well between 0.9 - 0.99)
#fraction <- 0.995
#stations_ams <- read.table("../Flomdata/Table_stations_periods.csv", sep=";",header = T)
#stations_ams<-stations_ams[!is.na(stations_ams[,1]),]

#add snumber into unique_list

#setwd('//nve/fil/h/HM/Interne Prosjekter/Flomkart/Data/Ny_Data')
#load in discharge data
# Count the number of observations that are no NA
#' Function for calculating number of values that are not NA in a vector
#'
#' @param xx
#'
#' @return integer
#' @export
#'
#' @examples
sum_nona<-function(xx){
  sum(!is.na(xx))
}

#' Wrapping the 'get_recession_time' function to get recession times for a list of stations.
#' Calculates average recession times in days, based on method described in Skaugen & Onof, 2014.
#' Skaugen, T. and Onof, C. (2014): A rainfall-runoff model parameterized from GIS and runoff data. Hydrol. Process., 28, 4529-4542.
#' @param fraction quantile of the pdf of recession coefficients lamda used in calculation. A high value is recommended
#' @param stations_periods_file a file with list of stations and data periods to be used for estimationg recession
#' @param dailydata folder with daildata
#' @param outfile file for storing results
#'
#' @return a data object with recession times in days for all catchments
#' @export
#'
#' @examples extract_recessiontimes_allstations(fraction=0.995,
#' "../inst/Example_data/Flooddata/Table_stations_periods.csv",
#' "../inst/Dailydata", "../inst/Example_data/Flooddata/recessiontimes.txt")
extract_recessiontimes_allstations<-function(fraction=0.995,
  stations_periods_file="../Data/Flooddata/Table_stations_periods.csv",
  dailydata="../Data/Dailydata",
  outfile="../Data/Flooddata/recessiontimes.txt"){
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



#' Title
#'
#' @param fraction quantile of the pdf of recession coefficients lamda used in calculation. A high value is recommended
#' @param stationnumber station number given as
#' @param path_dd path for daily data
#' @param active_station if active station is 1, ignore d_last and use data until end of data period
#' @param d_first first year for extracting recession data
#' @param d_last last year for extracting recession data
#' @param d_exclude years to be excluded
#'
#' @return recession time in days for the sepcified catchment
#' @export
#'
#' @examples
get_recession_time<-function(fraction=0.995,stationnumber=200011, path_dd='../Dogndata',active_station=0,
                      d_first=1880,d_last=2015,d_exclude){

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


