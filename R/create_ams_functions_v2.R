extract_ams_allstations<-function(stations_periods_file="../Data/Flooddata/Table_stations_periods.csv",dailydata="../Data/Dailydata",
                                  subdailydata="../Data/Subdaily",outfile="../Data/Flooddata/amsvalues.txt"){
  stations_ams <- read.table(stations_periods_file, sep=";",header = T)  
  stations_ams<-stations_ams[!is.na(stations_ams[,1]),]
  myams<-NA
  for(i in 1:nrow(stations_ams)){
    myams_temp<-get_amsdata(stations_ams[i,1],dailydata,subdailydata, stations_ams[i,4],
                            stations_ams[i,2],stations_ams[i,3],stations_ams[i,5],stations_ams[i,6],
                            stations_ams[i,7],stations_ams[i,8])
    if(i==1|is.na(myams)) myams<-myams_temp
    else if(!is.na(myams_temp))myams<-rbind(myams,myams_temp) 
  }
  write.table(myams[,c(1,2,4,5,8,7)],file=outfile,row.names=FALSE)
  return(myams[,c(1,2,4,5,8,7)])
}


get_subdaily_period<-function(s_dat){
  ns<-length(s_dat$date)
  tdiff<-as.numeric(s_dat$date[2:ns])-as.numeric(s_dat$date[1:ns-1])
  
  #Find which datapoints are separated by less than 23 hours and 59 minutes
  tsf<-which(tdiff<86300) 
  
  # I need at least 4 such datapoints in a time series in order to be considered as subdaily data
  if(is.na(tsf[1])|length(tsf)<4) out<-NA
  else{
    ll=length(tsf)
    # maybe this test is not necessary, but sometimes there are two data points in a row 
    # that are separated by less than 1 day whereas the rest of the data are actually at a daily resolution
    # Here it is tested that datapoint n and n+2 should be separated by less than two days.
    i2<-which(tsf[3:ll]-tsf[1:(ll-2)]==2)
    if(is.na(i2[1]))out<-NA
    else out<-s_dat[tsf[i2[1]]:ns,]
  }
  return(out) 
}


# Count the number of observations that are no NA
sum_nona<-function(xx){
  sum(!is.na(xx))
}  

get_amsdata<-function(stationnumber=200011, path_dd='../Dogndata',path_sd='../Findata',active_station=0,
                      d_first=1880,d_last=2015,d_exclude=NA,s_first=1880,s_last=2015,s_exclude=NA){

## loop for each station, find data to fill in, add row with new data results to correct place
  
  #extract regine and main numbers for later matching
  reginenr <- floor(stationnumber/100000)
  mainnr <- stationnumber-reginenr*100000

  #load discharge-data folder for daily data and extract station numbers
  myfiles_day <- list.files(path_dd)
  snumbers_day <- substr(myfiles_day,1,nchar(myfiles_day)-4)  
  
  #load discharge-data folder for subdaily data and extract station numbers
  myfiles_knekk <- list.files(path_sd)
  snumbers_knekk <- substr(myfiles_knekk,1,nchar(myfiles_knekk)-4)
  
  # find snumbers location for day and knekk data for loading discharge (vf) data in
  loc_day <- which(snumbers_day == stationnumber)
  loc_knekk <- which(snumbers_knekk == stationnumber)
  if(length(loc_day) ==0)return(NA)  
  #load in daily series    
  if(length(loc_day) > 0){
    dailydat <- read.table(paste(path_dd,'/',myfiles_day[loc_day], sep=""),sep=" ")
    colnames(dailydat) <- c("orig_date", "vf")
    dailydat$date <- as.Date(dailydat$orig_date, format = "%Y%m%d")  
    dailydat$year <-  as.numeric(format(as.Date(dailydat$date), "%Y")) 
    #set -9999 as NA
    dailydat[dailydat == -9999] <- NA
    daily_years <- as.numeric(na.omit(unique(dailydat$year)))
    #set ams_years as daily_years, if both daily and knekk exist, fix below to both
    ams_years <- daily_years
  }
  
  #load in subdaily series
  if(!(length(loc_knekk) == 0 | is.na(s_first))){
    knekkdat <- read.table(paste(path_sd,'/',myfiles_knekk[loc_knekk], sep=""),sep=" ")
    colnames(knekkdat) <- c("orig_date", "vf")
    knekkdat$date <- as.POSIXct(knekkdat[,1], format = "%Y%m%d/ %H%M")
    knekkdat$date_nohour <- as.Date(knekkdat$date)
    knekkdat$year <-  as.numeric(format(as.Date(knekkdat$date), "%Y")) 
    #set -9999 as NA
    ns<-length(knekkdat$date)
    
    # Get only period with subdaily data  
    knekkdat<-get_subdaily_period(knekkdat)
    
    if(is.na(knekkdat))s_first<-NA  
    else{
      if(min(na.omit(knekkdat$year))>s_last) s_first<-NA  
      knekkdat[knekkdat == -9999] <- NA
      knekk_years <- as.numeric(na.omit(unique(knekkdat$year)))
    }
  }
    
  #get all years to fill in data if both knekk and daily series exist
#    if(length(daily_years) > 2 & length(knekk_years) > 2 ){
#      ams_years <- as.numeric(na.omit(unique(ams_updat$year[which(ams_updat$regine_area == reginenr & ams_updat$main_nr == mainnr)])))
#    }

# Decide which years to use for extracting a AMS timeseries.
#  d_first_year=d_first
#  d_last_year=stations_ams[i,3]
  
# get the first and last year in the data file.
  last_year<-max(unique(dailydat$year),na.rm=TRUE)
  first_year<-min(unique(dailydat$year),na.rm=TRUE)
                                
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
  dailydat<-dailydat[is.element(dailydat$year,d_years),]


  # Check that each year has more than 363 observations
  length_OK<-(as.numeric(unlist(by(dailydat[,2],dailydat[,4],sum_nona,simplify=TRUE)))>363)
  d_years<-d_years[length_OK]
  # Limit the subdaily data to the selected years:
  dailydat<-dailydat[is.element(dailydat$year,d_years),]  
  
  
  # Extract the annual maxima for daily values:
  daily_ams<-as.numeric(unlist(by(dailydat[,2],dailydat[,4],max,na.rm=TRUE)))
  
  get_max_date<-function(myind){
    dat_temp<-dailydat[dailydat$year==d_years[myind],]
    mdate<-dat_temp[head(which(dat_temp[,2]==daily_ams[myind]),1),3]
    mdate
  } 
  nmax<-length(daily_ams)
  
  # Extract the dates for the daily maximums
  daily_ams_dates<-as.Date(sapply(c(1:nmax),get_max_date),origin="1970-01-01")
  
#####################################################################################################  
  
  # Decide which years to use for extracting a AMS timeseries for subdaily data:
  if(!(length(loc_knekk) == 0 | is.na(s_first))){
  
    # use the last year in the data file.
    last_year<-max(unique(knekkdat$year),na.rm=TRUE)
    first_year<-min(unique(knekkdat$year),na.rm=TRUE)
  
    if(active_station==1)s_last=last_year # if it is an active flood station, use the last years in the datafile
    if(last_year<s_last)s_last=last_year
    if(first_year>s_first)s_first=first_year
  
    # make a sequence from first to last year  
    f_years<-seq(s_first,s_last,by=1)
 
    # Exclude the specified years.
    excl<-as.character(s_exclude)
    excluded_years<-NA
    if(nchar(excl)>0)excluded_years<-eval(parse(text=paste("c(",excl,")",sep=""))) 
    f_years<-f_years[!is.element(f_years,excluded_years)]
   
    # Use only years where daily maximums are extracted.(accounts for missing values in the daily data)
    f_years<-f_years[!is.na(match(f_years,d_years))]
    if(length(f_years)==0)s_first<-NA
    else{  
      # Limit the subdaily data to the selected years:
      knekkdat<-knekkdat[is.element(knekkdat$year,f_years),]  
      # limit to years with subdaily data!
      f_years<-f_years[is.element(f_years,unique(knekkdat[,5]))]  
      if(length(f_years)==0)s_first<-NA
      else{
        # Count the number of observations each year
        # Check that each year has more than 360 observations
        length_OK<-(as.numeric(unlist(by(knekkdat[,2],knekkdat[,5],sum_nona,simplify=TRUE)))>100)
        f_years<-f_years[length_OK]
        if(length(f_years)==0)s_first<-NA
        else{
          knekkdat<-knekkdat[is.element(knekkdat$year,f_years),]  
          # Extract the annual maxima for subdaily values:
          fine_ams<-as.numeric(unlist(by(knekkdat[,2],knekkdat[,5],max,na.rm=TRUE)))
        
          get_max_date_f<-function(myind){
            dat_temp<-knekkdat[knekkdat$year==f_years[myind],]
            mdate<-dat_temp[head(which(dat_temp[,2]==fine_ams[myind]),1),3]
          mdate
          }
        
          nmax<-length(fine_ams)
          fine_ams_times<- as.POSIXct(sapply(c(1:nmax),get_max_date_f),origin="1970-01-01 00:00:00 CEST")
          fine_ams_dates<- as.Date(fine_ams_times)
        }
      }
    }
  }
  
  
#######################################################################################################################
# Now it is time to chech if the fine resolution and daily resolution floods comes from the same event.  
# assume that maximum data for the subdaily data is a subset of the maximums for the daily data. (no subdaily maximum for a year
#  where we have no daily maximum)
  ddd<-data.frame(d_years,daily_ams_dates,daily_ams)

  if((length(loc_knekk) == 0 | is.na(s_first))){
    fff<-data.frame(d_years,fine_ams_dates=NA,fine_ams=NA,fine_ams_times=NA)  
    mytotal<-merge(ddd,fff,by=1,all.x=TRUE) 
  }
  else{
    fff<-data.frame(f_years,fine_ams_dates,fine_ams,fine_ams_times) 
    # merge the daily and subdaily data  
    mytotal<-merge(ddd,fff,by=1,all.x=TRUE)
    # The subdaily ams values should be larger than teh daily ams values  
    mytotal[which(mytotal$fine_ams<mytotal$daily_ams),c(4,5,6)]<-NA  

    # find years when the daily and subdaily events are not from the same event  
    tdiff<-which(abs(mytotal$daily_ams_dates-mytotal$fine_ams_dates)>1)
    if(length(tdiff)>0){
      for(i1 in 1 : length(tdiff)){
        mytotal<-insertRow(mytotal,tdiff[i1],dailydat,knekkdat)
        if(i1<length(tdiff))tdiff[i1+1:length(tdiff)]=tdiff[i1+1:length(tdiff)]+1
      }
    } 
  }
  mytotal<-cbind(regine=reginenr,main=mainnr,mytotal)
 return(mytotal) 
  
}


  
  
insertRow <- function(existingDF,r,dailydat,knekkdat) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- existingDF[r+1,]
  
  update_daily=r
  update_subdaily=r
  if(existingDF$daily_ams_dates[r]<existingDF$fine_ams_dates[r])update_daily=r+1
  else update_subdaily=r+1   
     
  # update the sub_daily values for the day with the annual maximum for daily values
  jj=which(knekkdat[,4]>=existingDF$daily_ams_dates[update_subdaily]-1 & knekkdat[,4]<=existingDF$daily_ams_dates[update_subdaily]+1)
  f_temp<-knekkdat[jj,]
  existingDF$fine_ams[update_subdaily]<-max(f_temp[,2],na.rm=TRUE)
  if(existingDF$fine_ams[update_subdaily]<existingDF$daily_ams[update_subdaily]){
    existingDF[update_subdaily,c(4,5,6)]<-NA  
  }
  else {
    existingDF$fine_ams_times[update_subdaily]<-f_temp[head(which(existingDF$fine_ams[update_subdaily]==f_temp[,2]),1),3] 
    existingDF$fine_ams_dates[update_subdaily]<-as.Date(existingDF$fine_ams_times[update_subdaily])
  }  
     
  #update the daily values
  jj=which(dailydat[,3]>=existingDF$fine_ams_dates[update_daily]-1 & dailydat[,3]<=existingDF$fine_ams_dates[update_daily]+1)
  f_temp<-dailydat[jj,]       
  existingDF$daily_ams[update_daily]<-max(f_temp[,2],na.rm=TRUE)       
  existingDF$daily_ams_dates[update_daily]<-f_temp[head(which(existingDF$daily_ams[update_daily]==f_temp[,2]),1),3]                  
  existingDF
}
