## Script calculates peak over threshold floods (POT) based on chosen percentile (set value in line 20)
# based on AMS-endelig datasett
# and calculates FGPs (flood generating processes) for POT floods using concentration time = 2 days 
# and recession times calculated based on Skaugen&Onof 2014. The FGP method is described closer in Vormoor et al, 2016.
# script for recession times is found here: \\nve\fil\h\HM\Interne Prosjekter\Flomkart\Datakvalitet\Endelig_liste_mai_2016\scripts\calculate_recessiontimes.R
# 10.06.2016, LESC (lena schlichting)
#########################

### choose threshold value for POT (eg 99-percentile: => 0.99)

#p_threshold = 0.98
#amsfile='../Flomdata/amsvalues.txt'
#path_dd='../Dogndata'

#TTR_3x <- 6     
#pratio= 2./3.
#' Extracting POT-values for a list of stations
#' Should first extract AMS values
#' @param amsfile file with annual maximum data. Only years with ams will be used
#' @param dailydata  folder with daily data
#' @param p_threshold the thershold for selecting flood values given as emprical quantile of all data
#' @param TTR_3X The minimum time between two independent flood peaks
#' @param pratio The minimum flow between two flood peaks should be less than pratio times the first flood peak.
#' @param outfile The file for storing ams values
#'
#' @return
#' @export
#'
#' @examples extract_pot_allstations(amsfile='inst/Example_data/Flooddata/amsvalues.txt',
#' dailydata="inst/Example_data/Dailydata",
#' p_threshold = 0.98, TTR_3x = 6,pratio= 2.0/3.0, 
#' outfile="inst/Example_data/Flooddata/potvalues.txt")
#' 
extract_pot_allstations(amsfile='inst/Example_data/Flooddata/amsvalues.txt',
                        dailydata="inst/Example_data/Dailydata",
                        p_threshold = 0.98, TSEP = 6,pratio= 2.0/3.0, 
                        outfile="inst/Example_data/Flooddata/potvalues.txt")  

extract_pot_allstations<-function(amsfile='inst/Example_data/Flooddata/amsvalues.txt',
                                  dailydata="inst/Example_data/Dailydata",
                                  p_threshold = 0.98, TSEP = 6,pratio= 2.0/3.0, 
                                  outfile="inst/Example_data/Flooddata/potvalues.txt"){

  
  
  floods<-read.table(amsfile, header=TRUE,sep=";")
  stationnumbers=floods$regine*100000+floods$main
  
  sn_unique<-unique(floods$regine*100000+floods$main)
  mypot<-NA
  for(i in 1:length(sn_unique)){
# First extract years from the AMS file    
    f_years<-unique(as.numeric(format(as.Date(floods$daily_ams_dates[stationnumbers==sn_unique[i]]), "%Y")))
# Then get the POT floods.
    
    mypot_temp<-get_pot(snumb=sn_unique[i],f_years=f_years,path_dd=dailydata,
                        p_threshold=p_threshold,TSEP=TSEP,pratio=pratio)    

    if(i==1|is.na(mypot)) mypot<-mypot_temp
    else if(!is.na(mypot_temp))mypot<-rbind(mypot,mypot_temp)
  }
  write.table(mypot,file=outfile,row.names=FALSE)
  return(mypot)
}


get_pot<-function(snumb=200011,f_years=NA,path_dd="inst/Example_data/Dailydata",
                  p_threshold=0.98, TSEP = 6,pratio= 2./3.){
  
  reginenr=as.integer(snumb/100000)
  mainnr=snumb-reginenr*100000
  myfiles_day <- list.files(path_dd)
  snumbers_day <- as.integer(substr(myfiles_day,1,nchar(myfiles_day)-4))  
  loc_day <- which(snumb==snumbers_day)
  if(length(loc_day) ==0)return(NA)
  #load in daily series
  else{
    dailydat <- read.table(paste(path_dd,'/',myfiles_day[loc_day], sep=""),sep=" ")
    colnames(dailydat) <- c("orig_date", "vf")
    dailydat$date <- as.Date(dailydat$orig_date, format = "%Y%m%d")
    dailydat$year <-  as.numeric(format(as.Date(dailydat$date), "%Y"))
    #set -9999 as NA
    dailydat[dailydat == -9999] <- NA
    daily_years <- as.numeric(na.omit(unique(dailydat$year)))

    dailydat<-dailydat[is.element(dailydat$year,f_years),]  

# Get the quantile used as threshold    
    qt<-quantile(as.numeric(dailydat[,2]),p_threshold)

# find alle data abpove the thershold
    above<-dailydat[,2]>qt

# find times when the threshold is crossed    
    intersect.points<-which(diff(above)!=0)
#    intersect.points[which(above[intersect.points]==FALSE)]

# find up and dwon crossings over the thershold    
    up.cross<-intersect.points[!above[intersect.points]]+1
    down.cross<-intersect.points[above[intersect.points]]
  
  
# Internal function for extracting flood peaks. To be used with sapply  
    get_floodpeaks<-function(ii){
      fevent<-dailydat[up.cross[ii]:down.cross[ii],]
      if(any(is.na(fevent[,2]))) {
        pflood=NA
        pflood_date=NA
      }
      else {
        pflood=max(fevent[,2])
        pflood_date=fevent[which(pflood==fevent[,2]),3][1]
      }
      return(list(pflood,pflood_date))
    }    

# Extract flood peaks as the maximums of a bunch of data above the threshold  
    flood_peaks<-sapply(c(1:length(up.cross)),get_floodpeaks,simplify=TRUE)
    flood_dates<-as.Date(unlist(flood_peaks[2,]),origin = "1970-01-01")
    flood_peaks<-as.numeric(unlist(flood_peaks[1,]))

# Select independent floods that are separated by at least 6 days: 
# first calculate the time lags between successive floods
    gapLengths = c(1000, diff(flood_dates))
# The group events that are separated by less than TRR_3x days to the same cluster    
    clusterNumbers = cumsum(gapLengths > TSEP)

# internal function to extract maximum values and dates    
    get_cmax<-function(xx){
      return(as.character(xx[which.max(xx[,2]),])  )
    }

# Get the maximum values for each cluster    
    floods_indep<-by(data=cbind(flood_dates,flood_peaks),INDICES=clusterNumbers,FUN = get_cmax)
    floods_matrix<-matrix(as.numeric(unlist(floods_indep)),ncol=2,byrow=TRUE)  
  
  
#cluster daily data between flood peaks
    bclusters<-bclusters<-cumsum(as.numeric(dailydat[,3])%in%floods_matrix[,1])
  
#Find the minimum in each cluster between two successive flood peaks
    minbetween<-as.numeric(by(dailydat[,2],bclusters,min,na.rm=TRUE))
    minbetween<-minbetween[2:length(minbetween)]

#Find the new clusters so that two floods where the minimum streamflow between 
#them is higher than 2/3 of the firts peak, belongs to the same event
    nb=length(minbetween)-1
    fclusters<-cumsum(c(1,floods_matrix[1:nb,2])*pratio>c(0,minbetween[1:nb]))
# Find the maximum of these new clusters
    floods_indep_2<-by(data=floods_matrix,INDICES=fclusters,FUN = get_cmax)

# Wrap it into a matrix that is returned
    floods_matrix_2<-matrix(as.numeric(unlist(floods_indep_2)),ncol=2,byrow=TRUE)  
return(data.frame(regine=reginenr,main=mainnr,date=as.Date(floods_matrix_2[,1],origin = "1970-01-01"),
             flood=as.numeric(floods_matrix_2[,2]),threshold=qt))
}
}

  