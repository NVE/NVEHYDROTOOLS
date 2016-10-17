#' @title Extracting flood generating process for a set of stations
#' @description Flood generating process is calculated as as the fraction of flood runoff casued by rain (0-1)
#' for a set of stations and identified flood events. time series of catchment average rain and snow melt is
#' extracted from the SeNorge model and is performed using the function 'extract_ams_allstations'. The recession times
#' are calculated using 'extract_recessiontimes_allstations'
#'  See "get_fgp" for details
#'
#' @param floodfile File with the flood data. Created by the function 'extract_ams_allstations' or 'extract_pot_allstations'
#' @param rainfile File with the rain data. Created by the function 'get_metdataforfloods'
#' @param snowfile File with the snow melt data. Created by the function 'get_metdataforfloods'
#' @param recessionfile File with recession times. Created by the function 'extract_recessiontimes_allstations'
#' @param outfile File for storing the fgp. Similar to the input flood file, but with a colomn of fgp added.
#' @param cfgp column where the fgp will be written.
#'
#' @return dataframe with floods where fgp is included. The data frame is also written to file.
#' @export
#'
#' @examples get_fgp_allstations(floodfile='inst/Example_data/Flooddata/amsvalues.txt',rainfile='inst/Example_data/klimadata/aveR.txt',
#' snowfile='inst/Example_data/klimadata/aveS.txt',recessionfile='inst/Example_data/flooddata/recessiontimes.txt',
#' outfile='inst/Example_data/Flooddata/ams_and_fgp.txt')
#'


get_fgp_allstations<-function(floodfile='inst/Example_data/Flooddata/amsvalues.txt',rainfile='inst/Example_data/klimadata/aveR.txt',
                              snowfile='inst/Example_data/klimadata/aveS.txt',
                              recessionfile='inst/Example_data/flooddata/recessiontimes.txt',
                              outfile='inst/Example_data/Flooddata/ams_and_fgp.txt'
                              cfgp=4){

  floods<-read.table(floodfile, header=TRUE,sep=";")
  raindata<-read.table(rainfile)
  snowdata<-read.table(snowfile)
  recession<-read.table(recessionfile,header=TRUE)
# Concentration time is 2 days for all catchments
  ctime=2

# Dates for the metdata are extracted from the rownames
  metdates<-as.Date(rownames(snowdata))
# Station names used in the met data files
  snames=substr(colnames(snowdata),2,nchar(colnames(snowdata)))
# Stattion names from the flood file
  snames_floods<-paste(floods[,1],'.',floods[,2],'.0',sep='')

# Make a vecttor with unique station numbers
  mystations<-unique(snames_floods)

# Loop on all stations:
  for(i in 1 : length(mystations)){
    loc_rain<-which(mystations[i]==snames)
    sfgp<-matrix(get_fgp(cfloods=floods[snames_floods==mystations[i],],crain=raindata[,loc_rain],
            csnow=snowdata[,loc_rain],cdates=metdates,ctime=2,rtime=recession[i,4]),ncol=1)
    if(i==1)myfgp=sfgp
    else myfgp=rbind(myfgp,sfgp)
  }
  nv=dim(floods)[2]
  floods_fgp<-floods[,c(c(1:cfgp),c(cfgp:nv))]
  
  colnames(floods_fgp)[cfgp]='FGP'
  floods_fgp[,cfgp]<-myfgp
  write.table(floods_fgp,file=outfile,row.names=FALSE)
  return(floods_fgp)
}

#' @title Calculates flood generating processes (fgp) for floods
#' @description Flood generating process is calculated as as the fraction of flood runoff casued by rain (0-1)
#' for a set of identified flood events.
#' For each flood events, the sum of rain and snow melt is caclulated for a time window
#' given as the sum of concentration time and recession in front of the flood event.
#' The FGP method is described closer in Vormoor et al, 2016.
#' 10.06.2016, LESC (lena schlichting)
#########################
#' Extracting flood generating process as the fraction of flood runoff casued by rain (0-1)
#' for a list of floods specified by dates for one station
#'
#' @param cfloods, Dataframe with floods alsocontaining the date of the flood event
#' @param crain Array of rain precipitation for each day for one catchmnents
#' @param csnow Array of snow melt for each day for one catchment
#' @param Dates Dates for or the metdata
#' @param Ctime Concentration time of the catchments.Is 2 days for all catchments
#' @param rtime Recession time for one station
#' @return vector with fgp for ecah of the stations.
#' @export
#'
#' @examples  sfgp<-get_fgp(cfloods=floods[snames_floods==mystations[i],],crain=raindata[,loc_rain],
#' csnow=snowdata[,loc_rain],cdates=metdates,ctime=2,rtime=recession[i,4]),ncol=1)
#'
#'


get_fgp<-function(cfloods,crain,csnow,cdates,ctime=2,rtime=6){
  flood_dates<-as.Date(cfloods[,3])
  met_end<-match(flood_dates,cdates)
  met_start<-met_end-ctime-rtime

  ext_fgp<-function(ii){
    if(is.na(met_start[ii]))frain<-NA
    else{
      train<-sum(raindata[met_start[ii]:met_end[ii],loc_rain])
      tsnow<- sum(snowdata[met_start[ii]:met_end[ii],loc_rain])
      frain<-train/(train+tsnow)
    }
  }


  if(sum(!is.na(met_start))>0){
#    met_start<-met_end - ctime - rtime
    frain<-sapply(1:length(met_end),ext_fgp,simplify=array)
  }
  else frain=rep(NA,length(flood_dates))
  return(frain)
}


