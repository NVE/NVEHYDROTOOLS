#' @title Extract catchment characteristics for a set of stations
#' @description Extract catchment characteristics for a set of stations 
#' form a file with catchment caharcteristics from almost all sations
#' @param cp_all_stations File with the catchment characteristics for all stations  
#' @param floodstations File listing the stations used for flood analysis
#' @param cp_floodstations File for writing the catchment characteristivs for the flood stations only
#'
#' @return a list with the stations missing catchmnt characteristocs and the catchment characteristics for the remaining station
#' @examples create_CatchmentParameters(cp_all_stations='inst/Complete_data/CatchmentCharacteristics/Hydra_FeltparTabell.csv',
#' floodstations= 'inst/Complete_data/Flooddata/Table_stations_periods.csv',
#' cp_floodstations='inst/Complete_data/CatchmentCharacteristics/cp_floodstations.txt')

create_CatchmentParameters<-function(cp_all_stations='inst/Complete_data/CatchmentCharacteristics/Hydra_FeltparTabell.csv',
                                     floodstations= 'inst/Complete_data/Flooddata/Table_stations_periods.csv',
                                     cp_floodstations='inst/Complete_data/CatchmentCharacteristics/cp_floodstations.txt')
{
all_catchment_data<-read.table(cp_all_stations,sep=";",header=TRUE)
slist<-read.table(floodstations,sep=";",header=TRUE)

# To check i have all the catchments:

missing_stations<-slist[is.na(match(slist[,1],all_catchment_data[,1]/1000)),1]
cp_flood<-all_catchment_data[!is.na(match(all_catchment_data[,1]/1000,slist[,1])),]
write.table(cp_flood,file=cp_floodstations,sep=";",row.names=FALSE)
out<-list()
out$missing<-missing_stations
out$cp<-cp_flood
return(out)
}


                        
