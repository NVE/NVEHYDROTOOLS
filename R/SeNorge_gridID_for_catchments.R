
#' @title Extracting SeNorge grid-ids for catchments
#' @description The SeNorge grid ids starts at 0 or 1 (depending on coding language) in upper left corner,
#' and increase line by line. The coordinates are given in UTM33N and the center of the lower left cell
#' has coordinates c(-74500, 6450500). Grid size is 1000 meters. The dimensjon of the grid is 1195,1550
#' Shape file with catchment boundaries for NVE gauging stations might be downloaded from
#' http://nedlasting.nve.no/gis/, and you should select HYDROLOGISKE DATA->Totalnedbørfelt til målestasjon
#'
#' @param c_ids The lis of catchmen ID's that is specified as "RegineNumber.MainNumber.0"
#' @param c_shape The shape file with the catchment boundaries for the gauging stations
#' @param c_layer The layer of the shape file that is to be extracted. Normally the same name as the shape-file.
#' @param llcenter Te lower left cell center of the SeNorge grid system (UTM33N)
#' @param gridsize Gridsize in meters
#' @param griddim Number of gridcells in x and y directions
#' @param myprojection Projection might be specified. If not use the proj4-string from c_shape.
#' @param outfile File where the GridCell ids for each catchment is stored
#'
#' @return a list of grid-ids, where each element of the list represent a catchment. The names of the grid elemnts are copied from c_ids"RegineNumber.MainNumber.0"
#' @export
#' @importFrom rgdal readOGR
#' @importFrom sp proj4string GridTopology SpatialGridDataFrame over
#'
#' @examples
#' grid_id_Narsjo<-gridcell_list("2.11.0",
#' "//nve/fil/h/HM/Interne Prosjekter/Flomkart/Data/GISData/Hydrologi_TotalNedborfeltMalestasjon.shp",
#' c_layer="Hydrologi_TotalNedborfeltMalestasjon",
#' snr_translation="inst/Example_data/CatchmentCharacteristics/Feltnr_flomkart_til_feltnr_GIS.txt"
#' outfile='inst/Example_data/GISData/CID.txt')
#'
#' grid_id_all_catchments<-gridcell_list(NA,
#' "//nve/fil/h/HM/Interne Prosjekter/Flomkart/Data/GISData/Hydrologi_TotalNedborfeltMalestasjon.shp",
#' c_layer="Hydrologi_TotalNedborfeltMalestasjon",
#' snr_translation="inst/Excample_data/CatchmentCharacteristics/Feltnr_flomkart_til_feltnr_GIS.txt"
#' outfile='inst/Example_data/GISData/CID.txt')

gridcell_list<-function(c_ids=c("2.11.0","12.13.0"),c_shape="Hydrologi_TotalNedborfeltMalestasjon.shp",c_layer=NA,
snr_translation="inst/Example_data/CatchmentCharacteristics/Feltnr_flomkart_til_feltnr_GIS.txt",
 llcenter=c(-74500, 6450500), gridsize=c(1000,1000), griddim=c(1195,1550), myprojection=NA,outfile=
   'inst/Example_data/GISData/CID.txt'){

  if (!require('rgdal')) {
    stop('The package rgdal was not installed')
  }
  if (!require('rgeos')) {
    stop('The package rgeos was not installed')
  }

if(is.na(c_layer))
c_layer=strsplit(c_shape,'.',fixed=TRUE)[[1]][1]
all_catchments <- readOGR(c_shape,c_layer)


# Translate from GIS station numbers to flood map stationnumbers
snumber_T<-read.table(snr_translation,sep=";")
rnr=as.integer(snumber_T[,1]/100000)
hnr=snumber_T[,1]-rnr*100000
snumber_FK<-paste(rnr,'.',hnr,'.0',sep="")
rnr=as.integer(snumber_T[,2]/100000)
hnr=snumber_T[,2]-rnr*100000
snumber_GIS<-paste(rnr,'.',hnr,'.0',sep="")


# Select the catchments
if(is.na(c_ids))selected_catchments<-all_catchments
else {
  selected_stations_GIS<-c_ids
  smat<-match(snumber_FK,c_ids)
  selected_stations_GIS[na.omit(smat)]<-snumber_GIS[which(!is.na(smat))]
  selected_catchments<-all_catchments[which(all_catchments$stID%in%selected_stations_GIS),]
}
if(length(selected_catchments)==0) stop("No catchments selected. c_ids should be given as regine_number.main_number.0")

# define the projection:
if(is.na(myprojection))
myprojection=proj4string(all_catchments)

# First define coordinates and dimensions for the grid
grd <- GridTopology(cellcentre.offset=llcenter, cellsize=gridsize, cells.dim=griddim)

# then create the grid
senorge_grd <- SpatialGridDataFrame(grd,
                               data=data.frame(id=0:(prod(griddim)-1)),
                               proj4string=CRS(myprojection))


#Do the overlay
out<-over(selected_catchments,senorge_grd,returnList=TRUE)
names(out)<-selected_catchments$stID
# Change names to the station numbers used in the flood map project
om<-match(snumber_GIS,names(out))
names(out)[na.omit(om)]<-snumber_FK[which(!is.na(om))]
# Change the output to a dataframe
out.df = as.data.frame(do.call(rbind, out))
c_temp<-matrix(unlist(strsplit(rownames(out.df),'.',fixed=TRUE)),ncol=4,byrow=TRUE)
out.df$CNumber=paste(c_temp[,1],'.',c_temp[,2],'.',c_temp[,3],sep='')
out.df<-out.df[,c(2,1)]
write.table(out.df,row.names=FALSE,file=outfile)
return(out.df)
}

