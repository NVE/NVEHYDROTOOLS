
#' Title
#'
#' @param c_ids
#' @param c_shape
#' @param c_layer
#' @param llcenter
#' @param gridsize
#' @param griddim
#' @param myprojection
#'
#' @return
#' @export
#'
#' @examples
gridcell_list<-function(c_ids=c("2.11.0","12.13.0"),c_shape="Hydrologi_TotalNedborfeltMalestasjon.shp",c_layer=NA,
 llcenter=c(-74500, 6450500), gridsize=c(1000,1000), griddim=c(1195,1550), myprojection=NA){

  if (!require('rgdal')) {
    stop('The package rgdal was not installed')
  }
  if (!require('rgeos')) {
    stop('The package rgeos was not installed')
  }

if(is.na(c_layer))
c_layer=strsplit(c_shape,'.',fixed=TRUE)[[1]][1]
all_catchments <- rgdal::readOGR(c_shape,c_layer)

# Select the catchments
if(is.na(c_ids))selected_catchments<-all_catchments
else selected_catchments<-all_catchments[which(all_catchments$stID%in%c_ids),]
if(length(selected_catchments)==0) stop("No catchments selected. c_ids should be given as regine_number.main_number.0")

# define the projection:
if(is.na(myprojection))
myprojection=sp::proj4string(all_catchments)

# First define coordinates and dimensions for the grid
grd <- sp::GridTopology(cellcentre.offset=llcenter, cellsize=gridsize, cells.dim=griddim)

# then create the grid
senorge_grd <- sp::SpatialGridDataFrame(grd,
                               data=data.frame(id=0:(prod(griddim)-1)),
                               proj4string=CRS(myprojection))


#Do the overlay
out<-sp::over(selected_catchments,senorge_grd,returnList=TRUE)
names(out)<-selected_catchments$stID
return(out)
}

