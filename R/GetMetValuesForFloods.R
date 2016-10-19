#' @title Title Extract catchment average precipitation, rain, snowmelt, temperature and runoff.
#' @description The catchment averages are extracted from the SeNorge data. This dataset is available upon request.
#' Need to run the function'grid_cell_list' in order to extract the catchment id of each grid cell.
#' All results are written to files, one set of file for daily averages and one for long term mothly averages
#' @param gridid File with gridIDs of SeNorge grids for each catchment. Created by the function 'gridcell_list'
#' @param first_day The first day of the time sequence
#' @param last_day The last day of the time sequence
#' @param station_file List of stations if only a subset from the gridid-list is needed
#' @param snr_translation Translation to GIS-station numbers if necesessary
#' @param metfolder folder where SeNorge Met-grids are stored
#' @param snowfolder folder where the SeNorge snow grids are stored
#' @param hbvfolder folder where the SeNorge water balance modellin grids are stored
#' @param outfolder folder for witing the results
#'
#' @return list with tables of daily and average monthly values
#' @export
#'
#' @examples get_metdataforfloods(gridid='inst/Example_data/GISData/CID.txt',first_day=as.Date("1961/1/1"),last_day=as.Date("1961/12/31"),
#' station_file="inst/Excample_data/Flooddata/Table_stations_periods.csv",
#' metfolder="U:/metdata/",snowfolder="U:/snowsim/",hbvfolder="Z:/gwbsim/",outfolder="inst/Excample_data/Flooddata/")

get_metdataforfloods<-function(gridid='inst/Example_data/GISData/CID.txt',first_day=as.Date("1961/1/1"),last_day=as.Date("1990/12/31"),
station_file="inst/Flooddata/Table_stations_periods.csv",
metfolder='U:/metdata/',snowfolder='U:/snowsim/',hbvfolder='Z:/gwbsim/',outfolder="inst/Excample_data/Flooddata/")
{
noc=1852250    # antall celler i seNorge-grid
NoData=10000

# Hack to account for difference beween storing data for a hydrologic year (Version 1.0 and for calendar year (version 2.0 and higher))
ncc<-nchar(metfolder)
cyear=TRUE
if(substr(metfolder,(ncc-7),(ncc-1))=='metdata')cyear=FALSE
#

mdates<-seq(first_day, last_day, by="days") # periode som data skal tas ut for
# U:\ is \\hdata\grid
# Z:\ is \\hdata\grid2
FileFolder=metfolder  # her ligger nedbørs og temperaturdata
FileFolder2=snowfolder  # her ligger snødata
FileFolder3=hbvfolder # her ligger avrenningsdata
par1='tm'      # temperatur
par2='rr'      # nedbor
par3='qsw'     # Snøsmelting
par4='gwb_q'   # Avrenning

# read in the gri cell identification as a data frame
station_gridid<-read.table(gridid,header=TRUE)

#Change it to a list:
lgridid<-split(station_gridid$id, list(station_gridid$CNumber))

slist<-read.table(station_file,sep=";",header=TRUE)

rnr=as.integer(slist[,1]/100000)
hnr=slist[,1]-rnr*100000
selected_stations<-paste(rnr,'.',hnr,'.0',sep="")


stations_sel_index<-match(selected_stations,names(lgridid))


ncatchments=length(selected_stations)            #Antall stasjoner

myears=substr(mdates,1,4)
mmonths=substr(mdates,6,7)
mdays=substr(mdates,9,10)

ndays=length(mdates)

# Matriser for å lagre data
aveT<-matrix(nrow=ndays,ncol=ncatchments)
aveP<-matrix(nrow=ndays,ncol=ncatchments)
aveR<-matrix(nrow=ndays,ncol=ncatchments)
aveS<-matrix(nrow=ndays,ncol=ncatchments)
aveQ<-matrix(nrow=ndays,ncol=ncatchments)

for (i in 1:ndays){
print(ndays)
print(i)
    HyYear=myears[i]
    Year=myears[i]

    if ( as.numeric(mmonths[i]) >= 9 )          # Sjekk hydrologisk aar
      HyYear=toString(as.numeric(myears[i])+1)
     Day=mdays[i]
	  Month=mmonths[i]

    ##  tar ut temperaturdata. De er lagret på binærfil, unsigned integer 2 bit
	  MetYear=HyYear
	  if(cyear)MetYear=Year
    dataT=paste(FileFolder,par1,'/',MetYear,'/',par1,'_',Year,'_',Month,'_',Day,'.bil',sep="")
    fc <- file(dataT,"rb")
    Tgrid<-readBin(fc, what="integer", n=noc, size = 2, signed = FALSE)
    close(fc)
	Tgrid[Tgrid > 9999]<-NA

    ##  tar ut nedbørsdata	De er lagret på binærfil, unsigned integer 2 bit
    dataP=paste(FileFolder,par2,'/',MetYear,'/',par2,'_',Year,'_',Month,'_',Day,'.bil',sep="")
	fc <- file(dataP,"rb")
    Pgrid<-readBin(fc, what="integer", n=noc, size = 2, signed = FALSE)
    close(fc)


    dataS=paste(FileFolder2,par3,'/',HyYear,'/',par3,'_',Year,'_',Month,'_',Day,'.bil',sep="")
    fc <- file(dataS,"rb")
    Sgrid<-readBin(fc, what="integer", n=noc, size = 1, signed = FALSE)
    close(fc)

    dataQ=paste(FileFolder3,par4,'/',HyYear,'/',par4,'_',Year,'_',Month,'_',Day,'.bil',sep="")
    fc <- file(dataQ,"rb")
    Qgrid<-readBin(fc, what="integer", n=noc, size = 2, signed = FALSE)
    close(fc)

	Pgrid[Pgrid > 9999]<-NA
	Rgrid=Pgrid
    Rgrid[Tgrid<=2736]=0	# Nedbør for temperatur lavere enn 0.5 oC settes til null for å få regnnedbør
	Sgrid[Sgrid == 255]<-NA # NA verdi her er 255
	Sgrid[Sgrid == 254]<-0	# 254 angir at det ikke er snø i ruta
	ns=ncatchments
	Qgrid[Qgrid > 65500]<-NA

#Beregner middelverdier
    aveT[i,]<-sapply(seq(ns),function(ns){ round(mean(Tgrid[lgridid[[stations_sel_index[ns]]]+1],na.rm=TRUE),5)},simplify= "array")
    aveP[i,]<-sapply(seq(ns),function(ns){ round(mean(Pgrid[lgridid[[stations_sel_index[ns]]]+1],na.rm=TRUE),5)},simplify= "array")
    aveR[i,]<-sapply(seq(ns),function(ns){ round(mean(Rgrid[lgridid[[stations_sel_index[ns]]]+1],na.rm=TRUE),5)},simplify= "array")
    aveS[i,]<-sapply(seq(ns),function(ns){ round(mean(Sgrid[lgridid[[stations_sel_index[ns]]]+1],na.rm=TRUE),5)},simplify= "array")
    aveQ[i,]<-sapply(seq(ns),function(ns){ round(mean(Qgrid[lgridid[[stations_sel_index[ns]]]+1],na.rm=TRUE),5)},simplify= "array")
}

# Gjør om til oC og mm
aveT=aveT*0.1-273.1
aveP=aveP*0.1
aveR=aveR*0.1
aveQ=aveQ*0.1


colnames(aveT)<-selected_stations
rownames(aveT)<-as.character(mdates)

colnames(aveP)<-selected_stations
rownames(aveP)<-as.character(mdates)

colnames(aveR)<-selected_stations
rownames(aveR)<-as.character(mdates)

colnames(aveS)<-selected_stations
rownames(aveS)<-as.character(mdates)

colnames(aveQ)<-selected_stations
rownames(aveQ)<-as.character(mdates)

# Get long term monthly averages
SaveT<-aggregate(aveT, list(mmonths), mean)
SaveP<-aggregate(aveP, list(mmonths), mean)
SaveR<-aggregate(aveR, list(mmonths), mean)
SaveS<-aggregate(aveS, list(mmonths), mean)
SaveQ<-aggregate(aveQ, list(mmonths), mean)

write.table(aveQ,paste(outfolder,"aveQ.txt",sep=""))
write.table(SaveQ,paste(outfolder,"SaveQ.txt",sep=""))

write.table(aveT,paste(outfolder,"aveT.txt",sep=""))
write.table(SaveT,paste(outfolder,"SaveT.txt",sep=""))

write.table(aveP,paste(outfolder,"aveP.txt",sep=""))
write.table(SaveP,paste(outfolder,"SaveP.txt",sep=""))

write.table(aveR,paste(outfolder,"aveR.txt",sep=""))
write.table(SaveR,paste(outfolder,"SaveR.txt",sep=""))

write.table(aveS,paste(outfolder,"aveS.txt",sep=""))
write.table(SaveS,paste(outfolder,"SaveS.txt",sep=""))

out<-list(SaveT, SaveP, SaveR, SaveS, SaveQ, aveT, aveP,aveR, aveS, aveQ)
return(out)
}



