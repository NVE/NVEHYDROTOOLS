# NVEHYDROTOOLS

R package for processing flood data at NVE.This includes to 

(1) Extract annual maximum floods from daily and subdaily streamflow obsrvations

(2) Extract catchment averages for daily temperatur, rain, snowmelt and runoff based on SeNorge model results

(3) Estimate a recession time for catchments based on daily streamflow observations. 

(4) Estimate the relative contribution of rain precipitation for individual floods based on the SeNorge rain and snow melt data. 

(5) Extract independent flood peaks over threshold using a percentile of the empirical 
distribution of daily flows a a threhold and independence criterions based on the time between successive peaks
and the minimum flow between peaks.

## Installation

For windows and RStudio, install the following packages:

```R
install.packages("devtools")
install.packages("curl")
```

Install the package with the following code:

```R
library(devtools)
install_github("fbaffie/NVEHYDROTOOLS")
```
The installation comes with an example dataset in the directory 'inst'. The full dataset is 
located in //nve/fil/h/HM/Interne Prosjekter/Flomkart/Data
or available by request.
## Example for extracting annual maximum data for a set of stations

The following code returns annual maximums for selected stations, bouth daily and subdaily. 
Results are written to a specified file where each line is a flood event. If the annual maximum
daily and subdaily floods are from the same event, one flood value is given per year, in the other case
two flood events is given per year.

The ams data are written to file.
```R
library(NVEHYDROTOOLS)
amsdata<-extract_ams_allstations(stations_periods_file="inst/Example_data/Flooddata/Table_stations_periods.csv",
dailydata="inst/Example_data/Dailydata", subdailydata="inst/Example_data/Subdaily",
outfile="inst/Example_data/Flooddata/amsvalues.txt")
```



## Example for extracting POT floods from a set of stations
The ams-file is used to select for which stations and years the POT will be extracted. POT is extracted for daily streamflow only.

The pot data are written to file.
```R
amfile='inst/Example_data/Flooddata/amsvalues.txt'
dfolder="inst/Example_data/Dailydata"
outfile="inst/Example_data/Flooddata/potvalues.txt"
potdata<-extract_pot_allstations(amsfile=amfile, dailydata=,dfolder,p_threshold = 0.98, TSEP = 6,pratio= 2.0/3.0, outfile=outfile)
```

## Example for extracting SeNorge grid IDs for all stations
Shape file with catchment boundaries for NVE gauging stations might be downloaded from
http://nedlasting.nve.no/gis/, and you should select the GIS dataset HYDROLOGISKE DATA->Totalnedbørfelt til målestasjon

The GridIDs are written to file

```R
shapef <- '//nve/fil/h/HM/Interne Prosjekter/Flomkart/Data/GISData/Hydrologi_TotalNedborfeltMalestasjon.shp'
slayer<- 'Hydrologi_TotalNedborfeltMalestasjon'
snr_t="inst/Example_data/CatchmentCharacteristics/Feltnr_flomkart_til_feltnr_GIS.txt"
outfile<-'inst/Example_data/GISData/CID.txt'
grid_id_example_catchments<-gridcell_list(NA,c_shape=shapef,snr_translation=snr_t,c_layer=slayer,outfile=outfile)
```

## Example for extracting SeNorge grid IDs for the example stations

```R
example_stations<-c("1.37.0","2.1.0","2.11.0","2.13.0","2.15.0","2.21.0","2.25.0","2.28.0","2.32.0")
shapef <- '//nve/fil/h/HM/Interne Prosjekter/Flomkart/Data/GISData/Hydrologi_TotalNedborfeltMalestasjon.shp'
slayer<- 'Hydrologi_TotalNedborfeltMalestasjon'
snr_t="inst/Example_data/CatchmentCharacteristics/Feltnr_flomkart_til_feltnr_GIS.txt"
outfile<-'inst/Example_data/GISData/CID.txt'
grid_id_example_catchments<-gridcell_list(c_ids=example_stations,c_shape=shapef,snr_translation=snr_t,c_layer=slayer,outfile=outfile)
```


## Example for downloading catchment averaged SeNorge data for a set of stations
This requires access to the complete SeNorge model data. Avaliable on request.
The outputs are written to the files aveP.txt, aveQ.txt, aveR.txt, aveS.txt and aveT.txt
```R
sfile<-'inst/Example_data/Flooddata/Table_stations_periods.csv'
grid_id_example_catchments<-'inst/Example_data/GISData/CID.txt'
outf<-'inst/Example_data/Flooddata/'
metinf<-get_metdataforfloods(gridid=grid_id_example_catchments,first_day=as.Date("1961/1/1"),last_day=as.Date("1961/01/31"),
station_file=sfile,metfolder="U:/metdata/",snowfolder="U:/snowsim/",hbvfolder="Z:/gwbsim/",outfolder=outf)
```

## Example for calculating recession time for a set of stations
The recessiontime is the number of days needed for the catchment moisture to be at 1% of its initial value. Needed for assessing 
flood generating process.
```R
spfile<-"inst/Example_data/Flooddata/Table_stations_periods.csv"
dfolder<-"inst/Example_data/Dailydata"
outfile<-"inst/Example_data/Flooddata/recessiontimes.txt"
recessions<-extract_recessiontimes_allstations(fraction=0.995,spfile,dfolder,outfile)
```


## Example for extracting flood generating processes for annual maximum floods
Daily values of the catchment averaged rain and snow melt is provided in the example dataset.
The outputs are written to a file where a column of fgps is added to the original file with flood values.

```R
fdata<-'inst/Example_data/Flooddata/amsvalues.txt'
rdata<-'inst/Example_data/Flooddata/aveR.txt'
sdata<-'inst/Example_data/Flooddata/aveS.txt'
resdata<-'inst/Example_data/Flooddata/recessiontimes.txt'
outf<-'inst/Example_data/Flooddata/ams_and_fgp.txt'
fgp<-get_fgp_allstations(floodfile=fdata, rainfile=rdata,snowfile=sdata,recessionfile=resdata,outfile=outf,cfgp=4)
```

## Example for extracting flood generating processes for peak over threshold floods
Daily values of the catchment averaged rain and snow melt is provided in the example dataset.
The outputs are written to a file where a column of fgps is added to the original file with flood values.

```R
fdata<-'inst/Example_data/Flooddata/potvalues.txt'
rdata<-'inst/Example_data/Flooddata/aveR.txt'
sdata<-'inst/Example_data/Flooddata/aveS.txt'
resdata<-'inst/Example_data/Flooddata/recessiontimes.txt'
outf<-'inst/Example_data/Flooddata/pot_and_fgp.txt'
fgp<-get_fgp_allstations(floodfile=fdata, rainfile=rdata,snowfile=sdata,recessionfile=resdata,outfile=outf,cfgp=4)
```


**Scripts developed during Flomkartproject:**


*Lena Schlichting and Kolbjørn Engeland 2016.*

