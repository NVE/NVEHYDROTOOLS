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
two flood events is given per year:

```R
library(NVEDATA)
amsdata<-extract_ams_allstations(stations_periods_file="inst/Example_data/Flooddata/Table_stations_periods.csv",
dailydata="inst/Example_data/Dailydata", subdailydata="inst/Example_data/Subdaily",
outfile="inst/Example_data/Flooddata/amsvalues.txt")
```

The ams data are written to file.

## Example for extracting SeNorge grid IDs for all stations
Shape file with catchment boundaries for NVE gauging stations might be downloaded from
http://nedlasting.nve.no/gis/, and you should select the GIS dataset HYDROLOGISKE DATA->Totalnedbørfelt til målestasjon

```R
shapef <- '//nve/fil/h/HM/Interne Prosjekter/Flomkart/Data/GISData/Hydrologi_TotalNedborfeltMalestasjon.shp'
slayer<- 'Hydrologi_TotalNedborfeltMalestasjon'
outfile<-'inst/Example_data/GISData/CID.txt'
grid_id_all_catchments<-gridcell_list(NA,shapef,slayer,outfile)
```

## Example for extracting SeNorge grid IDs for the example stations

```R
example_stations<-c("1.37.0","1.200.0","2.1.0","2.10.0","2.11.0","2.13.0","2.15.0","2.17.0","2.21.0","2.25.0","2.28.0","2.32.0")
shapef <- '//nve/fil/h/HM/Interne Prosjekter/Flomkart/Data/GISData/Hydrologi_TotalNedborfeltMalestasjon.shp'
slayer<- 'Hydrologi_TotalNedborfeltMalestasjon'
outfile<-'inst/Example_data/GISData/CID.txt'
grid_id_example_catchments<-gridcell_list(c_ids=example_stations,c_shapef,c_slayer,outfile=outfile)
```


## Example for downloading catchment averaged SeNorge data for a set of stations
This requires access to the complete SeNorge model data. Avaliable on request.
The outputs are written to the files aveP.txt, aveQ.txt, aveR.txt, aveS.txt and aveT.txt
```R
sfile<-'inst/Example_data/Flooddata/Table_stations_periods.csv'
str<-'inst/Example_data/CatchmentCharacteristics/Feltnr_flomkart_til_feltnr_GIS.txt'

get_metdataforfloods(gridid=grid_id_example_catchments,first_day=as.Date("1961/1/1"),last_day=as.Date("1961/12/31"),
station_file=sfile, snr_translation=str,
' metfolder="U:/metdata/",snowfolder="U:/snowsim/",hbvfolder="Z:/gwbsim/",outfolder="inst/Excample_data/Flooddata/")
```


## Example for extracting flood generating processes
Daily values of the catchment averaged rain and snow melt is given in the example dataset.
The outputs are written to a file where a olomn of fgps is added to the original file with flood values.

```R
fgp<-get_fgp_allstations(floodfile='inst/Example_data/Flooddata/amsvalues.txt',rainfile='inst/Example_data/Flooddata/aveR.txt',
snowfile='inst/Example_data/Flooddata/aveS.txt',recessionfile='inst/Example_data/Flooddata/recessiontimes.txt',
' outfile='inst/Example_data/Flooddata/ams_and_fgp.txt',cfgp=4)
```

## Example for extracting POT floods from a set of stations
The ams-file is used to select for which stations and years the POT will be extracted.
```R
extract_pot_allstations(amsfile='inst/Example_data/Flooddata/amsvalues.txt', dailydata="inst/Example_data/Dailydata",
p_threshold = 0.98, TTR_3x = 6,pratio= 2.0/3.0,
outfile="inst/Example_data/Flooddata/potvalues.txt")
```
**Scripts developed during Flomkartproject:**


*Lena Schlichting 2016.*

