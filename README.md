# NVEHYDROTOOLS

R package for processing flood data at NVE.

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
## Example for extracting ams data for a set of stations

The following code returns ams for selected stations:

```R
library(NVEDATA)
extract_ams_allstations(stations_periods_file="inst/Example_data/Flooddata/Table_stations_periods.csv",
dailydata="inst/Example_data/Dailydata", subdailydata="inst/Example_data/Subdaily",
outfile="inst/Example_data/Flooddata/amsvalues.txt")
```

The ams data are written to file.

## Example for extracting SeNorge grid IDs for all stations

```R
shapef <- '//nve/fil/h/HM/Interne Prosjekter/Flomkart/Data/GISData/Hydrologi_TotalNedborfeltMalestasjon.shp'
slayer<- 'Hydrologi_TotalNedborfeltMalestasjon'
grid_id_all_catchments<-gridcell_list(NA,shapef,slayer)
```


## Example for downloading catchment averaged SeNorge data for a set of stations

```R
sfile<-'inst/Excample_data/Flooddata/Table_stations_periods.csv'
str<-'inst/Excample_data/CatchmentCharacteristics/Feltnr_flomkart_til_feltnr_GIS.txt'

get_metdataforfloods(gridid=grid_id_all_catchments,first_day=as.Date("1961/1/1"),last_day=as.Date("1961/12/31"),
station_file=sfile, snr_translation=str,
' metfolder="U:/metdata/",snowfolder="U:/snowsim/",hbvfolder="Z:/gwbsim/",outfolder="inst/Excample_data/Flooddata/")
```
The outputs are written to the files aveP.txt, aveQ.txt, aveR.txt, aveS.txt and aveT.txt

## Example for extracting flood generating processes

```R
fgp<-get_fgp_allstations(amsfile='inst/Example_data/Flooddata/amsvalues.txt',rainfile='inst/Example_data/Flooddata/aveR.txt',
snowfile='inst/Example_data/Flooddata/aveS.txt',recessionfile='inst/Example_data/Flooddata/recessiontimes.txt',
' outfile='inst/Example_data/Flooddata/ams_and_fgp.txt')
```

The outputs are written to a file where a olomn of fgps is added to the original file wtho flood values

## Example for extracting POT floods from a set of stations

```R
extract_pot_allstations(amsfile='inst/Example_data/Flooddata/amsvalues.txt', dailydata="inst/Example_data/Dailydata",
p_threshold = 0.98, TTR_3x = 6,pratio= 2.0/3.0,
outfile="inst/Example_data/Flooddata/potvalues.txt")
```
**Scripts developed during Flomkartproject:**


*Lena Schlichting 2016.*

