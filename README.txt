Scripts developed during Flomkartproject:


Lena Schlichting 2016.

1) calculate_recessiontimes.R:
This script calculates average recession times in days, based on method described in Skaugen & Onof, 2014.
It loads in the AMS-table as stations to calculate recession times for, but can also be applied to other data. 
Required inputs (besides AMS-table or another overview table with station numbers (eg 2) is daily discharge data from HYDRA.
Results are written as a textfile.


2) get_POT_FGP_returnintervall_gumbel.R:
This script extracts POT (peak over threshold) floods (independent events, after Lang et al 1999) and FGPs (flood generating processes) as well as the corresponding return intervall 
(based on AMS-data, Gumbel). 
Station selection is based on AMS-table, but can also be applied to other station selections. 
A table for AMS-data is required in order to calculate return intervals block maxima, now set to Gumbel but can be adjusted to other distribution types).
If no return intervals for flood events required, comment this part just out and omit the AMS-table.
Discharge series (daily) are required, from HYDRA.

calculation of FGPs (flood generating processes): The FGP method is described closer in Vormoor et al, 2015 and 2016.
Recession times are required for FGP calculation (see script 1).
Return intervalls for POT-floods are calculated based on AMS-data and Gumbel distribution. Script from frequencyanalysis and bootstrapCI by Dave Hutchinson, headwateranalytics

The first part of the script adjusts the AMS-table and sets up the dataframe for writing results in (df_results).
The script then loops through each station, and calculates a Gumbel distribution based onthe AMS-dataset. Discharge series is loaded in, POT events above
treshold are selected and written in dataframe (df_POT). FGPs are then calculated for POTs. Then a check for independence of flood event (Lang 1999) is run 
(they are grouped if days-between are below threshold for independency and largest POT of group picked) and all dependent floods removed.
Return intervals for independent POTs are retrieved (approxfun from distribution), and POT-file saved (for each station seperately).
DF_result is saved after all stations are processed and saved (it contains average values).


3) create_ams_functions_v2.R
Excample of use:
setwd('C:/Users/koe/Documents/Flomkart/NVEHYDROTOOLS/R')
source('create_ams_functions_v2.R')

myams<-extract_ams_allstations("../Data/Flooddata/Table_stations_periods.csv",
                               "../Data/Dailydata","../Data/Subdaily","../Data/Flooddata/amsvalues.txt")

This script creates an AMS table with flood peak for daily data and hourly data.
The years for which the AMS valiues are to be extracted are specified in the file 
"../Data/Flooddata/Table_stations_periods.csv"
The largest daily flood is extracted for all specified years provided we have more than 363 observations that year.
The largest sub-daily flood is extracted only for years where we have daily floods.
If daily and subdaily floods are separated more than two days, they are assumed to represent two differen events.


Excample data are provided. For full dataset, You must download new HYDRA data and 
either adjust the path, or paste into current folder of SeNorge-data 
(//nve/fil/h/HM/Interne Prosjekter/Flomkart/Catchment_Data)


4: 
setwd('C:/Users/koe/Documents/Flomkart/NVEHYDROTOOLS/R')
source('create_ams_functions_v2.R')

SeNorge_gridID_for_catchments
library('rgdal')
library('rgeos')

source('SeNorge_gridID_for_catchments.R')
#GIS-data kan be loaded from fra http://nedlasting.nve.no/gis/, and you should select  
#HYDROLOGISKE DATA->Totalnedbørfelt til målestasjon

grid_id_all_catchments<-gridcell_list(NA,"E:/Data/GISData/Hydrologi_TotalNedborfeltMalestasjon.shp")

source('SeNorge_gridID_for_catchments')
tt1<-gridcell_list("2.11.0","Hydrologi_TotalNedborfeltMalestasjon.shp")
