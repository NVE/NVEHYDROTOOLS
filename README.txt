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


3) create_update_AMStable.R
This script updates the AMS table with flood peak for daily data and hourly data when new data years exist in HYDRA, as well as corresponding data @flood if knekk and daily arent same event
You must download new HYDRA data and either adjust the path, or paste into current folder of SeNorge-data (//nve/fil/h/HM/Interne Prosjekter/Flomkart/Catchment_Data)
It calculates flood generating process for floods as fraction_rain (fraction of rain contribution to flood, rest is snowmelt).
It reads in AMS-table for updating, recession times (for FGPs), discharge data (daily and knekkpunkt), and senorge data.
For each station, it gets years that need to be updated (addyear_years), and loops then through years: it subsets daily and knekk to current year and gets annual maximum plus date.
Then it calculates FGP (if date@daily and date@knekk are only +-1 day apart = same event, or if only knekkdata or only daily resolution exists) the FGPs and writes into AMS-table (ams_updat).
If date@daily and date@knekk are mroe than +-1 day apart = different event, then two lines are added into ams_updat. FGPs are calculated for both daily and knekk-value.
Corresponding dischagre is extracted for date of knekk/daily-flood and written into corresponding date file.
The finished table is saved as ams_updat.txt.