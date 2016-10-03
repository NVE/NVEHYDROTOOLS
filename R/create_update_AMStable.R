# library(timeDate)
# library(plyr)
# library(data.table)
# library(fields)
# library(lubridate)


#' This script updates the AMS table with flood peak for daily data and hourly data, as well as corresponding data @flood if knekk and daily arent same event
# it calculates flood generating process for floods as fraction_rain (fraction of rain contribution to flood, rest is snowmelt)
#' @import timeDate
#' @import plyr
#' @import data.table
#' @import fields
#' @import lubridate
#'
#' @return
#' @export
#'
#' @examples
UPDATE_AMS_TABLE_TODO <- function(){


#read in the selection of stations which are approved for Flomkart: not regulated (max minimally and not visibly) and good quality
ams <- read.table("//nve/fil/h/HM/Interne Prosjekter/Flomkart/Datakvalitet/Endelig_liste_mai_2016/AMS_table_HYDRA_endelig.txt", sep="\t",header = T)
ams$year <- (format(as.Date(ams$dt_flood_daily_date), "%Y"))
ams$nve <- as.character(ams$nve)

#load in list of AMS-stations
unique_ams <- read.table("//nve/fil/h/HM/Interne Prosjekter/Flomkart/Datakvalitet/Endelig_liste_mai_2016/AMS_unique_Stations_with_snumbers.txt", sep="\t",header = T)
unique_ams$nve <- as.character(unique_ams$nve)

# load recession times (calculated with: \\nve\fil\h\HM\Brukere\lesc\scripts\for_NVE\calculate_recessiontimes.R)
recessiontimes <- read.table("//nve/fil/h/HM/Interne Prosjekter/Flomkart/Datakvalitet/Endelig_liste_mai_2016/recession_times_lamda_0_995.txt",sep = "\t", header=T)

#load discharge-data folder daily
myfiles_day <- list.files('//nve/fil/h/HM/Interne Prosjekter/Flomkart/Data/Ny_Data/Flomkart_dogn')
snumbers_day <- substr(myfiles_day,1,nchar(myfiles_day)-4)

#load discharge-data folder on knekkpunkt resolution
myfiles_knekk <- list.files('//nve/fil/h/HM/Interne Prosjekter/Flomkart/Data/Ny_Data/Flomkart_knekk')
snumbers_knekk <- substr(myfiles_knekk,1,nchar(myfiles_knekk)-4)

#load in senorge list for FGP calculation
senorge_list <- as.character(list.files('//nve/fil/h/HM/Interne Prosjekter/Flomkart/Catchment_Data')  )

#save updates into ams_update
ams_updat <- ams


## loop for each station, find data to fill in, add row with new data results to correct place
for(i in 1:nrow(unique_ams)){

  #extract regine and main numbers for later matching
  reginenr <- unique_ams$regine_area[i]
  mainnr <- unique_ams$main_nr[i]
  snumber_current <- unique_ams$snumber[i]

  #load in senorge data for FP calculation (if already downloaded/shapefile exists)
  loc_senorge <- which(senorge_list == paste(reginenr, ".", mainnr, sep=""))
  if(length(loc_senorge) > 0){
    has_senorge <- 1
    #data for qsw: snowmelt
    senorge_string1 <- paste("//nve/fil/h/HM/Interne Prosjekter/Flomkart/Catchment_Data/",senorge_list[loc_senorge],"/",senorge_list[loc_senorge],"_SeNorge_qsw_1959_2014.dta",sep="")
    # data for temp
    senorge_string2 <- paste("//nve/fil/h/HM/Interne Prosjekter/Flomkart/Catchment_Data/",senorge_list[loc_senorge],"/",senorge_list[loc_senorge],"_SeNorge_tm_1959_2014.dta",sep="")
    # data for precip (>0.5C, only "rain")
    senorge_string3 <- paste("//nve/fil/h/HM/Interne Prosjekter/Flomkart/Catchment_Data/",senorge_list[loc_senorge],"/",senorge_list[loc_senorge],"_SeNorge_corr_rr_1959_2014.dta",sep="")
    senorge <- read.table(senorge_string1)
    senorge2 <- read.table(senorge_string2)
    senorge3 <- read.table(senorge_string3)
    #make dates
    colnames(senorge) <- c("year", "month", "day","snowmelt")
    senorge$date <- NA
    senorge$date <- as.Date(with(senorge, paste(year, month, day,sep="-")), "%Y-%m-%d")
    #add precip and temp
    colnames(senorge2) <- c("year", "month", "day","temp")
    senorge$temp <- NA
    senorge$temp <- senorge$temp
    senorge$precip <- NA
    senorge$precip <- senorge3$V4
  }

  # find snumbers location for day and knekk data for loading discharge (vf) data in
  loc_day <- which(snumbers_day == unique_ams$snumber[i])
  loc_knekk <- which(snumbers_knekk == unique_ams$snumber[i])

  #load in daily series
  if(length(loc_day) > 0){
    dailydat <- read.table(paste('//nve/fil/h/HM/Interne Prosjekter/Flomkart/Data/Ny_Data/Flomkart_dogn/',myfiles_day[loc_day], sep=""),sep=" ")
    colnames(dailydat) <- c("orig_date", "vf")
    dailydat$date <- as.Date(dailydat$orig_date, format = "%Y%m%d")
    dailydat$year <-  as.numeric(format(as.Date(dailydat$date), "%Y"))
    #set -9999 as NA
    dailydat[dailydat == -9999] <- NA
    daily_years <- as.numeric(na.omit(unique(dailydat$year)))
    #set ams_years as daily_years, if both daily and knekk exist, fix below to both
    ams_years <- daily_years
  }

  #load in knekkpunkt series
  if(length(loc_knekk) > 0){
    knekkdat <- read.table(paste('//nve/fil/h/HM/Interne Prosjekter/Flomkart/Data/Ny_Data/Flomkart_knekk/',myfiles_knekk[loc_knekk], sep=""),sep=" ")
    colnames(knekkdat) <- c("orig_date", "vf")
    knekkdat$date <- as.POSIXct(knekkdat[,1], format = "%Y%m%d/ %H%M")
    knekkdat$date_nohour <- as.Date(knekkdat$date)
    knekkdat$year <-  as.numeric(format(as.Date(knekkdat$date), "%Y"))
    #set -9999 as NA
    knekkdat[knekkdat == -9999] <- NA
    knekk_years <- as.numeric(na.omit(unique(knekkdat$year)))
    #set ams_years as daily_years, if both daily and knekk exist, fix below to both
    ams_years <- knekk_years
  }

  #get all years to fill in data if both knekk and daily series exist
    if(length(daily_years) > 2 & length(knekk_years) > 2 ){
      ams_years <- as.numeric(na.omit(unique(ams_updat$year[which(ams_updat$regine_area == reginenr & ams_updat$main_nr == mainnr)])))
    }


     #find years that don't exist in ams_updat yet - but do not include the early years that aren't listed in ams_updat yet - if they are missing, it is due to bad data quality
    dailydiff <- setdiff(daily_years, ams_years)
    diff_dailyyears <- dailydiff[which(dailydiff > max(ams_years, na.rm = T))]
    knekkdiff <- setdiff(knekk_years, ams_years)
    diff_knekkyears <- knekkdiff[which(knekkdiff > max(ams_years, na.rm = T))]

    #find for looping: how many years on knekk or daily are to add - take longer one
    if(length(diff_dailyyears) > 0 & length(diff_knekkyears) > 0){addyear_years <- unique(c(diff_dailyyears, diff_knekkyears))}

    #if only knekkpunkt data has numeric diff_knekkyears (meaning, daily data isnt available yet)
    if(length(diff_dailyyears) == 0 & length(diff_knekkyears) > 0){addyear_years <- unique(diff_knekkyears)}

    #if only daily data has numeric diff_dailyyears (meaning, knekk data isnt available yet)
    if(length(diff_dailyyears) > 0 & length(diff_knekkyears) == 0){addyear_years <- unique(diff_dailyyears)}




    #######################################
    #### get data for each new year which is not listed in ams-table yet:

    #find AMS value and date (take head(max) in case sensor is "stuck" for some time) for dailydat and knekkdat
    for (u in 1:length(addyear_years)){

      #check if at leat 364 dailydat entries, if so, continue (if not, too much data missing for acceptance as good year)
      if(length(which(dailydat$year == addyear_years[u])) > 363 | length(unique(knekkdat$date_nohour[which(knekkdat$year == addyear_years[u])])) > 363){

        ##get daily and knekk AMS floods
        #subset dailydat to specific year
        sub_daily <- dailydat[which(dailydat$year == addyear_years[u]),]
        if(nrow(sub_daily) > 0) {
          loc_max_daily <- head(which(sub_daily$vf == max(sub_daily$vf, na.rm = T)),1)
          #from daily series
          date_dailymax <- sub_daily$date[loc_max_daily]
          vf_dailymax <- sub_daily$vf[loc_max_daily]
        }
        if(nrow(sub_daily) == 0) {
          date_dailymax <- -99
          vf_dailymax <- -99
        }

        #subset knekkdat to specific year
        sub_knekk <- knekkdat[which(knekkdat$year == addyear_years[u]),]
        if(nrow(sub_knekk) > 0) {
          loc_max_knekk <- head(which(sub_knekk$vf == max(sub_knekk$vf, na.rm = T)),1)
          #from knekkpunkt series
          date_knekkmax <- sub_knekk$date[loc_max_knekk]
          day_knekkmax <- as.Date(date_knekkmax)
          vf_knekkmax <- sub_knekk$vf[loc_max_knekk]
        }


        #################################################
        #check for both dailydat and knekkdat data for year:
        #knekk and daily max are within +-1 day (then same event, otherwise different floods = need corresponding discharge and two lines)
        if(nrow(sub_daily) > 0 & nrow(sub_knekk) > 0){
        if(day_knekkmax == date_dailymax | day_knekkmax == date_dailymax-1 | day_knekkmax == date_dailymax+1){

          #write AMS for dailydat and knekkdat into ams_updat
          loc_lastyear_ams <- max(which(ams_updat$regine_area == reginenr & ams_updat$main_nr == mainnr))

          #get start of findata
          fin_start <- ams_updat$start_findata[loc_lastyear_ams]
          unregstart <- ams_updat$unreg_start[loc_lastyear_ams]
          unregend <- ams_updat$unreg_end[loc_lastyear_ams]
          nvenr <- ams_updat$nve[loc_lastyear_ams]

          #calculate FGP if senorge data exists: fraction_rain for date_dailymax: #########

          if(has_senorge == 1){
            #concentration time = 2
            conc_time <- 2
            #find stations recession time:
            loc_match <- which(recessiontimes$snumber == unique_ams$snumber[i])
            recess_time <- recessiontimes$recess_days[loc_match]

            if(length(recess_time)>0){
              loc_conc_time <- date_dailymax - (conc_time - 1)
              #snowmelt concentration time:
              melt_weightfull <- sum(senorge$snowmelt[loc_conc_time:date_dailymax],na.rm=T)
              #rain:
              rain_weightfull <- sum(senorge$precip[loc_conc_time:date_dailymax],na.rm=T)

              #get weighted values for recession time
              date_recesstime_end <- date_dailymax - (conc_time)
              date_most_distant <- date_recesstime_end - recess_time+1

              if(date_most_distant > 0){
                snowmelt_recess <- senorge$snowmelt[date_most_distant:date_recesstime_end]
                rain_recess <- senorge$precip[date_most_distant:date_recesstime_end]

                #sum snowmelt and RAIN and add the full-weighted concentration time
                sum_melt <- sum(snowmelt_recess,na.rm=T) + melt_weightfull
                sum_rain <- sum(rain_recess,na.rm=T) + rain_weightfull

                #determine FGP
                fraction_rain <- sum_rain/(sum_melt + sum_rain)
              }
            }
            #end FGP ############################
          }


          #add new row for loc_lastyear_ams+1 into ams_updat
          newrow <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
          ams_updat <- rbind(ams_updat[1:loc_lastyear_ams,], newrow, ams_updat[-(1:loc_lastyear_ams),])

          #fill in values into new NA values (one by one, as "mixed type" rows can't be added easily)
          ams_updat$regine_area[loc_lastyear_ams+1] <- reginenr
          ams_updat$main_nr[loc_lastyear_ams+1] <- mainnr
          ams_updat$value_q_daily[loc_lastyear_ams+1] <- vf_dailymax
          ams_updat$dt_flood_daily_date[loc_lastyear_ams+1] <- as.character(date_dailymax)
          ams_updat$value_q_culm[loc_lastyear_ams+1] <- vf_knekkmax
          ams_updat$dt_flood_culm_time[loc_lastyear_ams+1] <- as.character(date_knekkmax)
          ams_updat$start_findata[loc_lastyear_ams+1] <- fin_start
          ams_updat$unreg_start[loc_lastyear_ams+1] <- unregstart
          ams_updat$unreg_end[loc_lastyear_ams+1] <- unregend
          ams_updat$nve[loc_lastyear_ams+1] <- nvenr
          ams_updat$fraction_rain[loc_lastyear_ams+1] <- fraction_rain
          ams_updat$year[loc_lastyear_ams+1] <- addyear_years[u]

          #set fraction_rain back to NA (to avoid pasting into next station if current station has no daily or knekk data)
          fraction_rain <- NA
        }
        }


        #################################################
        #if knekk and daily max are more than +-1day apart, get corresponding flood values and write in two lines
        if(nrow(sub_daily) > 0 & nrow(sub_knekk) > 0){
          if(day_knekkmax != date_dailymax & day_knekkmax != date_dailymax-1 & day_knekkmax != date_dailymax+1){

            #write AMS for dailydat and knekkdat into ams_updat
            loc_lastyear_ams <- max(which(ams_updat$regine_area == reginenr & ams_updat$main_nr == mainnr))

            #get start of findata
            fin_start <- ams_updat$start_findata[loc_lastyear_ams]
            unregstart <- ams_updat$unreg_start[loc_lastyear_ams]
            unregend <- ams_updat$unreg_end[loc_lastyear_ams]
            nvenr <- ams_updat$nve[loc_lastyear_ams]

            #calculate FGP if senorge data exists: fraction_rain for date_dailymax: #########

            if(has_senorge == 1){
              #concentration time = 2
              conc_time <- 2
              #find stations recession time:
              loc_match <- which(recessiontimes$snumber == unique_ams$snumber[i])
              recess_time <- recessiontimes$recess_days[loc_match]

              if(length(recess_time)>0){
                loc_conc_time <- date_dailymax - (conc_time - 1)
                #snowmelt concentration time:
                melt_weightfull <- sum(senorge$snowmelt[loc_conc_time:date_dailymax],na.rm=T)
                #rain:
                rain_weightfull <- sum(senorge$precip[loc_conc_time:date_dailymax],na.rm=T)

                #get weighted values for recession time
                date_recesstime_end <- date_dailymax - (conc_time)
                date_most_distant <- date_recesstime_end - recess_time+1

                if(date_most_distant > 0){
                  snowmelt_recess <- senorge$snowmelt[date_most_distant:date_recesstime_end]
                  rain_recess <- senorge$precip[date_most_distant:date_recesstime_end]

                  #sum snowmelt and RAIN and add the full-weighted concentration time
                  sum_melt <- sum(snowmelt_recess,na.rm=T) + melt_weightfull
                  sum_rain <- sum(rain_recess,na.rm=T) + rain_weightfull

                  #determine FGP
                  fraction_rain_day <- sum_rain/(sum_melt + sum_rain)
                }
              }
              #end FGP ############################
            }


            #calculate FGP if senorge data exists: fraction_rain for day_knekkmax: #########

            if(has_senorge == 1){
              #concentration time = 2
              conc_time <- 2
              #find stations recession time:
              loc_match <- which(recessiontimes$snumber == unique_ams$snumber[i])
              recess_time <- recessiontimes$recess_days[loc_match]

              if(length(recess_time)>0){
                loc_conc_time <- day_knekkmax - (conc_time - 1)
                #snowmelt concentration time:
                melt_weightfull <- sum(senorge$snowmelt[loc_conc_time:day_knekkmax],na.rm=T)
                #rain:
                rain_weightfull <- sum(senorge$precip[loc_conc_time:day_knekkmax],na.rm=T)

                #get weighted values for recession time
                date_recesstime_end <- day_knekkmax - (conc_time)
                date_most_distant <- date_recesstime_end - recess_time+1

                if(date_most_distant > 0){
                  snowmelt_recess <- senorge$snowmelt[date_most_distant:date_recesstime_end]
                  rain_recess <- senorge$precip[date_most_distant:date_recesstime_end]

                  #sum snowmelt and RAIN and add the full-weighted concentration time
                  sum_melt <- sum(snowmelt_recess,na.rm=T) + melt_weightfull
                  sum_rain <- sum(rain_recess,na.rm=T) + rain_weightfull

                  #determine FGP
                  fraction_rain_knekk <- sum_rain/(sum_melt + sum_rain)
                }
              }
              #end FGP ############################
            }

            #find corresponding discharge for knekkseries at daily_max: plus minus 1 day
            loc_tmpknekk <- which(sub_knekk$date_nohour == date_dailymax | sub_knekk$date_nohour == date_dailymax-1 |sub_knekk$date_nohour == date_dailymax+1)
            sub_knekkdate <- sub_knekk[loc_tmpknekk,]
            if (nrow(sub_knekkdate) > 0){
              max_knekk_forday_vf <- max(sub_knekkdate$vf, na.rm=T)
              max_knekk_forday_date <- sub_knekkdate$date[which(sub_knekkdate$vf == max(sub_knekkdate$vf, na.rm=T))]
            }
            if (nrow(sub_knekkdate) == 0){
              max_knekk_forday_vf <- -99
              max_knekk_forday_date <- -99
            }

            #find corresponding discharge for knekkseries at daily_max: plus minus 1 day
            loc_tmpday <- which(sub_daily$date == day_knekkmax | sub_daily$date == day_knekkmax-1 |sub_daily$date == day_knekkmax+1)
            sub_daydate <- sub_daily[loc_tmpday,]
            if (nrow(sub_daydate) > 0){
              max_day_forday_vf <- max(sub_daydate$vf, na.rm=T)
              max_day_forday_date <- sub_daydate$date[which(sub_daydate$vf == max(sub_daydate$vf, na.rm=T))]
            }
            if (nrow(sub_daydate) == 0){
              max_day_forday_vf <- -99
              max_day_forday_date <- -99
            }

            #add two new rows for loc_lastyear_ams+1 into ams_updat
            newrow <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)

            ams_updat <- rbind(ams_updat[1:loc_lastyear_ams,], newrow, ams_updat[-(1:loc_lastyear_ams),])
            ams_updat <- rbind(ams_updat[1:loc_lastyear_ams+1,], newrow, ams_updat[-(1:loc_lastyear_ams+1),])

            #fill daily values into new NA values
            ams_updat$regine_area[loc_lastyear_ams+1] <- reginenr
            ams_updat$main_nr[loc_lastyear_ams+1] <- mainnr
            ams_updat$value_q_daily[loc_lastyear_ams+1] <- vf_dailymax
            ams_updat$dt_flood_daily_date[loc_lastyear_ams+1] <- as.character(date_dailymax)
            ams_updat$value_q_culm[loc_lastyear_ams+1] <- max_knekk_forday_vf
            ams_updat$dt_flood_culm_time[loc_lastyear_ams+1] <- as.character(max_knekk_forday_date)
            ams_updat$start_findata[loc_lastyear_ams+1] <- fin_start
            ams_updat$unreg_start[loc_lastyear_ams+1] <- unregstart
            ams_updat$unreg_end[loc_lastyear_ams+1] <- unregend
            ams_updat$nve[loc_lastyear_ams+1] <- nvenr
            ams_updat$fraction_rain[loc_lastyear_ams+1] <- fraction_rain_day
            ams_updat$year[loc_lastyear_ams+1] <- addyear_years[u]

            #fill knekk values into new NA values
            ams_updat$regine_area[loc_lastyear_ams] <- reginenr
            ams_updat$main_nr[loc_lastyear_ams] <- mainnr
            ams_updat$value_q_daily[loc_lastyear_ams] <- max_day_forday_vf
            ams_updat$dt_flood_daily_date[loc_lastyear_ams] <- as.character(max_day_forday_date)
            ams_updat$value_q_culm[loc_lastyear_ams] <- vf_knekkmax
            ams_updat$dt_flood_culm_time[loc_lastyear_ams] <- as.character(date_knekkmax)

            ams_updat$start_findata[loc_lastyear_ams] <- fin_start
            ams_updat$unreg_start[loc_lastyear_ams] <- unregstart
            ams_updat$unreg_end[loc_lastyear_ams] <- unregend
            ams_updat$nve[loc_lastyear_ams] <- nvenr
            ams_updat$fraction_rain[loc_lastyear_ams] <- fraction_rain_day
            ams_updat$year[loc_lastyear_ams] <- addyear_years[u]

            #set fraction_rain_knekk and fraction_rain_day as NA again
            fraction_rain_day <- NA
            fraction_rain_knekk <- NA

          }
        }

        ###########################################
        #if only knekk data exists for that year
        if(nrow(sub_daily) == 0 & nrow(sub_knekk) > 0){
          #write AMS for dailydat and knekkdat into ams_updat
          loc_lastyear_ams <- max(which(ams_updat$regine_area == reginenr & ams_updat$main_nr == mainnr))

          #get start of findata
          fin_start <- ams_updat$start_findata[loc_lastyear_ams]
          unregstart <- ams_updat$unreg_start[loc_lastyear_ams]
          unregend <- ams_updat$unreg_end[loc_lastyear_ams]
          nvenr <- ams_updat$nve[loc_lastyear_ams]

          #calculate FGP if senorge data exists: fraction_rain for knekkdata: #########

          if(has_senorge == 1){
            #concentration time = 2
            conc_time <- 2
            #find stations recession time:
            loc_match <- which(recessiontimes$snumber == unique_ams$snumber[i])
            recess_time <- recessiontimes$recess_days[loc_match]

            if(length(recess_time)>0){
              loc_conc_time <- day_knekkmax - (conc_time - 1)
              #snowmelt concentration time:
              melt_weightfull <- sum(senorge$snowmelt[loc_conc_time:day_knekkmax],na.rm=T)
              #rain:
              rain_weightfull <- sum(senorge$precip[loc_conc_time:day_knekkmax],na.rm=T)

              #get weighted values for recession time
              date_recesstime_end <- day_knekkmax - (conc_time)
              date_most_distant <- date_recesstime_end - recess_time+1

              if(date_most_distant > 0){
                snowmelt_recess <- senorge$snowmelt[date_most_distant:date_recesstime_end]
                rain_recess <- senorge$precip[date_most_distant:date_recesstime_end]

                #sum snowmelt and RAIN and add the full-weighted concentration time
                sum_melt <- sum(snowmelt_recess,na.rm=T) + melt_weightfull
                sum_rain <- sum(rain_recess,na.rm=T) + rain_weightfull

                #determine FGP
                fraction_rain <- sum_rain/(sum_melt + sum_rain)
              }
            }
            #end FGP ############################
          }


          #add new row for loc_lastyear_ams+1 into ams_updat

          newrow <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
          #newrow <- c(-99,-99,-99,-99,-99, -99,-99,-99,-99,-99,-99,-99)
          ams_updat <- rbind(ams_updat[1:loc_lastyear_ams,], newrow, ams_updat[-(1:loc_lastyear_ams),])
          # convert the date columns to Date objects:
          #ams_updat[loc_lastyear_ams,4] <- lapply(ams_updat[loc_lastyear_ams,4],as.character)
          #ams_updat[loc_lastyear_ams+1,6] <- data.frame(lapply(ams_updat[loc_lastyear_ams+1,6], as.character), stringsAsFactors=FALSE)

          #fill in values into new NA values (one by one, as "mixed type" rows can't be added easily)
          ams_updat$regine_area[loc_lastyear_ams+1] <- reginenr
          ams_updat$main_nr[loc_lastyear_ams+1] <- mainnr
          ams_updat$value_q_daily[loc_lastyear_ams+1] <- -99
          ams_updat$dt_flood_daily_date[loc_lastyear_ams+1] <- -99
          ams_updat$value_q_culm[loc_lastyear_ams+1] <- vf_knekkmax
          ams_updat$dt_flood_culm_time[loc_lastyear_ams+1] <- as.character(date_knekkmax)
          ams_updat$start_findata[loc_lastyear_ams+1] <- fin_start
          ams_updat$unreg_start[loc_lastyear_ams+1] <- unregstart
          ams_updat$unreg_end[loc_lastyear_ams+1] <- unregend
          ams_updat$nve[loc_lastyear_ams+1] <- nvenr
          ams_updat$fraction_rain[loc_lastyear_ams+1] <- fraction_rain
          ams_updat$year[loc_lastyear_ams+1] <- addyear_years[u]


          #set fraction_rain back to NA (to avoid pasting into next station if current station has no daily or knekk data)
          fraction_rain <- NA
        }


        ###########################################
        #if only daily data exists for that year
        if(nrow(sub_daily) > 0 & nrow(sub_knekk) == 0){
          #write AMS for dailydat and knekkdat into ams_updat
          loc_lastyear_ams <- max(which(ams_updat$regine_area == reginenr & ams_updat$main_nr == mainnr))

          #get start of findata
          fin_start <- ams_updat$start_findata[loc_lastyear_ams]
          unregstart <- ams_updat$unreg_start[loc_lastyear_ams]
          unregend <- ams_updat$unreg_end[loc_lastyear_ams]
          nvenr <- ams_updat$nve[loc_lastyear_ams]

          #calculate FGP if senorge data exists: fraction_rain for : #########

          if(has_senorge == 1){
            #concentration time = 2
            conc_time <- 2
            #find stations recession time:
            loc_match <- which(recessiontimes$snumber == unique_ams$snumber[i])
            recess_time <- recessiontimes$recess_days[loc_match]

            if(length(recess_time)>0){
              loc_conc_time <- date_dailymax - (conc_time - 1)
              #snowmelt concentration time:
              melt_weightfull <- sum(senorge$snowmelt[loc_conc_time:date_dailymax],na.rm=T)
              #rain:
              rain_weightfull <- sum(senorge$precip[loc_conc_time:date_dailymax],na.rm=T)

              #get weighted values for recession time
              date_recesstime_end <- date_dailymax - (conc_time)
              date_most_distant <- date_recesstime_end - recess_time+1

              if(date_most_distant > 0){
                snowmelt_recess <- senorge$snowmelt[date_most_distant:date_recesstime_end]
                rain_recess <- senorge$precip[date_most_distant:date_recesstime_end]

                #sum snowmelt and RAIN and add the full-weighted concentration time
                sum_melt <- sum(snowmelt_recess,na.rm=T) + melt_weightfull
                sum_rain <- sum(rain_recess,na.rm=T) + rain_weightfull

                #determine FGP
                fraction_rain <- sum_rain/(sum_melt + sum_rain)
              }
            }
            #end FGP ############################
          }


          #add new row for loc_lastyear_ams+1 into ams_updat
          newrow <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
          ams_updat <- rbind(ams_updat[1:loc_lastyear_ams,], newrow, ams_updat[-(1:loc_lastyear_ams),])
          # convert the date columns to Date objects:

          #fill in values into new NA values (one by one, as "mixed type" rows can't be added easily)
          ams_updat$regine_area[loc_lastyear_ams+1] <- reginenr
          ams_updat$main_nr[loc_lastyear_ams+1] <- mainnr
          ams_updat$value_q_daily[loc_lastyear_ams+1] <- vf_dailymax
          ams_updat$dt_flood_daily_date[loc_lastyear_ams+1] <- as.character(date_dailymax)
          ams_updat$value_q_culm[loc_lastyear_ams+1] <- -99
          ams_updat$dt_flood_culm_time[loc_lastyear_ams+1] <- -99
          ams_updat$start_findata[loc_lastyear_ams+1] <- fin_start
          ams_updat$unreg_start[loc_lastyear_ams+1] <- unregstart
          ams_updat$unreg_end[loc_lastyear_ams+1] <- unregend
          ams_updat$nve[loc_lastyear_ams+1] <- nvenr
          ams_updat$fraction_rain[loc_lastyear_ams+1] <- fraction_rain
          ams_updat$year[loc_lastyear_ams+1] <- addyear_years[u]

          #set fraction_rain aback to NA (to avoid pasting into next station if current station has no daily or knekk data)
          fraction_rain <- NA
        }

      }
    }



  #set marker if senorge data  exists back to zero
  has_senorge <- 0



}

#save updated dable as "AMS_table_HYDRA_updated.txt"
ams <- read.table("//nve/fil/h/HM/Interne Prosjekter/Flomkart/Datakvalitet/Endelig_liste_mai_2016/AMS_table_HYDRA_updated.txt", sep="\t",header = T)


}
