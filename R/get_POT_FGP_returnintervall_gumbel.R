# library(timeDate)
# library(plyr)
# library(data.table)
# require(Kendall)
# require(hydroGOF)
# library(ggplot2)
# library(scales)
# library(Rmisc)
# library(lmomco)
# library(evir)
# library(aqfig)
# library(MASS)
# library(verification)




# recession times were calculated based on Skaugen&Onof 2014.
# return intervalls for POT-floods are calculated based on AMS-data and Gumbel distribution. Script from frequencyanalysis and bootstrapCI by Dave Hutchinson, headwateranalytics


# #References
# Lang, M, Ouarda, T.B.M.J., Bob?e, B. (1999). Towards operational guidelines for over-threshold modeling. Journal of Hydrology. 225, 103-117.
# Skaugen, T. and Onof, C. (2014): A rainfall-runoff model parameterized from GIS and runoff data. Hydrol. Process., 28, 4529-4542.
# Vormoor, K., Lawrence, D., Heistermann, M, Bronsert, A. (2015). Climate change impacts on the seasonality and generation processes of floods - projections and uncertainties for catchments with mixed snowmelt/rainfall regimes. Hydro. Earth Syst. Sci., 19, 913-931.
# Vormoor, K., Lawrence, D., Schlichting, L., Wilson, D., Wong, W.K. (2016). Evidence for changes in the magnitude and frequency of observed rainfall vs. snowmelt driven floods in Norway. Journal of Hydrology. 538, 33-48.



#' This script calculates:
#  POT (peak over threshold) floods (independent events, after Lang et al 1999) and FGPs (flood generating processes) as well as the corresponding return intervall (based on AMS-data, Gumbel)
#  for each independent POT event above threshold based on chosen percentile (set value in line 23)
#'
#' @return
#' @import lmomco
#' @export
#'
#' @examples
RETURN_INTERVAL_TODO <- function() {

### choose threshold value for POT (eg 99-percentile: => 0.99)
threshold_value <- 0.99
# calculation of FGPs (flood generating processes): The FGP method is described closer in Vormoor et al, 2015 and 2016.
## load functions
# stations selection is based on AMS-datasett for Flomkart project, but you can load in any discharge series on daily resolution instead (evt adjust for different date format)
# frequencyanalysis and bootstrapCI by Dave Hutchinson, headwateranalytics
FrequencyAnalysis <- function( series, distribution, nep = nonexceeds()[2:29] ) {   #orig: nep=nonexceeds()


  # library(lmomco)

  distribution <- tolower(distribution)
  transformed <- FALSE

  # add log Pearson Type 3 to list of distributions supported
  # by lmomco package
  base.dist <- c('lp3', dist.list())

  if( any(distribution %in% base.dist) ) {

    # log transform series
    if( distribution == 'lp3' ) {
      series <- log10(series)
      transformed <- TRUE
      distribution <- 'pe3'
    }

    # compute L-moments
    samLmom <- lmom.ub(series)

    # estimate distribution parameters
    distPar <- lmom2par(samLmom, type = distribution)

    # compute quantiles for nonexceedances
    quant <- par2qua(f = nep, para = distPar)

    if( distribution == 'pe3' & transformed ) {
      distribution <- 'lp3'
      quant <- 10^quant
    }

    # return result as list object
    return(
      list(
        distribution = list(
          name = distribution,
          logTransformed = transformed,
          parameters = distPar),
        output = data.frame(nep = nep, rp = prob2T(nep), estimate = quant)
      ) )

  } else {
    stop(
      sprintf('Distribution \'%s\' not recognized!', distribution))
  }
}    #if crashes, use: nep=nonexceeds()[2:29]

BootstrapCI <- function(series, distribution, n.resamples=1E3, nep=nonexceeds()[2:29] , ci=0.95) {   #orig: nep=nonexceeds()

  # compute frequency analysis
  fa <- FrequencyAnalysis(series=series, distribution=distribution, nep=nep)

  # extract fitted model parameters and flag as to whether the
  # distribution is based on log transformed data
  base.params <- fa$distribution$parameters
  isTransformed <- fa$distribution$logTransformed

  # create output matrices to store parameter sets and quantile estimates
  param.sets <- matrix(NA, nrow = n.resamples, ncol = length(base.params$para))
  quantile.estimates <- matrix(NA, nrow = n.resamples, ncol = length(nep),
                               dimnames = list(NULL, nep) )

  # begin bootstrapping procedure
  for(i in 1:n.resamples) {

    valid.moments <- FALSE
    j <- 0

    # allow up to 20 re-tries to re-sample
    while(!valid.moments & j < 20) {

      # sample 'n' random variates from base distribution
      data <- rlmomco(n=length(series), base.params)

      # compute sample l-moments
      sample.moms = lmom.ub(data)

      valid.moments <- are.lmom.valid(sample.moms)
      j <- j + 1
    }

    # error handling
    if(!valid.moments) {
      stop("Bootstrapping failed to sample valid l-moments")
    } else {
      # estimate distribution parameters
      dist.par <- lmom2par(sample.moms, base.params$type)

      # store the distribution parameters
      param.sets[i,] <- dist.par$para

      # estimate quantiles at NEP
      estimated <- qlmomco(nep, dist.par)

      # convert quantile estimates to real values if
      # distribution was transformed
      if(isTransformed) estimated <- 10^estimated

      # store the quantiles at the desired AEP values
      quantile.estimates[i,] <- estimated
    }

  }

  # now calculate confidence limits for quantiles
  p <- c((1-ci)/2, (1+ci)/2)
  ci <- sapply(colnames(quantile.estimates),
               FUN=function(x){
                 quantile(quantile.estimates[,x], probs=p, na.rm=TRUE)})

  # now return list object containing output
  return(
    list(
      ci = data.frame(
        nonexceed_prob = nep,
        lower = as.vector(ci[1,]),
        true = fa$output$estimate,
        upper = as.vector(ci[2,]) ),
      parameters = param.sets,
      quantiles = quantile.estimates)
  )

}    #if crashes, use: nep=nonexceeds()[2:29]


#load in AMS_data
ams <- read.table("//nve/fil/h/HM/Interne Prosjekter/Flomkart/Datakvalitet/Endelig_liste_mai_2016/AMS_table_HYDRA_endelig_old.txt", sep="\t",header = T)
ams$year <- (format(as.Date(ams$dt_flood_daily_date), "%Y"))
ams$nve <- as.character(ams$nve)

loc_tmp <- which(ams$regine_area == 12 & ams$main_nr == 150)
ams$nve[loc_tmp] <- "12.150"
loc_tmp <- which(ams$regine_area == 2 & ams$main_nr == 280)
ams$nve[loc_tmp] <- "2.280"
loc_tmp <- which(ams$regine_area == 86 & ams$main_nr == 10)
ams$nve[loc_tmp] <- "86.10"


#load in list of AMS-stations
unique_ams <- read.table("//nve/fil/h/HM/Interne Prosjekter/Flomkart/Datakvalitet/Endelig_liste_mai_2016/AMS_unique_Stations.txt", sep="\t",header = T)
unique_ams$nve <- as.character(unique_ams$nve)

# load recession times (calculated with: )
recessiontimes <- read.table("//nve/fil/h/HM/Interne Prosjekter/Flomkart/Datakvalitet/Endelig_liste_mai_2016/recession_times_lamda_0_995.txt",sep = "\t", header=T)
recessiontimes$nve <- as.character(recessiontimes$nve)

# #load discharge-data
myfiles<-list.files('//nve/fil/h/HM/Interne Prosjekter/Flomkart/Data/Ny_Data/Flomkart_dogn')
snumbers<-substr(myfiles,1,nchar(myfiles)-4)

# get table of stations with senorge-data
waifiles<-list.files('//nve/fil/h/HM/Interne Prosjekter/Flomkart/Catchment_Data')
senorge_folder<-substr(waifiles,1,nchar(myfiles)-4)


#add snumber into unique_ams
old_vers <- read.table("//nve/fil/h/HM/Interne Prosjekter/Flomkart/Data/AMS_table_inkl_flomvarsling.txt", sep="\t",header = T)
unique_old_vers <- as.data.table(old_vers)
unique_old_vers <- unique_old_vers[,.SD[which.max(year)],by=snumber]
unique_ams$snumber <- NA

for(i in 1:nrow(unique_ams)){
  loc_tmp <- which(unique_old_vers$regine_enhet == unique_ams$regine_area[i] & unique_old_vers$hovednr == unique_ams$main_nr[i])
  if(length(loc_tmp) > 0){
    unique_ams$snumber[i] <- unique_old_vers$snumber[loc_tmp]
  }
}
unique_ams$snumber[2] <- 200015
unique_ams$snumber[36] <- 200603
unique_ams$snumber[39] <- 200611
unique_ams$snumber[52] <- 1200015

#nve number as.character
unique_ams$nve <- as.character(unique_ams$nve)
unique_ams$nve[20] <- "2.280"
unique_ams$nve[52] <- "12.150"
unique_ams$nve[188] <- "86.10"

#make dataframe for results
df_results <- data.frame(matrix(NA, nrow(unique_ams), ncol = 6))
colnames(df_results) <- c("snumber","regine","main","avg_floods_prstl99","avg_FGP","floods_pr_yr")


### SCRIPT and loop for each station
# read in discharge series, get peak over threshold event with date and flood value, calculate FGP, write out textfile per station
# and write summary in df_results

for (i in 1:nrow(unique_ams)){
  #find Q-series name
  loc_tmp <- which(snumbers == unique_ams$snumber[i])
  nve_nr <- unique_ams$nve[i]
  station_nr <- unique_ams$snumber[i]
  if (length(loc_tmp) > 0){

    # get Gumbel distribution for AMS data -> extract return intervall of POT flood in second step
    sub_ams <- ams[which(ams$regine_area == unique_ams$regine_area[i] & ams$main_nr == unique_ams$main_nr[i]),]
    sub_ams[sub_ams == -99] <- NA
    sub_ams[sub_ams == -9999] <- NA
    #remove NA in value_q_daily
    loc_na <- which(is.na(sub_ams$value_q_daily))
    if(length(loc_na) > 0){
      sub_ams <- sub_ams[-loc_na,]
    }
    #check if two value_q_daily for one year exist, if so, take the larger value_q_daily
    loc_duplicates <- which((duplicated(sub_ams$year) | duplicated(sub_ams$year, fromLast = TRUE)) == TRUE)
    for(z in 1:nrow(sub_ams)){
      if(z %in% loc_duplicates & !is.na(sub_ams$dt_flood_culm_time[z])){
        sub_ams <- sub_ams[-z,]
      }
    }
    ams_values <- sub_ams$value_q_daily
    if(length(which(!is.na(ams_values))) > 5){
    titel_Q <- paste("Gumbel distribution,", nve_nr)
    frequencyPlot_Q <- function(series, ci) {

      # determine plotting positions
      bwpeaks <- data.frame(PROB = pp(series, sort = FALSE), FLOW = series)
      xbreaks <- c(0.002,0.01,0.1,0.25,0.5,0.8,0.9,0.95,0.975,0.99,0.995, 0.998)
      log.range <- log10(range(series, ci[,-1], na.rm = TRUE))
      lower <- 10^floor(log.range[1])
      upper <- 10^ceiling(log.range[2])
      cap <- lower
      ybreaks <- NULL
      while(cap < upper) {
        ybreaks <- c(ybreaks, seq(cap, cap*9, by = cap))
        cap <- cap * 10
      }

      # now plot
      ggplot(bwpeaks) +
        geom_point(aes(x=PROB, y=FLOW)) +
        theme_bw() +
        scale_y_continuous(trans="log10", breaks=ybreaks, name="Discharge [m^3/s]") +
        scale_x_continuous(trans=probability_trans(distribution="norm"),
                           breaks=xbreaks, labels=signif(prob2T(xbreaks), digits=4),
                           name="Return period [yrs]") +
        geom_line(data=ci, aes(x=nonexceed_prob, y=true), color="blue") +
        geom_line(data=ci, aes(x=nonexceed_prob, y=lower), color="red", lty=2) +
        ggtitle(titel_Q)+
        geom_line(data=ci, aes(x=nonexceed_prob, y=upper), color="red", lty=2)

    }
    dist <- "gum"  # GPA: assumes distribution is not linear
    fa_Q <- FrequencyAnalysis(series=ams_values, distribution=dist)
    #if first value in fa_Q$output$estimates negative, remove line
    loc_neg <- which(fa_Q$output$estimate < 0)


    #read in discharge series
    name <- read.table(paste("//nve/fil/h/HM/Interne Prosjekter/Flomkart/Data/Ny_Data/Flomkart_dogn/", snumbers[loc_tmp],".txt", sep = ""))
    #name <- read.table(paste("//hdata/fou/Vannbalansekart/Data/Runoff_All/", read_name, sep = ""))
    station_nr <- unique_ams$snumber[i]
    colnames(name) <- c("datum","vf")
    peaks_pr_yr <- NA
    #remove negative values (-9999 as NA)
    name <- subset(name, vf >= 0)
    # extract year, start at full year
    name$date <- as.POSIXct(name$datum, format = "%Y%m%d")  #as.POSIXct(name$datum, format = "%Y%m%d")
    name$year <-  as.numeric(format(as.Date(name$date), "%Y"))    #as.numeric(format(name$date, "%Y"))

    #subset name to years from ams (which are unregulated and good quality)
    years_ams <- na.omit(unique(ams$year[which(ams$nve == nve_nr)]))
    loc_tmp <- which(name$year %in% years_ams)
    name <- name[loc_tmp,]
    max_year <- max(name$year,na.rm = T)

    # select POT events
    #only continue if after removing regulated data has still data AND max_year is > 1958 (only then senorge data)
    if (length(name$vf) > 0 ){  #for kariannes data: & max_year > 1959
      #only use years that are used in ams-file = considered good
      sub_ams <- ams[which(ams$nve == unique_ams$nve[i]),]
      sub_ams[sub_ams == -99] <- NA
      unique_years <- na.omit(unique(format(as.Date(sub_ams$dt_flood_daily_date), "%Y")))
      name <- name[which(name$year %in% unique_years),]

      if(nrow(name) > 3){
        thresh_value <- quantile(name$vf, threshold_value)

        if(length(unique_years) > 1){

          #### select POT-peaks which are independent
          sub_name <- name
          dates <- sub_name$date
          vf <- sub_name$vf
          daynumbers <- as.numeric(seq(1,length(dates),1))
          ##interpolate vf to 30min resolution
          vf_interp <- approx(daynumbers,vf, n = length(dates)*24*2, method = "linear")

          ##Find points where vf crosses above threshold.
          above <- vf_interp$y > thresh_value
          # Points always intersect when above=TRUE, then FALSE or reverse
          intersect.points<-which(diff(above)!=0)
          # get daynumber of POT start and end
          daynumber_cross <- vf_interp$x[intersect.points]

          if(length(daynumber_cross) > 0){
            daynumber_cross_odd <- daynumber_cross[seq(1, length(daynumber_cross), 2)]
            daynumber_cross_even <- daynumber_cross[seq(2, length(daynumber_cross), 2)]

            ##get max for each peak
            #2 cross.points give 1 POT event
            nr_POT <- length(intersect.points)/2
            intersect.points_odd <- intersect.points[seq(1, length(intersect.points), 2)]
            intersect.points_even <- intersect.points[seq(2, length(intersect.points), 2)]

            #make matrixes to fill
            POT_start <- (matrix(NA, nrow = nr_POT, ncol = 1))
            POT_end <- (matrix(NA, nrow = nr_POT, ncol = 1))
            df_POT <- data.frame(matrix(NA, nrow = nr_POT, ncol = 3))
            colnames(df_POT) <- c("max_POT","max_POT_day", "max_POT_date")


            # find max_POT for each POT-peak
            if (nr_POT >= 1 & length(intersect.points) > 0){
              for (v in 1:nr_POT){
                #find POT event : first intersection w threshold
                loc_POT_start <- which(vf_interp$x == daynumber_cross_odd[v])
                POT_start[v] <- vf_interp$x[loc_POT_start]
                loc_POT_end <- which(vf_interp$x == daynumber_cross_even[v])
                POT_end[v] <- vf_interp$x[loc_POT_end]
                #duration_POT_DAY[v] <- (abs(POT_start[v]-POT_end[v]))

                #find flood max of POT-event
                max_POT <- max(vf_interp$y[loc_POT_start:loc_POT_end])
                # find when POT-max of event happens
                loc_max_POT <- which(vf_interp$y == max_POT)
                # if flood peak is flat (due to sensor stuck etc) take FIRST value
                if (length(loc_max_POT) > 1){loc_max_POT <- head(loc_max_POT,n=1)}
                #daynumber of POT-maximum
                max_POT_day <- vf_interp$x[loc_max_POT]
                #date of POT-maximum
                POT_date <- name$date[round(max_POT_day)]
                df_POT$max_POT_date[v] <- as.character(POT_date)
                #write max_POT_day and max_POT in df_POT
                df_POT$max_POT[v] <- max_POT
                df_POT$max_POT_day[v] <- max_POT_day

              }
            }

            #difference between max_POT_day 1 and following one -> check if it is larger than 3x its TTR
            df_POT$max_POT_diff_day <- c(NA, diff(df_POT$max_POT_day))
            #TTR between max_POT_day and NEXT (not previous) peak
            df_POT$max_POT_diff_day_2nd <- NA
            for (y in 1:nr_POT){
              df_POT$max_POT_diff_day_2nd[y] <- abs(df_POT$max_POT_day[y] - df_POT$max_POT_day[y+1])
            }

            # based on Lang et al., 1999
            # (1a) check independence based on max_POT_diff_day (in regard to previous peak) larger than 3*TTR
            #adjust here the d !!!
            # take TTR as 2 days constant:
            TTR_3x <- 6
            df_POT$indep_TTR <- NA
            df_POT$indep_TTR[which(df_POT$max_POT_diff_day > TTR_3x)] <- TRUE
            df_POT$indep_TTR[which(df_POT$max_POT_diff_day <= TTR_3x)] <- FALSE
            #check if first peak is independent: if >3xTTR until start of observations
            start_datum <- name$date[1]
            if ((df_POT$max_POT_day[1] - TTR_3x) >  0){df_POT$indep_TTR[1] <- TRUE}

            #check if min discharge in through between two peaks if < 2/3 of FIRST peak
            # part between POT1-end and POT2-start
            through_end <- intersect.points_odd[2:length(intersect.points_odd)]
            through_start <- intersect.points_even[1:length(intersect.points_even)]
            df_POT$indep_min <- NA
            for (p in 1:nr_POT){
              ##calculate minimum discharge
              #find through min of between POT-events
              if (is.na(through_end[p]) == FALSE) {
                df_POT$min_POT[p] <- min(vf_interp$y[through_start[p]:through_end[p]])
              }
            }

            ##check if min_POT is < 2/3 of FIRST peak
            df_POT$indep_min[which(df_POT$max_POT >= (2/3)*df_POT$min_POT)] <- TRUE

            # new column if both indep_TTR and indep_min are TRUE --> this defines the start of the "POT group event": consecutive events w > threshold
            df_POT$indep_PEAK <- FALSE
            df_POT$indep_PEAK[which(df_POT$indep_TTR == TRUE & df_POT$indep_min == TRUE)] <- TRUE

            # column for group: from indep_PEAK[n] to indep_PEAK[n+1]
            df_POT$group_nr <- NA
            #predefine group numbers
            groupnr <- c(1:nr_POT)

            #remove peaks that aren't indep_mean = TRUE
            loc_remove <- which(df_POT$indep_min == FALSE)
            if(length(loc_remove) >0){df_POT <- df_POT[-loc_remove,]}


            ###################################################
            ##### get FGPs from senorge ################################
            df_POT$fraction_rain <- NA
            ### take each POT event, calculate fraction_rain, add in verified_peaks

            #find matching reg&main-nr of downloaded senorge data
            loc_senorge <- which(unique_ams$nve[i] == senorge_folder)
            station_string <- unique_ams$nve[i]

            if (length(loc_senorge) > 0){
              #data for qsw: snowmelt
              df_string1 <- paste("//nve/fil/h/HM/Interne Prosjekter/Flomkart/Catchment_Data/",station_string,"/",station_string,"_SeNorge_qsw_1959_2014.dta",sep="")
              # data for temp
              df_string2 <- paste("//nve/fil/h/HM/Interne Prosjekter/Flomkart/Catchment_Data/",station_string,"/",station_string,"_SeNorge_tm_1959_2014.dta",sep="")
              # data for precip (>0.5C, only "rain")
              df_string3 <- paste("//nve/fil/h/HM/Interne Prosjekter/Flomkart/Catchment_Data/",station_string,"/",station_string,"_SeNorge_corr_rr_1959_2014.dta",sep="")
              df <- read.table(df_string1)
              df2 <- read.table(df_string2)
              df3 <- read.table(df_string3)
              #make dates
              colnames(df) <- c("year", "month", "day","snowmelt")
              df$date <- NA
              df$date <- as.Date(with(df, paste(year, month, day,sep="-")), "%Y-%m-%d")
              #add precip and temp
              colnames(df2) <- c("year", "month", "day","temp")
              df$temp <- NA
              df$temp <- df2$temp
              df$precip <- NA
              df$precip <- df3$V4


              #concentration time = 2
              conc_time <- 2
              #recession time: use weights: 1) linear and 2) exponential
              loc_match <- which(recessiontimes$snumber == unique_ams$snumber[i])
              recess_time <- recessiontimes$recess_days[loc_match]

              #continue only if recession time exists
              if(length(recess_time)>0){
                loc_date <- NA   #reset loc_Date as NA
                for(u in 1:nrow(df_POT)){
                  #get date of POT-flood
                  if(!is.na(df_POT$max_POT_date[u])){flood_date <- df_POT$max_POT_date[u]}
                  #if(is.na(use_stations$date_DOGN[i])){flood_date <- as.Date(use_stations$date_TIME[i])}
                  #locate Qmax-Date in df (senorge)
                  loc_date <- which(as.character(df$date) == as.character(flood_date))
                  #get fully weighted concentration time: 2 days#############
                  if(length(loc_date) > 0){
                    loc_conc_time <- loc_date - (conc_time - 1)
                    #snowmelt concentration time:
                    melt_weightfull <- sum(df$snowmelt[loc_conc_time:loc_date],na.rm=T)
                    #rain:
                    rain_weightfull <- sum(df$precip[loc_conc_time:loc_date],na.rm=T)

                    #get weighted values for recession time
                    loc_recesstime_end <- loc_date - (conc_time)
                    loc_most_distant <- loc_recesstime_end - recess_time+1
                    #loop for multiplying weight with snowmelt and RAIN,
                    snowmelt_recess <- df$snowmelt[loc_most_distant:loc_recesstime_end]
                    rain_recess <- df$precip[loc_most_distant:loc_recesstime_end]

                    #sum snowmelt and RAIN and add the full-weighted concentration time
                    sum_melt <- sum(snowmelt_recess,na.rm=T) + melt_weightfull
                    sum_rain <- sum(rain_recess,na.rm=T) + rain_weightfull

                    #determine FGP
                    #fraction_snow <- sum_melt/(sum_melt + sum_rain)
                    fraction_rain <- sum_rain/(sum_melt + sum_rain)
                    #write fraction_rain into df_POT
                    df_POT$fraction_rain[u] <- fraction_rain
                  }

                }

              }
            }
            ### FGP end ###

            #loop from indep_PEAK[n] to indep_PEAK[n+1]-1 to define groups, then find each groups max
            startval <- 0
            for (v in 1:nr_POT){
              #find event, give group_nr
              if(df_POT$indep_PEAK[v] == TRUE){
                startval <- startval+1
                df_POT$group_nr[v] <- startval
              }
              #fill NA value with previous group number
              if (is.na(df_POT$group_nr[v]) & v > 1){
                df_POT$group_nr[v] <-  df_POT$group_nr[v-1]
              }
              if(is.na(df_POT$group_nr[v]) & v == 1){
                df_POT$group_nr[v] <- 1
              }
            }
            #find largest max_POT for each group
            #verfidied peaks
            df_POT$verified <- NA
            group_numbers <- c(1:max(df_POT$group_nr, na.rm = T))
            for (v in 1:length(group_numbers)){
              loc_group <- which(df_POT$group_nr == group_numbers[v])
              #find max for each group
              max_group <- max(df_POT$max_POT[loc_group])
              loc_max <- which(df_POT$max_POT == max_group)
              df_POT$verified[loc_max] <- TRUE
            }

            loc_verified_peaks <- which(df_POT$verified == TRUE)
            verified_peaks <- df_POT[,c("max_POT","max_POT_date","fraction_rain")]
            verified_peaks <- verified_peaks[loc_verified_peaks,]
            verified_peaks$ret_int <- NA

            ##### get return intervalls for verified peaks
            # use approxfun to extract return values - NA means that observed value is so small it doesnt appear on distribution curve
            fa_Q_approx <- approxfun(fa_Q$output$estimate,fa_Q$output$rp)
            #get return periods via approxfun
            for (k in 1:nrow(verified_peaks)){
              verified_peaks$ret_int[k] <- fa_Q_approx(verified_peaks$max_POT[k])
            }


            # save df_POT with station name
            save_name <- paste ("//nve/fil/h/HM/Interne Prosjekter/Flomkart/Data/Lena/MET_karianne/POT_files/POT_",station_nr,".txt", sep = "", collapse = NULL)
            write.table(verified_peaks,save_name, sep = "\t", row.names = F)

            # how many peaks pr year?
            nr_years <- length(unique(name$year))
            peaks_pr_yr <- length(loc_verified_peaks)/(nr_years)

            #write summay in df_results
            df_results$snumber[i] <- station_nr
            df_results$regine[i] <- unique_ams$regine_area[i]
            df_results$main[i] <- unique_ams$main_nr[i]
            df_results$floods_pr_yr[i] <- nrow(verified_peaks)/nr_years
            df_results$avg_FGP[i] <- mean(verified_peaks$fraction_rain, na.rm=T)
            df_results$avg_floods_prstl99[i] <- mean(verified_peaks$max_POT, na.rm=T)
          }
        }
      }
    }
    print(i)


  }
  }
}
write.table(df_results, "//nve/fil/h/HM/Interne Prosjekter/Flomkart/Data/Lena/MET_karianne/df_results_99_incl_returns.txt",sep = "\t", row.names = F)



}
