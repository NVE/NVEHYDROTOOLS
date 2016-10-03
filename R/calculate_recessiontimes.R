## This script calculates average recession times in days, based on method described in Skaugen & Onof, 2014.
# Skaugen, T. and Onof, C. (2014): A rainfall-runoff model parameterized from GIS and runoff data. Hydrol. Process., 28, 4529-4542.
# library(compare)



#' Calculates recession times
#'
#' @import compare
#' @return
#' @export
#'
#' @examples
recession_times <- function(){

#select lamda for fraction (worked well between 0.9 - 0.99)
fraction <- 0.995


#load data - AMS table or your own data instead
use_stations <- read.table("//nve/fil/h/HM/Interne Prosjekter/Flomkart/Datakvalitet/Endelig_liste_mai_2016/AMS_table_HYDRA_endelig.txt", sep="\t",header = T)
allstations <- read.table("//nve/fil/h/HM/Interne Prosjekter/Flomkart/Datakvalitet/Endelig_liste_mai_2016/AMS_unique_Stations.txt", sep="\t",header = T)
unique_list <- allstations
old_vers <- read.table("//nve/fil/h/HM/Interne Prosjekter/Flomkart/Data/AMS_table_inkl_flomvarsling.txt", sep="\t",header = T)
unique_old_vers <- as.data.table(old_vers)
unique_old_vers <- unique_old_vers[,.SD[which.max(year)],by=snumber]
unique_list$snumber <- NA

#add snumber into unique_list
for(i in 1:nrow(unique_list)){
  loc_tmp <- which(unique_old_vers$regine_enhet == unique_list$regine_area[i] & unique_old_vers$hovednr == unique_list$main_nr[i])
  if(length(loc_tmp) > 0){
    unique_list$snumber[i] <- unique_old_vers$snumber[loc_tmp]
  }
}
# fix if snumbers not read in as string
unique_list$snumber[2] <- 200015
unique_list$snumber[36] <- 200603
unique_list$snumber[39] <- 200611
unique_list$snumber[52] <- 1200015

setwd('//nve/fil/h/HM/Interne Prosjekter/Flomkart/Data/Ny_Data')
#load in discharge data
DataFolder<-"Flomkart_dogn"              # Navnet p? mappen med d?gndatafiler
myfiles<-list.files(DataFolder)  # 549 stations
myfiles<-myfiles[1:length(myfiles)-1]
snumbers<-substr(myfiles,1,nchar(myfiles)-4)
sorder<-order(as.integer(snumbers))
myfiles<-myfiles[sorder]                #sorts myfiles ascending
snumbers<-snumbers[sorder]

#######################
#create dataframe for filling lamda and recession time for each station
df_lamda <- data.frame(matrix(NA, nrow(unique_list), ncol = 4))
colnames(df_lamda) <- c("snumber", "lamda","recess_days","nve")

# set path
setwd('//nve/fil/h/HM/Interne Prosjekter/Flomkart/Data/Ny_Data/Flomkart_dogn')

### loop for each station
for(i in 1:nrow(unique_list)){
  #find data series to read in
  loc_tmp <- which(snumbers == unique_list$snumber[i])
  if(length(loc_tmp) > 0){
    loadname <- paste(snumbers[loc_tmp],".txt",sep = "",collapse=NULL)
    name = try(read.table(loadname,sep = " " ))
    colnames(name) <- c("datum","vf")

    #take negative values out
    name <- subset(name, vf >= 0)

    # extract year, start at full year
    name$date <- as.POSIXct(name$datum, format = "%Y%m%d")
    name$year <- as.numeric(format(name$date, "%Y"))
    firstyear <- name$year[1]
    paste(snumbers[i],"start in",firstyear)
    sub_firstyear <- subset(name, year == firstyear)

    if (nrow(sub_firstyear) < 365){
      # subset data to only contain from 2nd year on
      name <- subset(name, year > firstyear)
    }
    unique_years <- unique(name$year)

    if(length(unique_years) > 0){
      #keep from unique_years only the years that are left in AMStable (use_stations) after deleting the years from remove_table
      loc_AMS <- which(use_stations$regine_area == unique_list$regine_area[i] & use_stations$main_nr == unique_list$main_nr[i])
      station_AMS <- use_stations[loc_AMS,]
      station_AMS$year <- as.numeric(format(as.Date(station_AMS$dt_flood_daily_date), '%Y'))
      #take only years that are listed in use_stations$year
      loc_tmp <- which(name$year %in% unique(station_AMS$year))
      if (length(loc_tmp) > 0){
        name <- name[loc_tmp,]
        unique_years <- unique(name$year)

        #remove years that have less than 360 daily observations missing
        name$delete_line <- NA
        for (m in 1:length(unique_years)){
          loc_uniqueyear <- which(name$year == unique_years[m])
          if (length(loc_uniqueyear) < 360){
            name$delete_line[loc_uniqueyear] <- FALSE
          }
        }
        loc_delete <- which(name$delete_line == F)
        if (length(loc_delete) > 0){
          name <- name[-loc_delete,]
        }

        #only continue if unregulated years exist
        if (length(name$vf) > 0){
          Q = name$vf
          Q_d <- head(log(name$vf), -1) - tail(log(name$vf), -1)
          #get recession time for positive number for diff Q1 - Q2
          loc_tmp <- which(Q_d > 0)
          Q_d_positive <- Q_d[loc_tmp]
          # calculate lambda (eq 11 in paper)
          lamda <- data.frame(matrix(NA, length(Q_d_positive), ncol = 1))
          colnames(lamda) = ("lamda")
          for (u in 1:length(Q_d_positive)){

            lamda$lamda[u] <- Q_d_positive[u]

          }

          #calculate Empirical Cumulative Distribution Function
          verteil_fkt <- ecdf(lamda$lamda)
          #     plot(verteil_fkt, col = "red")

          # get the x corresponding to given y -> use linear interpolation
          inv_ecdf <- function(verteil_fkt){
            x <- environment(verteil_fkt)$x
            y <- environment(verteil_fkt)$y
            approxfun(y, x)
          }

          g <- inv_ecdf(verteil_fkt)
          value_lamda <- g(fraction)

          #calculate recessiontime t, stop when flow approximates 0.01
          t = -log(0.01) / value_lamda
          t_rounded <- round(t, digits = 0)

          #save value_lamda and t_rounded in df
          df_lamda$lamda[i] <- value_lamda
          df_lamda$recess_days[i] <- t_rounded
          df_lamda$snumber[i] <- unique_list$snumber[i]
          df_lamda$nve[i] <- unique_list$nve[i]

          print(i)
        }
      }
    }
  }
}

write.table(na.omit(df_lamda), "//nve/fil/h/HM/Interne Prosjekter/yourpath/yourfilename.txt",sep = "\t", row.names = F)

}

#write.table(na.omit(df_lamda), "//nve/fil/h/HM/Interne Prosjekter/Flomkart/Datakvalitet/Endelig_liste_mai_2016/recession_times_lamda_0_995.txt",sep = "\t", row.names = F)

