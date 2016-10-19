sfile<-'inst/Example_data/Flooddata/Table_stations_periods.csv'
grid_id_example_catchments<-'inst/Example_data/GISData/CID.txt'
outf<-'inst/Example_data/Flooddata/SeNorge2.1/'
metinf<-get_metdataforfloods(gridid=grid_id_example_catchments,first_day=as.Date("1958/1/1"),last_day=as.Date("2015/12/31"),
station_file=sfile,metfolder="U:/metdata/met_obs_v2.1/",snowfolder="U:/snowsim/snowsim_v2.0.1/",hbvfolder="Z:/gwbsim/",outfolder=outf)

