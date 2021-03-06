% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_ams_functions_v2.R
\name{get_amsdata}
\alias{get_amsdata}
\title{Extract annual maxima from daily time and subdaily time series for specified years}
\usage{
get_amsdata(stationnumber = 200011,
  path_dd = "inst/Excample_data/Dailydata",
  path_sd = "inst/Excample_data/Subdaily", active_station = 0,
  d_first = 1880, d_last = 2015, d_exclude = NA, s_first = 1880,
  s_last = 2015, s_exclude = NA)
}
\arguments{
\item{stationnumber}{The station number given as rrrmmmmm}

\item{path_dd}{Path to folder with daily data}

\item{path_sd}{Path to folder with subdaily data}

\item{active_station}{Is 1 if the station is active. Then also the most recent data ar used.}

\item{d_first}{First year to be used for daily data}

\item{d_last}{Last year to be used for daily data}

\item{d_exclude}{Years to be excluded for daily data}

\item{s_first}{First year to use for subdaily data}

\item{s_last}{Last year to use for subdaily data}

\item{s_exclude}{Years to exclude for subdaily data}
}
\value{
dataframe with station numbers, date an values for for daily and subdaily annual maxima
If they are from teh same event, they are written on the same line
if they are from different events, they are written on different lines
}
\description{
Use only years that has more than 363 observations.
If the annual maximqa for daily and subdaily time resolution is separated by two days or less, they are considered to
belong to the same flood event and they are written on the same line, and we have only one flood event for this year
if not, we have two flood flood event for this specifc year, and the daily and subdaily floo values are given for bith events.
For AMS analysis, only the maximum of these two should be used.
}
\examples{
get_amsdata(stationnumber=200011, path_dd='inst/Excample_data/Dailydata',
path_sd='inst/Excample_data/Subdaily',active_station=0,
d_first=1880,d_last=2015,d_exclude=NA,s_first=1880,s_last=2015,s_exclude=NA)
}

