source(here::here("R/ItalianMap.R"))

Data <- GetRawData()
last.date <- dplyr::last(Data$covid.regions$Date)