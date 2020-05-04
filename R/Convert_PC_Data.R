library(magrittr)

TreatOfficialData <- function(official.data.link, fields, new.fields, flag = FALSE) {
  data <- readr::read_csv(url(official.data.link),
                  col_types = readr::cols(data = readr::col_character())) %>% 
    dplyr::mutate(denominazione_regione = dplyr::case_when(
      grepl("P.A. Bolzano", denominazione_regione) ~ "Bolzano",
      grepl("P.A. Trento", denominazione_regione) ~ "Trento",
      grepl("Friuli Venezia Giulia", denominazione_regione) ~ "Friuli V.G.",
      grepl("Emilia-Romagna", denominazione_regione) ~ "Emilia Romagna",
      TRUE ~ denominazione_regione
    )) %>% 
    dplyr::mutate(data = stringr::str_replace(data, "T1", " 1")) %>% 
    dplyr::mutate(data = as.Date(as.POSIXct(.$data, format="%Y-%m-%d %H:%M:%S"))) %>% 
    dplyr::select_at(dplyr::vars(fields))
  
  colnames(data) <- new.fields
  
  if(flag) {
    data$Province %<>% 
      stringr::str_replace("Forl“-Cesena", "Forlì-Cesena")
  }
  
  return(data)
}

CalculateIncrement <- function(data, fields) {
  inc.data <- data %>% 
    dplyr::mutate(Date = as.Date(Date)) %>% 
    dplyr::arrange(Date) %>% 
    dplyr::group_by_at(dplyr::vars(fields)) %>% 
    dplyr::mutate_at(dplyr::vars(-Date, -fields), 
                     function(x) x - dplyr::lag(x, default = 0)) %>% 
    dplyr::ungroup()
  
  return(inc.data)
}

setwd("/home/davide_magno/ItalyCovid19/")

## Get last update

last.update <- "https://raw.githubusercontent.com/DavideMagno/ItalianCovidData/master/" %>% 
  paste0("Daily_Covis19_Italian_Data_Cumulative.csv") %>% 
  {readr::read_csv(url(.))} %>% 
  {dplyr::last(.$Date)}

## Input data
region.file <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"
region.fields <- c("data", "denominazione_regione", "ricoverati_con_sintomi", 
                   "terapia_intensiva", "isolamento_domiciliare", 
                   "dimessi_guariti", "deceduti", "tamponi")
region.new.fields <- c("Date", "Region", "Hospitalised", "In ICU", 
                       "Home Isolation", "Healed", "Dead", "Tests")  

province.file <-  "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv"
province.fields <- c("data", "denominazione_regione", "denominazione_provincia", 
                     "totale_casi")
province.new.fields <- c("Date", "Region", "Province", "Total Positive") 



## Run 

Dati.ufficiali <- TreatOfficialData(region.file, region.fields, region.new.fields)
Dati.ufficiali.province <- TreatOfficialData(province.file, province.fields, 
                                             province.new.fields, TRUE) %>%
  dplyr::distinct(.keep_all = TRUE)

Dati.ufficiali.increment <- CalculateIncrement(Dati.ufficiali, "Region")
Dati.ufficiali.province.increment <- CalculateIncrement(Dati.ufficiali.province, 
                                                        c("Region", "Province"))

if (dplyr::last(Dati.ufficiali$Date) == last.update) {
  message(paste(Sys.time(), "no update available"))
} else {
  message("Starting the update")
  ## Write Data 
  write.csv(Dati.ufficiali, "Daily Covis19 Italian Data Cumulative", 
            row.names = FALSE)
  write.csv(Dati.ufficiali, "Daily_Covis19_Italian_Data_Cumulative.csv", 
            row.names = FALSE)
  write.csv(Dati.ufficiali.increment, "Daily Covis19 Italian Data Incremental", 
            row.names = FALSE)
  write.csv(Dati.ufficiali.increment, "Daily_Covis19_Italian_Data_Incremental.csv", 
            row.names = FALSE)
  write.csv(Dati.ufficiali.province, "Daily_Covis19_Italian_Data_Province_Cumulative.csv", 
            row.names = FALSE)
  write.csv(Dati.ufficiali.province.increment, "Daily_Covis19_Italian_Data_Province_Incremental.csv", 
            row.names = FALSE)
  message("Performing Tests")
  ## Tests
  cum.total <- Dati.ufficiali %>% 
    dplyr::filter(Date == dplyr::last(Dati.ufficiali$Date)) %>% 
    dplyr::summarise_at(dplyr::vars(-Date, -Region), sum)
  
  inc.total <- Dati.ufficiali.increment %>% 
    dplyr::summarise_at(dplyr::vars(-Date, -Region), sum)
  
  pro.cum.total <- Dati.ufficiali.province %>% 
    dplyr::filter(Date == dplyr::last(Dati.ufficiali$Date)) %>% 
    dplyr::summarise_at("Total Positive", sum)
  
  pro.inc.total <- Dati.ufficiali.province.increment %>% 
    dplyr::summarise_at("Total Positive", sum)
  
  cross.total <- Dati.ufficiali %>% 
    dplyr::filter(Date == dplyr::last(Dati.ufficiali$Date)) %>%
    dplyr::summarise_if(is.numeric, sum) %>% 
    {rowSums(.) - .$Tests}
  
  testthat::test_that("totals", testthat::expect_identical(cum.total, inc.total))
  testthat::test_that("contagion1", 
                      testthat::expect_identical(as.numeric(pro.cum.total), 
                                                 cross.total))
  testthat::test_that("contagion2", 
                      testthat::expect_identical(as.numeric(pro.inc.total), 
                                                 cross.total))
  message("Writing Data")
  system("git add 'Daily Covis19 Italian Data Cumulative'")
  system("git add 'Daily Covis19 Italian Data Incremental'")
  system("git add Daily_Covis19_Italian_Data_Cumulative.csv")
  system("git add Daily_Covis19_Italian_Data_Incremental.csv")
  system("git add Daily_Covis19_Italian_Data_Province_Cumulative.csv")
  system("git add Daily_Covis19_Italian_Data_Province_Incremental.csv")
  system(paste("git commit -m 'Data updated to the", Sys.Date(), "'"))
  system("git push origin master")
  message("Upload Completed")
}

