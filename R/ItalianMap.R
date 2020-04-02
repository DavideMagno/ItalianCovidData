source(here::here("R/Analysis.R"))

FixTAG <- function(data) {
  data.tag <- data %>% 
    dplyr::filter(grepl("Trento", .$Region) | grepl("Bolzano", .$Region)) %>% 
    dplyr::group_by(Date) %>% 
    dplyr::summarise_if(is.numeric, sum) %>% 
    dplyr::mutate(Region = "Trentino-Alto Adige")
  
  data %<>%
    dplyr::filter(!grepl("Trento", .$Region), !grepl("Bolzano", .$Region)) %>% 
    dplyr::bind_rows(data.tag)
}

WrangleCovidData <- function(url) {
  readr::read_csv(url(url)) %>% 
    FixTAG %>% 
    CalculateTotal
}

GetRawData <- function() {
  address <- "https://raw.githubusercontent.com/DavideMagno/ItalianCovidData/master/"
  covid.province <- readr::read_csv(url(paste0(address,
                                               "Daily_Covis19_Italian_Data_",
                                               "Province_Cumulative.csv")))
  covid.regions <- WrangleCovidData(paste0(address,
                                           "Daily_Covis19_Italian_Data_",
                                           "Cumulative.csv"))
  covid.province.inc <- readr::read_csv(url(paste0(address,
                                                   "Daily_Covis19_Italian_Data_",
                                                   "Province_Incremental.csv")))
  covid.regions.inc <- WrangleCovidData(paste0(address,
                                               "Daily_Covis19_Italian_Data_",
                                               "Incremental.csv"))
  
  italy.province <- readRDS(here::here("ItalyCovidDashboard/data/gadm36_ITA_2_sp.rds"))
  italy.regions <- readRDS(here::here("ItalyCovidDashboard/data/gadm36_ITA_1_sp.rds"))
  
  italy.regions@data$NAME_1 %<>%
    stringr::str_replace("Apulia", "Puglia") %>% 
    stringr::str_replace("Friuli-Venezia Giulia", "Friuli V.G.") %>%
    stringr::str_replace("Sicily", "Sicilia") %>%
    stringr::str_replace("Emilia-Romagna", "Emilia Romagna") 
  
  italy.province@data$NAME_2 %<>% 
    stringr::str_replace("Mantua", "Mantova") %>% 
    stringr::str_replace("Florence", "Firenze") %>% 
    stringr::str_replace("Syracuse", "Siracusa") %>% 
    stringr::str_replace("Padua", "Padova") %>% 
    stringr::str_replace("Forli' - ", "Forlì-") %>% 
    stringr::str_replace("Pesaro E Urbino", "Pesaro e Urbino") %>% 
    stringr::str_replace("Reggio Di Calabria", "Reggio di Calabria") %>% 
    stringr::str_replace("Reggio Nell'Emilia", "Reggio nell'Emilia") %>% 
    stringr::str_replace("Monza and Brianza", "Monza e della Brianza") %>% 
    stringr::str_replace("Ogliastra", "Nuoro") %>% 
    stringr::str_replace("Olbia-Tempio", "Sassari") %>% 
    stringr::str_replace("Carbonia-Iglesias", "Sud Sardegna") %>% 
    stringr::str_replace("Medio Campidano", "Sud Sardegna")
  
  return(list(covid.province = covid.province, covid.regions = covid.regions,
              covid.province.inc = covid.province.inc, 
              covid.regions.inc = covid.regions.inc,
              italy.province = italy.province, italy.regions = italy.regions))
}

FilterAndPrepareToPlot <- function(Data, date, type, field = NA) {
  date <- as.Date(date, format = "%c")
  if (is.na(field)) field <- "Total"
  if (grepl("Region", type)) {
    italy <- Data$italy.regions
    italy$name <- italy@data$NAME_1
    
    covid <- Data$covid.regions %>% 
      dplyr::filter(Date == date) 
    if(grepl("Total", field)) {
      covid <- covid %>% 
        dplyr::select(Region, Total) 
    } else {
      covid <- covid %>%
        dplyr::select(Region, Total = !!rlang::enquo(field)) 
    }

    
  } else {
    italy <- Data$italy.province
    italy$name <- italy@data$NAME_2
    
    covid <- Data$covid.province %>% 
      dplyr::filter(Date == date) %>% 
      dplyr::select(-Date, -Region, Total = `Total Positive`) %>% 
      dplyr::filter(!grepl("In", .$Province))
  }
  
  italy$cases <- italy$name %>% 
    as.data.frame
  
  names(italy$cases) <- type
  
  italy$cases <- italy$cases %>% 
    dplyr::left_join(covid, by = type) %>% 
    dplyr::select_if(is.numeric) %>% 
    unlist %>% 
    unname
  
  return(italy)
}


  
