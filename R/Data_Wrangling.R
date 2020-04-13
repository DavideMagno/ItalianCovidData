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

CalculateItaly <- function(data) {
  data.italy <- data %>% 
    dplyr::group_by(Date) %>% 
    dplyr::summarise_if(is.numeric, sum) %>% 
    dplyr::mutate(Region = "Italy")
}

GetCovidData <- function(url) {
  data <- readr::read_csv(url(url))
  
  if("Tests" %in% colnames(data)) {
    data %<>% 
      FixTAG %>% 
      dplyr::mutate(Total = rowSums(dplyr::select_if(., is.numeric)),
                    Total = Total - Tests)
  } else {
    data %<>%
      dplyr::rename("Total" = `Total Positive`)
    }
}

WrangleCovidData <- function(data, data.inc, fixed.columns) {
  data.inc %<>%
    dplyr::mutate(Type = "Increment")
  
  data %<>% 
    dplyr::mutate(Type = "Cumulative") %>% 
    dplyr::bind_rows(data.inc)
  
  types <- data %>% 
    dplyr::select_if(is.numeric) %>% 
    colnames
  
  data %<>% 
    tidyr::pivot_longer(-c("Date", fixed.columns, "Type"),
                        names_to = "Field",
                        values_to = "Value") %>% 
    tidyr::pivot_wider(id_cols = c("Date", fixed.columns),
                       names_from = c("Type", "Field"),
                       values_from = "Value")
  

  
  return(list(data = data, types = types))
}

CalculateChange <- function(data, field) {
  ratio.header <- paste("Ratio", field, sep = "_")
  denominator <- rlang::sym(paste("Cumulative", field, sep = "_"))
  numerator <- rlang::sym(paste("Increment", field, sep = "_"))
  
  data %<>% 
    dplyr::transmute(!!ratio.header := 1/(!!denominator/!!numerator - 1))
  return(data)
}

CalculateReturns <- function(data, types) {
  
  final.data <- cbind(data, purrr::map_dfc(types, ~CalculateChange(data, .x))) %>% 
    tibble::as_tibble(.)
  
  return(final.data)
}

GetRawData <- function() {
  address <- "https://raw.githubusercontent.com/DavideMagno/ItalianCovidData/master/"
  
  files <- c("Cumulative.csv", "Incremental.csv", "Province_Cumulative.csv",
             "Province_Incremental.csv")
  
  covid <- purrr::map(files, ~GetCovidData(paste0(address,
                                                  "Daily_Covis19_Italian_Data_",
                                                  .x)))
  
  covid.region <- WrangleCovidData(covid[[1]], covid[[2]], "Region")
  
  covid.province <- WrangleCovidData(covid[[3]], covid[[4]], 
                                     c("Region", "Province"))

  covid.italy <- list(data = CalculateItaly(covid.region$data),
                      types = covid.region$types)
  
  covid.returns <- list(covid.region, covid.province, covid.italy) %>% 
    purrr::map(~CalculateReturns(.x$data, .x$types))
  
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
  
  return(list(covid.regions = covid.returns[[1]],
              covid.province = covid.returns[[2]],
              covid.italy = covid.returns[[3]],
              italy.province = italy.province, 
              italy.regions = italy.regions))
}