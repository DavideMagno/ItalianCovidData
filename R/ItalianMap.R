ExtractData <- function(italy, data, type) {
  italy$area %<>% 
    dplyr::left_join(data, by = type) %>% 
    dplyr::select_if(is.numeric) %>% 
    unlist %>% 
    unname
}

FilterAndPrepareToPlot <- function(Data, date, type, field = NA) {
  date <- as.Date(date, format = "%c")
  raw.data <- FilterData(Data, type)
  
  italy <- purrr::pluck(raw.data, "italy")
  
  covid <- purrr::pluck(raw.data, "data") %>% 
    dplyr::select(purrr::pluck(raw.data, "fixed.columns"), 
                  Total = dplyr::contains(paste0("Cumulative_",field))) 
  
  covid.last.cases <- covid %>% 
    dplyr::filter(Date == dplyr::last(Date))
  
  covid.given.date <- covid %<>% 
    dplyr::filter(Date == date) 
  
  italy$area <- tibble::tibble(!!type := italy$name)
  
  italy$last.cases <- ExtractData(italy, covid.last.cases, type)
  italy$cases <- ExtractData(italy, covid.given.date, type)
  
  return(italy)
}

DrawProxyMap <- function(italy, type, field) {
  #create a color palette to fill the polygons
  if (grepl("Region", type)) {
    bin <- c(1, unique(quantile(italy$last.cases, 
                                c(0, 0.025, 0.05, 0.15, 0.25, 0.3, 0.65, 0.85, 
                                  1))))
  } else {
    bin <- c(1, unique(quantile(italy$last.cases, 
                                c(0, 0.15, 0.30, 0.45, 0.55, 0.65, 0.85, 0.97, 
                                  1))))
  }
  if (grepl("Healed", field)) {
    pal <- leaflet::colorBin(c("#D6FFDA", "#B7EBBB", "#99D89C", 
                               "#7BC57D", "#5CB25D", "#3E9F3E", 
                               "#208C1F", "#027800"),
                             domain = italy$cases,
                             bins = bin,
                             na.color = "#ffffff")
  } else {
    pal <- leaflet::colorBin(c("#F6EEDB", "#ffe59c", "#f4c78a", 
                               "#e9aa78", "#df8d66", "#d47054", 
                               "#ca5342","#9A0A10"),
                             domain = italy$cases,
                             bins = bin,
                             na.color = "#ffffff")
  }
  
  
  #create a pop up (onClick)
  polygon_popup <- paste0(paste0("<strong>", type,": </strong>"), 
                          italy$name, "<br>", 
                          paste0("<strong>", field," cases: </strong>"), 
                          italy$cases)
  
  return(list(pal = pal, polygon_popup = polygon_popup))
}