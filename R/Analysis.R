library(magrittr)

FilterData <- function(Data, type, italy = FALSE) {
  if (grepl("Region", type)) {
    if (italy) {
      data <- Data$covid.italy
    } else {
      data <- Data$covid.regions
    }
    fixed.columns <- c("Date", "Region")
    italy <- Data$italy.regions
    italy$name <- italy@data$NAME_1
  } else {
    data <- Data$covid.province
    fixed.columns <- c("Date", "Region", "Province")
    italy <- Data$italy.province
    italy$name <- italy@data$NAME_2
  }
  
  return(list(data = data,
              fixed.columns = fixed.columns,
              italy = italy))
}

Extract <- function(Data, filter_by = "Italy", select_method = "Cumulative", 
                    type = "Region", select_field = "Total", 
                    start_date = as.Date("2020-02-24"), end_date = NULL) {
  italy <- ifelse("Italy" %in% filter_by, TRUE, FALSE)
  type <- ifelse("Italy" %in% filter_by, "Region", type)
  
  raw.data <- FilterData(Data, type, italy)
  
  data <- purrr::pluck(raw.data, "data") %>% 
    dplyr::filter(grepl(paste(filter_by, collapse="|"), .[[type]]),
                  Date >= start_date)
  
  if(grepl("Tests", select_method) & !grepl("Province",type)) {
    data %<>%
      dplyr::select(purrr::pluck(raw.data, "fixed.columns"),
                    paste("Increment", select_field, sep = "_"),
                    "Increment_Tests") %>% 
      dplyr::group_by(Region) %>% 
      dplyr::mutate_if(is.numeric, dplyr::funs(./WMA(Increment_Tests, 7, 
                                                     wts = seq(7,1))))
    
    colnames(data) <- gsub(paste0("Increment_"), "", colnames(data), 
                           fixed=TRUE)
  } else {
    data %<>%
      dplyr::select(purrr::pluck(raw.data, "fixed.columns"),
                    paste(select_method, select_field, sep = "_")) 
    
    colnames(data) <- gsub(paste0(select_method,"_"), "", colnames(data), 
                           fixed=TRUE)
  }
  
  if (is.null(start_date)) {
    data %<>% 
      dplyr::filter(Date >= as.Date("2020-02-24"))
  }
  
  if (!is.null(end_date)) {
    data %<>% 
      dplyr::filter(Date <= end_date)
  }
  
  return(data)
}

PrepareDataForExtraction <- function(regions, provinces, date.range, last.date,
                                     data.field) {
  if (is.null(provinces)) { 
    if (is.null(regions)) {
      filter_by <- "Italy"
    } else {
      filter_by <- regions
    }
    type <- "Region"
  } else {
    filter_by <- provinces
    type <- "Province"
  }
  start_date <- ifelse(is.null(date.range[1]), 
                       as.Date("2020-02-24"), date.range[1])
  end_date <- ifelse(is.null(date.range[2]), 
                     last.date, date.range[2])
  if (is.null(data.field) | !is.null(provinces)) {
    data.field <- "Total"
  } else {
    data.field <- data.field
  }
  
  return(list(filter_by = filter_by, type = type, start_date = start_date,
              end_date = end_date, data.field = data.field))
}