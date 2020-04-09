library(magrittr)

CalculateTotal <- function(data){
  data %<>% 
    dplyr::mutate(Total = rowSums(dplyr::select_if(., is.numeric)),
                  Total = Total - Tests)
  return(data)
}

ExtractDataXRegion <- function(data, type, name, field){
  if (is.list(type)) type <- type$id
  if (grepl("Province", type)) {
    data %<>% 
      dplyr::filter(grepl(name, !!rlang::sym(type))) %>% 
      dplyr::select(Date, Total = `Total Positive`)
  } else {
    if (grepl("Italy", name)){
      data %<>%
        dplyr::group_by(Date) %>% 
        dplyr::summarise_if(is.numeric, sum)
    } else {
      data %<>% 
        dplyr::filter(grepl(name, !!rlang::sym(type)))
    }
    data %<>% 
      dplyr::select(Date, Total = !!rlang::enquo(field), Tests)
      
  }
  return(data)
} 

# AnalisiRegione <- function(inc_data, cum_data, regione, field, average.window){
#   inc_data <- ExtractDataXRegion(inc_data, regione, field)
#   cum_data <- ExtractDataXRegion(cum_data, regione, field)
#   final.analysis <- cum_data %>% 
#     dplyr::left_join(inc_data, by = "Date") %>% 
#     dplyr::select(-Tests.x) %>% 
#     dplyr::filter(Total.x != 0) %>% 
#     dplyr::rename(Tests = Tests.y,
#                   `Cumulative Total` = Total.x,
#                   `Incremental Total` = Total.y) %>% 
#     dplyr::mutate(Average.tests = slider::slide_dbl(.$Tests, ~mean(.x), 
#                                                     .before = average.window)) %>% 
#     dplyr::mutate(`Ratio New Cases` = `Incremental Total`/`Cumulative Total`) %>% 
#     dplyr::mutate(`Ratio New Cases x test` = `Incremental Total`/Tests) %>%
#     dplyr::mutate(`Ratio New Cases x average test` = `Incremental Total`/Average.tests) %>%
#     dplyr::arrange(Date) 
#   
#   return(final.analysis)
# }

PrepDataExplorer <- function(data, regions, provinces, data.fields, date.range) {
  if(is.null(provinces)) {
    if(is.null(regions)) {
      df <- data %>% 
        dplyr::group_by(Date) %>% 
        dplyr::summarise_if(is.numeric, sum) %>% 
        dplyr::mutate(Region = "Italy")
    } else {
      df <- data %>%
        dplyr::filter(
          is.null(regions) | Region %in% regions
        )
    }
    if(is.null(data.fields)) {
      df %<>%
        dplyr::select(Date, Region, Hospitalised, `In ICU`, 
                      `Home Isolation`, Healed, Dead, Total, Tests)
    } else {
      df %<>%
        dplyr::select_at(dplyr::vars("Date", "Region", data.fields))
    }
  } else {
    df <- data %>%
      dplyr::filter(
        is.null(regions)     | Region %in% regions,
        is.null(provinces)   | Province %in% provinces
      ) %>% 
      dplyr::rename("Total" = "Total Positive")
  }
  if(is.na(date.range[1])) {
    df %<>%
      dplyr::filter(Date >= as.Date("2020-02-24"))
  } else {
    df %<>%
      dplyr::filter(Date >= date.range[1])
  }
  if(is.na(date.range[2])) {
    df %<>%
      dplyr::filter(Date <= last.date)
  } else {
    df %<>%
      dplyr::filter(Date <= date.range[2])
  }
}



