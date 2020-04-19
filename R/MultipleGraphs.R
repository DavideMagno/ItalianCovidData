get_plot_bootstrapjs_div <- function(plot_object_list, id_prefix) {
  #### local_function
  
  get_col_div <- function(plot_object_list, id_prefix, index, css_class = 'col-xs-12 col-sm-6')  {
    css_class <- paste0('col-sm-',12/length(plot_object_list))
    col_div <- div(class = css_class)
    
    if(length(plot_object_list) >= index) {
      plot_name <- paste0(id_prefix, '_', index)
      plot_output_object <- plotly::plotlyOutput(plot_name)
      plot_output_object <- plotly::renderPlotly(plot_object_list[[index]])
      col_div <- shiny::tagAppendChild(col_div, plot_output_object)
    }
    return(col_div)
  }
  #
  get_plot_div <- function(plot_object_list, id_prefix) {
    result_div <- div(class = 'container-fluid')
    row_div <- div(class = 'row')
    for(i in 1:length(plot_object_list)) {
      row_div <- shiny::tagAppendChild(row_div, get_col_div(plot_object_list, id_prefix, i))  
    }
    result_div <- shiny::tagAppendChild(result_div, row_div)
    return(result_div)
  }
  ####
  plot_output_list_div <- get_plot_div(plot_object_list, id_prefix)
  
  return(plot_output_list_div)
}

get_plot_object_list <- function(data.field, data, logscale, return.flag) {
  result_plot_list <- purrr::map(data.field, function(i) {
    if ("Province" %in% colnames(data)) {
      colour <- "Province"
    } else {
      colour <- "Region"
    }
    
    rmean7 <- data %>% 
      dplyr::group_by_at(dplyr::vars(colour)) %>%  
      dplyr::mutate_if(is.numeric,
                       dplyr::funs(stats::filter(., rep(1/7, 7), side=2))) 
    
    pp <- plotly::plot_ly(data) %>%  
      plotly::add_trace(x = ~Date, y = ~get(i), color = ~get(colour), 
                        type = "scatter",
                        mode = "lines+markers", opacity = 0.2,
                        showlegend = FALSE) %>% 
      plotly::add_lines(data = rmean7, x = ~Date, y = ~get(i), 
                        color = ~get(colour)) %>% 
      plotly::layout(legend = list(orientation = 'h'),
                     yaxis = list(type = logscale,
                                  title = i)) %>% 
      plotly::config(displayModeBar = FALSE)
    
    if(return.flag) {
      pp %<>% 
        plotly::layout(yaxis = list(tickformat = ".2%"))
    }
    return(pp)
  })
  return(result_plot_list)
}

get_plot_output_list_div <- function(data.field, data, logscale = FALSE,
                                     return.flag = FALSE) {
  plot_object_list <- get_plot_object_list(data.field, data, logscale, 
                                           return.flag)
  plot_output_div <- get_plot_bootstrapjs_div(plot_object_list, 'ui_plot')
  return(plot_output_div)
}