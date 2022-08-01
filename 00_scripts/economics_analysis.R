plot_forecast_function <-
function(title = "Inflation", model = "Arima") {
    
       
        data <- full_forecast_tbl %>% filter(str_detect(name, str_to_lower(title)),
                          str_detect(.model_desc, str_to_upper(model)) |
                       .model_desc %in% ("ACTUAL")) %>% 
            rename(Model = .model_desc, Date = .index, Rate = .value)
        
        
        g <- data %>% ggplot(aes(x = Date,   y = Rate)) +
        geom_line(aes(color      = Model)) +
        scale_x_date(date_breaks = "8 years", 
                     date_labels = "%Y") +
        scale_y_continuous(labels = scales::label_percent(scale = 1)) +
        
        scale_color_tq() +
        theme_tq() +
        labs(title = str_glue("U.S. {title} Rate Forecast Plot"), x = "", y = "") +
        theme(
            legend.title    = element_text(size =10), 
            legend.text     = element_text(size = 8),
            plot.title      = element_text(
                hjust       = 0.5,
                size        = 18,
                lineheight  = 0,
                family = "Times New Roman"
            ),
            axis.text = element_text(family = "Times New Roman", size = 12)
        )  
    
    plotly::ggplotly(g)
    
}
plot_analytics_function <-
function(...) {
    
    group_vars_expr <- quos(...)
    
    data <- full_forecast_tbl %>% filter(.model_desc %in% ("ACTUAL")) %>% 
        filter(name %in% c(!!!group_vars_expr)) %>% 
        rename(Model = .model_desc, Date = .index, Rate = .value, Name = name) %>% 
        mutate(Name  = str_to_title(str_replace(Name, pattern = "_", replacement = " "))) %>% 
        mutate(Name  = forcats::as_factor(Name))
    
    g <- data %>% ggplot(aes(x = Date, y = Rate)) +
        geom_line(aes(color      =  Name)) +
        scale_x_date(date_breaks = "8 years", 
                     date_labels = "%Y") +
        scale_y_continuous(labels = scales::label_percent(scale = 1)) +
        scale_color_tq() + 
        geom_hline(yintercept = 5, linetype = "twodash") +
        theme_tq() +
        labs(title = str_glue("U.S. Economic Rates Plot"), x = "", y = "") +
        theme(
            legend.title    = element_text(size =10), 
            legend.text     = element_text(size = 8),
            plot.title      = element_text(
                hjust       = 0.5,
                size        = 18,
                lineheight  = 0,
                family = "Times New Roman"
            ),
            axis.text = element_text(family = "Times New Roman", size = 12)
        )  
    
    plotly::ggplotly(g)
    
}
plot_seasonal_function <-
function(title = "Inflation"){
    
    full_forecast_tbl %>% filter(str_detect(name, str_to_lower(title)),
                                             .model_desc %in% ("ACTUAL")) %>% 
        
        rename(Model = .model_desc, Date = .index, Rate = .value) %>% 
        
        plot_seasonal_diagnostics(.date_var = Date, 
                                  .value = Rate, 
                                  .title = str_glue("{title} Seasonal Diagnostics")
        
        )

}
plot_stl_function <-
function(title = "Inflation"){
    
    full_forecast_tbl %>% filter(str_detect(name, str_to_lower(title)),
                                 .model_desc %in% ("ACTUAL")) %>% 
        
        rename(Model = .model_desc, Date = .index, Rate = .value) %>% 
        
        plot_stl_diagnostics(.date_var = Date, 
                                  .value = Rate, 
                                  .title = str_glue("{title} STL Diagnostics"),
                                  
        )
    
}
accuracy_metrics <-
function(title = "inflation_rate", model = "MODEL:ARIMA", metric = "mape") {
        
        value <- full_accuracy_tbl %>%  
            filter(str_detect(name, str_to_lower(title)),
                   str_detect(.model_desc, str_to_upper(model))) %>% 
            pull(metric) 
        
        ifelse(value == "-", value, as.numeric(value) %>% round(3))
            
        
    }
info_card <-
function(title, value,
                      main_icon = "chart-line",
                      bg_color = "default", text_color = "default") {
    
    div(
        class = "panel panel-default",
        style = "padding: 0px;",
        div(
            class = str_glue("panel-body bg-{bg_color} text-{text_color}"),
            p(class = "pull-right", icon(class = "fa-1x", main_icon)),
            h4(title),
            h5(value),

        )
    )
    
}
