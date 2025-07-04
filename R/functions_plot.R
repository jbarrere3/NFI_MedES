#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#### SCRIPT INTRODUCTION ####
#
#' @name functions_plot.R  
#' @description R script containing all functions relative to plots
#' @author Julien Barrere
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### -- Plots for main analyses ------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#' Function to plot the spatial trend of floristic ecosystem services
#' @param NFIMed_plot Plot level NFI data
#' @param data_services plot level ecosystem services 
#' @param service_table table listing services name and title
#' @param file.out Name of the file to save, including path
plot_spatial_services = function(NFIMed_plot, data_services, service_table, 
                                 sylvoER_shp_file, file.out){
  
  # Create output directory if needed
  create_dir_if_needed(file.out)
  
  # Average services across sylvoecoregions
  data_ser = NFIMed_plot %>%
    select(IDP, ecoregion) %>%
    left_join(data_services, by = "IDP") %>%
    gather(key = "service", value = "value", service_table$service) %>%
    group_by(ecoregion, service) %>%
    summarize(mean = mean(value, na.rm = TRUE)) %>%
    pivot_wider(names_from = "service", values_from = "mean") %>%
    ungroup()
  
  # Build dataset for plotting
  sylvoER = read_sf(sylvoER_shp_file, crs = 2154) %>%
    st_transform(crs = 4326) %>%
    select(ecoregion = codeser) %>%
    left_join(data_ser, by = "ecoregion") %>%
    gather(key = "service", value = "value", service_table$service)
  
  # Initialize the lists that will contain each plot
  plotlist.flux.out = vector(
    mode = "list", length = length(which(service_table$type == "flux")))
  plotlist.capacity.out = vector(
    mode = "list", length = length(which(service_table$type == "capacity")))
  
  # Initialize counter for capacity and flux
  k_flux = 0
  k_capacity = 0
  
  # Loop on all services
  for(i in 1:dim(service_table)[1]){
    plot.i = ne_countries(scale = "medium", returnclass = "sf") %>%
      ggplot(aes(geometry = geometry)) +
      geom_sf(fill = "#778DA9", show.legend = F, size = 0.2) + 
      geom_sf(data = subset(sylvoER, service == service_table$service[i]), 
              aes(fill = value)) +
      scale_fill_gradient(low = "#CCFF33", high = "#007200") +
      coord_sf(xlim = c(-5, 10), ylim = c(41.2, 51.5)) +
      theme(panel.background = element_rect(color = 'black', fill = 'white'), 
            panel.grid = element_blank(), 
            legend.title = element_blank(), 
            legend.key = element_blank(), 
            legend.position = c(0.9, 0.5),
            legend.background = element_rect(color = '#778DA9', fill = 'white'),
            plot.title = element_text(hjust = 0.5, size = 18), 
            axis.text = element_text(size = 13)) + 
      ggtitle(service_table$title[i])
    
    # Add plot i to the right list
    if(service_table$type[i] == "capacity"){
      k_capacity = k_capacity + 1
      plotlist.capacity.out[[k_capacity]] = plot.i
    }
    if(service_table$type[i] == "flux"){
      k_flux = k_flux + 1
      plotlist.flux.out[[k_flux]] = plot.i
    }
  }
  
  # Assemble plots
  plot.out = plot_grid(
    plot_grid(plotlist = plotlist.capacity.out, align = "hv", nrow = 3), 
    plot_grid(plotlist = plotlist.flux.out, align = "hv", nrow = 3),
    nrow = 1, rel_widths = c(1, 0.5), labels = c("(a) ES capacity", "(b) ES flux"), 
    scale = 0.9, align = "hv", label_size = 22)
  
  # - Save the plot
  ggsave(file.out, plot.out, width = 35, height = 35, 
         units = "cm", dpi = 600, bg = "white")
  
  # return the name of the plot exported
  return(file.out)
  
}


#' Function to run the first par of the analysis
#' @param models_plot List with the models fitted at plot level
#' @param data_services data containing all services per plot
#' @param dir.diagnostic directory where to store diagnostic plots
#' @param file.out name of the file for the main figure
make_plots_analysis1 = function(models_plot, service_table, 
                                dir.diagnostic, file.out){
  
  # Create if needed directory for diagnostic plots and main plot
  files.diagnostic = paste0(dir.diagnostic, "/diagnostic_",
                            service_table$service, ".jpg")
  create_dir_if_needed(files.diagnostic[1])
  
  # Identify explanatory variables
  terms = attr(terms(models_plot$models[[1]]), "term.labels")
  explanatory_vars = terms[-unique(c(grep("2", terms), c(grep(":", terms))))]
  
  # Initialize the lists that will contain each estimate plot
  plotlist.flux.out = vector(
    mode = "list", length = length(which(service_table$type == "flux")))
  plotlist.capacity.out = vector(
    mode = "list", length = length(which(service_table$type == "capacity")))
  
  # Initialize counter for capacity and flux
  k_flux = 0
  k_capacity = 0
  
  # Loop on all services
  for(i in 1:dim(service_table)[1]){
    
    # Make the estimate plot
    plot.i = data.frame(var = names(coefficients(models_plot$models[[i]])), 
                        estimate = as.numeric(coefficients(models_plot$models[[i]])), 
                        lwr = models_plot$interval[[i]][, 1], 
                        upr = models_plot$interval[[i]][, 2]) %>%
      mutate(estimate.abs = abs(estimate)) %>%
      filter(var != "(Intercept)") %>%
      arrange(estimate.abs) %>%
      mutate(var = factor(var, levels = .$var))  %>%
      mutate(S = case_when(lwr > 0 ~ "significantly positive (p < 0.05)", 
                           upr < 0 ~ "significantly negative (p < 0.05)", 
                           TRUE ~ "not-significant (p > 0.05)")) %>%
      ggplot(aes(x = estimate, y = var, color = S, fill = S)) +
      geom_errorbarh(aes(xmin = lwr, xmax = upr), height = 0.2) +
      geom_point(shape = 21) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      scale_color_manual(values = c("significantly positive (p < 0.05)" = "#8C1C13", 
                                    "significantly negative (p < 0.05)" = "#22577A", 
                                    "not-significant (p > 0.05)" = "#6C757D")) +
      scale_fill_manual(values = c("significantly positive (p < 0.05)" = "#BF4342", 
                                   "significantly negative (p < 0.05)" = "#38A3A5", 
                                   "not-significant (p > 0.05)" = "#CED4DA")) +
      labs(x = "Standardized Estimate", y = "") +
      theme(legend.position = "none", 
            panel.background = element_rect(fill = "white", color = "black"), 
            panel.grid = element_line(color = "lightgrey", linetype = "dotted", linewidth = 0.5),
            axis.text = element_text(size = 12), 
            plot.title = element_text(face = "bold", hjust = 0.5)) + 
      ggtitle(service_table$title[i])
    
    # Save diagnostic plot
    ggsave(filename = files.diagnostic[i], plot = models_plot$diag[[i]],
           width = 12, height = 5, dpi = 600 )
    
  }
  
  # Assemble plots
  plot.out = plot_grid(
    plot_grid(plotlist = plotlist.capacity.out, align = "hv", nrow = 3), 
    plot_grid(plotlist = plotlist.flux.out, align = "hv", nrow = 3),
    nrow = 1, rel_widths = c(1, 0.5), labels = c("(a) ES capacity", "(b) ES flux"), 
    scale = 0.9, align = "hv", label_size = 22)
  
  # - Save the plot
  ggsave(file.out, plot.out, width = 35, height = 40, 
         units = "cm", dpi = 600, bg = "white")
  
  # return the name of the plot exported
  return(c(file.out, files.diagnostic))
  
}

#' Function to map the temporal trend of each ecosystem services
#' @param temporal_trend Temporal trend of each service in each ecoregion
#' @param sylvoER_shp_file sylvoecoregion shapefile
#' @param service_table Table with distrib and and title of each service
#' @param file.out Name of the file to save, including path
map_temporal_trend = function(temporal_trend, service_table, sylvoER_shp_file, file.out){
  
  # Create output directory if needed
  create_dir_if_needed(file.out)
  
  # Prepare dataset
  data.plot = read_sf(sylvoER_shp_file, crs = 2154) %>%
    st_transform(crs = 4326) %>%
    select(ecoregion = codeser)
  
  
  # Initialize the lists that will contain each plot
  plotlist.flux.out = vector(
    mode = "list", length = length(which(service_table$type == "flux")))
  plotlist.capacity.out = vector(
    mode = "list", length = length(which(service_table$type == "capacity")))
  
  # Initialize counter for capacity and flux
  k_flux = 0
  k_capacity = 0
  
  # Loop on all variables to plot
  for(i in 1:dim(service_table)[1]){
    
    # Satial data for service i
    data.i = data.plot %>%
      left_join(temporal_trend %>% 
                  filter(service == service_table$service[i]) %>%
                  mutate(estimate = ifelse(pval < 0.05 & n >= 50, estimate, 0)) %>%
                  select(ecoregion, estimate), 
                by = "ecoregion")
    
    # Make the plot for service i
    plot.i = ne_countries(scale = "medium", returnclass = "sf") %>%
      ggplot(aes(geometry = geometry)) +
      geom_sf(fill = "#778DA9", show.legend = F, size = 0.2) + 
      geom_sf(data.i, mapping = aes(fill = estimate)) +
      scale_fill_gradient2(
        low = '#0FA3B1', mid = 'white', high = '#CB1B16', midpoint = 0) + 
      coord_sf(xlim = c(-5, 10), ylim = c(41.2, 51.5)) +
      theme(panel.background = element_rect(color = 'black', fill = 'white'), 
            panel.grid = element_blank(), 
            legend.title = element_blank(), 
            legend.key = element_blank(), 
            legend.position = c(0.9, 0.5),
            legend.background = element_rect(color = '#778DA9', fill = 'white'),
            plot.title = element_text(hjust = 0.5, size = 18), 
            axis.text = element_text(size = 13)) + 
      ggtitle(paste0("Trend in \n", gsub("\\(.+\\)", "", service_table$title[i]), 
                     " (%/yr)"))
    
    # Add plot i to the right list
    if(service_table$type[i] == "capacity"){
      k_capacity = k_capacity + 1
      plotlist.capacity.out[[k_capacity]] = plot.i
    }
    if(service_table$type[i] == "flux"){
      k_flux = k_flux + 1
      plotlist.flux.out[[k_flux]] = plot.i
    }
  }
  
  # Assemble plots
  plot.out = plot_grid(
    plot_grid(plotlist = plotlist.capacity.out, align = "hv", nrow = 3), 
    plot_grid(plotlist = plotlist.flux.out, align = "hv", nrow = 3),
    nrow = 1, rel_widths = c(1, 0.5), labels = c("(a) ES capacity", "(b) ES flux"), 
    scale = 0.9, align = "hv", label_size = 22)
  
  # - Save the plot
  ggsave(file.out, plot.out, width = 35, height = 35, 
         units = "cm", dpi = 600, bg = "white")
  
  # return the name of the plot exported
  return(file.out)
  
}

#' Analyse the drivers of temporal trens in ecosystem services
#' @param temporal_trend estimate per service and ecoregion of the temporal trend
#' @param data_explanatory_ser explanatory variables at ecoregion scale
#' @param service_table table indicating the title and distribution of each service
#' @param file.out Name of the file to save, including path
make_plot_analysis2 = function(temporal_trend, data_explanatory_ser, 
                               service_table, file.out){
  
  # Create output directory if needed
  create_dir_if_needed(file.out)
  
  # Explanatory variables
  var.expl = colnames(data_explanatory_ser)[-1]
  
  # Formula for model fitting
  formula.in = as.formula(paste("trend ~", paste(var.expl, collapse = " + ")))
  
  for(i in 1:dim(service_table)[1]){
    
    # Prepare dataset
    data.i = temporal_trend %>%
      filter(service == service_table$service[i]) %>%
      mutate(weight = dim(.)[1]*n/sum(n, na.rm = TRUE)) %>%
      select(ecoregion, trend = estimate, weight) %>%
      left_join(data_explanatory_ser, by = "ecoregion")  %>%
      mutate(across(c(4:dim(.)[2]), ~ scale(., center = TRUE, scale = TRUE)[, 1]))
    
    # Run model
    model.i = lm(formula.in, data = data.i, weights = weight)
    
    # Output of the model
    stat.i = tidy(model.i, conf.int = TRUE) %>% 
      filter(term != "(Intercept)") %>%
      mutate(service = service_table$service[i]) %>% 
      select(service, term, estimate, se = std.error, p.val = p.value, 
             conf.low, conf.high)
    
    # Add to final output data
    if(i == 1) out = stat.i
    else out = rbind(out, stat.i)
    
  }
  
  # Create a visualization of the estimates with confidence intervals
  plot.estimate.out <- out %>%
    left_join(service_table %>% select(service, title), 
              by = "service") %>%
    mutate(S = case_when(conf.low > 0 ~ "significantly positive (p < 0.05)", 
                         conf.high < 0 ~ "significantly negative (p < 0.05)", 
                         TRUE ~ "not-significant (p > 0.05)"), 
           title = paste0("Trend in \n", gsub("\\(.+\\)", "", title), " (%/yr)")) %>%
    ggplot(aes(x = estimate, y = term, color = S, fill = S)) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    geom_point(shape = 21) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    facet_wrap(~ title, scales = "free") +
    scale_color_manual(values = c("significantly positive (p < 0.05)" = "#8C1C13", 
                                  "significantly negative (p < 0.05)" = "#22577A", 
                                  "not-significant (p > 0.05)" = "#6C757D")) +
    scale_fill_manual(values = c("significantly positive (p < 0.05)" = "#BF4342", 
                                 "significantly negative (p < 0.05)" = "#38A3A5", 
                                 "not-significant (p > 0.05)" = "#CED4DA")) +
    labs(x = "Standardized Estimate", y = "") +
    theme(legend.position = "bottom", 
          panel.background = element_rect(fill = "white", color = "black"), 
          panel.grid = element_line(color = "lightgrey", linetype = "dotted", linewidth = 0.5),
          strip.background = element_blank(), 
          legend.title = element_blank(), 
          legend.key = element_blank(), 
          strip.text = element_text(face = "bold", size = 12), 
          legend.text = element_text(size = 14), 
          axis.text = element_text(size = 12))
  
  # Save the plot generated
  ggsave(file.out, plot.estimate.out, width = 30, height = 14, 
         units = "cm", dpi = 600, bg = "white")
  
  # Return the files generated
  return(file.out)
  
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### -- Plots for methods and supplementary ------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' Function to show the relation between each explanatory and response variables
#' @param data_explanatory explanatory variables compiled at the plot level
#' @param data_services dataset containing all services per plot
#' @param service_table table attributing a title to each service
#' @param file.out name of the file to save, including path
plot_exploratory = function(data_explanatory, data_services, service_table, file.out){
  
  # Create the output directory if needed
  create_dir_if_needed(file.out)
  
  # First, join the two datasets
  combined_data <- inner_join(data_explanatory, data_services, by = "IDP")
  
  # Identify your explanatory and response variables
  explanatory_vars <- c("ba.tot", "dqm", "str.div", "shannon", "richness", 
                        "elev", "pca_clim")
  response_vars <- service_table$service
  
  ## - Plot the relation between each explanatory variable and each response variable
  
  # Function to create binned data for one explanatory-response pair
  create_binned_data <- function(expl_var, resp_var, data, n_bins = 10) {
    data %>%
      # Create equal-sized bins based on explanatory variable
      mutate(bin = ntile(!!sym(expl_var), n_bins)) %>%
      group_by(bin) %>%
      summarise(
        expl_mean = mean(!!sym(expl_var), na.rm = TRUE),
        resp_mean = mean(!!sym(resp_var), na.rm = TRUE),
        resp_sd = sd(!!sym(resp_var), na.rm = TRUE),
        n = n(),
        resp_se = resp_sd / sqrt(n)
      ) %>%
      mutate(
        expl_var = expl_var,
        resp_var = resp_var
      )
  }
  
  # Create all combinations of explanatory and response variables
  combinations <- expand.grid(
    explanatory = explanatory_vars,
    response = response_vars,
    stringsAsFactors = FALSE
  )
  
  # Create binned data for all combinations
  binned_data <- map2_df(
    combinations$explanatory,
    combinations$response,
    ~ create_binned_data(.x, .y, combined_data)
  )
  
  # Create the grid plot
  plot.out <- ggplot(binned_data, aes(x = expl_mean, y = resp_mean)) +
    geom_errorbar(aes(ymin = resp_mean - resp_sd, ymax = resp_mean + resp_sd), 
                  width = 0, alpha = 0.5) +
    geom_point(shape = 21, color = "black", color = "lightblue") +
    geom_smooth(method = "loess", se = FALSE, color = "blue", size = 0.5) +
    facet_grid(resp_var ~ expl_var, scales = "free") +
    labs(x = "Explanatory variable (binned)", 
         y = "Response variable mean") +
    theme(
      panel.background = element_rect(fill = "white", color = "black"), 
      panel.grid = element_blank(), 
      strip.background = element_blank(), 
      axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
      axis.text.y = element_text(size = 6),
      strip.text = element_text(size = 8)
    )
  
  # - Save the plot
  ggsave(file.out, plot.out, width = 19, height = 14, 
         units = "cm", dpi = 600, bg = "white")
  
  # - Return the file saved
  return(file.out)
  
}


#' Function to map the main variables related to sylvoecoregions
#' @param data_explanatory_ser explanatory variables at the ecoregion level
#' @param sylvoER_shp_file sylvoecoregion shapefile
#' @param file.out Name of the file to save, including path
map_explanatory_ser = function(data_explanatory_ser, sylvoER_shp_file, file.out){
  
  # Create output directory if needed
  create_dir_if_needed(file.out)
  
  # Prepare dataset
  data.plot = read_sf(sylvoER_shp_file, crs = 2154) %>%
    st_transform(crs = 4326) %>%
    select(ecoregion = codeser) %>%
    left_join(data_explanatory_ser, by = "ecoregion") %>%
    gather(key = "var", value = "value", "dist.freq", "prop.management", 
           "pca1_str", "pca2_str", "pca_clim.mean")
  
  # Table linking each variable to a title
  table.var = data.frame(var = unique(data.plot$var)) %>%
    mutate(title = case_when(
      var == "dist.freq" ~ "Disturbance\nfrequency", 
      var == "prop.management" ~ "Proportion of\nmanaged plots", 
      var == "pca_clim.mean" ~ "Average climate\n(pca_clim)",
      var == "pca1_str" ~ "Dominant structure\naxis 1 (pca1_str)", 
      var == "pca2_str" ~ "Dominant structure\naxis 2 (pca2_str)"))
  
  # Initialize plotlist
  plotlist.out = vector(mode = "list", length = dim(table.var)[1])
  
  # Loop on all variables to plot
  for(i in 1:length(plotlist.out)){
    
    plotlist.out[[i]] = ne_countries(scale = "medium", returnclass = "sf") %>%
      ggplot(aes(geometry = geometry)) +
      geom_sf(fill = "#778DA9", show.legend = F, size = 0.2) + 
      geom_sf(data = subset(data.plot, var == table.var$var[i]), 
              aes(fill = value)) +
      scale_fill_gradient(low = "#B5E2FA", high = "#0FA3B1") +
      coord_sf(xlim = c(-5, 10), ylim = c(41.2, 51.5)) +
      theme(panel.background = element_rect(color = 'black', fill = 'white'), 
            panel.grid = element_blank(), 
            legend.title = element_blank(), 
            legend.key = element_blank(), 
            legend.position = c(0.9, 0.5),
            legend.background = element_rect(color = '#778DA9', fill = 'white'),
            plot.title = element_text(hjust = 0.5, size = 18), 
            axis.text = element_text(size = 13)) + 
      ggtitle(table.var$title[i])
    
  }
  
  # Assemble plots
  plot.out = plot_grid(plotlist = plotlist.out, align = "hv", nrow = 2, scale = 0.9)
  
  # - Save the plot
  ggsave(file.out, plot.out, width = 35, height = 25, 
         units = "cm", dpi = 600, bg = "white")
  
  # return the name of the plot exported
  return(file.out)
  
}


