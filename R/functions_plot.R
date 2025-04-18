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


#' Function to plot the temporal trend of floristic ecosystem services
#' @param NFIMed_plot Plot level NFI data
#' @param data_services plot level ecosystem services 
#' @param service_table table listing services name and title
#' @param file.out Name of the file to save, including path
plot_temporal_services = function(NFIMed_plot, data_services, service_table, file.out){
  
  # Create output directory if needed
  create_dir_if_needed(file.out)
  
  # Make the plot
  plot.out = NFIMed_plot %>%
    mutate(biome = case_when(
      ecoregion %in% c("J10", "J21", "J22", "J23", "J24", "J30", 
                       "J40", "K11", "K13") ~ "mediterranean_forest", 
      ecoregion %in% c("D12", "D11", "E10", "E20", "G30", "G22", "G42", "G70", 
                       "I11", "I12", "I13", "I21", "I22", "K12", "H10", "H21", 
                       "H22", "H30", "H41", "H42") ~ "temperate_mountain_forest", 
      TRUE ~ "temperate_plain_forest"
    )) %>%
    select(IDP, year, biome) %>%
    left_join(data_services, by = "IDP") %>%
    gather(key = "service", value = "value", service_table$service) %>%
    left_join(service_table, by = "service") %>%
    group_by(year, biome, title) %>%
    summarize(mean = mean(value, na.rm = TRUE), 
              lwr = quantile(value, 0.25, na.rm = TRUE), 
              upr = quantile(value, 0.75, na.rm = TRUE)) %>% 
    ungroup() %>%
    mutate(year = case_when(
      biome == "mediterranean_forest" ~ year + 0.2, 
      biome == "temperate_mountain_forest" ~ year - 0.2, 
      TRUE ~ year)) %>%
    ggplot(aes(x = year, y = mean, color = biome, fill = biome)) + 
    geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.15) + 
    geom_point(shape = 21) + 
    facet_wrap(~ title, scales = "free") + 
    geom_smooth(method = "loess") +
    ylab("Ecosystem services value") + 
    scale_color_manual(values = c(`mediterranean_forest` = "#BB3E03", 
                                  `temperate_mountain_forest` = "#001D3D", 
                                  `temperate_plain_forest` = "#386641")) + 
    scale_fill_manual(values = c(`mediterranean_forest` = "#EE9B00", 
                                 `temperate_mountain_forest` = "#0077B6", 
                                 `temperate_plain_forest` = "#A7C957")) + 
    theme(panel.background = element_rect(fill = "white", color = "black"), 
          panel.grid = element_blank(), 
          legend.title = element_blank(), 
          legend.key = element_blank(), 
          strip.background = element_blank())
  
  # - Save the plot
  ggsave(file.out, plot.out, width = 19, height = 9, 
         units = "cm", dpi = 600, bg = "white")
  
  # return the name of the plot exported
  return(file.out)
}

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
    plot_grid(plotlist = plotlist.capacity.out, align = "hv", nrow = 2), 
    plot_grid(plotlist = plotlist.flux.out, align = "hv", nrow = 2),
    nrow = 1, rel_widths = c(1, 0.5), labels = c("(a) ES capacity", "(b) ES flux"), 
    scale = 0.9, align = "hv", label_size = 22)
  
  # - Save the plot
  ggsave(file.out, plot.out, width = 38, height = 25, 
         units = "cm", dpi = 600, bg = "white")
  
  # return the name of the plot exported
  return(file.out)
  
}
