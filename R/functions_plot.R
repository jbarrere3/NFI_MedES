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
    select(IDP, year) %>%
    left_join(data_services, by = "IDP") %>%
    gather(key = "service", value = "value", service_table$service) %>%
    left_join(service_table, by = "service") %>%
    group_by(year, title) %>%
    summarize(mean = mean(value, na.rm = TRUE), 
              se = sd(value, na.rm = TRUE)) %>% 
    ggplot(aes(x = year, y = mean)) + 
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0) + 
    geom_point() + 
    facet_wrap(~ title, scales = "free") + 
    geom_smooth(method = "loess") +
    theme_bw() + 
    ylab("Ecosystem services\nvalue")
  
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
plot_spatial_services = function(NFIMed_plot, data_services, service_table, file.out){
  
  # Create output directory if needed
  create_dir_if_needed(file.out)
  
  # Spatialize the floristic data
  data_sf = NFIMed_plot %>%
    select(-ecoregion) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>%
    left_join(data_services, by = "IDP") %>%
    gather(key = "service", value = "value", service_table$service)
  
  # Initialize the list that will contain each plot
  plotlist.out = vector(mode = "list", length = dim(service_table)[1])
  
  # Loop on all services
  for(i in 1:dim(service_table)[1]){
    plotlist.out[[i]] = ne_countries(scale = "medium", returnclass = "sf") %>%
      ggplot(aes(geometry = geometry)) +
      geom_sf(fill = "#343A40", color = "gray", show.legend = F, size = 0.2) + 
      geom_sf(data = subset(data_sf, service == service_table$service[i]), 
              aes(color = value), size = 0.0001, shape = 20) +
      scale_color_gradient(low = "red", high = "green") +
      coord_sf(xlim = c(-5, 10), ylim = c(41.2, 51.1)) +
      theme(panel.background = element_rect(color = 'black', fill = 'white'), 
            panel.grid = element_blank(), 
            legend.title = element_blank(), 
            legend.key = element_blank(), 
            plot.title = element_text(face = "bold", hjust = 0.5)) + 
      ggtitle(service_table$title[i])
  }
  
  # Assemble plots
  plot.out = plot_grid(plotlist = plotlist.out, align = "hv", nrow = 2)
  
  # - Save the plot
  ggsave(file.out, plot.out, width = 25, height = 14, 
         units = "cm", dpi = 600, bg = "white")
  
  # return the name of the plot exported
  return(file.out)
}
