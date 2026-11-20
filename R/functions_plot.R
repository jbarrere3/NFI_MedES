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
#### -- Generic functions ------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#' Function to generate diagnostic plots
#' @param model model to diagnostic
#' @param response_var Response variable (service) considered
#' @param distribution character specifying the type of error distribution
generate_diagnostics <- function(model, response_var, distribution) {
  print(paste0("Generating diagnostics for model of ", response_var))
  # Get residuals and fitted values
  if (distribution == "pos0") {
    # Tweedie requires special handling
    residuals <- residuals(model, type = "pearson")
    fitted <- fitted(model)
    sqrt_abs_resid <- sqrt(abs(residuals))
  } else {
    residuals <- residuals(model, type = "deviance")
    fitted <- fitted(model)
    sqrt_abs_resid <- sqrt(abs(residuals))
  }
  
  diag_data <- tibble(
    fitted = fitted,
    residuals = residuals,
    sqrt_abs_resid = sqrt_abs_resid
  )
  
  # Residuals vs Fitted
  print("---plot residual vs fitted")
  p1 <- ggplot(diag_data, aes(fitted, residuals)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_smooth(se = FALSE, color = "blue") +
    labs(title = "Residuals vs Fitted", 
         subtitle = paste("Distribution:", distribution),
         x = "Fitted Values", y = "Residuals")
  
  # Q-Q Plot
  print("---qqplot")
  p2 <- ggplot(diag_data, aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(title = "Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles")
  
  # Scale-Location
  print("---scale-location")
  p3 <- ggplot(diag_data, aes(fitted, sqrt_abs_resid)) +
    geom_point(alpha = 0.6) +
    geom_smooth(se = FALSE, color = "blue") +
    labs(title = "Scale-Location", x = "Fitted Values", y = "√|Standardized Residuals|")
  
  # Combine plots
  (p1 | p2 | p3) + 
    plot_annotation(title = paste("Diagnostics for:", response_var))
}



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
    mutate(Climate = case_when(
      ecoregion %in% c("J10", "J21", "J22", "J23", "J24", 
                       "J30", "J40", "K11", "K13") ~ "Mediterranean", 
      ecoregion %in% c("H10", "H21", "H22", "H30", "H41", "H42", "K12", 
                       "D11", "D12", "I11", "I12", "I13", "I21", "I22") ~ "Alpine",
      TRUE ~ "Temperate"
    )) %>%
    select(IDP, ecoregion, Climate) %>%
    left_join(data_services, by = "IDP") %>%
    gather(key = "service", value = "value", service_table$service) %>%
    group_by(ecoregion, service, Climate) %>%
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
  plotlist.out = vector(
    mode = "list", length = dim(service_table)[1])
  
  # Loop on all services
  for(i in 1:dim(service_table)[1]){
    plot.i = ne_countries(scale = "medium", returnclass = "sf") %>%
      ggplot(aes(geometry = geometry)) +
      geom_sf(fill = "#778DA9", show.legend = F, size = 0.2) + 
      geom_sf(data = subset(sylvoER, service == service_table$service[i]), 
              aes(fill = value, color = Climate), size = 0.5) +
      scale_color_manual(values = c(`Alpine` = "#48CAE4", 
                                    `Temperate` = "#6A994E", 
                                    `Mediterranean` = "#EE964B")) +
      scale_fill_gradient(low = "#CCFF33", high = "#007200") +
      coord_sf(xlim = c(-5, 10), ylim = c(41.2,
                                          51.5)) +
      guides(color = "none") +                 
      theme(panel.background = element_rect(color = 'black', fill = 'white'), 
            panel.grid = element_blank(), 
            legend.title = element_blank(), 
            legend.key = element_blank(), 
            legend.position = c(0.9, 0.5),
            legend.background = element_rect(color = '#778DA9', fill = 'white'),
            axis.text = element_blank(), 
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 17)) + 
      ggtitle(paste0("(", letters[i], ") ", service_table$title[i]))
    
    # Add plot i to the list
    plotlist.out[[i]] = plot.i
  }
  
  # Assemble plots
  plot.out = plot_grid(plotlist = plotlist.out, align = "hv", nrow = 2)
  
  # - Save the plot
  ggsave(file.out, plot.out, width = 38, height = 20, 
         units = "cm", dpi = 600, bg = "white")
  
  # return the name of the plot exported
  return(file.out)
  
}


#' Function to plot the correlation between ecosystem services
#' @param NFIMed_plot Plot level NFI data
#' @param data_services plot level ecosystem services 
#' @param service_table table listing services name and title
#' @param dir.out Name of the directory containing files to save, including path
plot_pca_plot = function(
    NFIMed_plot, data_services, data_explanatory, service_table, dir.out){
  
  
  # Create output directory if needed
  create_dir_if_needed(paste0(dir.out, "/test"))
  
  # Run pca with ecosystem services
  # - Subset the dataset to be compatible with pca
  data.pca.in = data_services %>% dplyr::select(-climregul_dh.yr) %>% drop_na()
  # - Run pca
  pca = prcomp(data.pca.in %>% dplyr::select(-IDP), center = T, scale = T)
  # - Extract outputfor individual observations
  data.pca.out = data.pca.in %>%
    mutate(PC1 = get_pca_ind(pca)[[1]][, 1], 
           PC2 = get_pca_ind(pca)[[1]][, 2], 
           PC3 = get_pca_ind(pca)[[1]][, 3]) %>%
    filter(PC3 < quantile(.$PC3, 0.999)) %>%
    dplyr::select(IDP, PC1, PC2, PC3)
  # - Extract output for variables
  var_data <- as.data.frame(pca$rotation[, 1:3]) %>%
    mutate(service = rownames(.)) %>%
    left_join(service_table[, c("service", "title")], by = "service") %>%
    drop_na()
  var_percent <- pca$sdev^2 / sum(pca$sdev^2) * 100
  
  # Prepare data for plotting
  # - Reshape variable loadings into long format
  var_long = pca$rotation[, 1:3] %>%
    as.data.frame() %>%
    mutate(service = rownames(.)) %>%
    left_join(service_table[, c("service", "title")], by = "service") %>%
    drop_na() %>%
    pivot_longer(cols = c(PC1, PC2, PC3), names_to = "axis", values_to = "loading") %>%
    mutate(axis = factor(axis, levels = c("PC1", "PC2", "PC3")))
  # - Reshape individual coordinates into long format
  ind_long = data.pca.out %>%
    pivot_longer(cols = c(PC1, PC2, PC3), names_to = "axis", values_to = "coord") %>%
    mutate(axis = factor(axis, levels = c("PC1", "PC2", "PC3")))
  # - Percent variance per axis
  var_percent = pca$sdev^2 / sum(pca$sdev^2) * 100
  # - Build axis labels combining name and percent variance
  axis_labels = c(paste0("PCA1 (", round(var_percent[1], digits = 2), "%)"),
                  paste0("PCA2 (", round(var_percent[2], digits = 2), "%)"),
                  paste0("PCA3 (", round(var_percent[3], digits = 2), "%)"))
  names(axis_labels) = c("PC1", "PC2", "PC3")
  
  
  # Plot histograms of individual coordinates (top row)
  plot.hist = ind_long %>%
    ggplot(aes(x = coord)) +
    geom_histogram(bins = 20, fill = "lightblue", color = "grey") +
    facet_wrap(~ axis, nrow = 1, scales = "free",
               labeller = labeller(axis = axis_labels)) +
    ylab("Number of NFI plots") +
    xlab("") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    theme(panel.background = element_rect(color = "black", fill = "white"),
          panel.grid = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(size = 9))
  
  # Plot arrows of services on each axis (bottom row)
  plot.var = var_long %>%
    mutate(title = factor(title, levels = rev(service_table$title))) %>%
    ggplot(aes(x = title, xend = title, y = 0, yend = loading)) +
    geom_segment(arrow = arrow(length = unit(0.1, "cm"))) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_wrap(~ axis, nrow = 1, scales = "free_x",
               labeller = labeller(axis = axis_labels)) +
    coord_flip() + xlab("") + ylab("") +
    theme(panel.background = element_rect(fill = "white", color = "black"),
          panel.grid = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(size = 9),
          strip.placement = "outside",
          axis.text.y = element_text(size = 7))
  
  # List all explanatory variables, and their titles
  explanatory_vars = data.frame(
    var = c("ba.tot", "dqm", "str.div", "shannon", "elev", "pca_clim"), 
    title = c("Total\nbasal area", "Mean quadratic\ndiameter", "Structural\ndiversity", 
              "Tree species\ndiversity", "Elevation", "Mean climate\n(Hot-dry vs cold-wet)")
  )
  
  
  # Data to fit the statistical model
  # - Format the data
  data.model = data.pca.out %>%
    left_join(data_explanatory, by = "IDP") %>%
    mutate(PC1.yj = car::yjPower(.$PC1, lambda = 1.3), 
           PC2.yj = car::yjPower(.$PC2, lambda = 1.2), 
           PC3.yj = car::yjPower(.$PC3, lambda = -0.2))
  
  # - Scale explanatory variables
  data.model_scaled = data.model
  data.model_scaled[, explanatory_vars$var] = scale(data.model[, explanatory_vars$var], center = TRUE, scale = TRUE) 
  
  # Fit linear models with scaled (for estimates) and unscaled (for prediction) variables
  # - Initialize a list that will contain the fitted models
  list.models = vector(mode = "list", length = 3)
  names(list.models) = c("PC1", "PC2", "PC3")
  # - Scaled models
  list.models$PC1 = lm(PC1.yj ~ ba.tot + dqm + str.div + shannon + elev + pca_clim, data = data.model_scaled)
  list.models$PC2 = lm(PC2.yj ~ ba.tot + dqm + str.div + shannon + elev + pca_clim, data = data.model_scaled)
  list.models$PC3 = lm(PC3.yj ~ ba.tot + dqm + str.div + shannon + elev + pca_clim, data = data.model_scaled)
  
  # Loop on all models to extract coefficients, and residuals vs fitted values
  for(j in 1:length(names(list.models))){
    
    # Extract residuals vs fitted values
    residuals.j = data.frame(residuals = residuals(list.models[[j]]), 
                             fitted = fitted(list.models[[j]]), 
                             PCA = names(list.models)[j])
    
    # Extract coefficients
    coef.j = as.data.frame(car::Anova(list.models[[j]])) %>% 
      mutate(PCA = names(list.models)[j], 
             var = rownames(.)) %>% 
      filter(var !=  "Residuals") %>% 
      dplyr::select(`PCA`, `var`, `Fvalue` = `F value`, `p` = `Pr(>F)`) %>%
      left_join(data.frame(var = names(coefficients(list.models[[j]])), 
                           est = as.numeric(coefficients(list.models[[j]])), 
                           lwr = as.numeric(confint(list.models[[j]])[, 1]), 
                           upr = as.numeric(confint(list.models[[j]])[, 2])), 
                by = "var") %>%
      mutate(signif = ifelse(p < 0.05, "*", ""), 
             pvalue = scales::pvalue(p, accuracy = 0.05, add_p = TRUE)) %>% 
      dplyr::select(-p)
    rownames(coef.j) = NULL
    
    # Also update axis labeller with R2
    axis_labels[j] = paste0(axis_labels[j], "\nadjusted R2 = ", round(
      summary(list.models[[j]])$adj.r.squared, digits = 2))
    
    # Compile into single dataframes
    if(j == 1){
      coef = coef.j
      residuals = residuals.j
    }else{
      coef = rbind(coef, coef.j)
      residuals = rbind(residuals, residuals.j)
    }
  }
  
  
  # Plot estimates
  plot.estimates = coef %>%
    left_join(explanatory_vars, by = "var") %>%
    ggplot(aes(x = title)) + 
    geom_errorbar(aes(ymin = lwr, ymax = upr), color = "#4A4E69", width = 0.1) + 
    geom_point(aes(y = est), shape = 21, fill = "lightblue", color = "#4A4E69") + 
    xlab("") + ylab("Effect on PCA coordinate")  +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_wrap(~ PCA, nrow = 1, scales = "free_x",
               labeller = labeller(PCA = axis_labels)) +
    coord_flip() + 
    theme(panel.background = element_rect(fill = "white", color = "black"),
          panel.grid = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(size = 9),
          strip.placement = "outside",
          axis.text.y = element_text(size = 7))
  
  
  
  # Combine the two rows and align them vertically
  plot.pca = plot_grid(plot.hist, plot.var, plot.estimates, ncol = 1, align = "v",
                       rel_heights = c(1, 1, 1), labels = c("(a)", "(b)", "(c)"))
  
  # Save the PCA plot
  file.mainfig = paste0(dir.out, "/fig_main_plot_analyses.jpg")
  ggsave(file.mainfig, plot.pca, width = 19, 
         height = 17, units = "cm", dpi = 600, bg = "white")
  
  
  # Distribution of residuals
  plot.residuals.histogram = residuals %>%
    ggplot(aes(x = residuals))+ 
    geom_histogram(bins = 30, color = "grey", fill = "lightblue")  + 
    facet_wrap(~ PCA, scales = "free") +
    theme(panel.background = element_rect(fill = "white", color = "black"),
          panel.grid = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(size = 9),
          strip.placement = "outside",
          axis.text.y = element_text(size = 7), 
          plot.title = element_text(hjust = 0.5)) + 
    ggtitle("Residuals distribution")
  
  # Residuals vs Fitted values
  plot.residuals.fitted = residuals %>%
    ggplot(aes(x = fitted, y = residuals)) + 
    geom_point(alpha = 0.2) + 
    geom_smooth(method = "loess", color = "red", se = FALSE) + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + 
    ylab("Observed") + xlab("Predicted")  + 
    facet_wrap(~ PCA, scales = "free") +
    theme(panel.background = element_rect(fill = "white", color = "black"),
          panel.grid = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(size = 9),
          strip.placement = "outside",
          axis.text.y = element_text(size = 7), 
          plot.title = element_text(hjust = 0.5)) + 
    ggtitle("Residuals vs fitted")
  
  # Q-Q plot
  plot.residuals.qqplot = residuals %>%
    ggplot(aes(sample = residuals)) + stat_qq() + 
    stat_qq_line(color = "red") +
    facet_wrap(~ PCA, scales = "free") +
    ylab("Sample quantiles") + xlab("Theoretical quantiles")  + 
    theme(panel.background = element_rect(fill = "white", color = "black"),
          panel.grid = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(size = 9),
          strip.placement = "outside",
          axis.text.y = element_text(size = 7), 
          plot.title = element_text(hjust = 0.5)) + 
    ggtitle("Q-Q plot")
  
  # Combine all three residuals plots
  plot.residuals = plot_grid(plot.residuals.histogram, plot.residuals.fitted, 
                             plot.residuals.qqplot, align = "v", 
                             ncol = 1, labels = c("(a)", "(b)", "(c)"))
  
  # Save plot
  file_fig_residuals = paste0(dir.out, "/fig_supp_residuals.jpg")
  ggsave(file_fig_residuals, plot.residuals, width = 18, 
         height = 18, units = "cm", dpi = 600, bg = "white")
  
  
  # Prepare coefficients table for export as a tex file
  # - Round coefficients
  coef.tex = coef %>%
    mutate(Fvalue = round(Fvalue, digits = 2), 
           est = round(est, digits = 2), 
           lwr = round(lwr, digits = 2), 
           upr = round(upr, digits = 2))
  # - Compile into a text easy to paste
  text.coef = paste0(coef.tex[,1], " & ",
                     coef.tex[,2], " & ",
                     coef.tex[,3], " & ",
                     coef.tex[,4], " & ",
                     coef.tex[,5], " & ",
                     coef.tex[,6], " & ",
                     coef.tex[,7], " & ",
                     coef.tex[,8], " \\")
  # - Name of the file to save
  file.tex = paste0(dir.out, "/coef_models_plot.txt")
  # - Save the coefficients in the text file
  cat(text.coef, file = file.tex, sep = "\n", append = FALSE)
  
  # Return file saved 
  return(c(file.tex, file_fig_residuals, file.mainfig))
  
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
  
  
  # Initialize the list that will contain each plot
  plotlist.out = vector(
    mode = "list", length = dim(service_table)[1])
  
  # Boundaries for the scale of trends
  # bound.max = round(max(abs(subset(temporal_trend, pval_adj < 0.05 & n >= 50)$estimate)),
  #                   digits = 0) + 1
  bound.max = 15
  bound.min = -bound.max
  
  # Loop on all variables to plot
  for(i in 1:dim(service_table)[1]){
    
    # Subset data for service i
    data.i = temporal_trend %>% 
      filter(service == service_table$service[i]) %>%
      mutate(estimate = ifelse(pval_adj < 0.05 & n >= 50, estimate, 0))
    
    # Run a t-test to check whether the trend is sifnificantly different from 0
    test.i = t.test(data.i$estimate)
    
    
    # Plot a histogram of the trend
    # - Draw the histogram
    plot.histo.i = data.i %>%
      mutate(estimate.round = round(estimate, digits = 0)) %>%
      group_by(estimate.round) %>%
      summarize(n = n()) %>%
      # rbind(data.frame(estimate.round = setdiff(c(bound.min:bound.max), .$estimate.round), n = NA_real_)) %>%
      ggplot(aes(x = estimate.round, y = n, fill = estimate.round)) + 
      geom_bar(stat = "identity", aes(color = estimate.round)) + 
      scale_fill_gradient2(
        low = '#023E8A', mid = 'white', high = '#CB1B16', midpoint = 0, 
        limits = c(bound.min, bound.max)) +
      scale_color_gradient2(
        low = '#023E8A', mid = 'white', high = '#CB1B16', midpoint = 0, 
        limits = c(bound.min, bound.max)) +
      theme(panel.background = element_rect(color = 'black', fill = 'white'), 
            panel.grid = element_blank(), 
            legend.position = "none", 
            axis.text.x = element_text(size = 8), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(), 
            axis.title = element_blank()) +
      geom_text(data = data.frame(estimate.round = bound.min + 0.3*(bound.max), n = max(
        (data.i %>% mutate(estimate.round = round(estimate, digits = 0)) %>%
           group_by(estimate.round) %>% summarize(n = n()))$n)), 
        label = paste0("mean = ", round(test.i$estimate, digits = 2), "%\nt = ", 
                       round(test.i$statistic, digits = 2), ",", 
                       scales::pvalue(test.i$p.value, add_p = TRUE, accuracy = 0.01)), 
        vjust = "inward", size = 3) + 
      geom_vline(xintercept = 0, linetype = "dashed", color = 'black', linewidth = 0.5) + 
      xlim(bound.min, bound.max)
    # - Add a vertical line for the mean, depending on the estimate and pvalue
    if(test.i$estimate > 0 & test.i$p.value < 0.05) plot.histo.i = plot.histo.i + 
      geom_vline(xintercept = test.i$estimate, linetype = "dashed", color = '#CB1B16')
    if(test.i$estimate < 0 & test.i$p.value < 0.05) plot.histo.i = plot.histo.i + 
      geom_vline(xintercept = test.i$estimate, linetype = "dashed", color = '#0FA3B1')
    if(test.i$p.value >= 0.05) plot.histo.i = plot.histo.i + 
      geom_vline(xintercept = test.i$estimate, linetype = "dashed", color = 'lightgray')
    
    
    # Make the plot for service i
    plot.i = ne_countries(scale = "medium", returnclass = "sf") %>%
      ggplot(aes(geometry = geometry)) +
      geom_sf(fill = "#778DA9", show.legend = F, size = 0.2) + 
      geom_sf(data = (data.plot %>% left_join(
        data.i %>% select(ecoregion, estimate), by = "ecoregion")), 
        mapping = aes(fill = estimate)) +
      scale_fill_gradient2(
        low = '#0FA3B1', mid = 'white', high = '#CB1B16', midpoint = 0, 
        limits = c(bound.min, bound.max)) + 
      # coord_sf(xlim = c(-5, 10), ylim = c(41.2, 51.5)) +
      coord_sf(xlim = c(-5, 10), ylim = c(41.2, 54.5)) +
      theme(panel.background = element_rect(color = 'black', fill = 'white'), 
            panel.grid = element_blank(), 
            legend.position = "none",
            legend.background = element_rect(color = '#778DA9', fill = 'white'),
            axis.text = element_blank(), 
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 17)) +
      annotation_custom(ggplotGrob(plot.histo.i), 
                        xmin = -5, xmax = 10, ymin = 51.5, ymax = 55) +
      ggtitle(paste0("(", letters[i], ") Trend in \n", gsub("\\(.+\\)", "", service_table$title[i]),
                     " (%/yr)"))
    
    # Add plot i to the list
    plotlist.out[[i]] = plot.i
  }
  
  # Assemble plots
  plot.out = plot_grid(plotlist = plotlist.out, align = "hv", nrow = 2)
  
  # - Save the plot
  ggsave(file.out, plot.out, width = 40, height = 27, 
         units = "cm", dpi = 600, bg = "white")
  
  # return the name of the plot exported
  return(file.out)
  
}


#' Analyse the drivers of temporal trens in ecosystem services
#' @param temporal_trend estimate per service and ecoregion of the temporal trend
#' @param data_explanatory_ser explanatory variables at ecoregion scale
#' @param service_table table indicating the title and distribution of each service
#' @param dir.out directory where to save the main figure
#' @param dir.supp.out directory where to save supplementary figures
#' @param dir.table.out directory where to save the table of results
make_plot_analysis_ecoregion = function(
    temporal_trend, data_explanatory_ser, service_table, 
    dir.main.out, dir.supp.out, dir.table.out){
  
  # Create output directory if needed
  create_dir_if_needed(paste0(dir.main.out, "/test"))
  create_dir_if_needed(paste0(dir.supp.out, "/test"))
  create_dir_if_needed(paste0(dir.table.out, "/test"))
  
  # Explanatory variables
  var.expl = colnames(data_explanatory_ser)[-1]
  
  # Table with label for each explanatory variable
  table.var.expl = data.frame(var.expl = var.expl) %>%
    left_join(
      data.frame(var.expl = c("dist.freq", "prop.management", "pca1_str", 
                              "pca2_str", "pca_clim.mean"), 
                 title = c("Disturbance\nfrequency", "Proportion of\nmanaged plots", 
                           "Dominant structure\n(coppice to even-aged)", 
                           "Dominant structure\n(coppice-with-standards\nto uneven-aged)", 
                           "Average climate\n(cold-wet to\nhot-dry)")), 
      by = "var.expl")
  
  # Formula for model fitting
  formula.in = as.formula(paste("trend ~", paste(
    paste0(var.expl, "_scaled"), collapse = " + ")))
  
  # Loop on all services
  for(i in 1:dim(service_table)[1]){
    
    # Prepare dataset
    data.i = temporal_trend %>%
      filter(service == service_table$service[i]) %>%
      mutate(weight = dim(.)[1]*n/sum(n, na.rm = TRUE)) %>%
      select(ecoregion, trend = estimate, weight) %>%
      left_join(data_explanatory_ser, by = "ecoregion")  %>%
      mutate(across(c(4:dim(.)[2]), ~ scale(., center = TRUE, scale = TRUE)[, 1], 
                    .names = "{.col}_scaled"))
    
    # Run model
    model.i = lm(formula.in, data = data.i, weights = weight)
    
    # Dignostic plot to save
    diag.i = generate_diagnostics(model.i, service_table$service[i], "norm")
    ggsave(filename = paste0(dir.supp.out, "/diagnostic2_", service_table$service[i], ".jpg"),
           plot = diag.i, width = 12, height = 5, dpi = 600 )
    
    # Output of the model
    stat.i = tidy(model.i, conf.int = TRUE) %>% 
      filter(term != "(Intercept)") %>%
      mutate(service = service_table$service[i]) %>% 
      mutate(Fval = car::Anova(model.i)$`F value`[c(1:dim(.)[1])], 
             pval = car::Anova(model.i)$`Pr(>F)`[c(1:dim(.)[1])], 
             pval_char = scales::pvalue(pval, add_p = TRUE, accuracy = 0.01)) %>%
      select(service, term, estimate, se = std.error, Fval,
             p.val = pval, conf.low, conf.high, pval_char) %>%
      mutate(term = gsub("\\_scaled", "", term)) 
    
    # Initialize data to make predictive plot
    data.pred.i = data.frame(service = character(0), term = character(0), 
                             term.value = numeric(0), fit = numeric(0), 
                             lwr = numeric(0), upr = numeric(0))
    
    # Loop on each explanatory variable
    for(j in 1:length(var.expl)){
      
      # Explanatory variable j
      var.j = var.expl[j]
      
      # Make predictions setting other explanatory variables to their mean
      newdata.ij = data.i %>%
        mutate(across(all_of(paste0(var.expl[-j], "_scaled")), ~ 0)) %>%
        cbind.data.frame(predict(model.i, newdata = ., interval = "confidence", 
                                 level = 0.95))
      
      # Export to the main dataset
      data.pred.i = data.pred.i %>%
        rbind(data.frame(service = service_table$service[i], term = var.j, 
                         term.value = data.i[, var.j], fit = newdata.ij$fit, 
                         lwr = newdata.ij$lwr, upr = newdata.ij$upr))
    }
    
    # Compile a statistic table and prediction dataframe
    if(i == 1){
      stat.out = stat.i
      data.pred = data.pred.i
    } else {
      stat.out = rbind(stat.out, stat.i)
      data.pred = rbind(data.pred, data.pred.i)
    } 
    
  }
  
  # Initialize plotlist
  plotlist.out = vector(mode = "list", length = length(var.expl))
  
  # Color vector
  vec.col = c("#014F86", "#3D405B", "#81B29A", "#F2CC8F", "#FF99C8", "#3C6E71", 
              "#FAA307", "#8C1C13", "#E07A5F")[c(1:dim(service_table)[1])]
  names(vec.col) = service_table$title
  
  # Make the plot
  plot.out = data.pred %>%
    left_join(stat.out %>% 
                mutate(Significance = ifelse(
                  p.val < 0.05, "p < 0.05", "p >= 0.05")) %>%
                select(service, term, Significance), by = c("term", "service")) %>%
    left_join(service_table %>% select(service, Service = title), 
              by = "service") %>%
    left_join(table.var.expl %>% rename(term = var.expl), by = "term") %>%
    ggplot(aes(x = term.value, color = Service, fill = Service)) + 
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.15, color = NA) + 
    geom_line(aes(y = fit, linetype = Significance)) + 
    scale_linetype_manual(values = c(`p < 0.05` = "solid", `p >= 0.05` = "dashed")) + 
    ylab("Ecosystem service\ntrend (%/year)") +
    facet_wrap(~ title, scales = "free") + 
    xlab("Ecoregion-level explanatory variable") + 
    geom_hline(yintercept = 0, linetype = "solid", color = "#403D39") +
    scale_color_manual(values = vec.col) +
    scale_fill_manual(values = vec.col) +
    theme(panel.background = element_rect(color = 'black', fill = 'white'), 
          panel.grid = element_blank(),
          strip.background = element_blank(), 
          legend.position = "top", 
          legend.title = element_blank(), 
          legend.key = element_blank(), 
          strip.text = element_text(size = 12), 
          axis.title = element_text(size = 12))
  
  
  
  # Format statistical table for supplementary material
  # - File of the latex table
  file.table.out = paste0(dir.table.out, "/table_main_analysis_trend.tex")
  # - Last formatting on the table
  table.out = stat.out %>%
    mutate(term = case_when(term == "pca" ~ "pca1_climate", 
                            term %in% c("pca1", "pca2") ~ paste0(term, "_structure"), 
                            TRUE ~ term), 
           estimate = round(estimate, digits = 3), 
           Fval = round(Fval, digits = 2), 
           se = round(se, digits = 3), 
           service = ifelse(term == var.expl[1], service, "")) %>%
    select(service, term, estimate, se, Fval, pval = pval_char)
  # - Make latex table
  print(xtable(table.out, type = "latex", label = "analysis2_stat_table",
               caption = "Statistical outputs of the models testing the effect of 
               ecoregion-level factors on ecosystem services temporal trends"), 
        include.rownames=FALSE, include.colnames = TRUE, size = "\\scriptsize",
        hline.after = seq(from = 0, to = dim(stat.out)[1], by = length(var.expl)), 
        caption.placement = "top", file = file.table.out, floating = TRUE, placement = "!h")
  
  
  # - Save the plot
  file.out = paste0(dir.main.out, "/fig_main_trends.jpg")
  ggsave(file.out, plot.out, width = 23, height = 15, 
         units = "cm", dpi = 600, bg = "white")
  
  # return the name of the plot exported
  return(c(file.out, file.table.out, 
           paste0(dir.supp.out, "/diagnostic2_", service_table$service[i], ".jpg")))
  
  
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### -- Plots for methods and supplementary ------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

#' Function to make a plot of the frequency of plant species present or not in PFAF
plot_pfaf_freq = function(flora.species.with.score_updated, NFIMed_flora, file.out){
  
  # Create output directory if needed
  create_dir_if_needed(file.out)
  
  # Remove NA in the score table
  scores_nona = drop_na(flora.species.with.score_updated)
  
  # Cross frequency in data with presence of score in the PFAF database
  data_flora = NFIMed_flora %>%
    rename(species.or = species) %>%
    group_by(species.or) %>%
    summarize(n = n(), 
              cover = mean(cover.mean, na.rm = TRUE)) %>%
    mutate(freq = n/length(unique(NFIMed_flora$IDP)), 
           score_exist = ifelse(species.or %in% scores_nona$species.or, 
                                "Score known", 
                                "Score unknown")) %>%
    ungroup() %>% group_by(score_exist) %>% mutate(n_score = n()) %>% ungroup() %>%
    mutate(score_exist = paste0(score_exist, "\n(n = ", n_score, ")"))
  
  # Plot the pattern
  plot.out = data_flora %>% 
    ggplot(aes(x = cover, y = freq)) + 
    geom_point(alpha = 0.2) + 
    facet_wrap(~ score_exist) + 
    xlab("Average cover (%) when the species is observed") + 
    ylab("Frequency in the NFI plots") + 
    theme_bw()
  
  # - Save the plot
  ggsave(file.out, plot.out, width = 15, height = 6, 
         units = "cm", dpi = 600, bg = "white")
  
  # return the name of the plot exported
  return(file.out)
}
