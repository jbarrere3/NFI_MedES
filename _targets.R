#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#### SCRIPT INTRODUCTION ####
#
#' @name _targets.R  
#' @description R script to launch the target pipeline
#' @author Julien BARRERE
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Options and packages ----------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load targets
library(targets)
# Load functions
lapply(grep("R$", list.files("R"), value = TRUE), function(x) source(file.path("R", x)))
# install if needed and load packages
packages.in <- c("dplyr", "ggplot2", "RCurl", "httr", "tidyr", "data.table", 
                 "sp", "sf", "stringr", "taxize", "rnaturalearth", 
                 "rnaturalearthdata", "cowplot")
for(i in 1:length(packages.in)) if(!(packages.in[i] %in% rownames(installed.packages()))) install.packages(packages.in[i])
# Targets options
options(tidyverse.quiet = TRUE)
tar_option_set(packages = packages.in)
set.seed(2)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Targets workflow --------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list(
  # Download files
  tar_target(files, get_FrenchNFI(), format = "file"), 
  
  # Load raw data
  tar_target(FrenchNFI_tree_raw, fread(files[grep("ARBRE", files)])), 
  tar_target(FrenchNFI_plot_raw, fread(files[grep("PLACETTE", files)])), 
  tar_target(FrenchNFI_deadwood_raw, fread(files[grep("BOIS_MORT", files)])), 
  tar_target(FrenchNFI_flora_raw, fread(files[grep("FLORE", files)])), 
  tar_target(FrenchNFI_species, fread(files[grep("espar", files)])), 
  
  # Format raw data
  tar_target(NFIMed_plot, format_plot(FrenchNFI_plot_raw)), 
  tar_target(NFIMed_tree, format_tree(NFIMed_plot, FrenchNFI_tree_raw, FrenchNFI_species)), 
  tar_target(NFIMed_flora, format_flora(NFIMed_plot, FrenchNFI_flora_raw)), 
  
  # Get information on floristic data
  # - Make a vector of all species present
  tar_target(flora.species.list, make_flora.species.list(NFIMed_flora)), 
  # - Get the full taxonomic information from the plant list
  tar_target(flora.species_file, export_flora.species(
    flora.species.list, "export/flora_species.csv"), format = "file"), 
  # - Extract edibility score and medical use from the PFAF database
  tar_target(flora.species.with.score_file, get_pfaf_file(
    flora.species_file, "export/flora_species_with_scores.csv")), 
  # - Add edibility and medicinal score to the species list
  tar_target(flora.species.with.score, merge_species_scores(
    flora.species.list, flora.species.with.score_file)), 
  
  # Calculate plot-level ecosystem services
  tar_target(services_flora, get_services_flora(
    NFIMed_flora, flora.species.with.score)), 
  
  # Plot the data
  tar_target(fig_temporal_flora, plot_temporal_flora_services(
    NFIMed_plot, services_flora, "export/fig/fig_temporal_flora.jpg"), 
    format = "file"), 
  tar_target(fig_spatial_flora, plot_spatial_flora_services(
    NFIMed_plot, services_flora, "export/fig/fig_spatial_flora.jpg"), 
    format = "file")
  
)