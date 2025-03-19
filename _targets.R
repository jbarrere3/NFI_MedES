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
                 "sp", "sf", "stringr", "taxize", "rnaturalearth", "WorldFlora",
                 "rnaturalearthdata", "cowplot", "readxl")
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
  
  # Load raw NFI data
  tar_target(FrenchNFI_tree_raw, fread(files[grep("ARBRE", files)])), 
  tar_target(FrenchNFI_plot_raw, fread(files[grep("PLACETTE", files)])), 
  tar_target(FrenchNFI_deadwood_raw, fread(files[grep("BOIS_MORT", files)])), 
  tar_target(FrenchNFI_flora_raw, fread(files[grep("FLORE", files)])), 
  tar_target(FrenchNFI_ecology_raw, fread(files[grep("ECOLOGIE", files)])), 
  tar_target(FrenchNFI_species, fread(files[grep("espar", files)])), 
  
  # Format raw NFI data
  tar_target(NFIMed_plot, format_plot(FrenchNFI_plot_raw)), 
  tar_target(NFIMed_tree, format_tree(NFIMed_plot, FrenchNFI_tree_raw, FrenchNFI_species)), 
  tar_target(NFIMed_flora, format_flora(NFIMed_plot, FrenchNFI_flora_raw)), 
  
  # Soil and climate data
  # - Local topography (LS-factor) from the European Soil Data Centre
  tar_target(LS_file, "data/Soil/EU_LS_Mosaic_100m.tif", format = "file"),
  # - Soil Erodibility (K-factor) from the European Soil Data Centre
  tar_target(K_file, "data/Soil/K_new_crop.tif", format = "file"), 
  # - Annual precipitation from CHELSA portal
  tar_target(chelsa_prec_file, "data/Climate/CHELSA_bio10_12.tif", format = "file"),
  # - Extract climate and soil data for each NFI plot
  tar_target(clim_and_soil, extract_clim_and_soil(
    NFIMed_plot, LS_file, K_file, chelsa_prec_file)),
  
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
    flora.species.list, flora.species.with.score_file, tree.species_info)), 
  
  # Get more information on tree data
  # - File of the WorldFlora database
  tar_target(WorldFlora_file, "data/WorldFlora/classification.csv", format = "file"), 
  # - Get information on the group and family of each species
  tar_target(tree.species_info, get_info_species.tree(
    NFIMed_tree, WorldFlora_file)),
  # - Files necessary for carbon and timber calculation
  tar_target(coef_allometry_file, "data/Carbon/recap.csv", format = "file"), 
  tar_target(coef_volume_file, "data/Carbon/CoefficientsEmerge.csv", format = "file"),
  tar_target(wood.density_file, "data/Carbon/WOODBASICDENSITYFOR156TREEFORESTSPECIES-11-04-2022.xlsx", 
             format = "file"),
  
  # Filter plots to remove outliers
  tar_target(plots_filtered, filter_plots(NFIMed_tree)),
  
  # Calculate plot-level ecosystem services
  # - from floristic data
  tar_target(services_flora, get_services_flora(
    NFIMed_flora, flora.species.with.score)), 
  # - from tree-level data
  tar_target(services_tree, get_services_tree(
    NFIMed_tree, FrenchNFI_species, coef_allometry_file,  coef_volume_file,
    wood.density_file, tree.species_info, FrenchNFI_plot_raw)),
  # - merge data together
  tar_target(data_services, merge_service(
    list.in = list(flora = services_flora, tree = services_tree), plots_filtered)),
  # - table with the list and title of each service
  tar_target(service_table, data.frame(
    service = c("ab.medicinal", "ab.edibility", "Cmass_kg.ha", "timber.volume_m3.ha"), 
    title = c("Abundance of\nmedicinal plants",  "Abundance of\nedible plants", 
              "Carbon stored\n(kg.ha)", "Timber volume\n(m3.ha)"))),
  
  # Plot the data
  tar_target(fig_temporal, plot_temporal_services(
    NFIMed_plot, data_services, service_table, "export/fig/fig_temporal.jpg"), 
    format = "file"),
  tar_target(fig_spatial, plot_spatial_services(
    NFIMed_plot, data_services, service_table, "export/fig/fig_spatial.jpg"),
    format = "file")
  
)