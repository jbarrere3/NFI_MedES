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
                 "rnaturalearthdata", "cowplot", "readxl", "terra", "stats", 
                 "factoextra", "purrr", "broom", "car", "tweedie", "glmnet", 
                 "patchwork", "statmod")
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
  tar_target(NFIMed_deadwood, format_deadwood(
    NFIMed_plot, FrenchNFI_deadwood_raw, FrenchNFI_species)),
  
  # Soil and climate data
  # - Local topography (LS-factor) from the European Soil Data Centre
  tar_target(LS_file, "data/Soil/EU_LS_Mosaic_100m.tif", format = "file"),
  # - Soil Erodibility (K-factor) from the European Soil Data Centre
  tar_target(K_file, "data/Soil/K_new_crop.tif", format = "file"), 
  # - Annual precipitation from CHELSA portal
  tar_target(chelsa_prec_file, "data/Climate/CHELSA_bio12_1981-2010_V.2.1.tif", 
             format = "file"),
  # - monthly precipitation of wettest month from CHELSA portal
  tar_target(chelsa_precmax_file, "data/Climate/CHELSA_bio13_1981-2010_V.2.1.tif", 
             format = "file"),
  # - potential evapotranspiration calculated with Penman from CHELSA portal
  tar_target(chelsa_pet_file, "data/Climate/CHELSA_pet_penman_mean_1981-2010_V.2.1.tif", 
             format = "file"),
  # - sum of growing degree days above 5 degrees from CHELSA portal
  tar_target(chelsa_sgdd_file, "data/Climate/CHELSA_gdd5_1981-2010_V.2.1.tif", 
             format = "file"),
  # - Extract climate and soil data for each NFI plot
  tar_target(clim_and_soil, extract_clim_and_soil(
    NFIMed_plot, LS_file, K_file, chelsa_prec_file, chelsa_precmax_file)),
  # - Sylvoecoregions shapefile
  tar_target(sylvoER_shp_file, "data/GIS/ser_l93.shp", format = "file"),
  # - Elevation raster for France
  tar_target(elevation_raster, "data/GIS/France_metropolitaine.tif", format = "file"),
  
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
  # - Scores browsed from litterature from species not in PFAF
  tar_target(score_species_not_in_pfaf_file, "data/PFAF/species_not_in_pfaf_updated.xlsx", 
             format = "file"),
  # - Update flora.species.with.score with the scores sourced from litterature by Philip
  tar_target(flora.species.with.score_updated, update_flora.species.with.score(
    score_species_not_in_pfaf_file, flora.species.with.score)),
  
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
    NFIMed_flora, flora.species.with.score_updated)), 
  # - from tree-level data
  tar_target(services_tree, get_services_tree(
    NFIMed_tree, NFIMed_plot, NFIMed_deadwood, FrenchNFI_species, 
    coef_allometry_file, coef_volume_file, wood.density_file, tree.species_info)),
  # - erosion mitigation from ecological data
  tar_target(services_erosion, get_service_erosion(
    FrenchNFI_ecology_raw, NFIMed_tree, clim_and_soil, tree.species_info)),
  # - merge data together
  tar_target(data_services, merge_service(
    list.in = list(flora = services_flora, tree = services_tree, erosion = services_erosion), 
    plots_filtered)),
  # - table with the list and title of each service
  tar_target(service_table, data.frame(
    service = c("ab.medicinal", "ab.edibility", "Csequestr_kg.ha.yr", 
                "Cstock_t.ha", "timber.volume_m3.ha", "erosion.mitig"), 
    title = c("Abundance of\nmedicinal plants",  "Abundance of\nedible plants", 
              "Carbon sequestrated\n(kg.ha.yr)", "Carbon stock\n(t.ha)", 
              "Timber volume\n(m3.ha)", "Erosion mitigation\n(t.ha.yr)"), 
    type = c("capacity", "capacity", "flux", "capacity", "capacity", "flux"), 
    distrib = c("beta", "beta", "pos0", "pos", "pos0", "pos"))),
  
  # Compile all explanatory variables
  tar_target(data_explanatory, compile_explanatory(
    NFIMed_tree, NFIMed_plot, chelsa_prec_file, chelsa_pet_file, 
    chelsa_sgdd_file, elevation_raster)),
  tar_target(data_explanatory_ser, compile_explanatory_ser(
    FrenchNFI_plot_raw, data_explanatory, "export/fig/supplementary/figsup_pcastr.jpg")),
  tar_target(temporal_trend, get_temporal_trend(
    data_services, NFIMed_plot, service_table)),
  
  # Plot the data
  # - Methods
  tar_target(fig_map_ser, map_explanatory_ser(
    data_explanatory_ser, sylvoER_shp_file, "export/fig/fig_map_ser.jpg"), 
    format = "file"),
  # - Main analysis
  tar_target(fig_spatial, plot_spatial_services(
    NFIMed_plot, data_services, service_table, sylvoER_shp_file, 
    "export/fig/fig_spatial.jpg"), format = "file"), 
  tar_target(fig_analysis1, make_plots_analysis1(
    data_explanatory, data_services, service_table, "export/fig/supplementary/diag1", 
    "export/fig/fig_model1.jpg"), format = "file"),
  tar_target(fig_map_trends, map_temporal_trend(
    temporal_trend, service_table, sylvoER_shp_file, 
    "export/fig/fig_map_trends.jpg"), format = "file"),
  # - Supplementary material
  tar_target(figsup_exploratory, plot_exploratory(
    data_explanatory, data_services, service_table, 
    "export/fig/supplementary/figsup_exploratory.jpg"), format = "file")
  
)