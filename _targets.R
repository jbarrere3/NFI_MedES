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
                 "sp", "sf")
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
  
  # Format data
  tar_target(NFIMed_plot, format_plot(FrenchNFI_plot_raw)), 
  tar_target(NFIMed_tree, format_tree(NFIMed_plot, FrenchNFI_tree_raw, FrenchNFI_species)), 
  tar_target(NFIMed_flora, format_flora(NFIMed_plot, FrenchNFI_flora_raw))
  
)