#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#' @name functions_data.R  
#' @description R script containing all functions relative to data
#               importation and formatting
#' @author Julien Barrere
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### -- Generic functions ------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#' Get file from its url and write it on disk, at a specified location. 
#' @param dir.name.in Directory where the file should be written (ex: "data/BWI")
#' @param url.in URL where to download the file.
get_and_write <- function(dir.name.in, url.in){
  
  # Write directories if they do not exist
  path.in <- strsplit(dir.name.in, "/")[[1]]
  for(i in 1:length(path.in)){
    if(i == 1) path.in_i <- path.in[i]
    else path.in_i <- paste(path.in_i, path.in[i], sep = "/")
    if(!dir.exists(path.in_i)) dir.create(path.in_i)
  }
  
  # Write file on the disk
  url.in_split <- strsplit(url.in, "/")[[1]]
  file.in <- paste(dir.name.in, url.in_split[length(url.in_split)], sep = "/")
  if(!file.exists(file.in)){
    try(GET(url.in, authenticate('guest', ""), write_disk(file.in, overwrite = TRUE)))
    # Specific case of zip file: unzip and delete zip file
    if("zip" %in% strsplit(file.in, split = "\\.")[[1]]){
      unzip(file.in, exdir = dir.name.in, overwrite = T)
      print(paste0("---Getting and unzipping ", file.in))
      unlink(file.in)
    }else{print(paste0("---Getting ", file.in))}
  } 
}




#' Function to get the path of a file, and create directories if they don't exist
#' @param file.in character: path of the file, filename included (ex: "plot/plot.png")
create_dir_if_needed <- function(file.in){
  
  path.in <- strsplit(file.in, "/")[[1]]
  if(length(path.in) > 1){
    for(i in 1:(length(path.in)-1)){
      if(i == 1) path.in_i <- path.in[i]
      else path.in_i <- paste(path.in_i, path.in[i], sep = "/")
      if(!dir.exists(path.in_i)) dir.create(path.in_i)
    }
  }
}

#' Write a table on disk
#' @param table.in dataframe to write on the disk
#' @param file.in Name (and path) of the file on the disk
write_on_disk <- function(table.in, file.in){
  create_dir_if_needed(file.in)
  write.table(table.in, file = file.in, row.names = F, sep = ",")
  return(file.in)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### -- Extraction and formatting of raw data ------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



#' Function to write all FRENCH NFI data on disk
#' @return a dataframe containing the directories where each file is stored, and the url to get these files
get_FrenchNFI <- function(){
  
  dir.in = c("data/FrenchNFI")
  url.in = c("https://inventaire-forestier.ign.fr/dataifn/data/export_dataifn_2005_2023.zip")
  get_and_write(dir.in, url.in)
  return(paste(dir.in, list.files(dir.in), sep = "/"))
}



#' Function to extract basic plot-level information for all plots in study area
#' @param FrenchNFI_plot_raw Plot level raw data from NFI
format_plot = function(FrenchNFI_plot_raw){
  
  FrenchNFI_plot_raw %>%
    # Only keep first census
    filter(VISITE == 1) %>%
    # # Filter ecological regions
    # filter(SER %in% c("J10", "J21", "J22", "J23", "J24", 
    #                   "J30", "J40", "K11", "K12", "K12")) %>%
    # Extract the stand structure
    mutate(sver_sfo = case_when(
      !is.na(SFO) ~ SFO, 
      is.na(SFO) & SVER %in% c("2", "6") ~ 1, 
      is.na(SFO) & SVER == "4" ~ 2, 
      is.na(SFO) & SVER == "5" ~ 3, 
      is.na(SFO) & SVER == "3" ~ 4, 
      is.na(SFO) & SVER %in% c("", "0", "X") ~ 0, 
      TRUE ~ NA)) %>%
    # Attribute structure based on the unified codification
    mutate(str = case_when(
      sver_sfo == 1 ~ "even", # even-aged (FR in original script)
      sver_sfo == 2 ~ "uneven", # uneven-aged (FI in original script)
      sver_sfo == 3 ~ "copfor", # coppice high forest (FT in original script) 
      sver_sfo == 4 ~ "cop", # coppice (T in original script)
      sver_sfo == 0 ~ "nostr", # no structure (PS in original script)
      TRUE ~ NA)) %>%
    # change coordinates
    st_as_sf(coords = c("XL", "YL"), crs = 2154, agr = "constant") %>%
    st_transform(crs = 4326) %>% 
    mutate(longitude = st_coordinates(.)[,1],
           latitude = st_coordinates(.)[,2])  %>%
    st_drop_geometry() %>%
    # Select columns
    select(IDP, year = CAMPAGNE, longitude, latitude, ecoregion = SER, str) 
  
}

#' Format tree-level data
#' @param NFIMed_plot formatted plot-level data
#' @param FrenchNFI_tree_raw raw tree-level data
#' @param FrenchNFI_species correspondance table for tree species 
format_tree = function(NFIMed_plot, FrenchNFI_tree_raw, FrenchNFI_species){
  
  FrenchNFI_tree_raw %>%
    # Filter to keep only the first visits
    mutate(id_temp = paste0(IDP, "_", CAMPAGNE)) %>%
    filter(id_temp %in% paste(NFIMed_plot$IDP, NFIMed_plot$year, sep = "_")) %>%
    left_join((FrenchNFI_species %>% 
                 select(`ESPAR` = `// espar`, `species` = `lib_cdref`) %>%
                 mutate(ESPAR = ifelse(ESPAR %in% as.character(c(1:9)), 
                                       paste0(0, ESPAR), ESPAR))), by = "ESPAR") %>%
    # Calculate diameter and basal area
    mutate(dbh = C13/(pi) * 1000, 
           ba = (pi*((dbh/1000)/2)^2)) %>%
    # Format the status of the tree
    mutate(status = case_when(VEGET == "0" ~ "alive",
                              VEGET %in% c("6", "7") ~ "harvested",
                              VEGET %in% c("1", "2", "5", "A", "C", "M", "T") ~ "dead",
                              VEGET == "N" ~ "lost")) %>%
    # Keep columns of interest
    select(IDT = A, IDP, ESPAR, species, status, dbh, ba, height = HTOT, 
           height.cut = HDEC, age = AGE, age.130 = AGE13, trunk.length = LFSD, 
           increment = IR5, volume = V, weight = W) %>%
    filter(!is.na(dbh))
  
}



#' Format the flora data
#' @param NFIMed_plot formatted plot-level data
#' @param FrenchNFI_flora_raw raw flora data
format_flora = function(NFIMed_plot, FrenchNFI_flora_raw){
  
  
  # Format the data with species code
  sp_raw = data.table::fread("data/FrenchNFI/metadonnees.csv", fill = TRUE)
  colnames(sp_raw) = "col1"
  data_species = separate(data.frame(col1 = sp_raw$col1), col1, sep = ";", 
                          into = c("col_code", "CD_REF", "species_FR", "species", "other")) %>%
    filter(col_code == "CDREF13") %>%
    select(CD_REF, species)
  
  
  
  # Format the flora data
  out = FrenchNFI_flora_raw %>%
    # Filter to keep only the first visits
    mutate(id_temp = paste0(IDP, "_", CAMPAGNE)) %>%
    filter(id_temp %in% paste(NFIMed_plot$IDP, NFIMed_plot$year, sep = "_")) %>%
    # Add species code
    left_join((data_species %>% mutate(CD_REF = as.integer(CD_REF))), by = "CD_REF") %>%
    # Keep columns of interest
    select(IDP, species, abundance = ABOND) %>%
    # Format abundance
    mutate(cover.min = case_when(abundance %in% c(1, 2) ~ 0, 
                                 abundance == 3 ~ 25, 
                                 abundance == 4 ~  50, 
                                 abundance == 5 ~ 75), 
           cover.max = case_when(abundance == 1 ~ 5, 
                                 abundance == 2 ~ 25, 
                                 abundance == 3 ~ 50, 
                                 abundance == 4 ~ 75, 
                                 abundance == 5 ~ 100), 
           cover.mean = (cover.min + cover.max)/2)
  
  # return output
  return(out)
}

#' Format deadwood data
#' @param NFIMed_plot formatted plot-level data
#' @param FrenchNFI_deadwood_raw raw deadwood data
#' @param FrenchNFI_species correspondance table for tree species 
format_deadwood = function(NFIMed_plot, FrenchNFI_deadwood_raw, FrenchNFI_species){
  
  FrenchNFI_deadwood_raw %>%
    # Filter to keep only the first visits
    mutate(id_temp = paste0(IDP, "_", CAMPAGNE)) %>%
    filter(id_temp %in% paste(NFIMed_plot$IDP, NFIMed_plot$year, sep = "_")) %>%
    rename(ESPAR = ESPAR_BM) %>%
    left_join((FrenchNFI_species %>% 
                 select(`ESPAR` = `// espar`, `species` = `lib_cdref`) %>%
                 mutate(ESPAR = ifelse(ESPAR %in% as.character(c(1:9)), 
                                       paste0(0, ESPAR), ESPAR))), by = "ESPAR") %>%
    # Add percentage of decay from class
    mutate(decay_prop = DECOMP*0.2 - 0.1) %>%
    # Keep columns of interest
    select(IDT = A, IDP, ESPAR, species, dbm = DBM, decay_class = DECOMP, 
           decay_prop) %>%
    filter(!is.na(dbm) & !is.na(species))
  
}
#' Function to remove plots based on outliers or otherr criterias
#' @param NFIMed_tree Tree data formatted
filter_plots = function(NFIMed_tree){
   
  # Filter plots based on different criteria
  out = unique((NFIMed_tree %>%
            # We need reliable height values for soil erosion
            mutate(is.height = ifelse(is.na(height), 0, 1)) %>%
            group_by(IDP) %>%
            mutate(n.height = sum(is.height, na.rm = TRUE)) %>%
            filter(n.height > 1) %>%
            # Remove plots with trees with unrealistic diameters
            filter(!any(dbh > 1300)))$IDP)
  
  # Return output
  return(out)
}



#' Function to extract climate and soil data for the calculation of erosion service
#' @param NFIMed_plot NFI plot data formatted
#' @param LS_file file containing LS-factor raster data
#' @param K_file file containing K-factor raster data
#' @param chelsa_prec_file file containing rainfall raster data from CHELSA
#' @param chelsa_precmax_file file containing monthly ranfall of wettest month
extract_clim_and_soil = function(
    NFIMed_plot, LS_file, K_file, chelsa_prec_file, chelsa_precmax_file){
  # Initialize output
  out = NFIMed_plot
  
  # Extract precipitations
  # - Read raster of annual precipitations and max precipitation
  raster_map = terra::rast(chelsa_prec_file)
  raster_mapmax = terra::rast(chelsa_precmax_file)
  # - Extract raster values
  out$pr <- as.numeric(terra::extract(
    raster_map, cbind(out$longitude, out$latitude))[, 1])
  out$prmax <- as.numeric(terra::extract(
    raster_mapmax, cbind(out$longitude, out$latitude))[, 1])/30
  
  # Extract K-factor and LS-factor
  # - Read raster
  raster_LS = terra::rast(LS_file)
  raster_K = terra::rast(K_file)
  # - Convert the latitude / longitude data of NFI plots to the soil raster projection
  out.coord.rast = NFIMed_plot %>%
    select(IDP, x = longitude, y = latitude) %>%
    st_as_sf(coords = c("x", "y"), crs = 4326, agr = "constant") %>%
    st_transform(crs = crs(raster_K)) %>%
    mutate(x = st_coordinates(.)[,1],
           y = st_coordinates(.)[,2]) %>%
    st_drop_geometry()
  # - Extract raster values
  out$K_factor <- as.numeric(terra::extract(
    raster_K, cbind(out.coord.rast$x, out.coord.rast$y))[, 1])
  out$LS_factor <- as.numeric(terra::extract(
    raster_LS, cbind(out.coord.rast$x, out.coord.rast$y))[, 1])
  
  # Return output
  return(out)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### -- Manage floristic metrics ------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#' Function to build a list of flora species with taxonomic details
#' @param NFIMed_flora Formatted flora data
make_flora.species.list = function(NFIMed_flora){
  
  # List all the species present
  list.species = unique(arrange(NFIMed_flora, species)$species)
  
  # Initialize the output dataframe
  data.species = data.frame(species.or = list.species) %>%
    mutate(species.full = NA_character_, 
           species = NA_character_) 
  
  # Loop on all species
  for(i in 1:length(list.species)){
    print(paste0("(", i, " / ", length(list.species), ") : ", list.species[i]))
    # Extract the ful name of the species
    name.i = list.species[i]
    # Split the full name in parts
    split.i = str_split(name.i, " ")[[1]]
    # Try to match directly the full name
    verified_i = try(expr = gna_verifier(list.species[i]), silent = TRUE)
    # If it works, add species name directly from the match
    if(!(any(class(verified_i) == "try-error"))){
      # Verify that the dataframe is not null
      if(dim(verified_i)[1] > 0){
        # Extract the name of the species
        data.species$species.full[i] = verified_i$matchedCanonicalSimple[1]
        # Add without the sub species
        if(length(str_split(data.species$species.full[i], " ")[[1]]) > 1){
          data.species$species[i] = paste(str_split(
            data.species$species.full[i], " ")[[1]][1:2], collapse = " ")
        }
      }
    } else {
      # Otherwise, try by splitting directly the name of the species
      if(length(split.i) > 3){
        verified_i = try(expr = gna_verifier(paste(split.i[1:2], collapse = " ")), silent = TRUE)
        # If it works, add species name directly from the match
        if(!(any(class(verified_i) == "try-error"))){
          # Verify that the dataframe is not null
          if(dim(verified_i)[1] > 0){
            # Extract the name of the species
            data.species$species.full[i] = verified_i$matchedCanonicalSimple[1]
            # Add without the sub species
            if(length(str_split(data.species$species.full[i], " ")[[1]]) > 1)
              data.species$species[i] = paste(str_split(
                data.species$species.full[i], " ")[[1]][1:2], collapse = " ")
          } 
        }
      }
    }
    
  }
  
  # Return the output data frame
  return(data.species)
  
}


#' Function to export a csv with flora genus and species
#' @param flora.species.list list of floristic species with taxonomy
#' @param file.out Name of the file to export, including path
export_flora.species = function(flora.species.list, file.out){
  
  # Format species list
  out = flora.species.list %>%
    # Remove species for which we don't have the full species name
    drop_na() %>%
    # Separate genus and species
    mutate(genus = gsub("\\ .+", "", species), 
           sp = gsub(".+\\ ", "", species)) %>%
    # Only keep the columns of interest
    select(genus, species = sp)
  
  # Export the generated dataset and return the file name
  return(write_on_disk(out, file.out))
  
} 


#' Function to extract the edibility and medicinal use of plant species from PFAF
#' @param flora.species_file csv file containing the name of flora species
#' @param flora.species_file.out csv file to export
get_pfaf_file = function(flora.species_file, flora.species_file.out){
  
  # Launch python script
  system(paste0("python3 Python/scrape_pfaf.py ", flora.species_file, 
                " ", flora.species_file.out), wait = TRUE)
  
  # Return the output
  return(flora.species_file.out)
} 


#' Function to add medicinal and edibility score to the species data base
#' @param flora.species.list Data frame listing species with taxonomic info
#' @param flora.species.with.score_file species score of medicinal and edibility
merge_species_scores = function(
    flora.species.list, flora.species.with.score_file, tree.species_info){
  
  # Format the edibility and medicinal metrics
  flora.species.with.score = fread(flora.species.with.score_file) %>%
    mutate(sp = paste(genus, species, sep = " "), 
           edibility = as.numeric(substr(edibility_score, 2, 2)), 
           medicinal = as.numeric(substr(medicinal_score, 2, 2))) %>%
    select(species = sp, edibility, medicinal)
  
  # Join to the species list
  out = flora.species.list %>%
    left_join(flora.species.with.score, by = "species") %>%
    mutate(edibility = ifelse(species %in% tree.species_info$species, 0, edibility), 
           medicinal = ifelse(species %in% tree.species_info$species, 0, medicinal))
  
  # Return output
  return(out)
  
}

#' Function to update the flora score database with species not in pfaf sourced by Philip
#' @param score_species_not_in_pfaf_file xlsx file containing the score of species not in pfaf
#' @param flora.species.with.score original file with medicinal and edibility score per species
update_flora.species.with.score = function(score_species_not_in_pfaf_file, 
                                           flora.species.with.score){
  
  # Read the excel file and format it to keep only medicinal and edible use
  score_species_not_in_pfaf = read_xlsx(score_species_not_in_pfaf_file, 
                                        sheet = "species_not_in_pfaf_updated") %>%
    mutate(species.or = ifelse(is.na(cover.percent), species, paste0(species, ", ", cover.percent))) %>%
    select(species.or, edibility.new = Edible, medicinal.new = Medicinal) %>%
    mutate(edibility.new = as.numeric(edibility.new), 
           medicinal.new = as.numeric(medicinal.new))
  
  # Add the new data to the original flora with score
  out = flora.species.with.score %>%
    left_join(score_species_not_in_pfaf, by = "species.or") %>%
    mutate(edibility = ifelse(is.na(edibility), edibility.new, edibility), 
           medicinal = ifelse(is.na(medicinal), medicinal.new, medicinal)) %>%
    select(-medicinal.new, -edibility.new)
  
  # Return the updated dataset
  return(out)
  
}



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### -- Manage tree metrics ------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' Function to extract the family and group of each tree species
#' @param NFIMed_tree Tree-level formatted NFI data
#' @param WorldFlora_file file of the WorldFlora database
get_info_species.tree = function(NFIMed_tree, WorldFlora_file){
  
  # Load database relating families to groups
  data(vascular.families)
  
  # Read WorldFlora database
  WFO.data = data.table::fread(WorldFlora_file)
  
  # Format species name and extract genus
  species.data = data.frame(species.original = unique(NFIMed_tree$species)) %>%
    drop_na() %>%
    mutate(species = species.original) %>%
    separate(species, sep = "\\ ", into = c("genus", "species", "adv", "subsp")) %>%
    mutate(species.true = case_when(
      is.na(species) ~ paste0(genus, " sp"), 
      species == "x" ~ paste(genus, adv, sep = " "), 
      TRUE ~ paste(genus, species, sep = " "))) %>%
    select(species.original, genus, species = species.true) %>%
    arrange(species) 
  
  # Connect using WorldFlora the genus to the family and the group
  genus.family = species.data %>%
    left_join((WFO.data %>% filter(taxonRank == "species") %>% 
                 select(species = scientificName, family) %>%
                 rbind(data.frame(species = 'Laburnum anagyroides', 
                                  family = "Fabaceae"))), 
              by = "species") %>%
    select(genus, family) %>%
    drop_na() %>%
    distinct() %>%
    left_join((vascular.families %>%
                 dplyr::select(family = Family, group = Group) %>%
                 rbind(data.frame(family = c("Viburnaceae", "Cephalotaxaceae"), 
                                  group = c("angiosperms", "gymnosperms")))), 
              by = "family")
  
  # Connect the two datasets
  out = species.data %>%
    left_join(genus.family, by = "genus")
  
  # Return the output
  return(out)
  
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### -- Calculate services ------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' Function to calculate services related to floristic data
#' @param NFIMed_flora floristic data at plot level
#' @param flora.species.with.score species-level data on edibility and medicinal use
get_services_flora = function(NFIMed_flora, flora.species.with.score){
  
  # Prepare data before plot-level calculation
  data = NFIMed_flora %>%
    # Add edibility and medicinal use
    left_join(flora.species.with.score %>%
                select(species = species.or, edibility, medicinal) %>%
                group_by(species) %>%
                summarize(edibility = mean(edibility, na.rm = TRUE), 
                          medicinal = mean(medicinal, na.rm = TRUE)) %>%
                ungroup(), by = "species") %>%
    # Replace NAs by 0s and normalize between 0 and 1
    mutate(edibility = ifelse(is.na(edibility), 0, edibility)/5, 
           medicinal = ifelse(is.na(medicinal), 0, medicinal)/5, 
           cover.mean = cover.mean/100) %>%
    # Only keep columns of interest
    select(IDP, species, abundance = cover.mean, edibility, medicinal)
  
  # Plot level calculation of medicinal and edible abundance, and shannon index
  out = data %>%
    group_by(IDP) %>%
    mutate(inv.med = 1 - abundance*medicinal, 
           inv.edi = 1 - abundance*edibility, 
           p = abundance/sum(abundance), 
           plnp = p*log(p)) %>%
    summarize(ab.medicinal = 1 - prod(inv.med), 
              #shannon = -sum(plnp), 
              ab.edibility = 1 - prod(inv.edi))
  
  # Return output
  return(out)
}


#' Calculate services from tree data
#' @param NFIMed_tree tree-level formatted NFI data
#' @param NFIMed_plot formatted plot-level NFI data
#' @param NFIMed_deadwood formatted plot-level NFI data
#' @param FrenchNFI_species correspondance btw species name and code
#' @param coef_allometry_file file containing allometry coefficients from Savine et al. (in prep)
#' @param coef_volume_file file containing tree volume coefficients from Deleuze et al. 2014
#' @param wood.density_file file containing tree density from XyloDensMap (Leban et al.)
#' @param tree.species_info tree-level taxonomic information
get_services_tree = function(
    NFIMed_tree, NFIMed_plot, NFIMed_deadwood, FrenchNFI_species, 
    coef_allometry_file, coef_volume_file, wood.density_file, tree.species_info){
  
  
  # - Read and process raw data
  coef_allometry_raw = fread(coef_allometry_file) %>%
    # Remove 0 in one letter codes
    mutate(espar = ifelse(espar %in% paste0("0", c(1:9)), 
                          gsub("0", "", espar), espar))
  # - Format species data to add allometry coefficients
  coef_allometry_per_species = tree.species_info %>%
    # Add species code 
    left_join((FrenchNFI_species %>%
                 select(`species.original` = `lib_cdref`, 
                        `espar` = `// espar`)), 
              by = "species.original") %>%
    # Attribute the code of beech when parameters are unknown (/!\ TO CHANGE LATER)
    mutate(espar = ifelse(espar %in% coef_allometry_raw$espar, espar, "9")) %>%
    # Join coefficients 
    left_join(coef_allometry_raw, by = "espar") %>%
    # Only keep columns of interest
    select(species = species.original, a1, a2, a3, a4, a5, a6, a7, b1, b2, b3, b4, b5, d1) %>%
    replace(is.na(.), 0) %>%
    distinct
  
  # Process volume data
  # - Read raw volume data
  coef_volume_raw = fread(coef_volume_file) %>%
    # Remove 0 in one letter codes
    mutate(speciesCode = ifelse(speciesCode %in% paste0("0", c(1:9)), 
                                gsub("0", "", speciesCode), speciesCode))
  # - Format species data to add volume coefficients
  coef_volume_per_species = tree.species_info %>%
    # Add species code 
    left_join((FrenchNFI_species %>%
                 select(`species.original` = `lib_cdref`, 
                        `speciesCode` = `// espar`)), 
              by = "species.original") %>%
    # Attribute the code for conifer and deciduous without coefficients
    mutate(speciesCode = case_when(
      speciesCode %in% coef_volume_raw$speciesCode ~ speciesCode, 
      !(speciesCode %in% coef_volume_raw$speciesCode) & group == "gymnosperms" ~ "otherConiferous", 
      !(speciesCode %in% coef_volume_raw$speciesCode) & group == "angiosperms" ~ "otherDeciduous", 
      speciesCode == "36" ~ "otherDeciduous", # Eucalyptus
      TRUE ~ NA)) %>%
    # Join coefficients 
    left_join(coef_volume_raw, by = "speciesCode") %>%
    # Only keep columns of interest
    select(species = species.original, aEmerge, bEmerge, cEmerge) %>%
    distinct()
  
  # Process wood density data
  # - Read raw file
  wood.density_raw = read_xlsx(wood.density_file, sheet = "Wood basic density") %>%
    select(`species.original` = `156 Species`, `wood.density` = BD) %>%
    drop_na()
  # - Join to species table
  wd_per_species = tree.species_info %>%
    left_join(wood.density_raw, by = "species.original") %>%
    # Average across all species 
    mutate(WD.avg = mean(wood.density, na.rm = TRUE)) %>%
    # Average by genus
    group_by(genus) %>%
    mutate(WD.genus = mean(wood.density, na.rm = TRUE)) %>%
    ungroup() %>%
    # Attribute averages for NA
    mutate(wood.density = case_when(
      is.na(wood.density) & !is.na(WD.genus) ~ WD.genus, 
      is.na(wood.density) & is.na(WD.genus) ~ WD.avg, 
      TRUE ~ wood.density)) %>%
    select(species = species.original, wood.density) %>%
    distinct()
  
  ## - Calculate deadwood volume on the ground
  deadwood.per.plot = NFIMed_deadwood %>%
    # Calculate volume by species, plot and decay class based on Huber's formula
    mutate(dbm2_cm = (dbm*100)^2) %>%
    group_by(IDP, species, decay_prop) %>%
    summarize(V_m3.ha = (pi^2/(8*12))*sum(dbm2_cm, na.rm = TRUE)) %>%
    ungroup() %>%
    # - join Wood density data
    left_join(wd_per_species, by = "species") %>%
    mutate(deadwood_kg.ha = V_m3.ha*wood.density*(1 - decay_prop)) %>%
    # Average by plot
    group_by(IDP) %>%
    summarize(volume_deadwood.lying_m3.ha = sum(V_m3.ha, na.rm = TRUE), 
              Cstock_deadwood.lying_kg.ha = 0.5*sum(deadwood_kg.ha, na.rm = TRUE)) %>%
    ungroup() %>%
    # Remove outliers
    mutate(
      volume_deadwood.lying_m3.ha = ifelse(
        volume_deadwood.lying_m3.ha > 1000, NA_real_, volume_deadwood.lying_m3.ha), 
      Cstock_deadwood.lying_kg.ha = ifelse(
        volume_deadwood.lying_m3.ha > 1000, NA_real_, Cstock_deadwood.lying_kg.ha))
  
  
  
  
  ## - Calculate carbon per tree - ##
  out = NFIMed_tree %>%
    
    # GET TREE HEIGHT FOR ALL INDIVIDUAL TREES
    # - Add plot structure (regular or irregular)
    left_join(NFIMed_plot %>% select(IDP, str), by = "IDP") %>%
    # - Add allometry coefficients
    left_join(coef_allometry_per_species, by = "species") %>%
    # - Calculate plot quadratic diameter
    group_by(IDP) %>%
    mutate(dbh_cm = dbh/10, 
           dbh0_cm = dbh/10 - increment*100, 
           ba.0 = pi*(dbh0_cm/200)^2,
           dbh2.W_cm = (dbh_cm^2)*weight, 
           dqm_cm = sqrt(sum(dbh2.W_cm, na.rm = TRUE)/sum(weight, na.rm  = TRUE))) %>%
    ungroup() %>%
    # - Calculate tree height from allometric relations
    mutate(height.allometry = case_when(
      str == "uneven" ~ 1.3 + a1*(1 + a2)*(1 + a5*ba)*(1 - exp(-a6*dqm_cm^a7))*(
        1/(1 + (b1*(1 + b2)*exp(-b5*ba))/((dbh_cm/dqm_cm)^d1))), 
      str %in% c("cop", "nostr") ~ 1.3 + a1*(1 + a4)*(1 + a5*ba)*(1 - exp(-a6*dqm_cm^a7))*(
        1/(1 + (b1*(1 + b4)*exp(-b5*ba))/((dbh_cm/dqm_cm)^d1))), 
      str == "copfor" ~ 1.3 + a1*(1 + a3)*(1 + a5*ba)*(1 - exp(-a6*dqm_cm^a7))*(
        1/(1 + (b1*(1 + b3)*exp(-b5*ba))/((dbh_cm/dqm_cm)^d1))), 
      str == "even" ~ 1.3 + a1*(1 + a5*ba)*(1 - exp(-a6*dqm_cm^a7))*(
        1/(1 + (b1*exp(-b5*ba))/((dbh_cm/dqm_cm)^d1))))) %>%
    # - Calculate tree height 5 years before from allometric relations
    mutate(height0.allometry = case_when(
      str == "uneven" ~ 1.3 + a1*(1 + a2)*(1 + a5*ba.0)*(1 - exp(-a6*dqm_cm^a7))*(
        1/(1 + (b1*(1 + b2)*exp(-b5*ba.0))/((dbh0_cm/dqm_cm)^d1))), 
      str %in% c("cop", "nostr") ~ 1.3 + a1*(1 + a4)*(1 + a5*ba.0)*(1 - exp(-a6*dqm_cm^a7))*(
        1/(1 + (b1*(1 + b4)*exp(-b5*ba.0))/((dbh0_cm/dqm_cm)^d1))), 
      str == "copfor" ~ 1.3 + a1*(1 + a3)*(1 + a5*ba.0)*(1 - exp(-a6*dqm_cm^a7))*(
        1/(1 + (b1*(1 + b3)*exp(-b5*ba.0))/((dbh0_cm/dqm_cm)^d1))), 
      str == "even" ~ 1.3 + a1*(1 + a5*ba.0)*(1 - exp(-a6*dqm_cm^a7))*(
        1/(1 + (b1*exp(-b5*ba.0))/((dbh0_cm/dqm_cm)^d1))))) %>%
    # - use allometric height when no observed height
    mutate(height = ifelse(is.na(height), height.allometry, height)) %>%
    # - Calculate a rectified height when below 5 meters
    mutate(height.rectif = ifelse(height < 5, 5, height), 
           height.rectif.allom = ifelse(height.allometry < 5, 5, height.allometry), 
           height0.rectif.allom = ifelse(height0.allometry < 5, 5, height0.allometry)) %>%
    # - Remove residual NAs in plots where there are trees with unknown height or weight
    group_by(IDP) %>% mutate(anyNA = any(is.na(height)) | any(is.na(weight))) %>% 
    ungroup() %>% filter(!anyNA) %>%
    
    # GET TREE AERIAL VOLUME FROM EMERGE COEFFICIENTS
    # - join volume coefficients
    left_join(coef_volume_per_species, by = "species") %>%
    # - calculate aerial volume based on emerge
    mutate(c130 = pi*dbh/1000, 
           volume.emerge = (height*c130^2)/(4*pi*(1-1.3/height.rectif)^2) * (
             aEmerge + bEmerge*sqrt(c130)/height.rectif + cEmerge*height.rectif/c130)) %>%
    # - calculate volume increment based on emerge and allometric height
    mutate(c130.0 = pi*dbh0_cm/100, 
           volume.emerge.allom = (height.allometry*c130^2)/(4*pi*(1-1.3/height.rectif.allom)^2) * (
             aEmerge + bEmerge*sqrt(c130)/height.rectif.allom + cEmerge*height.rectif.allom/c130), 
           volume.emerge.allom.0 = (height0.allometry*c130.0^2)/(4*pi*(1-1.3/height0.rectif.allom)^2) * (
             aEmerge + bEmerge*sqrt(c130)/height0.rectif.allom + cEmerge*height0.rectif.allom/c130), 
           volume.increment = (volume.emerge.allom - volume.emerge.allom.0)/5) %>%
    # - calculate root volume using expansion factor
    mutate(volume.root = volume*0.29) %>%
    # - timber volume (merchandable) and total volume per ha
    mutate(alive = ifelse(status == "alive", 1, 0), 
           volume.tot.living.ha = alive*(volume.emerge + volume.root)*weight, 
           volume.tot.deadwood.ha = (1 - alive)*(volume.emerge + volume.root)*weight, 
           volume.increment.ha = alive*volume.increment) %>%
    
    # GET dry mass and thus carbon mass
    # - join Wood density data
    left_join(wd_per_species, by = "species") %>%
    # - Calculate wood mass per ha for living, dead and increment
    mutate(mass.tot.living.ha = wood.density*volume.tot.living.ha, 
           mass.tot.deadwood.ha = wood.density*volume.tot.deadwood.ha*0.9, 
           mass.increment.ha = wood.density*volume.increment.ha) %>%
    # - Calculate carbon mass per ha
    mutate(Cmass.tot.ha = 0.5*(mass.tot.deadwood.ha + mass.tot.living.ha), 
           Cmass.tot.ha.yr = 0.5*mass.increment.ha) %>%
    # - sum per plot timber volume and carbon mass
    group_by(IDP) %>%
    summarize(timber.volume_m3.ha = sum(volume.tot.living.ha, na.rm = TRUE), 
              Cstock_standing_t.ha = sum(Cmass.tot.ha, na.rm = TRUE)/1000, 
              Csequestr_kg.ha.yr = sum(Cmass.tot.ha.yr, na.rm = TRUE), 
              volume_deadwood.standing_m3.ha = sum(volume.tot.deadwood.ha)) %>%
    # - Add lying dead wood data
    left_join(deadwood.per.plot, by = "IDP") %>%
    mutate(volume_deadwood_m3.ha = volume_deadwood.standing_m3.ha + volume_deadwood.lying_m3.ha, 
           Cstock_t.ha = Cstock_deadwood.lying_kg.ha/1000 + Cstock_standing_t.ha) %>%
    select(IDP, Csequestr_kg.ha.yr, 
           timber.volume_m3.ha,
           # volume_deadwood_m3.ha, 
           Cstock_t.ha)
  
  # Return output
  return(out)
  
}

#' Calculate soil erosion mitigation by forest
#' @param FrenchNFI_ecology_raw Raw ecological data
#' @param NFIMed_tree Tree data formatted
#' @param clim_and_soil climate and soil data extracted for each plot
#' @param tree.species_info Taxonomic info on each tree species
get_service_erosion = function(FrenchNFI_ecology_raw, NFIMed_tree, 
                               clim_and_soil, tree.species_info){
  
  
  # Attribute each plot to broadleaf, conifer or mixed. 
  plot.type = NFIMed_tree %>%
    # Calculate basal area per ha per tree
    mutate(ba.ha = ba*weight) %>%
    # Add classification of species as conifer / broadleaf
    left_join(tree.species_info %>% select(species = species.original, group), 
              by = "species") %>%
    # Calculate proportion of broadleaf / conifer per plot
    group_by(IDP, group) %>%
    summarise(ba.group = sum(ba.ha, na.rm = TRUE)) %>%
    ungroup() %>% group_by(IDP) %>%
    mutate(ba.group.prop = ba.group/sum(ba.group, na.rm = TRUE)) %>%
    # Re-format to have one column as broadleaf prop and one as conifer prop
    ungroup() %>% select(-ba.group) %>%
    pivot_wider(id_cols = "IDP", values_from = "ba.group.prop", 
                names_from = group) %>% select(-`NA`) %>%
    mutate(angiosperms = ifelse(is.na(angiosperms), 0, angiosperms), 
           gymnosperms = ifelse(is.na(gymnosperms), 0, gymnosperms)) %>%
    # - Rescale so that the sum is 1 (for plots where there is NA)
    mutate(sum_prop = angiosperms + gymnosperms, 
           angiosperms = angiosperms/sum_prop, 
           gymnosperms = gymnosperms/sum_prop) %>%
    # Attribute plot type depending on the relative proportion of conifer and broadleaf
    mutate(type = case_when(
      angiosperms > 0.85 & gymnosperms < 0.15 ~ "broadleaf", 
      gymnosperms > 0.85 & angiosperms < 0.15 ~ "conifer", 
      TRUE ~ "mixed")) %>%
    select(IDP, type)
  
  
  # Start from tree data
  out = NFIMed_tree %>%
    # Caclulate stand height
    group_by(IDP) %>%
    summarize(height.mean = mean(height, na.rm = TRUE)) %>%
    ungroup() %>%
    # Add canopy cover (FC) and ground vegetation cover (PC)
    left_join(FrenchNFI_ecology_raw %>%
                mutate(FC = LIGN2/10, PC = HERB/10) %>%
                select(IDP, FC, PC), by = "IDP") %>%
    # Add forest type
    left_join(plot.type, by = "IDP") %>%
    # Calculate C factor based on Guo et al. (2023) - Forest. Ecol. Manag.
    mutate(
      SCs = case_when(
        type == "broadleaf" ~ 0.283861*FC + 0.378175*PC, 
        type == "conifer" ~ 0.256368*FC + 0.378175*PC, 
        type == "mixed" ~ 0.292352*FC + 0.378175*PC), 
      C = case_when(
        SCs > 0.1 & SCs < 0.35 ~ 0.48*exp(-7.031*SCs), 
        SCs >= 0.35 ~ 0.002*SCs^(-2.359), 
        TRUE ~ NA_real_
      )) %>%
    # Add longitude, climate and soil variables
    left_join(clim_and_soil, by = "IDP") %>%
    # Calculate rainfall Erosivity (R)
    mutate(R = 0.117*pr*sqrt(prmax)*(2 + 0.015*longitude)) %>%
    # Calculate erosion mitigation
    mutate(erosion.mitig = R*LS_factor*K_factor*(1 - C)) %>%
    # Keep only columns of interest
    select(IDP, erosion.mitig)
  
  # Return output
  return(out)
  
}



#' Function to merge service data associated with different sources
#' @param list.in list of dataframe contianing plot-level services data
#' @param plots_filtered Plots to keep for the analyses
merge_service = function(list.in, plots_filtered){
  
  # Loop on all source of services
  for(i in 1:length(names(list.in))){
    
    # Format data from source i
    data.i = list.in[[i]] %>%
      filter(IDP %in% plots_filtered) %>%
      gather(key = "service", value = "service.value", colnames(.)[2:dim(.)[2]])
    
    # Compile into final dataset
    if(i == 1) data = data.i
    else data = rbind(data, data.i)
  }
  
  # Format into one dataframe with one service per column
  out = data %>% 
    # For a few data in carbon sequestration, slightly negative values instead of 0
    mutate(service.value = ifelse(service.value < 0, 0, service.value)) %>%
    spread(key = "service", value = "service.value")
  
  # Return output dataframe
  return(out)
  
}

#' Function to calculate all plot-level explanatory variables
#' @param NFIMed_tree Tree-level NFI data
#' @param NFIMed_plot Plot-level NFI data
#' @param chelsa_prec_file Chelsa raster of precipitation
#' @param chelsa_pet_file Chelsa raster of potential evapotranspiration 
#' @param chelsa_sgdd_file Chelsa raster of sgdd
#' @param elevation_raster Elevation raster for France metropolitan area
compile_explanatory = function(NFIMed_tree, NFIMed_plot, chelsa_prec_file, 
                               chelsa_pet_file, chelsa_sgdd_file, elevation_raster){
  
  # Complete dataset
  data.full = NFIMed_tree %>%
    mutate(ba.ha = pi*(dbh/2000)^2*weight)
  
  # Species-level data
  data.species = data.full %>%
    group_by(IDP, species) %>%
    summarize(ab = sum(ba.ha, na.rm = TRUE)) %>%
    ungroup() %>% group_by(IDP) %>%
    mutate(p = ab/sum(ab, na.rm = TRUE), 
           plnp = p*log(p)) %>%
    summarize(shannon = -sum(plnp, na.rm = TRUE), 
              richness = n()) %>%
    ungroup()
  
  # Structural data
  data.str = data.full %>%
    mutate(dbh2.W = (dbh^2)*weight) %>%
    group_by(IDP) %>%
    mutate(dbh.mean = weighted.mean(dbh, w = weight, na.rm = TRUE), 
           w.diffdbh2 = weight*(dbh - dbh.mean)^2) %>% 
    summarize(ba.tot = sum(ba.ha, na.rm = TRUE), 
              dqm = sqrt(sum(dbh2.W, na.rm = TRUE)/sum(weight, na.rm  = TRUE)), 
              str.div = sqrt(sum(w.diffdbh2, na.rm = TRUE)/sum(weight, na.rm  = TRUE))) %>%
    ungroup()
  
  # Climate data
  # - Initialize data
  data.clim = NFIMed_plot %>% select(IDP, longitude, latitude)
  # - Read raster of annual precipitations, pet, elevation and sgdd
  raster_pr = terra::rast(chelsa_prec_file)
  raster_pet = terra::rast(chelsa_pet_file)
  raster_sgdd = terra::rast(chelsa_sgdd_file)
  raster_elev = terra::rast(elevation_raster)
  # - Extract raster values
  data.clim$pr <- as.numeric(terra::extract(
    raster_pr, cbind(data.clim$longitude, data.clim$latitude))[, 1])
  data.clim$pet <- as.numeric(terra::extract(
    raster_pet, cbind(data.clim$longitude, data.clim$latitude))[, 1])
  data.clim$sgdd <- as.numeric(terra::extract(
    raster_sgdd, cbind(data.clim$longitude, data.clim$latitude))[, 1])
  data.clim$elev <- as.numeric(terra::extract(
    raster_elev, cbind(data.clim$longitude, data.clim$latitude))[, 1])
  # - Calculate water availability index
  data.clim = data.clim %>% mutate(wai = (pr - pet)/pr)
  # - Add pca axis
  pca = prcomp((data.clim %>% dplyr::select(sgdd, wai)), center = T, scale = T)
  data.clim$pca1 = get_pca_ind(pca)[[1]][, 1]
  
  # Compile all data together
  out = NFIMed_plot %>%
    left_join(data.str, by = "IDP") %>%
    left_join(data.species, by = "IDP") %>%
    left_join(data.clim %>% select(IDP, elev, sgdd, wai, pca_clim = pca1), by = "IDP")
  
  # Return output 
  return(out)
  
}

#' Compile all environmental variable used to study the temporal dynamics
#' @param FrenchNFI_plot_raw Raw French NFI data at the plot level
#' @param data_explanatory Explanatory variables at the plot level
#' @param file.str.out Name of the file to export for the plot of the structure PCA
compile_explanatory_ser = function(FrenchNFI_plot_raw, data_explanatory, 
                                   file.str.out){
  
  # Create directory for the structure PCA plot
  create_dir_if_needed(file.str.out)
  
  # Calculate harvest frequency
  harv.per.SER = FrenchNFI_plot_raw %>% 
    filter(VISITE == 1 & CAMPAGNE >= 2009) %>%
    mutate(GEST = GEST/2) %>%
    group_by(SER) %>%
    summarize(prop.management = mean(GEST, na.rm = TRUE)) %>%
    ungroup() %>% rename(ecoregion = SER)
  
  # Calculate disturbance frequency
  dist.per.SER = FrenchNFI_plot_raw %>%
    filter(VISITE == 2 & CAMPAGNE >= 2015) %>%
    mutate(dist.occurence = ifelse(NINCID == 0, 0, 1)) %>%
    group_by(SER) %>%
    summarize(dist.freq = mean(dist.occurence, na.rm = TRUE)) %>%
    ungroup() %>% rename(ecoregion = SER)
  
  # Calculate average climate per SER
  clim.per.SER = data_explanatory %>%
    group_by(ecoregion) %>%
    summarize(pca_clim.mean = mean(pca_clim, na.rm = TRUE))
  
  # Dominant structure per plot
  # - Average structure per type and ecoregion
  str.per.SER = data_explanatory %>%
    select(IDP, ecoregion, str) %>%
    mutate(str.bin = 1) %>%
    pivot_wider(names_from = "str", values_from = str.bin) %>%
    mutate(across(c(3:dim(.)[2]), ~ ifelse(is.na(.x), 0, .x))) %>%
    gather(key = "str", value = "occurence", unique(data_explanatory$str)) %>%
    group_by(ecoregion, str) %>%
    summarize(mean = mean(occurence, na.rm = TRUE)) %>%
    pivot_wider(values_from = mean, names_from = str) %>% ungroup()
  # - Make pca with str proportion
  pca = prcomp((str.per.SER %>% dplyr::select(-ecoregion)), center = T, scale = T)
  # - Final data 
  str.per.SER = str.per.SER %>%
    mutate(pca1_str = get_pca_ind(pca)[[1]][, 1], 
           pca2_str = get_pca_ind(pca)[[1]][, 2]) %>%
    select(ecoregion, pca1_str, pca2_str)
  # - Extract PCA results
  pca_data <- as.data.frame(pca$x[, 1:2])
  var_data <- as.data.frame(pca$rotation[, 1:2])
  var_data$vars <- rownames(var_data)
  var_percent <- pca$sdev^2 / sum(pca$sdev^2) * 100
  # - Create plot
  plot.str = ggplot() +
    # Individual points
    geom_point(data = pca_data, aes(x = PC1, y = PC2), color = "blue", alpha = 0.5) +
    # Variable arrows
    geom_segment(data = var_data, 
                 aes(x = 0, y = 0, xend = PC1*3, yend = PC2*3), 
                 arrow = arrow(length = unit(0.2, "cm")), 
                 color = "red") +
    # Variable labels
    geom_text(data = var_data, 
              aes(x = PC1*3.2, y = PC2*3.2, label = vars), 
              color = "red", size = 4) +
    # Axis labels with variance percentages
    labs(x = paste0("pca1_str (", round(var_percent[1], 1), "%)"),
         y = paste0("pca2_str (", round(var_percent[2], 1), "%)")) +
    theme(panel.background = element_rect(color = 'black', fill = 'white'), 
          panel.grid = element_blank()) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
    coord_fixed()  # Keep aspect ratio 1:1 for proper interpretation
  # - Save the plot
  ggsave(file.str.out, plot.str, width = 13, height = 13, 
         units = "cm", dpi = 600, bg = "white")
  
  # Compile all data
  out = dist.per.SER %>%
    left_join(harv.per.SER, by = "ecoregion") %>%
    left_join(str.per.SER, by = "ecoregion") %>%
    left_join(clim.per.SER, by = "ecoregion")
  
  # Return output dataset
  return(out)
  
}

#' Estimate the temporal trend of each service in each ecoregion
#' @param data_services service value per plot
#' @param NFIMed_plot NFI plot-level data
#' @param service_table table containing the title and distribution of each service
get_temporal_trend = function(data_services, NFIMed_plot, service_table){
  
  # Prepare the dataset for each model
  data.in = data_services %>%
    gather(key = "service", value = "service.value", colnames(.)[-1]) %>%
    left_join(NFIMed_plot %>% select(IDP, ecoregion, year), by = "IDP")
  
  # Prepare the output dataset
  out = expand.grid(service = service_table$service, 
                    ecoregion = unique(NFIMed_plot$ecoregion)) %>%
    mutate(estimate = NA_real_, estimate_lwr = NA_real_, estimate_upr = NA_real_, 
           estimate_se = NA_real_, pval = NA_real_, n = NA_real_) %>%
    left_join(service_table %>% select(service, distrib), by = "service")
  
  # Loop on all service - ecoregion combination
  for(i in 1:dim(out)[1]){
    
    # Prepare data
    # - subset for the right service and ecoregion
    data.i = data.in %>% 
      filter(service == out$service[i] & ecoregion == out$ecoregion[i]) %>%
      select(year, service.value) %>%
      drop_na() 
    # - Get the average service value the first year
    s0.i = (data.i %>%
      filter(year == min(year, na.rm = TRUE)) %>%
      summarize(mean = mean(service.value, na.rm = TRUE)))$mean
    # - Calculate change in percentage
    data.i = data.i %>% 
      mutate(service.trend.percent = 100*(service.value - s0.i)/s0.i, 
             time = year - min(data.i$year, na.rm = TRUE))
    
    # Run model depending on the distribution
    model.i = lm(service.trend.percent ~ 0 + time, data = data.i)
    
    # Output of the model
    stat.i = tidy(model.i, conf.int = TRUE) %>% filter(term != "(Intercept)")
    # Fill output dataset
    out$estimate[i] = stat.i$estimate
    out$estimate_lwr[i] = stat.i$conf.low
    out$estimate_upr[i] = stat.i$conf.high
    out$estimate_se[i] = stat.i$std.error
    out$pval[i] = stat.i$p.value
    out$n[i] = dim(data.i)[1]
  }
  
  # Return output
  return((out %>% select(-distrib)))
  
}
