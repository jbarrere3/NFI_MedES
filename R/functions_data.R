#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#' @name functions_data.R  
#' @description R script containing all functions relative to data
#               importation and formatting
#' @author Natheo Beauchamp, Julien Barrere
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
    # Filter ecological regions
    filter(SER %in% c("J10", "J21", "J22", "J23", "J24", 
                      "J30", "J40", "K11", "K12", "K12")) %>%
    # change coordinates
    st_as_sf(coords = c("XL", "YL"), crs = 2154, agr = "constant") %>%
    st_transform(crs = 4326) %>% 
    mutate(longitude = st_coordinates(.)[,1],
           latitude = st_coordinates(.)[,2])  %>%
    st_drop_geometry() %>%
    # Select columns
    select(IDP, year = CAMPAGNE, longitude, latitude, ecoregion = SER) 
  
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
           volume = V, weight = W) %>%
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
      # Extract the name of the species
      data.species$species.full[i] = verified_i$matchedCanonicalSimple[1]
      # Add without the sub species
      if(length(str_split(data.species$species.full[i], " ")[[1]]) > 1){
        data.species$species[i] = paste(str_split(
        data.species$species.full[i], " ")[[1]][1:2], collapse = " ")
      }
    } else {
      # Otherwise, try by splitting directly the name of the species
      if(length(split.i) > 3){
        verified_i = try(expr = gna_verifier(paste(split.i[1:2], collapse = " ")), silent = TRUE)
        # If it works, add species name directly from the match
        if(!(any(class(verified_i) == "try-error"))){
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
merge_species_scores = function(flora.species.list, flora.species.with.score_file){
  
  # Format the edibility and medicinal metrics
  flora.species.with.score = fread(flora.species.with.score_file) %>%
    mutate(sp = paste(genus, species, sep = " "), 
           edibility = as.numeric(substr(edibility_score, 2, 2)), 
           medicinal = as.numeric(substr(medicinal_score, 2, 2))) %>%
    select(species = sp, edibility, medicinal)
  
  # Join to the species list
  out = flora.species.list %>%
    left_join(flora.species.with.score, by = "species")
  
  # Return output
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
              ab.edibility = 1 - prod(inv.edi), 
              shannon = -sum(plnp))
  
  # Return output
  return(out)
}
