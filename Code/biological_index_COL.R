
library(tidyr)
library(dplyr)
library(tibble)
library(vegan)

SCORES <-list(

        BMWP <- list(
       
            "Tubificidae" = 1,
            "Naididae" = 1,
            
            "Culicidae" = 2 , 
            "Chironomidae" =  2,
            "Muscidae" = 2, 
            "Sciomyzidae" = 2,
            
            "Ceratopogonidae" = 3, 
            "Glossiphoniidae" = 3, 
            "Cyclobdellidae" = 3,
            "Hydrophilidae" = 3,
            "Physidae" = 3, 
            "Tipulidae" = 3,
            
            "Chrysomelidae" = 4,
            "Stratiomyidae" = 4,
            "Haliplidae" = 4,
            "Empididae" = 4, 
            "Dolicopodidae" = 4, 
            "Sphaeridae" = 4, 
            "Lymnaeidae" = 4, 
            "Hydraenidae" = 4, 
            "Hydrometridae" = 4,
            "Noteridae" = 4,
            
            "Belostomatidae" = 5, 
            "Gelastocoridae" = 5,
            "Hydropsychidae" = 5,
            "Mesoveliidae" = 5,
            "Nepidae" = 5, 
            "Planorbiidae" = 5, 
            "Pyralidae" = 5,
            "Tabanidae" = 5, 
            "Thiaridae" = 5,
            
            "Aeshnidae" = 6,
            "Ancylidae" = 6, 
            "Corydalidae" = 6,
            "Elmidae" = 6, 
            "Libellulidae" = 6, 
            "Limnichidae" = 6, 
            "Lutrochidae" = 6, 
            "Megapodagrionidae" = 6,
            "Sialidae" = 6, 
            "Staphylinidae" = 6,
            
            "Baetidae" = 7,
            "Caenidae" = 7,
            "Calopterygidae" = 7,
            "Coenagrionidae" = 7,
            "Corixidae" = 7,
            "Dixidae" = 7,
            "Dryopidae" = 7, 
            "Glossossomatidae" = 7,
            "Hyalellidae" = 7,
            "Hydroptilidae" = 7, 
            "Hydropsychidae" = 7, 
            "Leptohyphidae" = 7,
            "Naucoridae" = 7, 
            "Notonectidae" = 7, 
            "Planariidae" = 7,
            "Psychodidae" = 7, 
            "Scirtidae" = 7,
            
             "Gerridae" = 8,
            "Hebridae" = 8, 
            "Helicopsychidae" = 8, 
            "Hydrobiidae" = 8, 
            "Leptoceridae" = 8,
            "Lestidae" = 8,
            "Palaemonidae" = 8, 
            "Pleidae" = 8,
            "Pseudothelpusidae" = 8, 
            "Saldidae" = 8,
            "Simuliidae" = 8,
            "Veliidae" = 8,
            
            "Ampullariidae" = 9, 
            "Dytiscidae" = 9,
            "Ephemeridae" = 9, 
            "Euthyplociidae" = 9,
            "Gyrinidae" = 9,
            "Hydrobiosidae" = 9,
            "Leptophlebiidae" = 9, 
            "Philopotamidae" = 9, 
            "Polycentropodidae" = 9, 
            "Xiphocentronidae" = 9,
            
            "Anomalopsychidae" = 10, 
            "Atriplectididae" = 10,
            "Blepharoceridae" = 10, 
            "Calamoceratidae" = 10,
            "Ptilodactylidae" = 10, 
            "Chordodidae" = 10,
            "Gomphidae" = 10, 
            "Hidridae" = 10,
            "Lampyridae" = 10, 
            "Lymnessiidae" = 10, 
            "Odontoceridae" = 10, 
            "Oligoneuriidae" = 10, 
            "Perlidae" = 10, 
            "Polythoridae" = 10, 
            "Psephenidae" = 10
    ),
        IBF_ad <-list(  

            "Glossiphoniidae" = 7,
            "Tubificidae" = 9,
            "Naididae" = 9,
            "Ancylidae" = 4,
            "Physidae" = 7,
            "Hydrobiidae" = 7,
            "Limnaeidae" = 7,
            "Planorbidae" = 7,
            "Sphaeriidae" = 7,
            "Hyriidae" = 7,
            "Hyalellidae" = 4,
            "Hydracarina" = 6,
            "Baetidae" = 6,
            "Leptophlebiidae" = 0,
            "Leptohyphidae" = 3,
            "Oligoneuridae" = 0,
            "Aeshnidae" = 4,
            "Gomphidae" = 2,
            "Libellulidae" = 4,
            "Coenagríonidae" = 4,
            "Calopterygidae" = 2,
            "Polythoridae" = 0,
            "Perlidae" = 0,
            "Gripopterygidae" = 0,
            "Veliidae" = 5,
            "Gerridae" = 5,
            "Corixidae" = 5,
            "Notonectidae" = 5,
            "Belostomatidae" = 6,
            "Naucoridae" = 5,
            "Helicopsychidae" = 0,
            "Calamoceratidae" = 0,
            "Odontoceridae" = 0,
            "Leptoceridae" = 2,
            "Polycentropodidae" = 2,
            "Hydroptilidae" = 4,
            "Xiphocentronidae" = 2,
            "Hydrobiosidae" = 2,
            "Glossosomatidae" = 3,
            "Hydropsychidae" = 5,
            "Anomalopsychidae" = 0,
            "Philopotamidae" = 2,
            "Limnephilidae" = 3,
            "Pyralidae" = 6,
            "Ptilodactylidae" = 5,
            "Lampyridae" = 5,
            "Psephenidae" = 5,
            "Scirtidae" = 0,
            "Helodidae" = 5,
            "Staphylinidae" = 7,
            "Elmidae" = 5,
            "Dryopidae" = 5,
            "Gyrinidae" = 7,
            "Dytiscidae" = 7,
            "Hydrophilidae" = 7,
            "Hydraenidae" = 5,
            "Athericidae" = 0,
            "Blepharoceridae" = 0,
            "Simuliidae" = 5,
            "Tabanidae" = 6,
            "Tipulidae" = 5,
            "Limoniidae" = 6, 
            "Ceratopogonidae" = 6,
            "Dixidae" = 6,
            "Psychodidae" = 7,
            "Dolichopodidae" = 6,
            "Stratiomyidae" = 6,
            "Empididae" = 6,
            "Chironomidae" = 8,
            "Culicidae" = 8,
            "Muscidae" = 8,
            "Ephydridae" = 8,
            "Syrphidae" = 9,
            "Coenagrionidae" = 8,
            "Lymnaeidae" = 6,
            "Planariidae" = 5,
            "Lumbricidae" = 8
    ),
        
    IBF  <- list(

              "Ameletidae" = 0,
              "Baetidae" = 5,
              "Baetiscidae" = 4,
              "Caenidae" = 6,
              "Ephemerellidae" = 1,
              "Ephemeridae" = 3,
              "Heptageniidae" = 3,
              "Isonychiidae" = 2,
              "Leptophlebiidae" =	3,
              "Leptohyphidae" = 4,
              "Metretopodidae" = 2,
              "Oligoneuriidae" = 2,
              "Polymitarcyidae" = 2,
              "Potomanthidae" = 4,
              "Siphlonuridae" = 4,
              "Tricorythidae" = 4,
              "Aeshnidae" = 3,
              "Calopterygidae" = 6,
              "Coenagrionidae" = 8,
              "Cordulegastridae" = 3,
              "Corduliidae" = 2,
              "Gomphidae" = 3,
              "Lestidae" = 6,
              "Libellulidae" = 2,
              "Macromiidae" =2,
              "Capniidae" = 2,
              "Chloroperlidae" = 0,
              "Leuctridae" = 0,
              "Nemouridae" = 	2,
              "Peltoperlidae" = 0,
              "Perlidae" = 2,
              "Perlodidae" = 2,
              "Pteronarcyidae" = 0,
              "Taeniopterygidae" = 2,
              "Corixidae" = 5,
              "Apataniidae" = 3,
              "Brachycentridae" = 1,
              "Calamoceratidae" = 3,
              "Dipseudopsidae" = 5,
              "Glossosomatidae" = 1,
              "Goeridae" = 3,
              "Helicopsychidae" = 3,
              "Hydropsychidae" = 4,
              "Hydroptilidae" = 4,
              "Lepidostomatidae" = 1,
              "Leptoceridae" = 4,
              "Limnephilidae" = 3,
              "Molannidae" = 6,
              "Odontoceridae" = 0,
              "Philopotamidae" = 3,
              "Phryganeidae" = 4,
              "Polycentropodidae" = 6,
              "Psychomyiidae" = 2,
              "Rhyacophilidae" = 1,
              "Sericostomatidae" = 3,
              "Uenoidae" = 3,
              "Arctiidae" = 5,
              "Nepticulidae" = 5,
              "Pyralidae" = 5,
              "Curculionidae" = 5,
              "Dryopidae" = 5,
              "Dytiscidae" = 5,
              "Elmidae" = 4,
              "Gyrinidae" = 4,
              "Haliplidae" = 5,
              "Hydrophilidae" = 5,
              "Psephenidae" = 4,
              "Ptilodactylidae" = 3,
              "Scirtidae" = 5,
              "Corydalidae" = 4,
              "Sialidae" = 4,
              "Anthomyiidae" = 6,
              "Athericidae" = 4,
              "Blephariceridae" = 0,
              "Ceratopogonidae" = 6,
              "Chaoboridae" = 8,
              "Chironomidae" = 8,
              "Culicidae" = 8,
              "Dolichopodidae" = 4,
              "Dixidae" = 1,
              "Dolochopodidae" = 4,
              "Empididae" = 6,
              "Ephydridae" = 6,
              "Muscidae" = 6,
              "Psychodidae" = 8,
              "Ptychopteridae" = 9,
              "Scathophagidae" = 6,
              "Simuliidae" = 6,
              "Stratiomyidae" = 7,
              "Syrphidae" = 10,
              "Tabanidae" = 5,
              "Tanyderidae" = 3,
              "Tipulidae" = 3,
              "Arrenuridae" = 6,
              "Lebertiidae" = 6,
              "Atractideidae" = 6,
              "Mideopsidae" = 6,
              "Tyrellidae" = 6,
              "Limnesidae" = 6,
              "Limnocharidae" = 6,
              "Sperchonidae" = 6,
              "Unionicolidae" = 6,
              "Anthuridae" = 5,
              "Idoteidae" = 5,
              "Asellidae" = 8,
              "Crangonyctidae" = 6,
              "Gammaridae" = 6,
              "Oedicerotidae" = 5,
              "Hyalellidae" = 8,
              "Physidae" = 8,
              "Lymnaeidae" = 6,
              "Planorbidae" = 7,
              "Ancylidae" = 6,
              "Corbiculidae" = 6,
              "Dreisseniidae" = 8,
              "Sphaeriidae" = 6,
              "Pisidiidae" = 8,
              "Enchytraeidae" = 10,
              "Tubificidae" = 9,
              "Naididae" = 8,
              "Glossiphoniidae" = 6
      
    )
)

bmwp_COL <- function(data, ASPT = FALSE ) {
    
    ## Take a numeric matrix with the families in the rows and the sample sites
    ## in the colums. Return a data frame  with the BMWP Scores or ASPT
        
    family_scores <- SCORES$BMWP
    
    data <- data[row.names(data) %in% names(family_scores),]
    
    for (i in 1:nrow(data)) {
      
         family <- row.names(data)[i]
         presence <- data[i,] > 0
         
         data[i, presence] <- family_scores[[family]]
         
    }
      
    bmwp_scores <- colSums(data) 
    
    if (!ASPT) {
      
        quality_class <- vector("character", length = length(bmwp_scores))
        quality_number <- vector("integer", length = length(bmwp_scores))
        
        for (i in 1:length(bmwp_scores)) {
            
          if (bmwp_scores[i] <= 10) {
              quality_class[i] <- "Muy Pobre"
              quality_number[i] <- 1 }
          
          else if ( 10 < bmwp_scores[i] & bmwp_scores[i] <= 40 ) {
              quality_class[i] <- "Pobre" 
              quality_number[i] <- 2 }
          
          else if ( 40 < bmwp_scores[i] & bmwp_scores[i] <= 70 ) {
              quality_class[i] <- "Moderada"
              quality_number[i] <- 3 }
          
          else if ( 70 < bmwp_scores[i] & bmwp_scores[i] <= 100 ) {
              quality_class[i] <- "Buena"
              quality_number[i] <- 4 }
          
          else if ( 100 < bmwp_scores[i] ) {
              quality_class[i] <- "Excelente"
              quality_number[i] <- 5 }
        }
        
        data_frame <- data.frame(bmwp_scores, quality_class, quality_number )
        
        return(data_frame)
        
    } else { 
      
        total_families <-nrow(data) - colSums(data == 0) # Exclude non present families
        ASPT <- round(bmwp_scores / total_families , 2)
        
        return(ASPT)
    }
    
}

hillsenhoff_index <- function(data, scores_adjusted = TRUE ) {
  
  ## Take a numeric matrix with the families in the rows and the sample sites
  ## in the colums. Return a data frame  with the IBF index
  
  if (scores_adjusted) { 
    family_scores <- SCORES$IBF_ad ## Based in Andean Biotic Index Scores
  } else {   
    family_scores <- SCORES$IBF 
  }
  
  data <- data[row.names(data) %in% names(family_scores),]
  n_ind <- colSums(data) 
  
  for (i in 1:nrow(data)) {
    
      family <- row.names(data)[i]
      presence <- data[i,] > 0
      
      data[i, presence] <- data[i, presence] * family_scores[[family]]
   
  }
  
  hillsenhoff_scores <- round( colSums(data) / n_ind , digits = 2 )
  
  quality_class <- vector("character", length = length(hillsenhoff_scores))
  quality_number <- vector("integer", length = length(hillsenhoff_scores))
  
  for (i in 1:length(hillsenhoff_scores)) {
    
    if (hillsenhoff_scores[i] <= 3.75) {
      quality_class[i] <- "Excelente"
      quality_number[i] <- 7 }
    
    else if ( 3.75 < hillsenhoff_scores[i] & hillsenhoff_scores[i] <= 4.25 ) {
      quality_class[i] <- "Muy Buena" 
      quality_number[i] <- 6 }
    
    else if ( 4.25 < hillsenhoff_scores[i] & hillsenhoff_scores[i] <= 5 ) {
      quality_class[i] <- "Buena"
      quality_number[i] <- 5 }
    
    else if ( 5 < hillsenhoff_scores[i] & hillsenhoff_scores[i] <= 5.75 ) {
      quality_class[i] <- "Regular"
      quality_number[i] <- 4 }
    
    else if ( 5.75 < hillsenhoff_scores[i] & hillsenhoff_scores[i] <= 6.50 ) {
      quality_class[i] <- "Malsana"
      quality_number[i] <- 3 }
    
    else if ( 6.50 < hillsenhoff_scores[i] & hillsenhoff_scores[i] <= 7.25 ) {
      quality_class[i] <- "Nociva"
      quality_number[i] <- 2 }
    
    else if ( 7.25 < hillsenhoff_scores[i] ) {
      quality_class[i] <- "Muy Nociva"
      quality_number[i] <- 1 }
  }
  
  data_frame <- data.frame(hillsenhoff_scores, quality_class, quality_number )
  
  return(data_frame)
}

abundance_metrics <- function(data, by_stations = TRUE, absolute = FALSE ) {
  
  ## This function takes a data frame with organisms in the columns (first row with
  ## estations) and samples in the rows. Return a data frame with the
  ## absolute/relative abundance by each station
  
  estations <- names(data)[[1]]
  
  if (by_stations) {
    
    abundance <- group_by(data, across(1)) %>%
      summarise(across(where(is.numeric), sum)) %>%
      column_to_rownames(var = estations)
    
  } else {
    
    abundance <- data %>%
      summarise(across(where(is.numeric), sum))
  }
  
  total <- rowSums(abundance)
  
  relative_abundance <- round(( abundance / total ) * 100, 2)
  
  if (absolute) { 
    
    return(abundance)
    
  } else {
      
    return(relative_abundance)
    
  }
  
}
  
  
  
  
