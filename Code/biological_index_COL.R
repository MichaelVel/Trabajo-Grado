
data <- read.csv("Data/abundancia.csv") %>% 
    select(-c(Tipo_Estacion, Muestra)) %>%
    group_by(Estacion) %>%
    summarise(across(everything(), sum)) %>%
    column_to_rownames("Estacion") %>%
    as.matrix() %>%
    t() 
    

bmwp_COL <- function(data) {
    
    ## Take a numeric matrix with the families in the rows and the sample sites
    ## in the colums. Return a data frame  with the BMWP Scores and 
    
    
    family_scores <- list(
        
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
    )
    
    for (i in 1:nrow(data)) {
      
         if (row.names(data)[i] %in% names(family_scores)) {
             
             family <- row.names(data)[i]
             presence <- data[i,] > 0
             
             data[i, presence] <- family_scores[[family]]
         }
          
         else {
             
             presence <- data[i,] > 0
             
             data[i, presence] <- 0
         }
    }
      
    bmwp_scores <- colSums(data) 
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
}





