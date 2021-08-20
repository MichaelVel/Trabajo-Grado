source("../Code/metodos.R")
source("../Code/eda.R")
source("../Code/biological_index_COL.R")


## Load RLQ Data

RTable <- "../Data/rlq/RTable.csv"
LTable <- "../Data/rlq/LTable.csv"
QTable <- "../Data/rlq/QTable.csv"

RLQ <- load_rlq(RTable, LTable ,QTable)

parameters <- c("Temperatura", "OxDisuelto", "pH", 
                "Conductividad", "Turbidez") 

## Load Complete Data

complete <- read.csv("../Data/muestreo_completo.csv",
                     stringsAsFactors = FALSE)

# Images presentation

## Slide 1

## Slide 2

## Slide 3

## Slide 4

## Slide 5

## Slide 6

## Slide 7

## Slide 8

{
    data <- read.csv("../Data/rlq/LTable.csv") %>% 
        select(-c(Muestra)) %>%
        group_by(Estacion) %>%
        summarise(across(everything(), mean)) %>%
        column_to_rownames("Estacion") %>%
        as.matrix() %>%
        t() 
    
    index_table <- data.frame( 'BMWP COL' = bmwp_COL(data)$bmwp_scores,
                               'ASPT' = bmwp_COL(data, ASPT = TRUE),
                               'FBI'  = hillsenhoff_index(data)$hillsenhoff_scores,
                               check.names = FALSE)
    data <- RLQ$L_table %>% 
        select(-Muestra, -Tipo_Estacion) %>%
        group_by(Estacion) %>%
        summarise(across(everything(), mean)) %>%
        column_to_rownames("Estacion") 
    
    macroab1 <- complete %>% 
        group_by(Estacion, Muestra) %>% 
        summarise( Familias = n_distinct(Familia),
                   Total_Macro = sum(Individuos)) %>%
        select(-Muestra ) %>%
        group_by(Estacion) %>% 
        summarise(across(everything(), mean))  
    
    macroab2 <- summary_statistics(data = complete)$macroinvertebrates
    
    diversity_table <- data.frame(
        'H' = round(diversity(data, index = 'shannon'), 2),
        '1-D' = round(diversity(data, index = 'invsimpson'), 2),
        'n' = paste0(round(macroab1$Total_Macro, 0),
                     " (",macroab2$Total_Macro, ")"), 
        'k' = paste0(round(macroab1$Familias, 0),
                     " (",macroab2$Familias, ")"), 
        check.names = FALSE) 
    
    
    
    
    data_discussion <- data[c("Chironomidae", "Simuliidae", "Hyalellidae",
                              "Physidae", "Sphaeriidae", "Naididae" )]# to discussion
    data_discussion2 <- RLQ$Q_table[c("Chironomidae", "Simuliidae", "Hyalellidae",
                                      "Physidae", "Sphaeriidae", "Naididae" ),] %>%
        t() %>% 
        as.data.frame()
    # Working in beta diversity
    
    distance_matrix <- read.csv("../Data/other/abundancia_orden.csv") %>% 
        select(-Muestra) %>%
        group_by(Estacion) %>%
        summarise(across(.cols = everything() , mean)) %>%
        ungroup() %>%
        column_to_rownames(var = 'Estacion') %>%
        vegdist(method="bray", binary= FALSE ) 
    
    
    cluster <-  hclust(distance_matrix, method = "average")
    
    dendogram <- fviz_dend(cluster, k = 1, main = "", palette = 'black')
    
    layout <- c(
        area(t = 1, l = 1, b = 4, r = 3),
        area(t = 1, l = 3, b = 2, r = 4)
    )
    
    div_indx <- diversity_table %>%
        tableGrob(theme = ttheme_default(base_size = 10)) 
    
    dendogram + 
        div_indx+
        plot_layout(design = layout, guides = 'collect') +
        plot_annotation(tag_levels = 'A') 
}

## Slide 9

{
    
data <- read.csv("../Data/rlq/LTable.csv") %>% 
    unite(Names, Estacion, Muestra) %>%
    column_to_rownames("Names") %>%
    as.matrix() %>%
    t() 

index_table <- data.frame( 'BMWP_COL' = bmwp_COL(data)$bmwp_scores,
                           'ASPT' = bmwp_COL(data, ASPT = TRUE),
                           'FBI'  = hillsenhoff_index(data)$hillsenhoff_scores,
                           check.names = FALSE) %>%
    rownames_to_column("Names") %>%
    separate(Names, c("Estacion", "Muestra"))




BMWP <- index_table %>%
    ggplot(mapping = aes(x = Estacion, y = .data[['BMWP_COL']])) +
    geom_boxplot() + theme_classic()

ASPT <- index_table %>%
    ggplot(mapping = aes(x = Estacion, y = .data[['ASPT']])) +
    geom_boxplot() + theme_classic()

FBI <- index_table %>%
    ggplot(mapping = aes(x = Estacion, y = .data[['FBI']])) +
    geom_boxplot() + theme_classic()

layout <- "
AAAABBBB
AAAABBBB
AAAABBBB
##CCCC##
##CCCC##
##CCCC##
"
BMWP + ASPT + FBI +
    plot_layout(design = layout)

}

## Slide 10

{
rlq1 <- make_rlq(RLQ$R_table, RLQ$L_table, RLQ$Q_table, parameters = parameters )

group <- factor(RLQ[["R_table"]]$Tipo_Estacion)
estations <- cbind(Grupo = group, rlq1$lR) 
mean_estations <- estations %>% 
    group_by(Grupo) %>%
    summarise(across(.cols = everything() , mean))

custom_palette <- c("#FF6F00FF", "#C71000FF", "#008EA0FF", "#8A4198FF")
rasgos <-  c("A) Tipo Alimento", 
             "B) Habitos Alimenticios", 
             "C) Respiracion",
             "D) Forma Corporal", 
             "E) Movilidad",
             "F) Tamaño Maximo")

{
    par(mfrow= c(2,3))
    
    {
        plot(1, type="n", xlab="", ylab="", 
             ylim = c(-3,3), xlim = c(-3,3), axes = FALSE);
        palette(custom_palette)
        points(estations[[2]], estations[[3]],
               col = estations$Grupo, pch = 18);
        text(mean_estations[[2]], mean_estations[[3]],
             labels =  mean_estations$Grupo,
             col = custom_palette)
        par(new= TRUE);
        palette(gray(0:1));
        s.arrow(rlq1$co[1:6,], boxes = TRUE,  clabel = 2,
                ylim = c(-1,1), xlim = c(-1.5,1.5),
                sub = rasgos[1], csub = 2.5);
        # s.label(rlq1$l1, boxes = FALSE, add.plot = TRUE, clabel = 2);
        
    }
    
    {
        plot(1, type="n", xlab="", ylab="", 
             ylim = c(-3,3), xlim = c(-3,3), axes = FALSE);
        palette(custom_palette)
        points(estations[[2]], estations[[3]],
               col = estations$Grupo, pch = 18);
        text(mean_estations[[2]], mean_estations[[3]],
             labels =  mean_estations$Grupo,
             col = custom_palette)
        par(new= TRUE);
        palette(gray(0:1));
        s.arrow(rlq1$co[7:11,], boxes = TRUE,  clabel = 2,
                ylim = c(-1,1), xlim = c(-1.5,1.5),
                sub = rasgos[2], csub = 2.5);
        # s.label(rlq1$l1, boxes = FALSE, add.plot = TRUE, clabel = 2);
        
    }
    
    {
        plot(1, type="n", xlab="", ylab="", 
             ylim = c(-3,3), xlim = c(-3,3), axes = FALSE);
        palette(custom_palette)
        points(estations[[2]], estations[[3]],
               col = estations$Grupo, pch = 18);
        text(mean_estations[[2]], mean_estations[[3]],
             labels =  mean_estations$Grupo,
             col = custom_palette)
        par(new= TRUE);
        palette(gray(0:1));
        s.arrow(rlq1$co[12:15,], boxes = TRUE,  clabel = 2,
                ylim = c(-1,1), xlim = c(-1.5,1.5),
                sub = rasgos[3], csub = 2.5);
        # s.label(rlq1$l1, boxes = FALSE, add.plot = TRUE, clabel = 2);
        
    }
    
    {
        plot(1, type="n", xlab="", ylab="", 
             ylim = c(-3,3), xlim = c(-3,3), axes = FALSE);
        palette(custom_palette)
        points(estations[[2]], estations[[3]],
               col = estations$Grupo, pch = 18);
        text(mean_estations[[2]], mean_estations[[3]],
             labels =  mean_estations$Grupo,
             col = custom_palette)
        par(new= TRUE);
        palette(gray(0:1));
        s.arrow(rlq1$co[16:19,], boxes = TRUE,  clabel = 2,
                ylim = c(-1,1), xlim = c(-1.5,1.5),
                sub = rasgos[4], csub = 2.5);
        # s.label(rlq1$l1, boxes = FALSE, add.plot = TRUE, clabel = 2);
        
    }
    
    {
        plot(1, type="n", xlab="", ylab="", 
             ylim = c(-3,3), xlim = c(-3,3), axes = FALSE);
        palette(custom_palette)
        points(estations[[2]], estations[[3]],
               col = estations$Grupo, pch = 18);
        text(mean_estations[[2]], mean_estations[[3]],
             labels =  mean_estations$Grupo,
             col = custom_palette)
        par(new= TRUE);
        palette(gray(0:1));
        s.arrow(rlq1$co[20:25,], boxes = TRUE,  clabel = 2,
                ylim = c(-1,1), xlim = c(-1.5,1.5),
                sub = rasgos[5], csub = 2.5);
        # s.label(rlq1$l1, boxes = FALSE, add.plot = TRUE, clabel = 2);
        
    }
    
    {
        plot(1, type="n", xlab="", ylab="", 
             ylim = c(-3,3), xlim = c(-3,3), axes = FALSE);
        palette(custom_palette)
        points(estations[[2]], estations[[3]],
               col = estations$Grupo, pch = 18);
        text(mean_estations[[2]], mean_estations[[3]],
             labels =  mean_estations$Grupo,
             col = custom_palette)
        par(new= TRUE);
        palette(gray(0:1));
        s.arrow(rlq1$co[26:30,], boxes = TRUE,  clabel = 2,
                ylim = c(-1,1), xlim = c(-1.5,1.5),
                sub = rasgos[6], csub = 2.5);
        # s.label(rlq1$l1, boxes = FALSE, add.plot = TRUE, clabel = 2);
    }
}

}

## Slide 11
## Slide 12
## Slide 13

