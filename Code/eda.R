## Load packages 

library(e1071)
library(ggplot2)
library(dplyr)
library(factoextra)
library(ade4)
library(tibble)
library(tidyr)
library(kableExtra)
library(flextable)
library(patchwork)
library(Hotelling)

## Load and cleaning data

load_data <- function(ref_estations = FALSE) {
    
    ## Accesory function to load and filter the data
    
    file1 <- "Data/rio_frio.csv"
    file2 <- "Data/rio_neusa.csv"
    
    join_data <- function(data1, data2){
        
        data_1 <- read.csv(data1, stringsAsFactors = FALSE)
        data_2 <- read.csv(data2, stringsAsFactors = FALSE) 
        data_2 <- data_2[names(data_2) %in% names(data_1)]
        data_joined <- bind_rows(data_1, data_2)
        return(data_joined)
        }
    
    data <- join_data(file1,file2) %>% subset(Estacion != 'Neu5') 
    data$tmp <- paste0(data$Estacion, '_', data$Muestra) ## temporal variable
    
    data <- data %>%     ## All stations with 6 samples
        subset(tmp != 'Neu1_11' & tmp != 'Neu1_4') %>%
        subset(tmp != 'Neu2_11' & tmp != 'Neu2_8' & tmp != 'Neu2_4') %>%
        subset(tmp != 'Neu3_11' & tmp != 'Neu3_10') %>%
        subset(tmp != 'Neu6_11' & tmp != 'Neu6_10' & 
               tmp != 'Neu2_8' & tmp != 'Neu2_1') %>%
        subset(tmp != 'Neu7_11' & tmp != 'Neu7_10' & tmp != 'Neu7_1') %>%
        subset(select = -tmp)
        
    
    if (ref_estations) {
        
        rio_frio <- c('Fri1', 'Fri2', 'Fri3', 'Fri4')
        rio_frio2 <- c('Fri5')
        rio_neusa2 <- c('Neu4', 'Neu6', 'Neu7')
        rio_neusa <- c('Neu1', 'Neu2', 'Neu3')
        
        data$Tipo_Estacion[data$Estacion %in% rio_frio] <- 'FrioB'
        data$Tipo_Estacion[data$Estacion %in% rio_frio2] <- 'FrioA'
        data$Tipo_Estacion[data$Estacion %in% rio_neusa2] <- 'NeusaB'
        data$Tipo_Estacion[data$Estacion %in% rio_neusa] <- 'NeusaA'
        
        return(data)
        
    } else {
    
        return(data)
    
    }
}

filter_data <- function(data, parameters) {
    
    parameters <- c("Tipo_Estacion", "Estacion", "Muestra", parameters) 
    
    data[parameters] %>% 
        distinct()  %>%
        group_by(Tipo_Estacion, Estacion, Muestra) %>%
        summarise(across(.cols = everything() , mean)) %>% 
        return()
    
}
    
load_rlq <- function(parameters = NULL ){
    
    ## Return an object with the R, L, and Q matrices, by default
    ## work with physichochemicals parameters 
    
    if (is.null(parameters)) {
        
    parameters <- c("Temperatura", "Ox_disuelto", "pH", 
                        "Conductividad", "Turbidez") }
    
    enviromental <- load_data(ref_estations = TRUE) %>% 
                    filter_data(parameters)
    
    abundance <- read.csv('Data/abundancia.csv', stringsAsFactors = FALSE)
    
    traits <- read.csv('Data/rasgos_funcionales.csv',
                       stringsAsFactors = FALSE, encoding = 'UTF-8') %>% 
              column_to_rownames( var = "Familia") %>%      
              subset(rownames(.) %in% names(abundance))               
              
    all <-  load_data()
    
    RLQ <- list(R_table = enviromental, Q_table = traits,
                L_table = abundance, all_table = all)
    
    return(RLQ)
    
}     

## Summary statistics (mean, median, quantiles, sd, skewness, kurtosis)

summary_statistics <- function(...) {
    
    ## Make a summary of the data for station by default. Pass 'by_sample = TRUE'
    ## argument to make a summary by each sampling event.  
    
    summary_data  <- function(funct, by_sample = FALSE, macroinvertebrates = FALSE) {
        
        if (macroinvertebrates) { 
            
            if (by_sample) {
                
                load_data() %>% 
                    group_by(Estacion, Muestra) %>% 
                    summarise( Number_Families = n_distinct(Familia),
                               Total_Macro = sum(Individuos)) %>%
                    return()
                
            } else {
                
                load_data() %>% 
                    group_by(Estacion) %>% 
                    summarise( Familias = n_distinct(Familia),
                               Total_Macro = sum(Individuos)) %>%
                    return()   
            }
            
        } else {
            
            if (by_sample) {
                
                load_data() %>% 
                    group_by(Estacion, Muestra) %>% 
                    summarise( Temperatura =funct(Temperatura),
                               Ox_disuelto = funct(Ox_disuelto),
                               pH = funct(pH),
                               Conductividad = funct(Conductividad),
                               Turbidez = funct(Turbidez)) %>% ## Añadir hidrologicos
                    return()
                
            } else {
                
                load_data() %>% 
                    group_by(Estacion) %>% 
                    summarise( Temperatura =funct(Temperatura),
                               Ox_disuelto = funct(Ox_disuelto),
                               pH = funct(pH),
                               Conductividad = funct(Conductividad),
                               Turbidez = funct(Turbidez)) %>%
                    return()
            }
            
        }
    }
    
    mean <- summary_data(mean, ...)
    median <- summary_data(median, ...)
    sd <- summary_data(sd, ...)
    skewness <- summary_data(skewness, ...)
    kurtosis <- summary_data(kurtosis, ...)
    macroinvertebrates <- summary_data(macroinvertebrates = TRUE ,...)
    
    summary_stats <- list(mean = mean, median = median, sd = sd, 
                               skewness = skewness, kurtosis = kurtosis, 
                               macroinvertebrates = macroinvertebrates)
    return(summary_stats)
}

make_manova <- function(funct = hotelling.test ,type = 'fisicoquimicos', est1, est2 ) {
    
    data <- load_data(ref_estations = TRUE)
    
    if (type == 'fisicoquimicos') {
        
        parameters <- c("Temperatura", "Ox_disuelto", "pH", 
                        "Conductividad", "Turbidez") 
        
        data_physicochemicals <- filter_data(data, parameters)
        
        
        data_physicochemicals <- filter(data_physicochemicals, 
                                        Tipo_Estacion == est1 |
                                            Tipo_Estacion == est2)
       
        results_manova <- funct(cbind(Temperatura, Ox_disuelto, pH, 
                                   Conductividad, Turbidez) ~ Tipo_Estacion, 
                                 data = data_physicochemicals )
        
       
        return(results_manova)
        
     } else if (type == 'hidrologicos') {
        
        parameters <- c("Temperatura", "Ox_disuelto", "pH", 
                        "Conductividad", "Turbidez") 
        
        data <- filter_data(data, parameters)
        data_hydrological  <- data[parameters]
        }
}

# results_manova <- list(
#     NeuA_NeuB <-  make_manova(est1 = "NeusaA", est2 = "NeusaB"),
#     NeuA_FrioA =  make_manova(est1 = "NeusaA", est2 = "FrioA"),
#     NeuA_FrioB =  make_manova(est1 = "NeusaA", est2 = "FrioB"),
#     NeuB_FrioA =  make_manova(est1 = "NeusaB", est2 = "FrioA"),
#     NeuB_FrioB =  make_manova(est1 = "NeusaB", est2 = "FrioB"),
#     FrioA_FrioB =  make_manova(est1 = "FrioA", est2 = "FrioB")
# )
#print(hotelling.test(xs + ys + zs ~ group))

## Graphical  eda

univariate_graphical_eda <- function(type_plot, parameters = NULL){
    
    ## Make the boxplot, histogram or qq plot for the passed parameters, 
    ## by default physicochemical parameters. 
    
    if (is.null(parameters)) {
        parameters <- c("Temperatura", "Ox_disuelto", "pH", 
                        "Conductividad", "Turbidez")}
    ggobjtect_parameters <- list()
    
    eda_plots  <- function(parameter , type_plot = "boxplot" ){
        
        if (type_plot == "boxplot") { 
            plot <- load_data() %>%
                ggplot(mapping = aes(x = Estacion, y = .data[[parameter]])) +
                geom_boxplot() + theme_classic()
            
            return(plot)
            
        } else if (type_plot == "histogram") {
            
            plot <- load_data() %>%
                ggplot(mapping = aes(.data[[parameter]])) +
                geom_histogram() +
                facet_wrap(vars(Estacion)) +
                theme_classic()
            
            
            return(plot)
            
        } else if (type_plot == "quantile-normal") {
            
            plot <- load_data() %>%
                ggplot(data, mapping = aes(sample = .data[[parameter]])) +
                stat_qq() + 
                stat_qq_line(fullrange = TRUE) +
                facet_wrap(vars(Estacion)) + theme_classic()
            
            return(plot)
        }
    }
    
    for (i in 1:length(parameters)){
            
        parameter <- parameters[i]
            
        ggobjtect_parameters[[parameter]] <- eda_plots(parameter, 
                                                       type_plot)
            
    }
    
    return(ggobjtect_parameters)
    
}
          
make_pca <- function(type = 'fisicoquimicos', plot = TRUE, ... ) {
    
    ## perform the PCA analysis and return a list with  the ggplot objects
    ## relevants to the analysis. If plot = FALSE, returns the raw results
    ## of the analysis. 
    
    data <- load_data(ref_estations = TRUE)
    
    plots_pca_analysis <- function(results_pca, plot_type = "Estacion") {
        
        eigenvalues <- fviz_eig(results_pca, main = "", 
                                ylab = "% Varianza", xlab = "Componentes") +
          theme_classic()
        
        
        individuals <- fviz_pca_ind(results_pca,
                                    col.ind = "cos2", 
                                    gradient.cols = c("#00AFBB", "#E7B800",
                                                      "#FC4E07"),
                                    repel = FALSE) +
          theme_classic()
        
        variables <- fviz_pca_var(results_pca,
                                  col.var = "contrib",
                                  gradient.cols = c("#00AFBB", "#E7B800",
                                                    "#FC4E07"),
                                  repel = TRUE,  title = "") +
          theme_classic()
        
        biplot <- fviz_pca_biplot(results_pca,
                                  label = "var", 
                                  habillage = data[[plot_type]],
                                  addEllipses = FALSE,
                                  legend = "left", 
                                  palette = "futurama", 
                                  title = "") 
        
        
        plot_pca <- list(eigenvalues = eigenvalues, 
                         individuals = individuals,
                         variables = variables, 
                         biplot = biplot)
        
        return(plot_pca)
        
    }
  
    if (type == 'fisicoquimicos') {
        
        parameters <- c("Temperatura", "Ox_disuelto", "pH", 
                       "Conductividad", "Turbidez") 
        
        data <- filter_data(data, parameters)
        data_physicochemicals <- data[parameters]
        
        results_pca <- prcomp(data_physicochemicals, scale. = TRUE)
        
        if (plot) {
            
            plot_pca <- plots_pca_analysis(results_pca, ...)
            return(plot_pca)
            
        } else {
            
            return(results_pca)
        }
        
    } else if (type == 'hidrologicos') {
        
        parameters <- c("Temperatura", "Ox_disuelto", "pH", 
                       "Conductividad", "Turbidez") 
        
        data <- filter_data(data, parameters)
        data_hydrological  <- data[parameters]
       
        results_pca <- prcomp(data_hydrological, scale. = TRUE)
        
        if (plot) {
            
            plot_pca <- plots_pca_analysis(results_pca, ...)
            return(plot_pca)
            
        } else {
            
            return(results_pca)
        }
    }
}

make_cluster <- function() {
        
    data <- load_data(ref_estations = TRUE)
    parameters <- c("Temperatura", "Ox_disuelto", "pH", 
                    "Conductividad", "Turbidez") 
    
    data_physicochemicals <- filter_data(data, parameters) %>%
                subset(select= -Muestra) %>%
                group_by(Tipo_Estacion, Estacion) %>%
                summarise(across(.cols = everything() , mean))
    
    distance_matrix <-  data_physicochemicals[-1] %>%
                        column_to_rownames(var = 'Estacion') %>%
                        scale() %>%
                        dist() 
    
    cluster <-  hclust(distance_matrix, method = "average")
    
    dendogram <- fviz_dend(cluster, k = 4, main = "", palette = 'futurama') +
                geom_hline(yintercept = 1.8, linetype = "dashed", colour = 'blue')
    
    results <- list(hc = cluster, dendogram = dendogram)
    
    return(results)
 
} 

  
## Fuzzy Correspondence Analysis and RLQ analysis

make_fca <- function() {

   ## This function takes as imput a fuzzy coded table of traits  in each site
   ## and a table of enviromental variables and perform a CCA. return a dudi
   ## object of the class pcaiv


   # with pcaiv

   object <- dudi.fca(traits, scannf = FALSE, nf = 3 )
   pcaiv(object, envir, scannf = FALSE, nf = 3)

   scatter(object, csub = 3, clab.moda = 1.5)

   # with varipart

   
}

make_rlq <- function(data = load_rlq() ) {
    
    
      abundance <- data$L_table %>%  
                   select(-Tipo_Estacion) %>%
                   unite(col = names, Estacion, Muestra) %>%
                   column_to_rownames('names') 
      
      abundance_test <- dudi.coa(abundance, scannf = FALSE, nf = 3)
  
      envir <- data$R_table %>% ungroup() %>%
             select(-Tipo_Estacion) %>%
             unite(col = names, Estacion, Muestra) %>%
             column_to_rownames('names') 
      
      envir_test <-  dudi.pca(envir, scannf = FALSE, nf = 3, 
                              row.w = abundance_test$lw)
    
      traits <- data$Q_table %>%
                prep.fuzzy.var(col.blocks = c(Tipo_Alimento = 8, 
                                              Habitos_Alimenticios = 7, 
                                              Respiracion = 5,
                                              Forma_Corporal = 4, 
                                              Movilidad = 7,
                                              Tamaño_Maximo= 6),
                               row.w = abundance_test$cw) 
      traits_test <-  dudi.fca(traits, scannf = FALSE, nf = 3)
      
    rlq1 <- rlq(envir_test, abundance_test, traits_test,  scannf = FALSE, nf = 2)
    
    # plot(rlq1)
    # summary(rlq1)
    # randtest(rlq1)
    # fourthcorner.rlq(rlq1,type="Q.axes")
    # a <- fourthcorner.rlq(rlq1,type="R.axes")
    return(rlq1)

}

