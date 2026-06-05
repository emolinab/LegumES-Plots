library(ggplot2)
library(magclass)
library(dplyr)
library(patchwork)
library(officer)
library(stringi)

## Set up scenarios and initial variables
scenarios <- c("SSP1","SSP2","SSP3","SSP4","SSP5") 

suppressWarnings(rm(data_list2))
runs <- c(SSP1 = "/p/projects/landuse/users/mbacca/Collaborations/LegumES/magpieSSP2_5/magpie/output/LegumES-H16EU-SSP1_2026-06-03_08.39.09/",
          SSP2 = "/p/projects/landuse/users/mbacca/Collaborations/LegumES/magpieSSP2_5/magpie/output/LegumES-H16EU-SSP2_2026-06-03_08.10.28/",
          SSP3 = "/p/projects/landuse/users/mbacca/Collaborations/LegumES/magpieSSP2_5/magpie/output/LegumES-H16EU-SSP3_2026-06-03_08.17.58/",
          SSP4 = "/p/projects/landuse/users/mbacca/Collaborations/LegumES/magpieSSP2_5/magpie/output/LegumES-H16EU-SSP4_2026-06-03_08.25.44/",
          SSP5 = "/p/projects/landuse/users/mbacca/Collaborations/LegumES/magpieSSP2_5/magpie/output/LegumES-H16EU-SSP5_2026-06-03_08.33.38/")    

runsReport <-setNames(paste0(runs,"report.mif"),names(runs))
years <- c(2015,2050)
plotsReport <- list()
EUR_Regions<- c("DEU", "EUC", "EUN", "EUS", "EUW")

## Read and combine report.mifs
data_list_temp <- lapply(scenarios, function(sce) {
  data <- read.report(runsReport[sce])[[1]][[1]]
  getNames(data) <- paste0(sce, ".", getNames(data))
  return(data)
})

data_list2 <- do.call(mbind, data_list_temp)

## Plotting functions

plotBars2Var <- function(dataPlot, years, title, units, region, ncol, fileFolder, facetVar){

## Data handling
  dataVariableSingle <- dataPlot[region,,]
  dataVariableSingle <- mbind(setNames(setYears(dataVariableSingle[,years[1],"SSP1"],years[2]), paste0(as.character(years[1]),".", getNames(dataVariableSingle, dim = 2))),dataVariableSingle[,years[1],,invert = TRUE])
  dfLong <- as.data.frame(dataVariableSingle, rev = TRUE)[,c("Region","Year","Data1","Data2","Value")] 
  names(dfLong) <- c("Region", "Year", "Scenario", "Variable","Value") # Layout estándar de magclass
  dfLong$Year <- as.numeric(gsub("y", "", as.character(dfLong$Year)))

## Plotting and save functions
plot <- ggplot(dfLong, aes(x = Scenario, y = Value, fill = Variable)) +
    geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.3) +
    
    scale_fill_brewer(palette = "Paired") +
    theme_minimal(base_family = "sans", base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 20, margin = margin(b = 15)),
      axis.title.x = element_text(face = "bold", size = 16, margin = margin(t = 12)),
      axis.title.y = element_text(face = "bold", size = 16, margin = margin(r = 12)),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      strip.text = element_text(size = 12, face = "bold", margin = margin(b = 8)),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 12),
      legend.text = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.spacing = unit(1.5, "lines"),
      plot.margin = margin(t = 15, r = 15, b = 15, l = 15)
    ) +
    facet_wrap(facets = facetVar, ncol = ncol, scales = "free_y") + 
    labs(title = title,
         y = units, 
         x = "Scenario", 
         fill = "Item")

    ggsave(
    filename =  paste0(fileFolder, title,".png"),
    plot = plot,
    width = 20,
    height = 20,
    dpi = 320,
    units = "cm"
  )

  return(plot)
}

plotBars3Var <- function(dataPlot, years, title, units, region, ncol, fileFolder, facetVar){

## Data handling
  dataVariableSingle <- dataPlot[region,,]
  dataVariableSingle <- mbind(setNames(setYears(dataVariableSingle[,years[1],"SSP1"],years[2]), 
  paste0(as.character(years[1]),".", as.vector(outer(getNames(dataVariableSingle, dim = 2), getNames(dataVariableSingle, dim = 3), paste, sep = ".")))),
  dataVariableSingle[,years[1],,invert = TRUE])

  dfLong <- as.data.frame(dataVariableSingle, rev = TRUE)[,c("Region","Year","Data1","Data2","Data3","Value")] 
  names(dfLong) <- c("Region", "Year", "Scenario", "Variable","Item","Value") # Layout estándar de magclass
  dfLong$Year <- as.numeric(gsub("y", "", as.character(dfLong$Year)))
  
## Plotting and save functions
plot <- ggplot(dfLong, aes(x = Scenario, y = Value, fill = Variable)) +
    geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.3) +    
    scale_fill_brewer(palette = "Paired") +
    theme_minimal(base_family = "sans", base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 20, margin = margin(b = 15)),
      axis.title.x = element_text(face = "bold", size = 16, margin = margin(t = 12)),
      axis.title.y = element_text(face = "bold", size = 16, margin = margin(r = 12)),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      strip.text = element_text(size = 12, face = "bold", margin = margin(b = 8)),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 12),
      legend.text = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.spacing = unit(1.5, "lines"),
      plot.margin = margin(t = 15, r = 15, b = 15, l = 15)
    ) +
    facet_wrap(facets = facetVar, ncol = ncol, scales = "free_y") + 
    labs(title = title,
         y = units, 
         x = "Scenario", 
         fill = "Item")

    ggsave(
    filename =  paste0(fileFolder, title,".png"),
    plot = plot,
    width = 20,
    height = 20,
    dpi = 320,
    units = "cm"
  )

  return(plot)
}

###### General Food demand #####################################################################

fileFolder<-"/p/projects/landuse/users/mbacca/Collaborations/LegumES/LegumES-Plots/Report/Plots/"
foodNames <- c(
  pulses      = "Demand|Food|Crops|Other crops|+|Pulses (Mt DM/yr)",
  soybean     = "Demand|Food|Crops|Oil crops|+|Soybean (Mt DM/yr)",
  groundnuts  = "Demand|Food|Crops|Oil crops|+|Groundnuts (Mt DM/yr)",
  tot_oil     = "Demand|Food|Crops|+|Oil crops (Mt DM/yr)",
  cereals     = "Demand|Food|Crops|+|Cereals (Mt DM/yr)",
  tot_crops   = "Demand|Food|+|Crops (Mt DM/yr)",
  ruminant    = "Demand|Livestock products|+|Ruminant meat (Mt DM/yr)",
  dairy       = "Demand|Livestock products|+|Dairy (Mt DM/yr)",
  tot_livestk = "Demand|Food|+|Livestock products (Mt DM/yr)"
)

foodData <- data_list2[, years, c(foodNames)]   

fooDataInt <- lapply(foodNames, \(x) foodData[, , x])

name_it <- function(x, short_name) {
  setNames(x, paste0(getNames(x, dim = 1), ".", short_name))
}

dataPlot <- mbind(
  name_it(fooDataInt$pulses,     "Pulses"),
  name_it(fooDataInt$soybean,    "Soybean"),
  name_it(fooDataInt$groundnuts, "Groundnuts"),
  name_it(fooDataInt$tot_oil - fooDataInt$soybean - fooDataInt$groundnuts,
          "Other oil crops"),
  name_it(fooDataInt$cereals,    "Cereals"),
  name_it(fooDataInt$tot_crops - fooDataInt$cereals - fooDataInt$tot_oil - fooDataInt$pulses,
          "Other crops"),
  name_it(fooDataInt$ruminant,   "Ruminant meat"),
  name_it(fooDataInt$dairy,      "Dairy"),
  name_it(fooDataInt$tot_livestk - fooDataInt$ruminant - fooDataInt$dairy,
          "Other livestock \n products")
)

plotsReport[["foodDemand"]] <- plotBars2Var(dataPlot, years, "Food Demand", "Mt DM/yr", "EUR",ncol=3,fileFolder = fileFolder, facetVar="Region")
plotsReport[["foodDemandPulses"]] <- plotBars2Var(dataPlot[,,c("Pulses","Soybean","Groundnuts")], years, "Food Demand Pulses", "Mt DM/yr", "EUR",ncol=3,fileFolder = fileFolder, facetVar="Region")

#####################################################################################


###### Pulses demand #####################################################################
 PulsesNames <- c(
  Food.Pulses = "Demand|Food|Crops|Other crops|+|Pulses (Mt DM/yr)",
  Feed.Pulses = "Demand|Feed|Crops|Other crops|+|Pulses (Mt DM/yr)",
  Material.Pulses = "Demand|Material|Crops|Other crops|+|Pulses (Mt DM/yr)",
  Seed.Pulses = "Demand|Seed|Crops|Other crops|+|Pulses (Mt DM/yr)",
  Processing.Pulses = "Demand|Processing|Crops|Other crops|+|Pulses (Mt DM/yr)",
  Bioenergy.Pulses = "Demand|Bioenergy|Crops|Other crops|+|Pulses (Mt DM/yr)",

  Food.Soybean = "Demand|Food|Crops|Oil crops|+|Soybean (Mt DM/yr)",
  Feed.Soybean = "Demand|Feed|Crops|Oil crops|+|Soybean (Mt DM/yr)",
  Material.Soybean = "Demand|Material|Crops|Oil crops|+|Soybean (Mt DM/yr)",
  Seed.Soybean = "Demand|Seed|Crops|Oil crops|+|Soybean (Mt DM/yr)",
  Processing.Soybean = "Demand|Processing|Crops|Oil crops|+|Soybean (Mt DM/yr)",
  Bioenergy.Soybean = "Demand|Bioenergy|Crops|Oil crops|+|Soybean (Mt DM/yr)",

  Food.Groundnuts = "Demand|Food|Crops|Oil crops|+|Groundnuts (Mt DM/yr)",
  Feed.Groundnuts = "Demand|Feed|Crops|Oil crops|+|Groundnuts (Mt DM/yr)",
  Material.Groundnuts = "Demand|Material|Crops|Oil crops|+|Groundnuts (Mt DM/yr)",
  Seed.Groundnuts = "Demand|Seed|Crops|Oil crops|+|Groundnuts (Mt DM/yr)",
  Processing.Groundnuts = "Demand|Processing|Crops|Oil crops|+|Groundnuts (Mt DM/yr)",
  Bioenergy.Groundnuts = "Demand|Bioenergy|Crops|Oil crops|+|Groundnuts (Mt DM/yr)",
  
  Food.Forage = "Demand|Food|+|Forage (Mt DM/yr)",
  Feed.Forage = "Demand|Feed|+|Forage (Mt DM/yr)",
  Seed.Forage = "Demand|Seed|+|Forage (Mt DM/yr)",
  Material.Forage = "Demand|Material|+|Forage (Mt DM/yr)",
  Processing.Forage = "Demand|Processing|+|Forage (Mt DM/yr)",
  Bioenergy.Forage = "Demand|Bioenergy|+|Forage (Mt DM/yr)"
)

pulsesDemand <- data_list2[, years, c(PulsesNames)]   
getNames(pulsesDemand) <- stri_replace_all_fixed(
  str = getNames(pulsesDemand),          
  pattern = PulsesNames,                
  replacement = names(PulsesNames ),    
  vectorize_all = FALSE              
)

plotsReport[["generalDemandPulses"]] <- plotBars3Var(pulsesDemand , years, "Pulses Demand", "Mt DM/yr", "EUR", ncol=2, fileFolder, facetVar="Item")


#####################################################################################


###### General Crop production #####################################################################
CropNames <- c(
  Pulses      = "Production|Crops|Other crops|+|Pulses (Mt DM/yr)",
  Soybean     = "Production|Crops|Oil crops|+|Soybean (Mt DM/yr)",
  Groundnuts  = "Production|Crops|Oil crops|+|Groundnuts (Mt DM/yr)",
  Forage      = "Production|+|Forage (Mt DM/yr)",
  Cereals     = "Production|Crops|+|Cereals (Mt DM/yr)",
  Total_Crops = "Production|+|Crops (Mt DM/yr)"
)

cropData <- data_list2[, years, c(CropNames)]   

cropDataInt <- lapply(CropNames, \(x) cropData[, , x])

dataPlotProd <- mbind(
  name_it(cropDataInt$Pulses,     "Pulses"),
  name_it(cropDataInt$Soybean,    "Soybean"),
  name_it(cropDataInt$Groundnuts, "Groundnuts"),
  name_it(cropDataInt$Cereals, "Cereals"),
  name_it(cropDataInt$Total_Crops - cropDataInt$Pulses - cropDataInt$Soybean - cropDataInt$Groundnuts - cropDataInt$Cereals, "Other Crops"),
  name_it(cropDataInt$Forage, "Forage")
)

plotsReport[["cropProduction"]] <- plotBars2Var(dataPlotProd, years, "Crop Production", "Mt DM/yr", EUR_Regions, ncol=3,fileFolder = fileFolder, facetVar="Region")
plotsReport[["cropProductionPulses"]] <- plotBars2Var(dataPlotProd[,,c("Pulses", "Soybean", "Groundnuts","Forage")], years, "Crop Production (Pulses)", "Mt DM/yr", EUR_Regions, ncol=3,fileFolder = fileFolder, facetVar="Region")

#####################################################################################
