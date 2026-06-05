library(ggplot2)
library(magclass)
library(dplyr)
library(patchwork)
library(stringi)

## Set up scenarios and initial variables
scenarios <- c("SSP1","SSP2","SSP3","SSP4","SSP5") 

suppressWarnings(rm(data_list2))
runsReport <- setNames(
  file.path("DataRunsLegumES", paste0("report_", tolower(scenarios), ".mif")),
  scenarios)
years <- c(2015,2050)
plotsReport <- list()
EUR_Regions<- c("DEU", "EUC", "EUN", "EUS", "EUW")

## ---------------------------------------------------------------------------
## Common colour coding (shared across all plots for consistency)
## Vector order = stacking order in the bars: neighbours are chosen to differ
## in both hue and lightness. Central commodities (Pulses, Soybean, Groundnuts,
## Oils, Oilcakes, Forage) get strong, intuitive, saturated colours; aggregate
## "other" categories are kept muted so the central commodities stand out.
## ---------------------------------------------------------------------------
itemColors <- c(
  ## Crops
  "Pulses"                                  = "#7F4F24",  # brown
  "Soybean"                                 = "#1B9E77",  # teal-green
  "Groundnuts"                              = "#FFD92F",  # yellow
  "Cereals"                                 = "#C8B68F",  # muted tan
  "Other crops"                             = "#9C8AA5",  # muted mauve-grey
  ## Primary processed products (between crops and animal products)
  "Oils"                                    = "#FF7F00",  # orange
  "Sugar and other \n primary processed \n products" = "#9E9E9E",  # grey
  ## Livestock
  "Ruminant meat"                           = "#E41A1C",  # red
  "Dairy"                                   = "#A6CEE3",  # light blue
  "Other livestock \n products"             = "#FB9A99",  # salmon
  ## Further central commodities (used in other plots, kept consistent)
  "Oilcakes"                                = "#B15928",  # sienna
  "Forage"                                  = "#4DAF4A"   # green
)

## Demand categories (purpose of demand), shared across plots
demandColors <- c(
  "Food"       = "#4DAF4A",  # green
  "Feed"       = "#FF7F00",  # orange
  "Material"   = "#6A3D9A",  # purple
  "Seed"       = "#A6761D",  # brown
  "Processing" = "#377EB8",  # blue
  "Bioenergy"  = "#E7298A"   # magenta
)

## Read and combine report.mifs
data_list_temp <- lapply(scenarios, function(sce) {
  data <- read.report(runsReport[sce])[[1]][[1]]
  getNames(data) <- paste0(sce, ".", getNames(data))
  return(data)
})

data_list2 <- do.call(mbind, data_list_temp)

## Plotting functions

plotBars2Var <- function(dataPlot, years, title, units, region, ncol, fileFolder, facetVar, palette = itemColors, width = 24, height = 24){

## Data handling
  dataVariableSingle <- dataPlot[region,,]
  dataVariableSingle <- mbind(setNames(setYears(dataVariableSingle[,years[1],"SSP1"],years[2]), paste0(as.character(years[1]),".", getNames(dataVariableSingle, dim = 2))),dataVariableSingle[,years[1],,invert = TRUE])
  dfLong <- as.data.frame(dataVariableSingle, rev = TRUE)[,c("Region","Year","Data1","Data2","Value")]
  names(dfLong) <- c("Region", "Year", "Scenario", "Variable","Value") # Layout estándar de magclass
  dfLong$Year <- as.numeric(gsub("y", "", as.character(dfLong$Year)))
  dfLong$Variable <- factor(dfLong$Variable, levels = names(palette)) # fix stacking/legend order

## Plotting and save functions
plot <- ggplot(dfLong, aes(x = Scenario, y = Value, fill = Variable)) +
    geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.3) +

    scale_fill_manual(values = palette, drop = TRUE, na.value = "grey70") +
    guides(fill = guide_legend(ncol = 1)) +
    theme_minimal(base_family = "sans", base_size = 20) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 30, margin = margin(b = 15)),
      axis.title.x = element_text(face = "bold", size = 26, margin = margin(t = 12)),
      axis.title.y = element_text(face = "bold", size = 26, margin = margin(r = 12)),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 20),
      axis.text.y = element_text(size = 20),
      strip.text = element_text(size = 22, face = "bold", margin = margin(b = 8)),
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 22),
      legend.text = element_text(size = 20),
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
    width = width,
    height = height,
    dpi = 320,
    units = "cm"
  )

  return(plot)
}

plotBars3Var <- function(dataPlot, years, title, units, region, ncol, fileFolder, facetVar, palette = demandColors, width = 24, height = 24){

## Data handling
  dataVariableSingle <- dataPlot[region,,]
  dataVariableSingle <- mbind(setNames(setYears(dataVariableSingle[,years[1],"SSP1"],years[2]),
  paste0(as.character(years[1]),".", as.vector(outer(getNames(dataVariableSingle, dim = 2), getNames(dataVariableSingle, dim = 3), paste, sep = ".")))),
  dataVariableSingle[,years[1],,invert = TRUE])

  dfLong <- as.data.frame(dataVariableSingle, rev = TRUE)[,c("Region","Year","Data1","Data2","Data3","Value")]
  names(dfLong) <- c("Region", "Year", "Scenario", "Variable","Item","Value") # Layout estándar de magclass
  dfLong$Year <- as.numeric(gsub("y", "", as.character(dfLong$Year)))
  dfLong$Variable <- factor(dfLong$Variable, levels = names(palette)) # fix stacking/legend order

## Plotting and save functions
plot <- ggplot(dfLong, aes(x = Scenario, y = Value, fill = Variable)) +
    geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.3) +
    scale_fill_manual(values = palette, drop = TRUE, na.value = "grey70") +
    theme_minimal(base_family = "sans", base_size = 20) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 30, margin = margin(b = 15)),
      axis.title.x = element_text(face = "bold", size = 26, margin = margin(t = 12)),
      axis.title.y = element_text(face = "bold", size = 26, margin = margin(r = 12)),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 20),
      axis.text.y = element_text(size = 20),
      strip.text = element_text(size = 22, face = "bold", margin = margin(b = 8)),
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 22),
      legend.text = element_text(size = 20),
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
    width = width,
    height = height,
    dpi = 320,
    units = "cm"
  )

  return(plot)
}

###### General Food demand #####################################################################

fileFolder<-"Report/Plots/"
foodNames <- c(
  pulses      = "Demand|Food|Crops|Other crops|+|Pulses (Mt DM/yr)",
  soybean     = "Demand|Food|Crops|Oil crops|+|Soybean (Mt DM/yr)",
  groundnuts  = "Demand|Food|Crops|Oil crops|+|Groundnuts (Mt DM/yr)",
  cereals     = "Demand|Food|Crops|+|Cereals (Mt DM/yr)",
  tot_crops   = "Demand|Food|+|Crops (Mt DM/yr)",
  ruminant    = "Demand|Livestock products|+|Ruminant meat (Mt DM/yr)",
  dairy       = "Demand|Livestock products|+|Dairy (Mt DM/yr)",
  tot_livestk = "Demand|Food|+|Livestock products (Mt DM/yr)",
  fish        = "Demand|Food|+|Fish (Mt DM/yr)",
  oils        = "Demand|Food|Secondary products|+|Oils (Mt DM/yr)",
  tot_sec     = "Demand|Food|+|Secondary products (Mt DM/yr)"
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
  name_it(fooDataInt$cereals,    "Cereals"),
  ## Other crops now includes other oil crops (raw): total crops minus the
  ## explicitly shown crop categories. The Oil crops aggregate cancels out.
  name_it(fooDataInt$tot_crops - fooDataInt$cereals - fooDataInt$pulses -
            fooDataInt$soybean - fooDataInt$groundnuts,
          "Other crops"),
  name_it(fooDataInt$ruminant,   "Ruminant meat"),
  name_it(fooDataInt$dairy,      "Dairy"),
  ## Other livestock products, with Fish folded in (per request)
  name_it(fooDataInt$tot_livestk - fooDataInt$ruminant - fooDataInt$dairy + fooDataInt$fish,
          "Other livestock \n products"),
  ## Secondary products: oils shown separately, rest aggregated
  name_it(fooDataInt$oils,       "Oils"),
  name_it(fooDataInt$tot_sec - fooDataInt$oils,
          "Sugar and other \n primary processed \n products")
)

plotsReport[["foodDemand"]] <- plotBars2Var(dataPlot, years, "Food Demand", "Mt DM/yr", "EUR",ncol=3,fileFolder = fileFolder, facetVar="Region")
plotsReport[["foodDemandPulses"]] <- plotBars2Var(dataPlot[,,c("Pulses","Soybean","Groundnuts")], years, "Food Demand for Legumes", "Mt DM/yr", "EUR",ncol=3,fileFolder = fileFolder, facetVar="Region")

#####################################################################################


###### Per-capita calorie supply (same categories as Food Demand) #####################
calNames <- c(
  pulses      = "Nutrition|Calorie Supply|Crops|Other crops|+|Pulses (kcal/capita/day)",
  soybean     = "Nutrition|Calorie Supply|Crops|Oil crops|+|Soybean (kcal/capita/day)",
  groundnuts  = "Nutrition|Calorie Supply|Crops|Oil crops|+|Groundnuts (kcal/capita/day)",
  cereals     = "Nutrition|Calorie Supply|Crops|+|Cereals (kcal/capita/day)",
  tot_crops   = "Nutrition|Calorie Supply|+|Crops (kcal/capita/day)",
  ruminant    = "Nutrition|Calorie Supply|Livestock products|+|Ruminant meat (kcal/capita/day)",
  dairy       = "Nutrition|Calorie Supply|Livestock products|+|Dairy (kcal/capita/day)",
  tot_livestk = "Nutrition|Calorie Supply|+|Livestock products (kcal/capita/day)",
  fish        = "Nutrition|Calorie Supply|+|Fish (kcal/capita/day)",
  oils        = "Nutrition|Calorie Supply|Secondary products|+|Oils (kcal/capita/day)",
  tot_sec     = "Nutrition|Calorie Supply|+|Secondary products (kcal/capita/day)"
)

calData <- data_list2[, years, c(calNames)]
calDataInt <- lapply(calNames, \(x) calData[, , x])

dataPlotCal <- mbind(
  name_it(calDataInt$pulses,     "Pulses"),
  name_it(calDataInt$soybean,    "Soybean"),
  name_it(calDataInt$groundnuts, "Groundnuts"),
  name_it(calDataInt$cereals,    "Cereals"),
  name_it(calDataInt$tot_crops - calDataInt$cereals - calDataInt$pulses -
            calDataInt$soybean - calDataInt$groundnuts,
          "Other crops"),
  name_it(calDataInt$ruminant,   "Ruminant meat"),
  name_it(calDataInt$dairy,      "Dairy"),
  ## Other livestock products, with Fish folded in (per request)
  name_it(calDataInt$tot_livestk - calDataInt$ruminant - calDataInt$dairy + calDataInt$fish,
          "Other livestock \n products"),
  name_it(calDataInt$oils,       "Oils"),
  name_it(calDataInt$tot_sec - calDataInt$oils,
          "Sugar and other \n primary processed \n products")
)

plotsReport[["calorieSupply"]] <- plotBars2Var(dataPlotCal, years, "Per-Capita Calorie Supply", "kcal/capita/day", "EUR", ncol=3, fileFolder = fileFolder, facetVar="Region")

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
  Bioenergy.Forage = "Demand|Bioenergy|+|Forage (Mt DM/yr)",

  Food.Oils = "Demand|Food|Secondary products|+|Oils (Mt DM/yr)",
  Feed.Oils = "Demand|Feed|Secondary products|+|Oils (Mt DM/yr)",
  Material.Oils = "Demand|Material|Secondary products|+|Oils (Mt DM/yr)",
  Seed.Oils = "Demand|Seed|Secondary products|+|Oils (Mt DM/yr)",
  Processing.Oils = "Demand|Processing|Secondary products|+|Oils (Mt DM/yr)",
  Bioenergy.Oils = "Demand|Bioenergy|Secondary products|+|Oils (Mt DM/yr)",

  Food.Oilcakes = "Demand|Food|Secondary products|+|Oilcakes (Mt DM/yr)",
  Feed.Oilcakes = "Demand|Feed|Secondary products|+|Oilcakes (Mt DM/yr)",
  Material.Oilcakes = "Demand|Material|Secondary products|+|Oilcakes (Mt DM/yr)",
  Seed.Oilcakes = "Demand|Seed|Secondary products|+|Oilcakes (Mt DM/yr)",
  Processing.Oilcakes = "Demand|Processing|Secondary products|+|Oilcakes (Mt DM/yr)",
  Bioenergy.Oilcakes = "Demand|Bioenergy|Secondary products|+|Oilcakes (Mt DM/yr)"
)

pulsesDemand <- data_list2[, years, c(PulsesNames)]   
getNames(pulsesDemand) <- stri_replace_all_fixed(
  str = getNames(pulsesDemand),          
  pattern = PulsesNames,                
  replacement = names(PulsesNames ),    
  vectorize_all = FALSE
)

## Special rule: oils are an intermediate product when "processed", so drop the
## Processing category for Oils only (avoids double-counting). Zeroed rather than
## removed to keep the use-type x commodity cross complete for plotBars3Var.
pulsesDemand[, , grep("\\.Processing\\.Oils$", getNames(pulsesDemand), value = TRUE)] <- 0

plotsReport[["generalDemandPulses"]] <- plotBars3Var(pulsesDemand , years, "Legumes Demand by Use Type", "Mt DM/yr", "EUR", ncol=3, fileFolder, facetVar="Item", width = 34, height = 26)


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
  name_it(cropDataInt$Total_Crops - cropDataInt$Pulses - cropDataInt$Soybean - cropDataInt$Groundnuts - cropDataInt$Cereals, "Other crops"),
  name_it(cropDataInt$Forage, "Forage")
)

plotsReport[["cropProduction"]] <- plotBars2Var(dataPlotProd, years, "Crop Production", "Mt DM/yr", EUR_Regions, ncol=3,fileFolder = fileFolder, facetVar="Region", width = 34, height = 26)
plotsReport[["cropProductionPulses"]] <- plotBars2Var(dataPlotProd[,,c("Pulses", "Soybean", "Groundnuts","Forage")], years, "Crop Production (legumes)", "Mt DM/yr", EUR_Regions, ncol=3,fileFolder = fileFolder, facetVar="Region", width = 34, height = 26)

#####################################################################################
