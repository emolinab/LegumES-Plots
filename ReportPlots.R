library(ggplot2)
library(magclass)
library(dplyr)
library(patchwork)
library(stringi)
library(legendry)

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

tradeColors <- c(
  "Imports"       = "#E6AB02",  # green
  "Exports"       = "#1D91C0",  # orange
  "Demand"        = "#CAB2D6"  # purple

)

## Read and combine report.mifs
data_list_temp <- lapply(scenarios, function(sce) {
  data <- read.report(runsReport[sce])[[1]][[1]]
  getNames(data) <- paste0(sce, ".", getNames(data))
  return(data)
})

data_list2 <- do.call(mbind, data_list_temp)

## Plotting functions

plotBars2Var <- function(dataPlot, years, title, units, region, ncol, fileFolder, facetVar, palette = itemColors, width = 24, height = 24, highlight = NULL, legendBreaks = NULL){

## Data handling
  dataVariableSingle <- dataPlot[region,,]
  dataVariableSingle <- mbind(setNames(setYears(dataVariableSingle[,years[1],"SSP1"],years[2]), paste0(as.character(years[1]),".", 
  getNames(dataVariableSingle, dim = 2))),dataVariableSingle[,years[1],,invert = TRUE])
  
  
  dfLong <- as.data.frame(dataVariableSingle, rev = TRUE)[,c("Region","Year","Data1","Data2","Value")]
  names(dfLong) <- c("Region", "Year", "Scenario", "Variable","Value") # Layout estándar de magclass
  dfLong$Year <- as.numeric(gsub("y", "", as.character(dfLong$Year)))
  dfLong$Variable <- factor(dfLong$Variable, levels = names(palette)) # fix stacking/legend order
  dfLong$Region <- factor(dfLong$Region, levels = region) # enforce facet order (e.g. EUR first)

## Nested x-axis: scenario tick + year bracket (2015 baseline vs SSP1-5 in 2050).
## The 2015 bar gets a blank tick so the year only appears in the bracket row.
  dfLong$Period <- ifelse(as.character(dfLong$Scenario) == "2015", "2015", "2050")
  tickLab       <- ifelse(as.character(dfLong$Scenario) == "2015", "", as.character(dfLong$Scenario))
  dfLong$xNest  <- factor(paste(tickLab, dfLong$Period, sep = "@"),
                          levels = c("@2015", paste0(scenarios, "@2050")))

## Plotting and save functions
plot <- ggplot(dfLong, aes(x = xNest, y = Value, fill = Variable)) +
   geom_col(data = filter(dfLong, Value >= 0), position = "stack", color = "white", linewidth = 0.3) +
    geom_col(data = filter(dfLong, Value < 0), position = "stack", color = "white", linewidth = 0.3) +
    geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.4) +

    scale_fill_manual(values = palette, drop = TRUE, na.value = "grey70",
                      breaks = if (is.null(legendBreaks)) ggplot2::waiver() else legendBreaks) +
    guides(fill = guide_legend(ncol = 1)) +
    theme_minimal(base_family = "sans", base_size = 20) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 30, margin = margin(b = 15)),
      axis.title.x = element_text(face = "bold", size = 26, margin = margin(t = 12)),
      axis.title.y = element_text(face = "bold", size = 26, margin = margin(r = 12)),
      axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, size = 20),
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
    scale_x_discrete(guide = legendry::guide_axis_nested(
                       drop_zero = FALSE,                                # draw the single 2015 bracket line
                       levels_text = list(
                         element_text(angle = 60, hjust = 1, vjust = 1), # scenario ticks: steep
                         element_text(angle = 0,  hjust = 0.5)))) +      # year brackets: horizontal
    labs(title = title,
         y = units, 
         x = NULL,
         fill = "Item")

  ## Optional: draw a frame around one facet to highlight it (e.g. EUR)
  if (!is.null(highlight)) {
    hlDF <- setNames(data.frame(factor(highlight, levels = region)), facetVar)
    plot <- plot +
      geom_rect(data = hlDF, inherit.aes = FALSE,
                aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
                fill = NA, colour = "black", linewidth = 1.8)
  }

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

plotBars3Var <- function(dataPlot, years, title, units, region, ncol, fileFolder, facetVar, palette = demandColors, width = 24, height = 24, highlight = NULL, legendBreaks = NULL){

## Data handling
  dataVariableSingle <- dataPlot[region,,]

  dataVariableSingleHist <-setYears(dataVariableSingle[,years[1],"SSP1"],years[2])
  getNames(dataVariableSingleHist) <- gsub("^SSP1\\.", paste0(years[1], "."), getNames(dataVariableSingleHist))

  dataVariableSingle <- mbind(dataVariableSingleHist, dataVariableSingle[,years[1],,invert = TRUE])

  dfLong <- as.data.frame(dataVariableSingle, rev = TRUE)[,c("Region","Year","Data1","Data2","Data3","Value")]
  names(dfLong) <- c("Region", "Year", "Scenario", "Variable","Item","Value") # Layout estándar de magclass
  dfLong$Year <- as.numeric(gsub("y", "", as.character(dfLong$Year)))
  dfLong$Variable <- factor(dfLong$Variable, levels = names(palette)) # fix stacking/legend order

## Nested x-axis: scenario tick + year bracket (2015 baseline vs SSP1-5 in 2050).
## The 2015 bar gets a blank tick so the year only appears in the bracket row.
  dfLong$Period <- ifelse(as.character(dfLong$Scenario) == "2015", "2015", "2050")
  tickLab       <- ifelse(as.character(dfLong$Scenario) == "2015", "", as.character(dfLong$Scenario))
  dfLong$xNest  <- factor(paste(tickLab, dfLong$Period, sep = "@"),
                          levels = c("@2015", paste0(scenarios, "@2050")))

## Plotting and save functions
plot <- ggplot(dfLong, aes(x = xNest, y = Value, fill = Variable)) +
      geom_col(data = filter(dfLong, Value >= 0), position = "stack", color = "white", linewidth = 0.3) +
    geom_col(data = filter(dfLong, Value < 0), position = "stack", color = "white", linewidth = 0.3) +
    scale_fill_manual(values = palette, drop = TRUE, na.value = "grey70",
                      breaks = if (is.null(legendBreaks)) ggplot2::waiver() else legendBreaks) +
    theme_minimal(base_family = "sans", base_size = 20) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 30, margin = margin(b = 15)),
      axis.title.x = element_text(face = "bold", size = 26, margin = margin(t = 12)),
      axis.title.y = element_text(face = "bold", size = 26, margin = margin(r = 12)),
      axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, size = 20),
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
    scale_x_discrete(guide = legendry::guide_axis_nested(
                       drop_zero = FALSE,                                # draw the single 2015 bracket line
                       levels_text = list(
                         element_text(angle = 60, hjust = 1, vjust = 1), # scenario ticks: steep
                         element_text(angle = 0,  hjust = 0.5)))) +      # year brackets: horizontal
    labs(title = title,
         y = units, 
         x = NULL,
         fill = "Item")

  ## Optional: draw a frame around one facet to highlight it (e.g. EUR)
  if (!is.null(highlight)) {
    hlDF <- setNames(data.frame(factor(highlight, levels = region)), facetVar)
    plot <- plot +
      geom_rect(data = hlDF, inherit.aes = FALSE,
                aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
                fill = NA, colour = "black", linewidth = 1.8)
  }

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

plotsReport[["cropProduction"]] <- plotBars2Var(dataPlotProd, years, "Crop Production", "Mt DM/yr", c("EUR", EUR_Regions), ncol=3,fileFolder = fileFolder, facetVar="Region", width = 34, height = 26, highlight = "EUR")
plotsReport[["cropProductionPulses"]] <- plotBars2Var(dataPlotProd[,,c("Pulses", "Soybean", "Groundnuts","Forage")], years, "Crop Production (legumes)", "Mt DM/yr", c("EUR", EUR_Regions), ncol=3,fileFolder = fileFolder, facetVar="Region", width = 34, height = 26, highlight = "EUR")

#####################################################################################


###### Cropland Nitrogen Budget ######################################################
## Diverging stacked bar: Inputs positive; Withdrawals + Balance (Surplus, Soil
## Organic Matter, Balanceflow) negative. Budget closes (Inputs = Withdrawals +
## Balance), so the positive and negative extents mirror each other.
nbNames <- c(
  fertilizer  = "Resources|Nitrogen|Cropland Budget|Inputs|+|Fertilizer (Mt Nr/yr)",
  manure_conf = "Resources|Nitrogen|Cropland Budget|Inputs|+|Manure Recycled from Confinements (Mt Nr/yr)",
  manure_stub = "Resources|Nitrogen|Cropland Budget|Inputs|+|Manure From Stubble Grazing (Mt Nr/yr)",
  res_ag_in   = "Resources|Nitrogen|Cropland Budget|Inputs|+|Recycled Aboveground Crop Residues (Mt Nr/yr)",
  res_bg_in   = "Resources|Nitrogen|Cropland Budget|Inputs|+|Recycled Belowground Crop Residues (Mt Nr/yr)",
  fix_symb    = "Resources|Nitrogen|Cropland Budget|Inputs|+|Biological Fixation Symbiotic Crops (Mt Nr/yr)",
  fix_free    = "Resources|Nitrogen|Cropland Budget|Inputs|+|Biological Fixation Freeliving Microorganisms (Mt Nr/yr)",
  deposition  = "Resources|Nitrogen|Cropland Budget|Inputs|+|Atmospheric Deposition (Mt Nr/yr)",
  seed        = "Resources|Nitrogen|Cropland Budget|Inputs|+|Seed (Mt Nr/yr)",
  ash         = "Resources|Nitrogen|Cropland Budget|Inputs|+|Ash from Burned Crop Residues (Mt Nr/yr)",
  harvested   = "Resources|Nitrogen|Cropland Budget|Withdrawals|+|Harvested Crops (Mt Nr/yr)",
  res_ag_out  = "Resources|Nitrogen|Cropland Budget|Withdrawals|+|Aboveground Crop Residues (Mt Nr/yr)",
  res_bg_out  = "Resources|Nitrogen|Cropland Budget|Withdrawals|+|Belowground Crop Residues (Mt Nr/yr)",
  surplus     = "Resources|Nitrogen|Cropland Budget|Balance|+|Nutrient Surplus (Mt Nr/yr)",
  som         = "Resources|Nitrogen|Cropland Budget|Balance|+|Soil Organic Matter (Mt Nr/yr)",
  balanceflow = "Resources|Nitrogen|Cropland Budget|Balance|+|Balanceflow (Mt Nr/yr)"
)

nbData <- data_list2[, years, c(nbNames)]
nbInt  <- lapply(nbNames, \(x) nbData[, , x])

dataPlotNB <- mbind(
  ## Inputs (positive)
  name_it(nbInt$fertilizer,  "Fertilizer"),
  name_it(nbInt$manure_conf, "Manure from confinements"),
  name_it(nbInt$manure_stub, "Manure from stubble grazing"),
  name_it(nbInt$res_ag_in,   "Recycled aboveground residues"),
  name_it(nbInt$res_bg_in,   "Recycled belowground residues"),
  name_it(nbInt$fix_symb,    "Legume symbiotic fixation"),
  name_it(nbInt$fix_free,    "Free-living fixation"),
  name_it(nbInt$deposition,  "Atmospheric deposition"),
  name_it(nbInt$seed,        "Seed"),
  name_it(nbInt$ash,         "Ash from burned residues"),
  ## Withdrawals (negative)
  name_it(-nbInt$harvested,  "Harvested crops"),
  name_it(-nbInt$res_ag_out, "Aboveground residues removed"),
  name_it(-nbInt$res_bg_out, "Belowground residues removed"),
  ## Balance (negative)
  name_it(-nbInt$surplus,    "Nutrient surplus"),
  name_it(-nbInt$som,        "Soil organic matter"),
  name_it(-nbInt$balanceflow,"Balanceflow")
)

## Colours by content: blues = synthetic/atmospheric, browns/khaki = organic
## recycling, greens = biological fixation, oranges = crop offtake, red = losses,
## greys = soil/calibration. Order = stacking order within each side.
nitrogenColors <- c(
  "Fertilizer"                    = "#3182BD",  # blue (synthetic)
  "Manure from confinements"      = "#8C510A",  # dark brown
  "Manure from stubble grazing"   = "#BF812D",  # medium brown
  "Recycled aboveground residues" = "#DFC27D",  # light khaki
  "Recycled belowground residues" = "#998A3C",  # dark khaki
  "Legume symbiotic fixation"     = "#238B45",  # strong green (legumes)
  "Free-living fixation"          = "#99D8C9",  # light teal-green
  "Atmospheric deposition"        = "#6BAED6",  # sky blue
  "Seed"                          = "#807DBA",  # purple
  "Ash from burned residues"      = "#BDBDBD",  # light grey
  ## Withdrawals + balance (negative), ordered bottom -> top toward zero
  "Nutrient surplus"              = "#E41A1C",  # red (losses) - bottom
  "Soil organic matter"           = "#525252",  # dark grey (soil storage)
  "Balanceflow"                   = "#969696",  # medium grey (calibration)
  "Belowground residues removed"  = "#A6611A",  # brown
  "Aboveground residues removed"  = "#FFD92F",  # yellow
  "Harvested crops"               = "#41AB5D"   # green (offtake) - nearest zero
)

## Legend should read top -> bottom in the same visual order as the stacked bar
nbLegendOrder <- c(
  "Fertilizer", "Manure from confinements", "Manure from stubble grazing",
  "Recycled aboveground residues", "Recycled belowground residues",
  "Legume symbiotic fixation", "Free-living fixation", "Atmospheric deposition",
  "Seed", "Ash from burned residues", "Soil organic matter",
  "Harvested crops", "Aboveground residues removed", "Belowground residues removed",
  "Balanceflow", "Nutrient surplus"
)

plotsReport[["nitrogenBudget"]] <- plotBars2Var(dataPlotNB, years, "Cropland Nitrogen Budget", "Mt Nr/yr", "EUR", ncol=3, fileFolder = fileFolder, facetVar="Region", palette = nitrogenColors, width = 30, height = 28, legendBreaks = nbLegendOrder)

#####################################################################################


###### Crop area #####################################################################
CropNamesArea <- c(
  Pulses      = "Resources|Land Cover|Cropland|Croparea|Crops|Other crops|+|Pulses (million ha)",
  Soybean     = "Resources|Land Cover|Cropland|Croparea|Crops|Oil crops|+|Soybean (million ha)",
  Groundnuts  = "Resources|Land Cover|Cropland|Croparea|Crops|Oil crops|+|Groundnuts (million ha)",
  Cereals     = "Resources|Land Cover|Cropland|Croparea|Crops|+|Cereals (million ha)",
  Forage      = "Resources|Land Cover|Cropland|Croparea|+|Forage (million ha)",
  Total_Crops = "Resources|Land Cover|Cropland|Croparea|+|Crops (million ha)"
)

cropDataArea <- data_list2[, years, c(CropNamesArea)]   

cropDataAreaInt <- lapply(CropNamesArea, \(x) cropDataArea[, , x])

dataPlotArea <- mbind(
  name_it(cropDataAreaInt$Pulses,     "Pulses"),
  name_it(cropDataAreaInt$Soybean,    "Soybean"),
  name_it(cropDataAreaInt$Groundnuts, "Groundnuts"),
  name_it(cropDataAreaInt$Cereals, "Cereals"),
  name_it(cropDataAreaInt$Total_Crops - cropDataAreaInt$Pulses - cropDataAreaInt$Soybean - cropDataAreaInt$Groundnuts - cropDataAreaInt$Cereals, "Other crops"),
  name_it(cropDataAreaInt$Forage, "Forage")
)

plotsReport[["cropArea"]] <- plotBars2Var(dataPlotArea, years, "Crop Area", "Million ha", "EUR", ncol=3,fileFolder = fileFolder, facetVar="Region")
plotsReport[["cropAreaPulses"]] <- plotBars2Var(dataPlotArea[,,c("Pulses", "Soybean", "Groundnuts")], years, "Crop Area (legumes)", "Million ha", "EUR", ncol=3,fileFolder = fileFolder, facetVar="Region")

#####################################################################################
###### Trade #####################################################################

 TradeNames <- c(
  Demand.Pulses ="Demand|Crops|Other crops|+|Pulses (Mt DM/yr)",
  Exports.Pulses = "Trade|Exports|Crops|Other crops|+|Pulses (Mt DM/yr)",
  Imports.Pulses = "Trade|Imports|Crops|Other crops|+|Pulses (Mt DM/yr)",
  Production.Pulses = "Production|Crops|Other crops|+|Pulses (Mt DM/yr)",
  
  Demand.Soybeans = "Demand|Crops|Oil crops|+|Soybean (Mt DM/yr)",
  Exports.Soybeans = "Trade|Exports|Crops|Oil crops|+|Soybean (Mt DM/yr)",
  Imports.Soybeans = "Trade|Imports|Crops|Oil crops|+|Soybean (Mt DM/yr)",
  Production.Soybeans = "Production|Crops|Oil crops|+|Soybean (Mt DM/yr)",
  
  Demand.Oils = "Demand|Crops|+|Oil crops (Mt DM/yr)",
  Exports.Oils = "Trade|Exports|Crops|+|Oil crops (Mt DM/yr)",
  Imports.Oils = "Trade|Imports|Crops|+|Oil crops (Mt DM/yr)",
  Production.Oils = "Production|Crops|+|Oil crops (Mt DM/yr)",

  Demand.Oilcakes = "Demand|Secondary products|+|Oilcakes (Mt DM/yr)",
  Exports.Oilcakes = "Trade|Exports|Secondary products|+|Oilcakes (Mt DM/yr)",
  Imports.Oilcakes = "Trade|Imports|Secondary products|+|Oilcakes (Mt DM/yr)",
  Production.Oilcakes = "Production|Secondary products|+|Oilcakes (Mt DM/yr)",

  Demand.Crops = "Demand|++|Crops (Mt DM/yr)",
  Exports.Crops = "Trade|Exports|+|Crops (Mt DM/yr)",
  Imports.Crops = "Trade|Imports|+|Crops (Mt DM/yr)",
  Production.Crops = "Production|+|Crops (Mt DM/yr)",

  Demand.Livestock = "Demand|++|Livestock products (Mt DM/yr)",
  Exports.Livestock = "Trade|Exports|+|Livestock products (Mt DM/yr)",
  Imports.Livestock = "Trade|Imports|+|Livestock products (Mt DM/yr)",
  Production.Livestock = "Production|+|Livestock products (Mt DM/yr)"
)

TradeData <- data_list2[, years, c(TradeNames)] 
TradeData[TradeData<0] <- 0

getNames(TradeData) <- stri_replace_all_fixed(
  str = getNames(TradeData),          
  pattern = TradeNames,                
  replacement = names(TradeNames),    
  vectorize_all = FALSE
)

TradeData <- mbind(TradeData[,,c("Exports","Demand","Production")],
                   (- TradeData[,,c("Imports")]))


plotsReport[["Trade"]] <- plotBars3Var(TradeData[,,c("Demand","Exports","Imports")], years, "Trade", "Mt DM/yr", "EUR", ncol=3, fileFolder, facetVar="Item", width = 34, height = 26, palette=tradeColors)
##################################################################################### ,"Imports"