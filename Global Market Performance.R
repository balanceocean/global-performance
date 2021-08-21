# Limpiamos el Global Environment
rm(list=setdiff(ls(), ""))

# Definimos las variables Globales
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Librerias que necesitamos
if (!require("quantmod")) devtools::install_github("quantmod"); library(quantmod)
if (!require("TTR")) devtools::install_github("dTTR"); library(TTR)
if (!require("ggplot2")) devtools::install_github("ggplot2"); library(ggplot2)
if (!require("dplyr")) devtools::install_github("dplyr"); library(dplyr)
if (!require("lubridate")) devtools::install_github("lubridate"); library(lubridate)
if (!require("maps")) devtools::install_github("maps"); library(maps)
if (!require("mapproj")) devtools::install_github("mapproj"); library(mapproj)

sDate <- "2020-01-01"

# Creating the Map
WorldData <- map_data('world') %>% filter(region != "Antarctica") -> WorldData
WorldData <- fortify(WorldData)

countryList <- WorldData$region
countryList <- countryList[!duplicated(countryList)]

# Get access to the DataBase
DDBB <- read.csv("ETF-Mercados.csv", 
                 sep = ";", 
                 header = FALSE,
                 stringsAsFactors = FALSE)
colnames(DDBB) <- c("Country", "Ticket")

# Check if all the countries in our DDBB are in the Worldmap function
DDBB$Check <- NULL
for(i in 1:nrow(DDBB)){
  for(j in countryList){
    if(DDBB$Country[i] == j){
      DDBB$Check[i] <- "OK"
      break
    }else{
      DDBB$Check[i] <- "Not Found"  
    }
  }
}

# Download price data
eDate           <- Sys.Date()
frecuency       <- "daily"
portfolioPrices <- NULL
assets          <- DDBB$Ticket

i <- 1
for (Ticker in assets){
  portfolioPrices <- cbind(portfolioPrices, getSymbols.yahoo(Ticker, 
                                                             from = sDate, 
                                                             to = eDate, 
                                                             periodicity = frecuency, 
                                                             auto.assign = FALSE)[,4])
  print(paste("We have downloaded the Ticket: ", Ticker, " (", DDBB$Country[i], ")", sep = ""))
  i <- i + 1
}

# Returns and YTD calculations
portfolioPrices      <- na.omit(portfolioPrices)
portfolioReturns     <- ROC(portfolioPrices, n = 1, type = "discrete")
portfolioReturns[1,] <- 0

portfolioYTD     <- cumsum(portfolioReturns)

portfolioReturns <- round(portfolioReturns*100, digits = 2)
portfolioYTD     <- round(portfolioYTD*100, digits = 2)

# Updating the DDBB with the new information
DDBB$YTD   <- t(portfolioYTD[nrow(portfolioYTD), ])
DDBB$Daily <- t(portfolioReturns[nrow(portfolioReturns), ])

# Color Code
  # Daily Return
  DDBB$Daily <- na.omit(DDBB$Daily)
  
  DDBB$DailyCol <- NULL
  for(i in 1:nrow(DDBB)){
    if(DDBB$Daily[i] <= -1.5){
      DDBB$DailyCol[i] <- "<< -1.5%"  
    }else if(DDBB$Daily[i] > -1.5 & DDBB$Daily[i] <= -1.0){
      DDBB$DailyCol[i] <- "-1.5% to -1.0%"  
    }else if(DDBB$Daily[i] > -1.0 & DDBB$Daily[i] <= -0.5){
      DDBB$DailyCol[i] <- "-1.0% to -0.5%"  
    }else if(DDBB$Daily[i] > -0.5 & DDBB$Daily[i] <= 0){
      DDBB$DailyCol[i] <- "-0.5% to 0.0%"  
    }else if(DDBB$Daily[i] > 0 & DDBB$Daily[i] <= 0.5){
      DDBB$DailyCol[i] <- "0.0% to 0.5%"  
    }else if(DDBB$Daily[i] > 0.5 & DDBB$Daily[i] <= 1.0){
      DDBB$DailyCol[i] <- "0.5% to 1.0%"  
    }else if(DDBB$Daily[i] > 1.0 & DDBB$Daily[i] <= 1.5){
      DDBB$DailyCol[i] <- "1.0% to 1.5%"  
    }else{
      DDBB$DailyCol[i] <- ">> 1.5%"
    }
  }
  
  # YTD Return
  DDBB$YTD <- na.omit(DDBB$YTD)
  
  DDBB$YTDCol <- NULL
  for(i in 1:nrow(DDBB)){
    if(DDBB$YTD[i] <= -15){
      DDBB$YTDCol[i] <- "<< -15.0%"  
    }else if(DDBB$YTD[i] > -15 & DDBB$YTD[i] <= -10){
      DDBB$YTDCol[i] <- "-15.0% to -10.0%"  
    }else if(DDBB$YTD[i] > -10 & DDBB$YTD[i] <= -5){
      DDBB$YTDCol[i] <- "-10.0% to -5.0%"  
    }else if(DDBB$YTD[i] > -5 & DDBB$YTD[i] <= 0){
      DDBB$YTDCol[i] <- "-5.0% to 0.0%"  
    }else if(DDBB$YTD[i] > 0 & DDBB$YTD[i] <= 5){
      DDBB$YTDCol[i] <- "0.0% to 5.0%"  
    }else if(DDBB$YTD[i] > 5 & DDBB$YTD[i] <= 10){
      DDBB$YTDCol[i] <- "5.0% to 10.0%"  
    }else if(DDBB$YTD[i] > 10 & DDBB$YTD[i] <= 15){
      DDBB$YTDCol[i] <- "10.0% to 15.0%"  
    }else{
      DDBB$YTDCol[i] <- ">> 15.0%"
    }
  }

  # Daily Returns
  worstDaily        <- min(DDBB$Daily)
  worstCountryDaily <- DDBB$Country[which.min(DDBB$Daily)]
  bestDaily         <- max(DDBB$Daily)
  bestCountryDaily  <- DDBB$Country[which.max(DDBB$Daily)]
  
  daily <- ggplot() + 
    geom_map(data = WorldData, map = WorldData,
             aes(x = long, y = lat, group = group, map_id = region),
             fill = "gray", colour = "white", size = 0.5) +
    coord_map("rectangular", lat0 = 0, xlim = c(-180,180), ylim = c(-60, 90)) +
    geom_map(data = DDBB, map = WorldData,
             aes(fill = DailyCol, map_id = Country),
             colour="white", size = 0.5) +
    scale_fill_manual(limits = c("<< -1.5%",
                                 "-1.5% to -1.0%", 
                                 "-1.0% to -0.5%",
                                 "-0.5% to 0.0%",
                                 "0.0% to 0.5%",
                                 "0.5% to 1.0%",
                                 "1.0% to 1.5%",
                                 ">> 1.5%"), 
                      values = c("<< -1.5%" = "red",
                                 "-1.5% to -1.0%" = "brown", 
                                 "-1.0% to -0.5%" = "orange",
                                 "-0.5% to 0.0%" = "yellow",
                                 "0.0% to 0.5%" = "light green",
                                 "0.5% to 1.0%" = "turquoise",
                                 "1.0% to 1.5%" = "blue",
                                 ">> 1.5%" = "green"),
                      drop = FALSE) +
    scale_y_continuous(breaks = c()) +
    scale_x_continuous(breaks = c()) +
    labs(fill = "Return (%)",
         title = paste("World Financial Markets Daily Return - ", last(index(portfolioPrices)), sep = ""), 
         subtitle = paste("Best Country: ", bestCountryDaily, " with ", bestDaily, "% / Worst Country: ", worstCountryDaily, " with ", worstDaily, "%"),
         caption = "By: Carlos Jimenez (@cjimenezdiaz)\nData Source: Blackrock, Global X Funds and VanEck Vectors",
         x = "",
         y = "") +
    theme_bw() +
    theme(panel.border = element_blank())
  
  
  # YTD Return
  worstYTD        <- min(DDBB$YTD)
  worstCountryYTD <- DDBB$Country[which.min(DDBB$YTD)]
  bestYTD         <- max(DDBB$YTD)
  bestCountryYTD  <- DDBB$Country[which.max(DDBB$YTD)]
  
  ytd <- ggplot() + 
    geom_map(data = WorldData, map = WorldData,
             aes(x = long, y = lat, group = group, map_id = region),
             fill = "darkgray", colour = "white", size = 0.5) +
    coord_map("rectangular", lat0 = 0, xlim = c(-180,180), ylim = c(-60, 90)) +
    geom_map(data = DDBB, map = WorldData,
             aes(fill = YTDCol, map_id = Country),
             colour = "white", size = 0.5) +
    scale_fill_manual(limits = c("<< -15.0%",
                                 "-15.0% to -10.0%", 
                                 "-10.0% to -5.0%",
                                 "-5.0% to 0.0%",
                                 "0.0% to 5.0%",
                                 "5.0% to 10.0%",
                                 "10.0% to 15.0%",
                                 ">> 15.0%"), 
                      values = c("<< -15.0%" = "red",
                                 "-15.0% to -10.0%" = "brown", 
                                 "-10.0% to -5.0%" = "orange",
                                 "-5.0% to 0.0%" = "yellow",
                                 "0.0% to 5.0%" = "lightgreen",
                                 "5.0% to 10.0%" = "turquoise",
                                 "10.0% to 15.0%" = "blue",
                                 ">> 15.0%" = "green"),
                      drop = FALSE) +
    scale_y_continuous(breaks = c()) +
    scale_x_continuous(breaks = c()) +
    labs(fill = "Return (%)",
         title = paste("World Financial Markets YTD - ", last(index(portfolioPrices)), sep = ""), 
         subtitle = paste("Best Country: ", bestCountryYTD, " with ", bestYTD, "% / Worst Country: ", worstCountryYTD, " with ", worstYTD, "%"),
         caption = "By: Carlos Jimenez (@cjimenezdiaz)\nData Source: Blackrock, Global X Funds and VanEck Vectors",
         x = "",
         y = "") +
    theme_bw() +
    theme(panel.border = element_blank())

  daily
  ytd
  
ggsave(file = "Graficos Creados/World-Market-DailyP.png", plot = daily, dpi = 600, width = 8, height = 5)
ggsave(file = "Graficos Creados/World-Market-YTD.png", plot = ytd, dpi = 600, width = 8, height = 5)
