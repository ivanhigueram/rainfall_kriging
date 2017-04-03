library(raster)
library(rgeos)
library(spacetime)
library(gstat)
library(rgdal)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(data.table)

setwd("~/Dropbox/BANREP/Linea_Negra_R/Data/")
colombia_dpto <- readOGR(dsn = "Colombia_Deptos", layer = "Colombia_Deptos") %>%
  spTransform(CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

setwd("~/Dropbox/BANREP/Linea_Negra_R/Data/")
prec <- brick("World_Climate/prec_1km.tif")

prec_depto <- raster::extract(prec, colombia_dpto, 
                              fun = mean, na.rm = TRUE,
                              df = TRUE, sp = TRUE)

#Load shapes
setwd("~/Dropbox/BANREP/Deforestacion/Datos/")
colombia_municipios <- readOGR(dsn = "Municipios", layer = "Municipios", stringsAsFactors = FALSE) %>%
  spTransform(CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


#Lluvias IDEAM
setwd("~/Dropbox/IDEAM Lluvias/")
stations_data <- fread("Estaciones_IDEAM.csv")
stations_geo <-  readOGR(dsn = "Catalogo_estaciones_IDEAM_V9_Enero_2017/", layer = "Catalogo_estaciones_IDEAM_V9_Enero_2017", stringsAsFactors = FALSE) %>%
  spTransform(CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#Filter for the Guajira department 
guajira <- colombia_municipios %>%
  .[.$COD_DANE_D == 44, ]

stations_guajira <- stations_geo %>%
  .[.$DEPTO == "LA GUAJIRA", ]

#Process rainfall data (read and aggregate by station/month)
rainfall_stations <- list.files() %>%
  .[str_detect(., "lluvia")] %>%
  lapply(., function(x){
    fread(x) %>%
      mutate(date = as.Date(date)) %>%
      mutate(month = format(date, "%m")) %>%
      group_by(station, year, month, latitude, longitude) %>%
      summarize(rainfall_month = sum(rainfall, na.rm = T))
  }) %>% ldply() %>% merge(., stations_data, by.x = "station", by.y = "CODIGO CAT.", all.x = T)


#Filter station/month data base
rainfall_guajira <- rainfall_stations %>%
  filter(DEPTO == "LA GUAJIRA") 
  # %>%
  # group_by(MPIO, year) %>%
  # summarize(rainfall_year = mean(rainfall_month, na.rm = T)) 

rainfall_dpto <- rainfall_stations %>%
  group_by(DEPTO, year) %>%
  summarize(rainfall_year = mean(rainfall_month, na.rm = T))
  # %>%
  # mutate(date = str_c(year, month, sep = "-"))


# SPDF (Data frame to Spatial Object)
rainfall_guajira_df <- rainfall_guajira %>%
  select(-V24, -V25) %>%
  mutate(date = str_c(year, month, "01", sep = "-")) %>%
  mutate(date = as.Date(date)) %>%
  mutate_at(c("LONGITUD", "LATITUD"), function(x){
    str_replace(x, ",", ".") %>%
      as.numeric()
    }) %>%
  na.omit() #For modeling, we need to remove NA's

x = rainfall_guajira_df$LONGITUD
y = rainfall_guajira_df$LATITUD
time = rainfall_guajira_df$date


a <- stConstruct(x = rainfall_guajira_df,
                 space = c("LONGITUD", "LATITUD"),
                 time = "date",
                 interval = T)
proj4string(a) = "+proj=longlat +datum=WGS84"


stplot(a[,,"rainfall_month"])


#Calculate variogram
var <- variogramST(rainfall_month ~ 1 , locations = a,
                   data = a, 
                   tunit = "days", 
                   assumeRegular = FALSE, na.omit = TRUE) 

#Precipitation time-line
p <- ggplot(rainfall_dpto, aes(x = year, y = rainfall_year, ,group = DEPTO, colour = DEPTO)) 
p <- p + geom_line()
p
ggplotly(p)
  
p <- ggplot(rainfall_guajira, aes(x = year, y = rainfall_year, ,group = MPIO, colour = MPIO))
p <- p + geom_line()
p
ggplotly(p)
