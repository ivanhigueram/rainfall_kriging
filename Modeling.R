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

#####################################################################################
#################################### LOAD DATA ######################################
#####################################################################################

# SET DIRECTORIES
data <- "~/Dropbox/BANREP/IDEAM Lluvias/"

# NATIONAL GEOMETRIES
setwd(data)
colombia_dpto <- readOGR(dsn = "Municipios", layer = "Municipios") %>%
  spTransform(CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


# STATIONS DATA
setwd(str_c(data, "Stations_data"))
stations_data <- fread("Estaciones_IDEAM.csv")
stations_geo <-  readOGR(dsn = ".", layer = "Catalogo_estaciones_IDEAM_V9_Enero_2017", stringsAsFactors = FALSE) %>%
  spTransform(CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


# RAINFALL DATA - IDEAM (Processed from .txt to .csv by ideam.R code in the same repository)
setwd(str_c(data, "Rainfall_data"))

rainfall_stations <- list.files() %>%
  .[str_detect(., "lluvia")] %>%
  lapply(., function(x){
    fread(x) %>%
      mutate(date = as.Date(date)) %>%
      mutate(month = format(date, "%m")) %>%
      group_by(station, year, month, latitude, longitude) %>%
      summarize(rainfall_month = sum(rainfall, na.rm = T))
  }) %>% ldply() %>% merge(., stations_data, by.x = "station", by.y = "CODIGO CAT.") 


######### RAINFALL DATA - BIVAND ET. AL. WORLD CLIM ###########
# setwd("~/Dropbox/BANREP/Linea_Negra_R/Data/")               #
# prec <- brick("World_Climate/prec_1km.tif")                 #
#                                                             #
# prec_depto <- raster::extract(prec, colombia_dpto,          #
#                               fun = mean, na.rm = TRUE,     #
#                               df = TRUE, sp = TRUE)         #
###############################################################


#####################################################################################
##################### SPATIAL KRIGING TO ALL COLOMBIA (ONE YEAR) ####################
#####################################################################################
rainfall_1990 <- rainfall_stations %>% filter(year == "1990", month == "12") %>%
  select(station, rainfall_month, month, LATITUD, LONGITUD) %>%
  mutate_at(vars(matches("UD")), funs(as.numeric(gsub(",", ".", gsub("\\.", "", .)))))

#df to spatial object (there is a dplyr way?) and fit the variogram
coordinates(rainfall_1990) <- ~ LONGITUD + LATITUD

#Calculate and fit variogram (gstat package)
rain.vgm <- variogram(rainfall_month ~ 1,
                      data = rainfall_1990, 
                      locations = ~ LONGITUD + LATITUD) # calculates sample variogram values 

rain.fit <- fit.variogram(rain.vgm, model = vgm("Sph")) # fit model

#Load the points to estimate (gridded data: rasters!)
est_grid <- brick(str_c(data, "Estimations/", "loss_year_brick_1km.tif")) %>%
  aggregate(., 4) %>%
  as.data.frame(., xy = T, na.rm = T) %>%
  select(x, y)

# LET'S KRIG!
coordinates(est_grid) <- ~ x + y
coordinates(rainfall_1990) <- ~ LONGITUD + LATITUD
proj4string(rainfall_1990) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(est_grid) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

rain.kriged <- krige(rainfall_month ~ 1, rainfall_1990, est_grid, model = rain.fit)


rain.kriged %>% as.data.frame %>%
  ggplot(aes(x=x, y=y)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
  scale_fill_gradient(low = "yellow", high="red") +
  scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
  theme_bw()


parKrige <- function(cl, s, old_data, model_fit) {
  # split the prediction points s
  idx <- clusterSplit(cl, 1:length(s)) 
  ssplt <- lapply(idx, function(i) s[i,])
  # compute the predictions in parallel
  v <- clusterApply(cl, ssplt, function(x, old_data, model){
    gstat::krige(formula = formula("rainfall_month ~ 1"), data = old_data, newdata = x, model = model_fit)
    })
  # assemble and return the results
  merge <- function(x, f) do.call("c", lapply(x, f))
  s.o <- point(s)
  s.o$zhat <- merge(v, function(y) y$zhat)
  s.o$sigma2hat <- merge(v, function(y) y$sigma2hat)
  return(s.o)
}

#Parallel :) 
no_cores <- detectCores() - 1
cl <- makeForkCluster(no_cores)
# clusterExport(cl, "rainfall_1990", "formula")
parKrige(cl, s = est_grid, old_data = rainfall_1990, model_fit = rain.fit)
stopCluster(cl)

########################################################################################
################# LEST'S EXPERIMENT WITH A SMALL DEPARTMENT: GUAJIRA ###################
########################################################################################


#Filter for the Guajira department
guajira <- colombia_municipios %>%
  .[.$COD_DANE_D == 44, ]

stations_guajira <- stations_geo %>%
  .[.$DEPTO == "LA GUAJIRA", ]

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
