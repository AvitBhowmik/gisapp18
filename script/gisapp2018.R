
##########################################################################################
##########################################################################################
##							Spatial Interpolation with R 								##
##								GIS Application											##
##							   March 20-21, 2017										##
##							  Dr. Avit K. Bhowmik										##
##########################################################################################
##########################################################################################

##########################################################################################
##										Day 1			 								##
##									March 20, 2017										##
##########################################################################################

## Set the working directory

setwd("/Users/avitbhowmik/Teaching_Supervision/Uni-Landau/GIS_Application_2018/gisapp18/data")


## Read and visualize "Shapefiles" - spatial objects

## Load the spatial package

library(maptools)

## Spatial Polygons, i.e. Area

bd.boundary <- readShapePoly("bd_boundary")

class(bd.boundary)

summary(bd.boundary)

bbox(bd.boundary)

## Visit http://spatialreference.org/ref/epsg/25832/

proj4string(bd.boundary) <-
CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

## Access coordinates and data

bd.boundary@bbox

bd.boundary@data

bd.boundary@data$UNREG1


# Plot shapefile

plot(bd.boundary)

spplot(bd.boundary["UNREG1"])


## Point spatial data from comma-separated (or text) values. Visualizing on polygons.

bd.heavy.metal <- read.csv("bd_heavy_metal.csv", sep=",", header=TRUE)

class(bd.heavy.metal)

head(bd.heavy.metal)

tail(bd.heavy.metal)


######################################################

## Exercise 1: Is bd.heavy.metal a space-time data? ##
## If so, what are the space-time attributes of it? ##

######################################################

## Convert into SpatialPointsDataFrame and visualize them

coordinates(bd.heavy.metal) <- ~LONG_DEG+LAT_DEG
proj4string(bd.heavy.metal) <-
CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

class(bd.heavy.metal)

head(bd.heavy.metal@coords)

head(bd.heavy.metal@data)

spplot(bd.heavy.metal, 4:ncol(bd.heavy.metal@data), xlim=c(87.9, 92.9), ylim=c(20.6, 26.8),
	sp.layout=list("sp.polygons", bd.boundary, col="gray"),
	col.regions=colorRampPalette(c("blue", "green", "yellow", "orange", "red"))(100),
	scales=list(draw=T), colorkey=T)
	
# library(ggmap)
# library(rgdal)
# bd.map <- get_map(location = "bd", source="google", zoom = 7)

# bd.metal <- spTransform(bd.heavy.metal, CRS("+init=epsg:3857"))

# plot(bd.metal, bgMap = bd.map, pch = 16, cex = .5)

# spplot(bd.metal, 4,
# sp.layout = list(panel.ggmap, bd.map, first = TRUE),
# col.regions=colorRampPalette(c("blue", "green", "yellow", "orange", "red"))(100),
# scales=list(draw=T), colorkey=T)


## Save it as a shape file

# writePointsShape(bd.heavy.metal, "bd_heavy_metal")


# Spatial Grids, i.e. Rasters

library(raster)

bd.elv <- raster("bd_elv.tif")

bd.elv[Which(bd.elv<=0, cells=TRUE)] <- NA

class(bd.elv)

plot(bd.elv)

plot(bd.boundary, add=T)

plot(bd.heavy.metal, add=T, col="red")

bd.elv.grid <- as(bd.elv, "SpatialGridDataFrame")

summary(bd.elv.grid)

spplot(bd.elv.grid, sp.layout=list("sp.lines", as(bd.boundary, "SpatialLines")),
	col.regions=topo.colors(100), scales=list(draw=T))


################################################################

## Exercise 2: Load the other rasters from the data directory ##
## Explore them from the plot ##

################################################################

bd.soc <- raster("bd_soc.tif")
bd.scc <- raster("bd_scc.tif")
bd.ph <- raster("bd_ph.tif")
bd.wc <- raster("bd_wc.tif")
bd.pop.dens <- raster("bd_pop_dens.tif")

## Extract values of the spatial predictors at the ground water sampling points

plot(bd.scc)

bd.heavy.metal@data$elv <- extract(bd.elv, bd.heavy.metal)
bd.heavy.metal@data$soc <- extract(bd.soc, bd.heavy.metal)

head(bd.heavy.metal@data)

#############################################################

## Exercise 3: Extract values of SCC, pH, WC and POP_DENS ##

#############################################################

bd.heavy.metal@data$scc <- extract(bd.scc, bd.heavy.metal)
bd.heavy.metal@data$ph <- extract(bd.ph, bd.heavy.metal)
bd.heavy.metal@data$wc <- extract(bd.wc, bd.heavy.metal)
bd.heavy.metal@data$popdens <- extract(bd.pop.dens, bd.heavy.metal)

# Distance between sampling points

dis.pts <- spDists(bd.heavy.metal@coords, longlat=TRUE)

max(dis.pts)

min(dis.pts[which(dis.pts!=0)])

## Remove duplicated from data

zerodist(bd.heavy.metal, zero=0.008333333)
bd.heavy.metal <- remove.duplicates(bd.heavy.metal)

writePointsShape(bd.heavy.metal, "bd_heavy_metal")


## Read the shapefile with lead concentration and predictor values

library(maptools)

setwd("/Users/avitbhowmik/Teaching_Supervision/Uni-Landau/gisapp17/data")

bd.heavy.metal <- readShapePoints("bd_heavy_metal")

head(bd.heavy.metal@data)
tail(bd.heavy.metal@data)

## Remove NA values from data

bd.heavy.metal <- bd.heavy.metal[!is.na(bd.heavy.metal@data$As),]

##################################################################

# Exercise 3: Remove the NA values for the predictors in one line
# of code

##################################################################

for(i in which(colnames(bd.heavy.metal@data)=="elv"):
	ncol(bd.heavy.metal@data)){
	bd.heavy.metal <- bd.heavy.metal[!is.na(bd.heavy.metal@data[,i]),]}

head(bd.heavy.metal@data)

##################################################################

# Exercise 4: Explore R objects and rgeos package
# What are the location (coordinates) of the
# sampling points with the highest and lowest As concentration?
# Plot them on a map of Bangladesh.
# Calculate the Great Cricle distance between them.

##################################################################

# Solve 4

# bd.heavy.metal@coords[which(bd.heavy.metal@data$As==max(bd.heavy.metal@data$As)),]

# bd.heavy.metal@coords[which(bd.heavy.metal@data$As==min(bd.heavy.metal@data$As)),]

# bd.boundary <- readShapePoly("bd_boundary")

# plot(bd.boundary)

# plot(bd.heavy.metal[which(bd.heavy.metal@data$As==max(bd.heavy.metal@data$As)),], add=T, col="red")

# plot(bd.heavy.metal[which(bd.heavy.metal@data$As==min(bd.heavy.metal@data$As)),], add=T, col="red")

# library(rgeos)

# gDistance(bd.heavy.metal[which(bd.heavy.metal@data$As==max(bd.heavy.metal@data$As)),],
# bd.heavy.metal[which(bd.heavy.metal@data$As==min(bd.heavy.metal@data$As)),])

## Check for Skewness

library(moments)

skewness(bd.heavy.metal@data$As)

## High positive skewness, hence we will log transform the data before
## any statistical analysis

# Spatial trend in the data, stationarity

summary(lm(log(bd.heavy.metal@data$As)~bd.heavy.metal@coords[,1]+bd.heavy.metal@coords[,2]))

summary(lm(log(As)~elv+soc+scc+ph+wc+popdens, data=bd.heavy.metal@data))

summary(lm(log(As)~elv+soc+scc+ph+wc+popdens+LONG_DEG+LAT_DEG, data=bd.heavy.metal@data))

bd.heavy.metal@data <- data.frame(bd.heavy.metal@data, bd.heavy.metal@coords)
step(lm(log(As)~elv+soc+scc+ph+wc+popdens+LONG_DEG+LAT_DEG, data=bd.heavy.metal@data))

summary(lm(log(As) ~ scc + ph + wc + LONG_DEG, data=bd.heavy.metal@data))


#################################################################################
## Fitting variogram
#################################################################################

## Variogram modelling

library(gstat)

## Without predictors

var.As <- variogram(log(As)~1, bd.heavy.metal)
plot(var.As)

## With cutoff
var.As <- variogram(log(As)~1, bd.heavy.metal, cutoff=700)
plot(var.As)

## With spatial predictors

var.As <- variogram(log(As)~scc + ph + wc + LONG_DEG,
	bd.heavy.metal)
plot(var.As)

## Explore variogram models

vgm()

## Fit an exponential model

vmod.As.exp <- fit.variogram(object=var.As, model=vgm(psill=2.5, model="Exp",
	range=180, nugget=0.9), fit.method=1)
	
plot(var.As, vmod.As.exp)

##################################################################

# Exercise 5: Fit a spherical model and check the error statistics

##################################################################

# Spherical model

vmod.As.sph <- fit.variogram(object=var.As, model=vgm(psill=2.5, model="Sph",
	range=180, nugget=0.9))

plot(var.As, vmod.As.sph)

## Compare Variogram Models
attr(vmod.As.exp, "SSErr")
attr(vmod.As.sph, "SSErr")

###########################################################################################################

# Spatial interpolation

###########################################################################################################

## Prepare prediction grid

library(raster)

bd.elv.grid <- as(raster("bd_elv.tif"), "SpatialGridDataFrame")
bd.soc.grid <- as(raster("bd_soc.tif"), "SpatialGridDataFrame")
bd.scc.grid <- as(raster("bd_scc.tif"), "SpatialGridDataFrame")
bd.ph.grid <- as(raster("bd_ph.tif"), "SpatialGridDataFrame")
bd.wc.grid <- as(raster("bd_wc.tif"), "SpatialGridDataFrame")
bd.pop.dens.grid <- as(raster("bd_pop_dens.tif"), "SpatialGridDataFrame")

bd.predictors.grid <- bd.elv.grid

bd.predictors.grid@data <- data.frame(bd.predictors.grid@data, bd.soc.grid@data,
	bd.scc.grid@data, bd.ph.grid@data, bd.wc.grid@data, bd.pop.dens.grid@data,
	coordinates(bd.predictors.grid))
	
head(bd.predictors.grid@data)

colnames(bd.heavy.metal@data)

colnames(bd.predictors.grid@data) <- c("elv", "soc", "scc", "ph", "wc", "popdens",
	"LONG_DEG", "LAT_DEG")

spplot(bd.predictors.grid[1:6])

## Adjusting projection systems

proj4string(bd.heavy.metal) <- CRS(proj4string(bd.predictors.grid))

plot(bd.heavy.metal)


## Weighted least square prediction

krg_wls <- krige(log(As)~scc + ph + wc + LONG_DEG,
bd.heavy.metal, newdata=bd.predictors.grid)

head(krg_wls@data)

krg_wls@data$var1.pred <- exp(krg_wls@data$var1.pred)

spplot(krg_wls["var1.pred"], col.regions=colorRampPalette(c("blue", "green", "yellow",
	"orange", "red"))(1000), sp.layout=list("sp.polygons", bd.boundary),
	colorkey=T, xlab="Logitude", ylab="Latitude", scales=list(draw=T))

## Universal Kriging or Kriging with external drift

krg_As <- krige(log(As) ~ scc + ph + wc + LONG_DEG,
bd.heavy.metal, newdata=bd.predictors.grid, model=vmod.As.sph)

krg_As@data$var1.pred <- exp(krg_As@data$var1.pred)

spplot(krg_As["var1.pred"], col.regions=colorRampPalette(c("blue", "green", "yellow",
	"orange", "red"))(1000), sp.layout=list("sp.polygons", bd.boundary),
	colorkey=T, xlab="Logitude", ylab="Latitude", scales=list(draw=T))


## Cross validation

Cross_val_wls <- krige.cv(log(As)~scc + ph + wc + LONG_DEG, bd.heavy.metal)
head(Cross_val_wls@data)

Cross_val_krg <- krige.cv(log(As)~scc + ph + wc + LONG_DEG, bd.heavy.metal,
model=vmod.As.sph)

library(hydroGOF)

#Root mean squarred error (RMSE)

rmse(Cross_val_wls$var1.pred, Cross_val_wls$observed)
rmse(Cross_val_krg$var1.pred, Cross_val_krg$observed)

# Index of agreement
d(Cross_val_wls$var1.pred, Cross_val_wls$observed)
d(Cross_val_krg$var1.pred, Cross_val_krg$observed)

## Calculate the hazard quotient (HQ) values by comparing to
## the thresholds for drinking water, and acute and chronic risk

krg_As@data$hq <- krg_As@data$var1.pred/7.2
	
spplot(krg_As["hq"], col.regions=colorRampPalette(c("blue", "green", "yellow",
	"orange", "red"))(1000), sp.layout=list("sp.polygons", bd.boundary),
	colorkey=T, xlab="Logitude", ylab="Latitude", scales=list(draw=T))

################################# End Day 1 #################################
#############################################################################


##########################################################################################
##										Day 2			 								##
##									March 21, 2017										##
##########################################################################################

## Project Work

################################# End #######################################
#############################################################################
#############################################################################
