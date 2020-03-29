library(tmap)
library(tmaptools)
library(raster)
library(rgdal)
library(rgeos)

#rgdal & rgeos are dependencies of tmap
 ###
# 1 # 
 ###
##load Napa Data attained from USGS Earth Explorer

napaDEM <- raster("Screenshots for Data Collection/napa_1arc.tif")

#quick plot to have an idea of what the raw tif file looks like#
#both are indecipherable#
qtm(napaDEM)
plot(napaDEM)

 ###
# 2 #
 ###

#convert this data to a hillshade#
crs(napaDEM)
slope <- terrain(napaDEM, opt="slope")
aspect <- terrain(napaDEM, opt="aspect")
hillshade <- hillShade(slope, aspect, 40, 270)

#plot hillshade with grey palette and no legend to see difference in "plot" mode
tmap_mode("plot")+
tm_shape(hillshade)+
  tm_raster(palette = grey(0:100/100), legend.show = FALSE) 

######Now you have your basemap for any plots of this region

 ###
# 3 #
 ###

#plot napashp on top of DEM from https://geodata.lib.berkeley.edu/catalog/ark28722-s7x88m

napashp <- readOGR(dsn = "Screenshots for Data Collection/Napa County SHP WGS84", layer = "s7x88m")

tmap_mode("plot")+
  tm_shape(hillshade)+
  tm_raster(palette = grey(0:100/100), legend.show = FALSE)+ 
  tm_shape(napashp)+
  tm_polygons(alpha = 0, lwd = 5, border.col = "red")

##cut DEM into shape of polygon and put into ClimateBC##

napacrop <- crop(napaDEM, napashp)## if you plot this it plots the extent of the polygon (rectangle)
napamask <- mask(napacrop, napashp)## the mask is used to use the outline of the actualy polygon shapefile
writeRaster(napamask, "manualnapaDEM", format = "ascii")
##ClimateNA only accepts csv & asc file types, this is why I chose "ascii" format
##Run this new asc file through ClimateNA (it took my computer ~1.5 hours; 
###just let it run in the background in the morning if you have enough computing power to continue through the day

##check if the data is correct
customnapaDEM <- raster("manualnapaDEM.asc")
tm_shape(manualnapaDEM)+
  tm_raster()

 ###
# 4 #
 ###

## plot the climate projections

Tmaxnapa <- raster("manualnapaDEM/2100S/Tmax_sm.asc")
PPTnapa <- raster("manualnapaDEM/2100S/PPT_sm.asc")
bcvin <- raster("bcvin_raster.asc")

#if you plot Tmaxnapa as it is now, you will notice the data values are on the order of 10E2
#the model computes the data and scales this by a factor of 10
#create an overlay of ther raster data to downscale these data 

Tmax_plot <- overlay(Tmaxnapa, fun = function(r1){r1/10})

#now you can plot the Summer Temperature data accurately
#can plot 

napahsplot <- tmap_mode("plot")+
  tm_shape(hillshade)+
  tm_raster(palette = grey(0:100/100), legend.show = FALSE)+
  tm_shape(Tmax_plot)+
  tm_raster(title = "Temperature (degrees C)", style = 'cont', alpha = .7)+
  tm_layout(main.title = "RCP 8.5: 2100 Predicted Mean Maximum Summer Temperature", legend.outside = TRUE, frame = FALSE)

napatopoview <- tmap_mode("view")+
  tm_basemap("Esri.WorldTopoMap")+
  tm_shape(Tmax_plot)+
  tm_raster(title = "Temperature (degrees C)", style = 'cont', alpha = .7)+
  tm_layout(title = "RCP 8.5: 2100 Predicted Mean Maximum Summer Temperature", legend.outside = TRUE, frame = FALSE)

 ###
# 5 #
 ###

#Save your work to your system.

tmap_save(napahsplot, filename = "napaTmax_hs_plot.png")
tmap_save(napatopoview, filename = "napaTmax_topo_view.html")

####END####


