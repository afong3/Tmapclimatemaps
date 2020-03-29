###tmap plotting###
###All these maps MUST be projected in lat/long, WGS84 to plot basemaps and hillshades for this CDEM###

library(tmap)
library(tmaptools)
library(sp)
library(raster)


utm11 <-  "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs"
latlong <- "+proj=longlat +datum=WGS84 +no_defs"

##RCP 2.6 Predictions##
Tmax2019_26 <- raster("bcvin_raster/2019S_26/Tmax_sm.asc")
Tmax2100_26 <- raster("bcvin_raster/2100S_26/Tmax_sm.asc")
crs(Tmax2019_26) <- latlong
crs(Tmax2100_26) <- latlong
plot_2019 <- overlay(Tmax2019_26,
                     fun=function(r1){return(r1/10)})
plot_2100 <- overlay(Tmax2100_26,
                     fun=function(r1){return(r1/10)})
##Normalized data between 1981 and 2010##

control <- raster("bcvin_raster/Normal_1981_2010S/Tmax_sm.asc")

##RCP 2.6 Predictions##
Tmax2019_26 <- raster("bcvin_raster/2019S_26/Tmax_sm.asc")
Tmax2050_26 <- raster("bcvin_raster/2050S_26/Tmax_sm.asc")
Tmax2100_26 <- raster("bcvin_raster/2100S_26/Tmax_sm.asc")

##RCP 4.5 Predictions##
Tmax2050_45 <- raster("bcvin_raster/2050S_45/Tmax_sm.asc")
Tmax2100_45 <- raster("bcvin_raster/2100S_45/Tmax_sm.asc")

##RCP 8.5 Predictions##
Tmax2019_85 <- raster("bcvin_raster/2019S_85/Tmax_sm.asc")
Tmax2040_85 <- raster("bcvin_raster/2040S_85/Tmax_sm.asc")
Tmax2050_85 <- raster("bcvin_raster/2050S_85/Tmax_sm.asc")
Tmax2060_85 <- raster("bcvin_raster/2060S_85/Tmax_sm.asc")
Tmax2080_85 <- raster("bcvin_raster/2080S_85/Tmax_sm.asc")
Tmax2100_85 <- raster("bcvin_raster/2100S_85/Tmax_sm.asc")



crs(Tmax2019_85) <- latlong
crs(Tmax2040_85) <- latlong
crs(Tmax2050_85) <- latlong
crs(Tmax2060_85) <- latlong
crs(Tmax2080_85) <- latlong
crs(Tmax2100_85) <- latlong
crs(Tmax2050_26) <- latlong
crs(Tmax2100_26) <- latlong
crs(Tmax2050_45) <- latlong
crs(Tmax2100_45) <- latlong


PPT_sm2019 <- raster("bcvin_raster/2019S_85/PPT_sm.asc")
PPT_sm2050 <- raster("bcvin_raster/2050S_85/PPT_sm.asc")
PPT_sm2100 <- raster("bcvin_raster/2100S_85/PPT_sm.asc")
PPT_w2100 <- raster("bcvin_raster/2100S_85/PPT_wt.asc")
ppt_control <- raster("bcvin_raster/Normal_1981_2010S/PPT_sm.asc")

crs(PPT_sm2019) <- latlong
crs(PPT_sm2050) <- latlong
crs(PPT_sm2100) <- latlong
crs(control) <- latlong

##The output data from ClimateBC is increased by a factor of 10
##The function reverts the scale from 10x(degree C) to (degree C)
norm2050_26 <- overlay(control, Tmax2050_26, fun = function(r1,r2){return((r2-r1)/10)})
norm2100_26 <- overlay(control, Tmax2100_26, fun = function(r1,r2){return((r2-r1)/10)})

norm2050_45 <- overlay(control, Tmax2050_45, fun = function(r1,r2){return((r2-r1)/10)})
norm2100_45 <- overlay(control, Tmax2100_45, fun = function(r1,r2){return((r2-r1)/10)})


norm2019_85 <- overlay(control, Tmax2019_85, fun = function(r1,r2){return((r2-r1)/10)})
norm2040_85 <- overlay(control, Tmax2040_85, fun = function(r1,r2){return((r2-r1)/10)})
norm2050_85 <- overlay(control, Tmax2050_85, fun = function(r1,r2){return((r2-r1)/10)})
norm2060_85 <- overlay(control, Tmax2060_85, fun = function(r1,r2){return((r2-r1)/10)})
norm2080_85 <- overlay(control, Tmax2080_85, fun = function(r1,r2){return((r2-r1)/10)})
norm2100_85 <- overlay(control, Tmax2100_85, fun = function(r1,r2){return((r2-r1)/10)})

controlplot <- overlay(control, fun = function(r1){return(r1/10)})
crs(controlplot) <- latlong

Tmax2040_85_plot <- overlay(Tmax2040_85, fun = function(r1){return(r1/10)})
Tmax2060_85_plot <- overlay(Tmax2060_85, fun = function(r1){return(r1/10)})
Tmax2080_85_plot <- overlay(Tmax2080_85, fun = function(r1){return(r1/10)})
Tmax2100_85_plot <- overlay(Tmax2100_85, fun = function(r1){return(r1/10)})

crs(Tmax2040_85_plot) <- latlong
crs(Tmax2060_85_plot) <- latlong
crs(Tmax2080_85_plot) <- latlong
crs(Tmax2100_85_plot) <- latlong


crs(Tmax2040_85_plot) <- latlong
crs(Tmax2060_85_plot) <- latlong
crs(Tmax2080_85_plot) <- latlong
crs(Tmax2100_85_plot) <- latlong

###Hillshade Backgrounds###

#1 Creating from .asc file
CDEM <- raster("CDEM.asc")
crs(CDEM) <- latlong
slope <- terrain(CDEM,opt="slope")
aspect <- terrain(CDEM, opt="aspect")
hillshade <- hillShade(slope, aspect, 40, 270)

CDEM_WGS <- raster("BCvinWGS84_20m/CDEM.asc")
slope2 <- terrain(CDEM_WGS, opt = "slope")
aspect2 <- terrain(CDEM_WGS, opt = "aspect")
hs_WGS_sa <- hillShade(slope2, aspect2, 40, 270)

#2 Using the CDEM hillshade file 
hs_dl_NAD <- raster("BCvinNAD83_75arc/HILLSHADE.tif")
hs_dl_WGS <- raster("BCvinWGS84_20m/HILLSHADE.tif")

tmap_mode("plot")+
tm_shape(hs_dl_NAD)+
  tm_raster(palette = grey(0:100/100))

tmap_mode("plot")+
  tm_shape(hs_dl_WGS)+
  tm_raster(palette = grey(0:100/100))+

tmap_mode("plot")+
  tm_shape(hs_WGS_sa)+
  tm_raster(palette = grey(0:100/100), legend.show = FALSE)

#Different Basemaps for Same raster files#####
####I think these two are the best two available but there are a lot more at  https://leaflet-extras.github.io/leaflet-providers/preview/####
### if in tmap_mode("view") this shows an interactive map with these variables with capabilities to toggle on and off###

#
toggle_temps_topo <- tmap_mode("view")+
  tm_basemap("Esri.WorldTopoMap") +
  tm_shape(Tmax2040_85_plot)+
  tm_raster(alpha = .5, title = "2040 (Degrees C)") +
  tm_shape(Tmax2060_85_plot) +
  tm_raster(alpha = .5, title = '2060 (Degrees C)')+
  tm_shape(Tmax2080_85_plot) + 
  tm_raster(alpha = .5, title = '2080 (Degrees C)') + 
  tm_shape(Tmax2100_85_plot) +
  tm_raster(alpha = .5, title = "2100 (Degrees C)") +
  tm_scale_bar() +
  tm_minimap() + 
  tm_layout(title = "Mean Maximum Summer Temperatures (C)")



small_breaks <- tmap_mode("view")+
  tm_basemap("Esri.WorldTopoMap") +
  tm_shape(Tmax2100_85_plot) +
  tm_raster(alpha = .7, title = "Degrees C", 
            breaks = c(35, 35.5, 36, 36.5, 37, 37.5, 38, 38.5, 39, 39.5, 40, 40.5, 41, 41.5, 42, 42.5),
            palette = get_brewer_pal("Oranges", n = 16 )) +
  tm_scale_bar() +
  tm_minimap()+
  tm_layout(title = "RCP 8.5: 2100 Mean Max Summer Temperatures")

small_stack <- stack(Tmax2060_85_plot, Tmax2080_85_plot, Tmax2100_85_plot)

small_breaks_facet <- tmap_mode("view")+
  tm_basemap("Esri.WorldTopoMap")+
  tm_shape(small_stack)+
  tm_raster(title = "Degrees C", style = "cont", alpha = .7)+
  tm_facets(free.scales = FALSE)+
  tm_layout(panel.labels = c("2060", "2080", "2100"), main.title = "RCP 8.5: Mean Maximum Summer Temperature", compass.type = "arrow", attr.outside = TRUE, legend.outside = TRUE, legend.outside.position = "right", frame = FALSE)

toggle_temps <- tmap_mode("view")+
  tm_basemap("Esri.WorldImagery") +
  tm_shape(Tmax2040_85_plot)+
  tm_raster(alpha = .5, title = "2040 (Degrees C)") +
  tm_shape(Tmax2060_85_plot) +
  tm_raster(alpha = .5, title = '2060 (Degrees C)')+
  tm_shape(Tmax2080_85_plot) + 
  tm_raster(alpha = .5, title = '2080 (Degrees C)') + 
  tm_shape(Tmax2100_85_plot) +
  tm_raster(alpha = .5, title = "2100 (Degrees C)") +
  tm_scale_bar() +
  tm_minimap() + 
  tm_layout(title = "Mean Maximum Summer Temperatures (C)")


tm_basemap("Esri.WorldTerrain") +
  tm_shape(Tmax2040_85_plot)+
  tm_raster(alpha = .5, title = "2040") +
  tm_shape(Tmax2060_85_plot) +
  tm_raster(alpha = .5, title = '2060')+
  tm_shape(Tmax2080_85_plot) + 
  tm_raster(alpha = .5, title = '2080') + 
  tm_shape(Tmax2100_85_plot) +
  tm_raster(alpha = .5, title = "2100") +
  tm_scale_bar() +
  tm_minimap()+
  tm_layout(title = "Mean Maximum Summer Temperatures (C)")
  
tm_basemap("Esri.WorldTopoMap") +
  tm_shape(controlplot)+
  tm_raster(alpha = .7, title = "Max Summer Temps")+
  tm_scale_bar() +
  tm_minimap()+
  tm_layout(title = "Normalized 1980-2010")


##comparing RCP datasets (normalized)##



RCP_mult <- stack(norm2050_26, norm2100_26, norm2050_45, norm2100_45, norm2050_85, norm2100_85)
crs(RCP_mult) <- latlong
Tmaxbreaks <- seq(0,15, by = 1)

RCPpalette <- get_brewer_pal("Oranges", n = length(Tmaxbreaks))

bbBCvin <- bb(norm2100_85)

static_facet <- tmap_mode("plot")+
  tm_shape(hillshade, bbox = bbBCvin) +
  tm_raster(palette = grey(0:100/100), legend.show = FALSE, alpha = .8)+
  tm_shape(RCP_mult) +
  tm_raster(breaks = Tmaxbreaks, title = "Temperature (degrees C)", palette = RCPpalette, style = "cont")+
  tm_facets(free.scales = FALSE, ncol = 2) + 
  tm_layout(panel.labels = c("2050: RCP 2.6", "2100: RCP 2.6", "2050: RCP 4.5", "2100: RCP 4.5", "2050: RCP 8.5", "2100: RCP 8.5"), main.title = "Increase in Mean Maximum Summer Temperature", compass.type = "arrow", attr.outside = TRUE, legend.outside = TRUE, legend.outside.position = "right", frame = FALSE)+
  tm_scale_bar(position = c("RIGHT", "TOP"))+
  tm_compass(position = c("LEFT", "TOP"))

tm_shape(hillshade) +
  tm_raster(palette = grey(0:50/50), legend.show = FALSE, alpha = .8)+
  tm_shape(RCP_mult) +
  tm_raster(breaks = Tmaxbreaks, title = "Temperature (degrees C)", style = "cont")+
  tm_facets(free.scales = FALSE, ncol = 2) + 
  tm_layout(panel.labels = c("2050: RCP 2.6", "2100: RCP 2.6", "2050: RCP 4.5", "2100: RCP 4.5", "2050: RCP 8.5", "2100: RCP 8.5"), main.title = "Increase in Mean Maximum Summer Temperature", compass.type = "arrow", attr.outside = TRUE, legend.outside = TRUE, legend.outside.position = "right", frame = FALSE)+
  tm_scale_bar(position = c("RIGHT", "TOP"))+
  tm_compass(position = c("LEFT", "TOP"))

#######
#######Precipitation

#for hillshade basemap
CDEM <- raster("CDEM.asc")
crs(CDEM) <- latlong
slope <- terrain(CDEM,opt="slope")
aspect <- terrain(CDEM, opt="aspect")
hillshade <- hillShade(slope, aspect, 40, 270)


ppt <- stack(ppt_control, PPT_sm2050, PPT_sm2100)

PPTbreaks <- seq(0,160, by = 5)

PPTpalette <- get_brewer_pal("Blues", n = length(PPTbreaks))

bbBCvin <- bb(PPT_sm2019)


#style = "order" makes it easier to see skewed data and large ranges of data
  
tmap_mode("view")
tm_basemap("Esri.WorldImagery")+
  tm_shape(ppt) +
  tm_raster(title = "Precipitation (mm)", palette = PPTpalette, style = "cont")+
  tm_facets(free.scales = FALSE) + 
  tm_layout(panel.labels = c("2019", "2050", "2100"), legend.show = FALSE, frame = FALSE)+
  tm_scale_bar(position = c("RIGHT", "TOP"))
  
  

#without hillshade background 
tmap_mode("plot")
tm_shape(ppt) +
  tm_raster(breaks = PPTbreaks, title = "Precipitation (mm)", palette = PPTpalette, style = "cont")+
  tm_facets(free.scales = FALSE) + 
  tm_layout(panel.labels = c("1980-2010", "2050", "2100"), main.title = "Summer Precipitation RCP 8.5", compass.type = "arrow", attr.outside = TRUE, legend.outside = TRUE, legend.outside.position = "right", frame = FALSE)+
  tm_scale_bar(position = c("RIGHT", "TOP"))+
  tm_compass(position = c("LEFT", "TOP"))

tm_shape(ppt) +
  tm_raster(title = "Precipitation (mm) [cont]", palette = PPTpalette, style = "cont")+
  tm_facets(free.scales = FALSE) + 
  tm_layout(panel.labels = c("1980-2010", "2050", "2100"), main.title = "Summer Precipitation RCP 8.5", compass.type = "arrow", attr.outside = TRUE, legend.outside = TRUE, legend.outside.position = "right", frame = FALSE)+
  tm_scale_bar(position = c("RIGHT", "TOP"))+
  tm_compass(position = c("LEFT", "TOP"))

tmap_mode("plot")
tm_shape(hillshade, bbox = bbBCvin)+
  tm_raster(palette = grey(0:100/100), legend.show = FALSE, alpha = .8)+
  tm_shape(ppt) +
  tm_raster(breaks = PPTbreaks, title = "Precipitation (mm)", palette = PPTpalette, style = "cont")+
  tm_facets(free.scales = FALSE, ncol = 3) + 
  tm_layout(panel.labels = c("1980-2010", "2050", "2100"), main.title = "Summer Precipitation RCP 8.5", compass.type = "arrow", attr.outside = TRUE, legend.outside = TRUE, legend.outside.position = "right", frame = FALSE)+
  tm_scale_bar(position = c("RIGHT", "TOP"))+
  tm_compass(position = c("LEFT", "TOP"))

tmap_mode("plot")
tm_shape(hillshade, bbox = bbBCvin)+
  tm_raster(palette = grey(0:100/100), legend.show = FALSE, alpha = .8)+
  tm_shape(ppt) +
  tm_raster(breaks = PPTbreaks, title = "Precipitation (mm) [order]", palette = PPTpalette, style = "order")+
  tm_facets(free.scales = FALSE, ncol = 3) + 
  tm_layout(panel.labels = c("1980-2010", "2050", "2100"), main.title = "Summer Precipitation RCP 8.5", compass.type = "arrow", attr.outside = TRUE, legend.outside = TRUE, legend.outside.position = "right", frame = FALSE)+
  tm_scale_bar(position = c("RIGHT", "TOP"))+
  tm_compass(position = c("LEFT", "TOP"))

#######End Precipitation
#######

######Summer Tmax RCP 8.5


CDEM <- raster("CDEM.asc")
crs(CDEM) <- latlong
slope <- terrain(CDEM,opt="slope")
aspect <- terrain(CDEM, opt="aspect")
hillshade <- hillShade(slope, aspect, 40, 270)

stack85 <- stack(controlplot, Tmax2040_85_plot, Tmax2060_85_plot, Tmax2080_85_plot, Tmax2100_85_plot)
crs(stack85) <- latlong
stack85breaks <- seq(20, 45, by = 5)

RCPpalette <- get_brewer_pal("Oranges", n = length(stack85breaks))

bbBCvin <- bb(norm2100_85)

tm_shape(hillshade) +
  tm_raster(palette = grey(0:100/100), legend.show = FALSE, alpha = .8)+
  tm_shape(stack85) +
  tm_raster(breaks = stack85breaks, title = "Temperature (degrees C) [cont]", style = "cont")+
  tm_facets(free.scales = FALSE, ncol = 5) + 
  tm_layout(panel.labels = c("Average 1980-2010", "2040", "2060", "2080", "2100"), main.title = "Mean Maximum Summer Temperature (RCP 8.5)", compass.type = "arrow", attr.outside = TRUE, legend.outside = TRUE, legend.outside.position = "right", frame = FALSE)+
  tm_scale_bar(position = c("RIGHT", "TOP"),text.size = .9)+
  tm_compass(position = c("LEFT", "TOP"))

tm_shape(hillshade) +
  tm_raster(palette = grey(0:100/100), legend.show = FALSE, alpha = .8)+
  tm_shape(stack85) +
  tm_raster( title = "Temperature (degrees C) [order]", style = "order")+
  tm_facets(free.scales = FALSE, ncol = 5) + 
  tm_layout(panel.labels = c("Average 1980-2010", "2040", "2060", "2080", "2100"), main.title = "Mean Maximum Summer Temperature (RCP 8.5)", compass.type = "arrow", attr.outside = TRUE, legend.outside = TRUE, legend.outside.position = "right", frame = FALSE)+
  tm_scale_bar(position = c("RIGHT", "TOP"),text.size = .9)+
  tm_compass(position = c("LEFT", "TOP"))
  
##Solo Control Plots##

tm_shape(hillshade) +
  tm_raster(palette = grey(0:100/100), legend.show = FALSE, alpha = .8)+
  tm_shape(controlplot, alpha = .7) +
  tm_raster(title = "Temperature (degrees C)", style = 'cont')+
  tm_layout(main.title = "Normalized Mean Maximum Summer Temperatures (1980-2010)", legend.outside = TRUE, frame = FALSE)

tm_shape(hillshade) +
  tm_raster(palette = grey(0:100/100), legend.show = FALSE, alpha = .8)+
  tm_shape(ppt_control, alpha = .7) +
  tm_raster(title = "Precipitation (mm)", style = 'cont', palette = get_brewer_pal("Blues"))+
  tm_layout(main.title = "Normalized Mean Summer Preciptiation (1980-2010)", legend.outside = TRUE, frame = FALSE)

qtm(controlplot, title = "Normal Mean Maximum Summer Temperatures 1980-2010")

