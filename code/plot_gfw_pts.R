library(sf)
library(ggplot2)

main_dir <- "/Users/robert.ahrens/Documents/projects/fkw_foreign_ll_interactions/"
data_dir <- "/Users/robert.ahrens/Documents/data"

coast<-st_read(file.pat(data_dir, "gis", "mhi", "Coastline.shp"))
sez<-st_read(file.pat(data_dir, "gis", "SEZ_WGS84", "SEZ_WGS84.shp"))
triangle<-st_read(file.pat(data_dir, "gis", "Triangle", "fkw_triangle.shp"))
eez<-st_read(file.pat(data_dir, "gis", "USMaritimeLimitsAndBoundariesSHP", "USMaritimeLimitsNBoundaries.shp"))

eez<-eez[which(eez$REGION=="Hawaiian Islands"),]
fkw_bdry <- st_read("/Users/robert.ahrens/Documents/Data/GIS/fkw_boundary/pFKW_MgmtArea_line.shp")
#transform fkw_bdry to LL
old_crs <- st_crs(fkw_bdry)
fkw_bdry <- st_transform(fkw_bdry, st_crs(sez))

xmin <- 175
xmax <- 360 - 130
ymin <- 10
ymax <- 35
#These are polylines not polygons so convert to polygon
eez_poly <- st_polygonize(st_shift_longitude(eez))
fkw_bdry_poly <- st_polygonize(st_shift_longitude(fkw_bdry))
pts_bbox <- data.frame("lon" = c(xmin, xmin, xmax, xmax), "lat" = c(ymin, ymax, ymax, ymin))
pts_bbox <- st_as_sf(pts_bbox, coords = c("lon", "lat"), crs = st_crs(sez))
grid5x5 <- st_make_grid(pts_bbox, cellsize = 5, what = "polygons", square = TRUE)
load("/Users/robert.ahrens/Documents/Projects/FKW/foreign/gfw_sf_pts_fkw.Rdata")

clr <- c(rgb(204 / 255, 85 / 255, 0 / 255, 0.2),
 rgb(242 / 255, 140 / 255, 40 / 255, 0.2),
 rgb(45 / 255, 114 / 255, 178 / 255, 0.2),
 rgb(128 / 255, 166 / 255, 219 / 255, 0.2))

#nice plot of the GFW effort distribution
png(file = "/Users/robert.ahrens/Documents/Projects/FKW/foreign/combined_effort.png", width = 9.4, height = 6, units = "in", res = 300)
    plot(st_geometry(pts[which(pts$flag != "USA" & pts$in_fkw == 0),]), col = clr[1], pch = ".", xlab = "Longitude", ylab = "Latitude")
    plot(st_geometry(pts[which(pts$flag != "USA" & pts$in_fkw == 1),]), col = clr[2], pch = ".", add = TRUE)
    plot(st_geometry(pts[which(pts$flag == "USA" & pts$in_fkw == 0),]), col = clr[3], pch = ".", add = TRUE)
    plot(st_geometry(pts[which(pts$flag == "USA" & pts$in_fkw == 1),]), col = clr[4], pch = ".", add = TRUE)
    plot(st_geometry(grid5x5), border = "grey30", add = TRUE)
    plot(st_geometry(st_shift_longitude(eez[8,])), border = "black", add = TRUE)
    plot(st_geometry(st_shift_longitude(coast)), col = "black", add = TRUE)
#mtext("US (blue) and non-US (orange)", cex = 1.5, side = 3, line = -0.5, adj = 0.5)
axis(1,at = c(180,190,200,210,220,230), label = c("180","-170","-160","-150","-140","-130"))
axis(2)
dev.off()

