library(sf)
library(tidyverse)
library(terra)
library(stars)

main_dir <- "/Users/robert.ahrens/Documents/projects/fkw_foreign_ll_interactions/"
data_dir <- "/Users/robert.ahrens/Documents/data"


load(file.path(main_dir, "output", "effort_match.Rdata"))

abund <- read_csv(file.path("main_dir", "data", "Pc_2017Dens_3day_5.5_noOOB.csv")[,c("mlat", "mlon", "2017Avg.Dens")]
abund$id <- 1:nrow(abund)

r <- abund %>% select(mlon, mlat, "2017Avg.Dens") %>% rename(x = mlon, y = mlat, z = "2017Avg.Dens") # Pull off locations and pixel IDs
r <- r %>% rast(type='xyz', crs="EPSG:4326") %>%  st_as_stars(r)
r_bb <- st_bbox(r)


xmin <- 175
xmax <- 360 - 130
ymin <- 10
ymax <- 35
fkw_bdry <- st_read(file.path(data_dir, "gis", "fkw_boundary", "pFKW_MgmtArea_line.shp"))
fkw_bdry <- st_transform(fkw_bdry, st_crs(r))
#These are polylines not polygons so convert to polygon
fkw_bdry_poly <- st_polygonize(st_shift_longitude(fkw_bdry))
pts_bbox <- data.frame("lon" = c(xmin, xmin, xmax, xmax), "lat" = c(ymin, ymax, ymax, ymin))
pts_bbox <- st_as_sf(pts_bbox, coords = c("lon", "lat"), crs = st_crs(r))
grid5x5 <- st_as_sf(st_make_grid(pts_bbox, cellsize = 5, what = "polygons", square = TRUE))
grid5x5$ID<-1:dim(grid5x5)[1]
extract <- st_shift_longitude(st_intersection(fkw_bdry_poly,grid5x5))
tmp <- st_as_sf(st_extract(r, extract, FUN = function(x) mean(x,na.rm = TRUE)))
den_lookup <- data.frame("pid" = extract$ID, "mden" = tmp$z)

save(den_lookup , file = file.path(main_dir, "output", "den_lookup.Rdata")


#find proportioneffort_in of non-US effort in a particular area
ii <- which(effort_match$flag == 5)
nonus <- aggregate(effort_in~area,data = effort_match[-ii,],sum)
us <- aggregate(effort_in~area,data = effort_match[ii,],sum)

nonus$den <-den_lookup$mden[match(nonus$area,den_lookup$pid)]
us$den <-den_lookup$mden[match(us$area,den_lookup$pid)]

nonus <-nonus[-which(is.na(nonus$den)),]
us <-us[-which(is.na(us$den)),]
nonus$peff <- nonus$effort_in/sum(nonus$effort_in)
us$peff <- us$effort_in/sum(us$effort_in)

sum(nonus$peff*nonus$den,na.rm = TRUE)
sum(us$peff*us$den,na.rm = TRUE)

