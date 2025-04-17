
library(lubridate)
library(sf)


indx <- c(1:12)
iso2 <- c("JP", "KR", "TW", "VU", "CN", "SB", "PG", "FJ", "KI", "SM", "US", "")
iso3 <- c("JPN", "KOR", "TWN", "VUT", "CHN", "SLB", "PNG", "FJI", "KIR", "SMR", "USA", "")
flag <- data.frame("indx" = indx, "iso2" = iso2, "iso3" = iso3)

sez<-st_read("/Users/robert.ahrens/Documents/Data/GIS/SEZ_WGS84/SEZ_WGS84.shp")
triangle<-st_read("/Users/robert.ahrens/Documents/Data/GIS/Triangle/fkw_triangle.shp")
eez<-st_read("/Users/robert.ahrens/Documents/Data/GIS/USMaritimeLimitsAndBoundariesSHP/USMaritimeLimitsNBoundaries.shp")
eez<-eez[which(eez$REGION=="Hawaiian Islands"),]
fkw_bdry <- st_read("/Users/robert.ahrens/Documents/Data/GIS/fkw_boundary/pFKW_MgmtArea_line.shp")
#transform fkw_bdry to LL
old_crs <- st_crs(fkw_bdry)
fkw_bdry <- st_transform(fkw_bdry, st_crs(sez))

#These are polylines not polygons so convert to polygon
eez_poly <- st_polygonize(st_shift_longitude(eez))
fkw_bdry_poly <- st_polygonize(st_shift_longitude(fkw_bdry))
xmin <- -180
xmax <- -130
ymin <- 10
ymax <- 35
pts_bbox <- data.frame("lon" = c(175, 175, 230, 230), "lat" = c(10, 35, 35, 10))
pts_bbox <- st_as_sf(pts_bbox, coords = c("lon", "lat"), crs = st_crs(sez))
grid5x5 <- st_make_grid(pts_bbox, cellsize = 5, what = "polygons", square = TRUE)

flag_use <- c("CHN", "JPN", "KOR", "TWN", "USA", "VUT")
nflags <- length(flag_use)
nyrs <- 12
ncol <- 11
nrow <- 5
nareas <- ncol * nrow
xmin <- 175
xmax <- 360 - 130
ymin <- 10
ymax <- 35
prop_areas <- array(0,dim = c(nyrs, nareas, nflags, 2))

wcpfc_ll <- read.csv("/Users/robert.ahrens/Documents/data/RFMO_Data/WCPFC/WCPFC_L_PUBLIC_BY_FLAG_MON_8/WCPFC_L_PUBLIC_BY_FLAG_MON.CSV")
wcpfc_ll <- wcpfc_ll[, c("yy", "mm", "flag_id", "lat_short", "lon_short", "cwp_grid", "hhooks", "alb_n", "yft_n", "bet_n", "swo_n")]
wcpfc_ll <- wcpfc_ll[which(wcpfc_ll$yy >= 2012 & wcpfc_ll$yy <= 2023),]
wcpfc_ll <- wcpfc_ll[which(substr(wcpfc_ll$lat_short, 3, 3) == "N"),]

wcpfc_ll$lat <- as.numeric(substr(wcpfc_ll$lat_short, 1, 2)) + 2.5
wcpfc_ll$lon <- as.numeric(substr(wcpfc_ll$lon_short, 1, 3))
ii <- which(substr(wcpfc_ll$lon_short, 4, 4) == "W")
wcpfc_ll$lon[ii] <- 360 - wcpfc_ll$lon[ii] + 2.5
wcpfc_ll$lon[-ii] <- wcpfc_ll$lon[-ii] + 2.5
# filter to area of interest
wcpfc_ll <- with(wcpfc_ll,wcpfc_ll[which(lat >= ymin & lat < ymax & lon >= xmin & lon < xmax),])
ccol <- floor((wcpfc_ll$lon - 2.5 - 175) / 5) + 1
crow <- floor((wcpfc_ll$lat - 2.5 - 10) / 5) + 1
wcpfc_ll$ref55 <- (crow - 1) * ncol + ccol
#filter down to only iso2 <- c("JP", "KR", "TW", "VU", "CN", "SB", "PG", "FJ", "KI", "SM", "US", "") 
wcpfc_ll$ttuna <- rowSums(wcpfc_ll[,c("alb_n", "yft_n", "bet_n")])
wcpfc_ll$tot <- rowSums(wcpfc_ll[,c("alb_n", "yft_n", "bet_n", "swo_n")])
wcpfc_ll <- wcpfc_ll[which(wcpfc_ll$flag_id %in% c("CN", "JP", "KR", "TW", "US", "VU")),]
wcpfc_ll$hooks <- wcpfc_ll$hhooks * 100

wcpo_area_prop <- aggregate(cbind(ttuna,tot) ~ flag_id + yy + ref55, data = wcpfc_ll, sum)
wcpo_area_prop$flag_ind <- match(wcpo_area_prop$flag_id, c("CN", "JP", "KR", "TW", "US", "VU"))
#flag_id   yy ref55 ttuna   tot flag_ind

for(i in seq_along(wcpo_area_prop$flag_ind )){
   yr <- wcpo_area_prop$yy[i] - 2011
   cc <- wcpo_area_prop$ref55[i]
   flg <- wcpo_area_prop$flag_ind[i]
   tot <- wcpo_area_prop$tot[i]
   ttuna <- wcpo_area_prop$ttuna[i]
   prop_areas[yr, cc, flg, 1] <- prop_areas[yr, cc, flg, 1] + ttuna
   prop_areas[yr, cc, flg, 2] <- prop_areas[yr, cc, flg, 2] + tot
}

iattc_ll <- read.csv("/Users/robert.ahrens/Documents/Data/RFMO_Data/IATTC/PublicLLTunaBillfish/PublicLLTunaBillfishNum.csv")
iattc_ll <- iattc_ll[, c("Year", "Flag", "LatC5", "LonC5", "Hooks", "ALBn", "BETn", "YFTn", "SWOn")]
iattc_ll <- iattc_ll[which(iattc_ll$Year >= 2012 & iattc_ll$Year <= 2023), ]
iattc_ll <- iattc_ll[which(iattc_ll$Flag %in% c("CHN", "JPN", "KOR", "TWN", "USA", "VUT")), ]
iattc_ll$lat <- iattc_ll$LatC5
iattc_ll$lon <- 360 + iattc_ll$LonC5
iattc_ll <- iattc_ll[which(iattc_ll$LatC5 >= 10 & iattc_ll$LatC5 <= 35), ]
iattc_ll <- iattc_ll[which(iattc_ll$LonC5 > -150 & iattc_ll$LonC5 < -130), ]
ccol <- floor((iattc_ll$lon - 2.5  - 175) / 5) + 1
crow <- floor((iattc_ll$lat - 2.5 - 10) / 5) + 1
iattc_ll$ref55 <- (crow - 1) * ncol + ccol
#epo_ll$prop <- rowSums(epo_ll[,c("ALBn", "BETn", "YFTn")])/rowSums(epo_ll[,c("ALBn", "BETn", "YFTn", "SWOn")])
iattc_ll$ttuna <- rowSums(iattc_ll[,c("ALBn", "BETn", "YFTn")])
iattc_ll$tot <- rowSums(iattc_ll[,c("ALBn", "BETn", "YFTn", "SWOn")])

#aggregate gfw to flag year and 5x5 area for in and out

epo_area_prop <- aggregate(cbind(ttuna,tot) ~  Flag+ Year + ref55, data = iattc_ll, sum)
epo_area_prop$Flag <- match(epo_area_prop$Flag, c("CHN", "JPN", "KOR", "TWN", "USA", "VUT"))

for(i in seq_along(epo_area_prop$Flag)){
   yr <- epo_area_prop$Year[i] - 2011
   cc <- epo_area_prop$ref55[i]
   flg <- epo_area_prop$Flag[i]
   tot <- epo_area_prop$tot[i]
   ttuna <- epo_area_prop$ttuna[i]
   prop_areas[yr, cc, flg, 1] <- prop_areas[yr, cc, flg, 1] + ttuna
   prop_areas[yr, cc, flg, 2] <- prop_areas[yr, cc, flg, 2] + tot
}

save(prop_areas, file = "/Users/robert.ahrens/Documents/Projects/FKW/foreign/agg_data_prop.Rdata")
load("/Users/robert.ahrens/Documents/Projects/FKW/foreign/agg_data_prop.Rdata")
# now plot map for each contry
png(file = "/Users/robert.ahrens/Documents/Projects/FKW/foreign/prop_tuna.png", width = 14, height = 12, units = "in", res = 300)
par(mfcol =c(3,2), mgp=c(1,1,0), mai=c(0.5, 0.5, 0.2, 0.1), omi = c(0.5, 0.5,0 ,0))
for (i in 1:nflags){
    tmp <- prop_areas[, ,i ,1] / prop_areas[, ,i ,2]
    plot(st_geometry(grid5x5), border = "grey30")
    plot(st_geometry(st_shift_longitude(eez)), border = "black", add = TRUE)
    plot(st_geometry(fkw_bdry_poly), col = "transparent", border = "orange", add = TRUE)
    axis(1, at = c(180,190,200,210,220,230), label = c("180","-170","-160","-150","-140","-130"), cex.axis = 1.5)
    axis(2, cex.axis = 1.5)
    mtext(c("CHN", "JPN", "KOR", "TWN", "USA", "VUT")[i],side = 3, line = 0)
    cnt <- 0
    for(j in 1:nrow){
        for (k in 1:ncol){
          cnt <- cnt + 1
          x <- (k - 1) * 5 + 175
          y <- (j - 1) * 5 + 10
          spcr <- 0.2
          wd <- (5 - 2 * spcr) / nyrs 
          x1 <- seq(x + spcr, x + (5 - 2 * spcr - wd), length = nyrs)
          x2  <- x1 + wd
          y1 <- rep(y, nyrs)
          y2 <- y + 4.9 * tmp[, cnt]
          clr <- rgb(0, 0.3, 0.8, 0.2)
          rect(x1, y1, x2, y2, col = "steelblue", border = "grey")
        }
    }
}
mtext("Longitude", side = 1, line = 1, cex = 1.5, outer = TRUE)
mtext("Latitude", side = 2, line = 1, cex = 1.5, outer = TRUE)
dev.off()

png(file = "/Users/robert.ahrens/Documents/Projects/FKW/foreign/prop_tuna_ldsc.png", width = 14, height = 8, units = "in", res = 300)
par(mfcol =c(2,3), mgp=c(1,1,0), mai=c(0.5, 0.5, 0.2, 0.1), omi = c(0.5, 0.5,0 ,0))
for (i in 1:nflags){
    tmp <- prop_areas[, ,i ,1] / prop_areas[, ,i ,2]
    plot(st_geometry(grid5x5), border = "grey30")
    plot(st_geometry(st_shift_longitude(eez)), border = "black", add = TRUE)
    plot(st_geometry(fkw_bdry_poly), col = "transparent", border = "orange", add = TRUE)
    axis(1, at = c(180,190,200,210,220,230), label = c("180","-170","-160","-150","-140","-130"), cex.axis = 1.5)
    axis(2, cex.axis = 1.5)
    mtext(c("CHN", "JPN", "KOR", "TWN", "USA", "VUT")[i],side = 3, line = 0)
    cnt <- 0
    for(j in 1:nrow){
        for (k in 1:ncol){
          cnt <- cnt + 1
          x <- (k - 1) * 5 + 175
          y <- (j - 1) * 5 + 10
          spcr <- 0.2
          wd <- (5 - 2 * spcr) / nyrs 
          x1 <- seq(x + spcr, x + (5 - 2 * spcr - wd), length = nyrs)
          x2  <- x1 + wd
          y1 <- rep(y, nyrs)
          y2 <- y + 4.9 * tmp[, cnt]
          clr <- rgb(0, 0.3, 0.8, 0.2)
          rect(x1, y1, x2, y2, col = "steelblue", border = "grey")
        }
    }
}
mtext("Longitude", side = 1, line = 1, cex = 1.5, outer = TRUE)
mtext("Latitude", side = 2, line = 1, cex = 1.5, outer = TRUE)
dev.off()
