library(lubridate)
library(sf)
library(tidyverse)
library(ggplot2)
library(viridis)

flag_use <- c("CHN", "JPN", "KOR", "TWN", "USA", "VUT")
nflags <- length(flag_use)
nyrs <- 9
ncol <- 11
nrow <- 5
nareas <- ncol * nrow
xmin <- 175
xmax <- 360 - 130
ymin <- 10
ymax <- 35

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
pts_bbox <- data.frame("lon" = c(xmin, xmin, xmax, xmax), "lat" = c(ymin, ymax, ymax, ymin))
pts_bbox <- st_as_sf(pts_bbox, coords = c("lon", "lat"), crs = st_crs(sez))
grid5x5 <- st_make_grid(pts_bbox, cellsize = 5, what = "polygons", square = TRUE)

#this happy little section gets the mean density of the grid cells in the FKW management area 
abund <- read_csv("/Users/robert.ahrens/Documents/Projects/fkw/foreign/density/Pc_2017Dens_3day_5.5_noOOB.csv")[,c("mlat", "mlon", "2017Avg.Dens")]
abund$id <- 1:nrow(abund)

r <- abund %>% select(mlon, mlat, "2017Avg.Dens") %>% rename(x = mlon, y = mlat, z = "2017Avg.Dens") # Pull off locations and pixel IDs
r <- r %>% rast(type='xyz', crs="EPSG:4326") %>%  st_as_stars(r)
r_bb <- st_bbox(r)

grid5x5$ID<-1:dim(grid5x5)[1]
extract <- st_shift_longitude(st_intersection(fkw_bdry_poly,grid5x5))
tmp <- st_as_sf(st_extract(r, extract, FUN = function(x) mean(x,na.rm = TRUE)))
den_lookup <- data.frame("pid" = extract$ID, "mden" <- tmp$z)
save(den_lookup,file = "/Users/robert.ahrens/Documents/Projects/FKW/foreign/den_lookup.Rdata")



#read in Global Fishing watch and filter to the bounding box of the operational area of the fishery expanded to WCPFC 5x5 
#load("/Users/robert.ahrens/Documents/Data/GFW/gfw_dll.Rdata")
#gfw_dll <- gfw_dll[,c("date", "cell_ll_lat", "cell_ll_lon", "flag", "fishing_hours")]
#ii <- which(gfw_dll$cell_ll_lon < 0)
#gfw_dll$cell_ll_lon[ii] <- 360 + gfw_dll$cell_ll_lon[ii]
#ii <- which(gfw_dll$cell_ll_lat >= ymin & gfw_dll$cell_ll_lat < ymax & gfw_dll$cell_ll_lon >= xmin & gfw_dll$cell_ll_lon < xmax)
#gfw_dll_hi <- gfw_dll[ii,]
#save(gfw_dll_hi, file = "/Users/robert.ahrens/Documents/Data/GFW/gfw_dll_hi.Rdata")



load("/Users/robert.ahrens/Documents/projects/fkw/foreign/gfw_dll_hi_2012_2023.Rdata")

flag_use <- c("CHN", "JPN", "KOR", "TWN", "USA", "VUT")
nflags <- length(flag_use)
syr <- as.numeric(min(year(gfw_dll_hi$date)))
eyr <- as.numeric(max(year(gfw_dll_hi$date)))
nyrs <- eyr - syr + 1
xmin <- 175
xmax <- 360 - 130
ymin <- 10
ymax <- 35
ncol <- (xmax - xmin) / 5
nrow <- (ymax - ymin) / 5
nareas <- ncol * nrow

gfw_dll_hi <- gfw_dll_hi[which(gfw_dll_hi$flag %in% flag_use),]

#add a 5x5 grid reference starting with 175, 10 as 1 and increasing eastward and northward sequentially with 11 cell per row
#for a 10 column by 5 row matrix for a total of 55 cell 1-55
ccol <- floor((gfw_dll_hi$cell_ll_lon - xmin) / 5) + 1
#there are some points at 35 that need to be adjusted from row 6 to row 5
crow <- floor((gfw_dll_hi$cell_ll_lat - ymin) / 5) + 1
crow[which(crow > 5)] <- 5
gfw_dll_hi$ref55 <- (crow - 1) * ncol + ccol

gfw_dll_hi$in_fkw <- 0
gfw_dll_hi$year <- year(gfw_dll_hi$date)
gfw_dll_hi$month <- month(gfw_dll_hi$date)
# create a sf point object and see if they are in the fkw boundary
pts <- st_as_sf(gfw_dll_hi, coords = c("cell_ll_lon", "cell_ll_lat"), crs = 4326)
#pts <- st_shift_longitude(pts)
tmp <- st_within(pts, fkw_bdry_poly)
ii <- which(lengths(tmp) > 0)
gfw_dll_hi$in_fkw[ii] <- 1
pts$in_fkw[ii] <- 1

save(pts, file = "/Users/robert.ahrens/Documents/Projects/FKW/foreign/gfw_sf_pts_fkw.Rdata")
save(gfw_dll_hi, file = "/Users/robert.ahrens/Documents/Projects/FKW/foreign/gfw_all_fkw.Rdata")



load("/Users/robert.ahrens/Documents/Projects/FKW/foreign/gfw_all_fkw.Rdata")

#aggregate for for total effort in FKW area
gfw_prop_flag <- aggregate(fishing_hours ~ flag + in_fkw, data = gfw_dll_hi, sum)
gfw_prop_year <- aggregate(fishing_hours ~ flag + year + in_fkw, data = gfw_dll_hi, sum)
gfw_prop_area <- aggregate(fishing_hours ~ flag + year + ref55 + in_fkw, data = gfw_dll_hi, sum)
gfw_prop_area2 <- aggregate(fishing_hours ~ flag + ref55 + in_fkw, data = gfw_dll_hi, sum)

#get total effort
gfw_area_eff <- aggregate(fishing_hours ~ flag + year + ref55, data = gfw_dll_hi, sum)
#get nuneric values to make pointing in arrays easier
gfw_prop_flag$flag_ind <- match(gfw_prop_flag$flag, flag_use)
gfw_prop_year$flag_ind <- match(gfw_prop_year$flag, flag_use)
gfw_prop_area$flag_ind <- match(gfw_prop_area$flag, flag_use)
gfw_prop_area2$flag_ind <- match(gfw_prop_area2$flag, flag_use)
gfw_area_eff$flag_ind <- match(gfw_area_eff$flag, flag_use)

save(gfw_area_eff, file = "/Users/robert.ahrens/Documents/Projects/FKW/foreign/gfw_area_eff.Rdata")
save(gfw_prop_area, file = "/Users/robert.ahrens/Documents/Projects/FKW/foreign/gfw_prop.Rdata")

#array of for storage 1) gfw out_fkw 2) gfw in_fkw 3) gfw prop in 4) gfw total hours fished 5) rfmo total hooks 
agg_data <- array(dim = c(nyrs, nareas, nflags, 5))
prop_data <- array(dim = c(nareas, nflags, 3))

#put effort into matrix to calculate area proportion for plotting 
for(i in seq_along(gfw_prop_area2$flag_ind)){
   cc <- gfw_prop_area2$ref55[i] #area
   flg <- gfw_prop_area2$flag_ind[i] #flag
   fkw <- gfw_prop_area2$in_fkw[i] + 1 #in or out of fkw
   eff <- gfw_prop_area2$fishing_hours[i] #effort
   if(is.na(prop_data[cc, flg, fkw])) prop_data[cc, flg, fkw] <- 0
   prop_data[cc, flg, fkw] <- prop_data[cc, flg, fkw] + eff
}
# proportion of effort in the fkw 
prop_data[, , 3] <- prop_data[, , 2] / (prop_data[, , 2] + prop_data[, , 1])
#deal with NA and NaN 
for (i in 1:nflags){
    for(j in 1:nareas){
        if(is.na(prop_data[j, i, 1]) == FALSE) if (is.na(prop_data[j, i, 2]) == TRUE) prop_data[j, i, 3] <- 0
        if(is.na(prop_data[j, i, 1]) == TRUE) if(is.na(prop_data[j, i, 2]) == FALSE) prop_data[j, i, 3] <- 1
    }
}

# this is the finer resolution matrix that will be used to convert the RFMO effort to inside or outside
for(i in seq_along(gfw_prop_area$flag_ind)){
   yr <- gfw_prop_area$year[i] - (range(gfw_dll_hi$year)[1] - 1)
   cc <- gfw_prop_area$ref55[i]
   flg <- gfw_prop_area$flag_ind[i]
   fkw <- gfw_prop_area$in_fkw[i] + 1
   eff <- gfw_prop_area$fishing_hours[i]
   if(is.na(agg_data[yr, cc, flg, fkw])) agg_data[yr, cc, flg, fkw] <- 0
   if(is.na(agg_data[yr, cc, flg, 4])) agg_data[yr, cc, flg, 4] <- 0
   agg_data[yr, cc, flg, fkw] <- agg_data[yr, cc, flg, fkw] + eff
   agg_data[yr, cc, flg, 4] <- agg_data[yr, cc, flg, 4] + eff
}


save(prop_data, file = "/Users/robert.ahrens/Documents/Projects/FKW/foreign/prop_data.Rdata")
save(agg_data, file = "/Users/robert.ahrens/Documents/Projects/FKW/foreign/agg_data.Rdata")


wcpfc_ll <- read.csv("/Users/robert.ahrens/Documents/data/RFMO_Data/WCPFC/WCPFC_L_PUBLIC_BY_FLAG_MON_8/WCPFC_L_PUBLIC_BY_FLAG_MON.CSV")
wcpfc_ll <- wcpfc_ll[, c("yy", "mm", "flag_id", "lat_short", "lon_short", "cwp_grid", "hhooks")]
wcpfc_ll <- wcpfc_ll[which(wcpfc_ll$flag_id %in% c("CN", "JP", "KR", "TW", "US", "VU")),]
wcpfc_ll <- wcpfc_ll[which(wcpfc_ll$yy >= 2012 & wcpfc_ll$yy <= 2023),]
wcpfc_ll <- wcpfc_ll[which(substr(wcpfc_ll$lat_short, 3, 3) == "N"),]

wcpfc_ll$lat <- as.numeric(substr(wcpfc_ll$lat_short, 1, 2)) + 2.5
wcpfc_ll$lon <- as.numeric(substr(wcpfc_ll$lon_short, 1, 3))
ii <- which(substr(wcpfc_ll$lon_short, 4, 4) == "W")
wcpfc_ll$lon[ii] <- 360 - wcpfc_ll$lon[ii]
wcpfc_ll$lon <- wcpfc_ll$lon + 2.5

# filter to area of interest
wcpfc_ll <- with(wcpfc_ll,wcpfc_ll[which(lat >= ymin & lat < ymax & lon >= xmin & lon < 360 - 150 ),])

ccol <- floor((wcpfc_ll$lon - 2.5 - xmin) / 5) + 1
crow <- floor((wcpfc_ll$lat - 2.5 - ymin) / 5) + 1
wcpfc_ll$ref55 <- (crow - 1) * ncol + ccol
#filter down to only iso2 <- c("JP", "KR", "TW", "VU", "CN", "SB", "PG", "FJ", "KI", "SM", "US", "") 
wcpfc_ll$hooks <- wcpfc_ll$hhooks * 100
#aggregate gfw to flag year and 5x5 area for in and out
wcpo_area_eff <- aggregate(hooks ~ flag_id + yy + ref55, data = wcpfc_ll, sum)
wcpo_area_eff$flag_ind <- match(wcpo_area_eff$flag_id, c("CN", "JP", "KR", "TW", "US", "VU"))

for(i in seq_along(wcpo_area_eff$flag_ind)){
   yr <- wcpo_area_eff$yy[i] - 2011
   cc <- wcpo_area_eff$ref55[i]
   flg <- wcpo_area_eff$flag_ind[i]
   eff <- wcpo_area_eff$hooks[i]
   if(is.na(agg_data[yr, cc, flg, 5])) agg_data[yr, cc, flg, 5] <- 0   
   agg_data[yr, cc, flg, 5] <- agg_data[yr, cc, flg, 5] + eff
}

iattc_ll <- read.csv("/Users/robert.ahrens/Documents/data/RFMO_Data/IATTC/PublicLLTunaBillfish/PublicLLTunaBillfishNum.csv")
iattc_ll <- iattc_ll[, c("Year", "Flag", "LatC5", "LonC5", "Hooks", "ALBn", "BETn", "YFTn", "SWOn")]
iattc_ll <- iattc_ll[which(iattc_ll$Year >= 2012 & iattc_ll$Year <= 2020), ]
iattc_ll <- iattc_ll[which(iattc_ll$Flag %in% c("CHN", "JPN", "KOR", "TWN", "USA", "VUT")), ]
iattc_ll$lat <- iattc_ll$LatC5
iattc_ll$lon <- 360 + iattc_ll$LonC5
iattc_ll <- iattc_ll[which(iattc_ll$LatC5 >= 10 & iattc_ll$LatC5 <= 35), ]
iattc_ll <- iattc_ll[which(iattc_ll$LonC5 > -150 & iattc_ll$LonC5 < -130), ]
ccol <- floor((iattc_ll$lon - 2.5  - 175) / 5) + 1
crow <- floor((iattc_ll$lat - 2.5 - 10) / 5) + 1
iattc_ll$ref55 <- (crow - 1) * ncol + ccol

#aggregate gfw to flag year and 5x5 area for in and out
epo_area_eff <- aggregate(Hooks ~ Flag + Year + ref55, data = iattc_ll, sum)
epo_area_eff$flag_ind <- match(epo_area_eff$Flag, c("CHN", "JPN", "KOR", "TWN", "USA", "VUT"))

for(i in seq_along(epo_area_eff$flag_ind)){
   yr <- epo_area_eff$Year[i] - 2011
   cc <- epo_area_eff$ref55[i]
   flg <- epo_area_eff$flag_ind[i]
   eff <- epo_area_eff$Hooks[i]
   if(is.na(agg_data[yr, cc, flg, 5])) agg_data[yr, cc, flg, 5] <- 0  
   agg_data[yr, cc, flg, 5] <- agg_data[yr, cc, flg, 5] + eff
}

save(agg_data, file = "/Users/robert.ahrens/Documents/Projects/FKW/foreign/agg_data.Rdata")
library(lubridate)
library(sf)

load("/Users/robert.ahrens/Documents/Projects/FKW/foreign/prop_data.Rdata")
load("/Users/robert.ahrens/Documents/Projects/FKW/foreign/agg_data.Rdata")

#create matchup file
effort_match <- expand.grid(area = 1:nareas, year = 1:nyrs, flag = 1:nflags)
effort_match$gfw <- NA
effort_match$rfmo <- NA
effort_match$ratio <- NA
effort_match$use <- 1

for(i in seq_along(effort_match$ratio)){
    fl <- effort_match$flag[i]
    yr <- effort_match$year[i]
    cc <- effort_match$area[i]
    #fill in using flag average 
    ii <- which(gfw_prop_flag$flag_ind == fl & gfw_prop_flag$in_fkw == 0)
    iii <- which(gfw_prop_flag$flag_ind == fl & gfw_prop_flag$in_fkw == 1)
    res <- gfw_prop_flag$fishing_hours[iii]/(gfw_prop_flag$fishing_hours[ii] + gfw_prop_flag$fishing_hours[iii])
    if (res > 0) effort_match$ratio[i] <- res
    #fill in using flag and year 
    ii <- which(gfw_prop_year$flag_ind == fl & gfw_prop_year$year == (yr + 2011) & gfw_prop_year$in_fkw == 0)
    iii <- which(gfw_prop_year$flag_ind == fl & gfw_prop_year$year == (yr + 2011) & gfw_prop_year$in_fkw == 1)
    res <- gfw_prop_year$fishing_hours[iii]/(gfw_prop_year$fishing_hours[ii] + gfw_prop_year$fishing_hours[iii])
    if (length(res)>0) if(!is.na(res)) if(res > 0) effort_match$ratio[i]<- res
    #fill in using flag and year and area 
    ii <- which(gfw_prop_area$flag_ind == fl & gfw_prop_area$year == (yr + 2011) & gfw_prop_area$ref55 == cc & gfw_prop_area$in_fkw == 0)
    iii <- which(gfw_prop_area$flag_ind == fl & gfw_prop_area$year == (yr + 2011) & gfw_prop_area$ref55 == cc & gfw_prop_area$in_fkw == 1)
    res <- gfw_prop_area$fishing_hours[iii]/(gfw_prop_area$fishing_hours[ii] + gfw_prop_area$fishing_hours[iii])
    if (length(res)>0) if(!is.na(res)) if(res > 0) effort_match$ratio[i]<- res

}
save(effort_match, file = "/Users/robert.ahrens/Documents/Projects/FKW/foreign/effort_match.Rdata")

for(i in seq_along(effort_match$flag)){
    fg <- effort_match$flag[i]
    yy <- effort_match$year[i]
    cc <- effort_match$area[i]
    effort_match$gfw[i] <- agg_data[yy, cc, fg, 4]
    effort_match$rfmo[i] <- agg_data[yy, cc, fg, 5]
    #put a zero in if one is positive and the other is NA
    if (is.na(effort_match$gfw[i]) & !is.na(effort_match$rfmo[i]))effort_match$gfw[i] <- 0 
    if (!is.na(effort_match$gfw[i]) & is.na(effort_match$rfmo[i])) effort_match$rfmo[i] <- 0 
    if (is.na(effort_match$gfw[i]) & is.na(effort_match$rfmo[i])) effort_match$use[i] <- 0 

}
effort_match$effort_in <- effort_match$rfmo * effort_match$ratio
effort_match$lat_band <- floor((effort_match$area - 1) / 11) + 1
effort_match$lon_band <- effort_match$area%%11
effort_match$lon_band[which(effort_match$lon_band == 0)] <- 11

save(effort_match, file = "/Users/robert.ahrens/Documents/Projects/FKW/foreign/effort_match.Rdata")

#Make of area specific comparison between gfw and rfmo
load("/Users/robert.ahrens/Documents/Projects/FKW/foreign/effort_match.Rdata")
load("/Users/robert.ahrens/Documents/Projects/FKW/foreign/prop_data.Rdata")
load("/Users/robert.ahrens/Documents/Projects/FKW/foreign/agg_data.Rdata")
png(file = "/Users/robert.ahrens/Documents/Projects/FKW/foreign/peff_gfw_rfmo.png", width = 14, height = 12, units = "in", res = 300)
par(mfcol =c(3,2), mgp=c(1,1,0), mai=c(0.5, 0.5, 0.2, 0.1), omi = c(0.5, 0.5,0 ,0))
for (i in 1:nflags){
    tmp <- colSums(agg_data[,,i,4], na.rm = TRUE)/max(colSums(agg_data[,,i,4], na.rm = TRUE))
    tmp2 <- colSums(agg_data[,,i,5], na.rm = TRUE)/max(colSums(agg_data[,,i,5], na.rm = TRUE))
    #tmp <- tmp / max(tmp)
    #tmp2 <- agg_eff_data[,i] / max(agg_eff_data[,i],na.rm = TRUE)
    plot(st_geometry(grid5x5), border = "grey50")
        axis(1, at = c(180,190,200,210,220,230), label = c("180","-170","-160","-150","-140","-130"), cex.axis = 1.5)
        axis(2, cex.axis = 1.5)
    mtext(c("CHN", "JPN", "KOR", "TWN", "USA", "VUT")[i],side = 3, line = 0)
    cnt <- 0
    for(j in 1:nrow){
        for (k in 1:ncol){
          cnt <- cnt + 1
          x <- (k - 1) * 5 + xmin
          y <- (j - 1) * 5 + ymin    
         if(is.na(prop_data[cnt, i, 3]) == FALSE) rect(x+ 0.5, y, x + 1.5, y+4.8*prop_data[cnt, i, 3], col = "steelblue")
         if (tmp[cnt] > 0 ) rect(x + 2, y, x + 3, y+ 4.8 * tmp[cnt], col = rgb(0.2, 0.2, 0.2)) #GFW
         if (tmp2[cnt] > 0 ) rect(x + 3.5, y, x + 4.5, y + 4.8 * tmp2[cnt], col = rgb(0.7, 0.7, 0.7)) #RFMO
        }
    }
    plot(st_geometry(st_shift_longitude(eez)), border = "black", add = TRUE)
    plot(st_geometry(fkw_bdry_poly), col = "transparent", border = "orange", add = TRUE)

}
mtext("Longitude", side = 1, line = 1, cex = 1.5, outer = TRUE)
mtext("Latitude", side = 2, line = 1, cex = 1.5, outer = TRUE)
dev.off()

trfmo <- sum(effort_match$rfmo, na.rm = TRUE)
tgfw  <- sum(effort_match$gfw, na.rm = TRUE)
no_gfw <- sum(effort_match$rfmo[which(effort_match$gfw==0)], na.rm = TRUE)
no_rfmo <- sum(effort_match$gfw[which(effort_match$rfmo==0)], na.rm = TRUE)

no_gfw/trfmo*100
no_rfmo/tgfw*100


#find proportioneffort_in of non-US effort in a particular area
ii <- which(effort_match$flag!=5)
iii <- which(effort_match$flag==5)

nonus <- aggregate(effort_in~area,data = effort_match[ii,],sum)
us <- aggregate(effort_in~area,data = effort_match[iii,],sum)

load("/Users/robert.ahrens/Documents/Projects/FKW/foreign/den_lookup.Rdata")
nonus$den <-den_lookup$mden[match(nonus$area,den_lookup$pid)]
us$den <-den_lookup$mden[match(us$area,den_lookup$pid)]

nonus <-nonus[-which(is.na(nonus$den)),]
us <-us[-which(is.na(us$den)),]
nonus$peff <- nonus$effort_in/sum(nonus$effort_in)
us$peff <- us$effort_in/sum(us$effort_in)

sum(nonus$peff*nonus$den,na.rm = TRUE)
sum(us$peff*us$den,na.rm = TRUE)



(effort_match$lon_band[iii]*(effort_match$effort_in[iii]/sum(effort_match$effort_in[iii], na.rm = TRUE)),na.rm = TRUE)
sum(effort_match$lat_band[iii]*(effort_match$effort_in[iii]/sum(effort_match$effort_in[iii], na.rm = TRUE)),na.rm = TRUE)


tmp <- effort_match[which(effort_match$flag!=5),]


sum(tmp$effort_in[which(tmp$area>44)],na.rm=TRUE)/sum(tmp$effort_in,na.rm = TRUE)

effort_match$rfmo[which(effort_match$flag!=5)]
effort_match$lat_band <- effort_match$area%%11
effort_match$lat_band[which(effort_match$lat_band == 0)]<-11

effort_match$fkw_effort <- effort_match$rfmo * effort_match$ratio

aggregate(fkw_effort~flag+year,data = effort_match, sum)

ftable(xtabs(effort_in ~ flag + year, data = effort_match[which(effort_match$flag != 5 & !is.na(effort_match$effort_in)),]))
ftable(xtabs(effort_in ~ flag + year, data = effort_match[which(!is.na(effort_match$effort_in)),]))



nonus_effort <- colSums(ftable(xtabs(effort_in ~ flag + year, data = effort_match[which(effort_match$flag != 5 & effort_match$use == 1),])))


urlfile="https://raw.githubusercontent.com/PIFSC-Protected-Species-Division/HI-Pelagic-FKWs/main/data/03_pFKW_FishTakes_All_2001-2022.csv"

fkw_inter<-read.csv(urlfile, header = TRUE)

all <- table(fkw_inter[which(fkw_inter$InMgmtArea == TRUE),]$TripArrYear)
msi <- table(fkw_inter[which(fkw_inter$InMgmtArea == TRUE & fkw_inter$InjDet %in% c("Serious","Dead","CBD")),]$TripArrYear)

mean(msi/all)
sd(msi/all)


#RFMO coverage 2012 to 2022
rfmo_cov <- c(0.87, 0.865, 0.876, 0.873, 0.871, 0.854, 0.862, 0.868, 0.859, 0.857, 0.863)
mean(rfmo_cov)
sd(rfmo_cov)



#Calculate interation rate FKW + BF 2014-2023
load("/Users/robert.ahrens/Documents/Projects/FKW/foreign/effort_match.Rdata")
effort_yr_fl <- ftable(xtabs(effort_in ~ flag + year, data = effort_match[which(effort_match$flag != 5 & effort_match$use == 1),]))
nonus_effort <- colSums(ftable(xtabs(effort_in ~ flag + year, data = effort_match[which(effort_match$flag != 5 & effort_match$use == 1),])))
hps <- read.csv("/Users/robert.ahrens/Documents/projects/fkw/foreign/hooksperset.csv")
sets <- colSums(effort_yr_fl[,1:12]/unname(as.matrix(hps[,2:13])))

ds_fkw_inter <- c(55, 27, 39, 45, 52, 80, 22, 81, 51, 35)
ss_fkw_inter <- c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0)
#load in logbook to get total DSLL effort in hooks
load("/Users/robert.ahrens/Documents/data/logbook/output/log_hdr_2005_2023.Rdata")

dsll_effort <- aggregate(HOOKSSET~HAUL_YEAR, data = log_hdr_df[which(log_hdr_df$HAUL_YEAR >= 2014 & log_hdr_df$HAUL_YEAR <= 2023 & log_hdr_df$FLEET == "HI" & log_hdr_df$SETDEPTH == "D"),],sum)$HOOKSSET
dsll_effort2 <- aggregate(HOOKSSET~HAUL_YEAR, data = log_hdr_df[which(log_hdr_df$HAUL_YEAR >= 2012 & log_hdr_df$HAUL_YEAR <= 2023 & log_hdr_df$FLEET == "HI" & log_hdr_df$SETDEPTH == "D"),],sum)$HOOKSSET

ssll_effort <- aggregate(HOOKSSET~HAUL_YEAR, data = log_hdr_df[which(log_hdr_df$HAUL_YEAR >= 2014 & log_hdr_df$HAUL_YEAR <= 2023 & log_hdr_df$FLEET == "HI" & log_hdr_df$SETDEPTH == "S"),],sum)$HOOKSSET

ds_int_rate <- ds_fkw_inter/dsll_effort
ds_int_rate_mean <- mean(ds_int_rate)
ds_int_rate_se <- sd(ds_int_rate)
ds_int_rate_high <- ds_int_rate_mean + ds_int_rate_se * qt(0.975,(length(ds_int_rate)-1))
ds_int_rate_low <- ds_int_rate_mean - ds_int_rate_se * qt(0.975,(length(ds_int_rate)-1))

ss_int_rate <- ss_fkw_inter/ssll_effort
ss_int_rate_mean <- mean(ss_int_rate)
ss_int_rate_se <- sd(ss_int_rate)
ss_int_rate_high <- ss_int_rate_mean + ss_int_rate_se * qt(0.975,(length(ss_int_rate)-1))
ss_int_rate_low <- ss_int_rate_mean - ss_int_rate_se * qt(0.975,(length(ss_int_rate)-1))

wcp_int_rate_mean <- 0.00070821
wcp_int_rate_high <- 0.0008759439
wcp_int_rate_low <- 0.0005497946

ds_m <-round(nonus_effort * ds_int_rate_mean, 1)
ds_l95 <- round(nonus_effort * ds_int_rate_low, 1)
ds_u95 <- round(nonus_effort * ds_int_rate_high, 1)

ss_m <-round(nonus_effort * ss_int_rate_mean, 1)
ss_l95 <- round(nonus_effort * ss_int_rate_low, 1)
ss_u95 <- round(nonus_effort * ss_int_rate_high, 1)

rop_m <-round(sets * wcp_int_rate_mean, 1)
rop_l95 <- round(sets * wcp_int_rate_low, 1)
rop_u95 <- round(sets * wcp_int_rate_high, 1)


df <- data.frame("Type" = rep(c("DSLL", "SSLL", "ROP"),each = length(2012:2023)),
  "Year" = rep(2012:2023, 3),
  "Mean" = c(ds_m, ss_m, rop_m),
  "L95" = c(ds_l95, ss_l95, rop_l95),
  "U95" = c(ds_u95, ss_u95, rop_u95))

df$L95[which(df$L95 < 0)] <- 0

1/ss_int_rate_mean
1/ss_int_rate_high
1/ss_int_rate_low
mean(df$Mean[which(df$Type=="SSLL")]);sd(df$Mean[which(df$Type=="SSLL")]);sd(df$Mean[which(df$Type=="SSLL")])/mean(df$Mean[which(df$Type=="SSLL")])


write.csv(df, file = "/Users/robert.ahrens/Documents/Projects/FKW/foreign/non_us_fkw_interactions.csv" , row.names = FALSE)

png(file = "/Users/robert.ahrens/Documents/Projects/FKW/foreign/plots/fkw_inter.png", width = 8, height = 5, units = "in", res = 300)
ggplot(df, aes(x = Year, y = Mean, colour = Type, fill = Type)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = L95, ymax = U95, color = Type), linetype = 1, alpha = 0.1) +
  labs(x = "Year", y = "Estimated FKW interactions", colour = "") +
#  scale_color_viridis(discrete = TRUE, option = "D")+
#  scale_fill_viridis(discrete = TRUE) +
  scale_colour_manual(values = c("DSLL" = "Steelblue4", "SSLL"= "steelblue3","ROP" = "steelblue2")) +
  scale_fill_manual(values = c("DSLL" = "Steelblue4", "SSLL"= "steelblue3","ROP" = "steelblue2")) +
  theme_light() +
  guides(fill="none")
dev.off()



tmp <- data.frame("Year" = as.numeric(rep(2012:2023,2)),
  "Type" = rep(c("Non-US","US"), each = 2023 - 2012 + 1),
  "Effort" = as.numeric(c(nonus_effort,dsll_effort2)/1e6))
tmp2 <- data.frame("Year" = 2012:2023,
  "Percent" = nonus_effort/(nonus_effort + dsll_effort2)*100)


png(file = "/Users/robert.ahrens/Documents/Projects/FKW/foreign/plots/us_vs_nonus_effort.png", width = 8, height = 5, units = "in", res = 300)
ggplot() + 
    geom_area(aes(x = Year, y = Effort, fill = Type), data = tmp) +
    scale_fill_manual(values = alpha(c("steelblue4","steelblue2"),0.4)) +
    geom_line(aes(x = Year, y = Percent*3), linewidth = 1, data = tmp2) +
    geom_point(aes(x = Year, y = Percent*3), pch = 19, size = 2, color = "black", data = tmp2) +
    scale_y_continuous(sec.axis = sec_axis(~ ./3, name = "% non-US")) +
    labs(x = "Year", y = "Millions of hooks", colour = "") + 
    theme_light()
dev.off()

