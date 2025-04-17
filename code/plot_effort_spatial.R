library(sf)

main_dir <- "/Users/robert.ahrens/Documents/projects/fkw_foreign_ll_interactions/"


file.path((main_dir, "output", )
#Make of area specific comparison between gfw and rfmo
load(file.path(main_dir, "output", "effort_match.Rdata"))
load(file.path(main_dir, "output", "prop_data.Rdata"))
load(file.path(main_dir, "output", "agg_data.Rdata"))

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

#Make of area specific comparison between gfw and rfmo
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
