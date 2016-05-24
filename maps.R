rm(list=ls())
library("raster")
library("rworldmap")
library("SDMTools")
library("plotrix")
#### graphic variables ####
col_temp <- rainbow(20, start = 0, end=.7)
col_prec <- rainbow(20, start = 0, end=.7)
continent <- getMap() 
#### raw data ###
setwd("/Users/afr/Desktop/PhD/Lomolino_maps/Data/")
present_temp <- raster(read.asciigrid("000_amean_temp.txt", as.image = T))
LGM_temp <- raster(read.asciigrid("021_amean_temp.txt", as.image = T))
present_prec <- raster(read.asciigrid("000_prep.txt", as.image = T))
LGM_prec <- raster(read.asciigrid("021_prep.txt", as.image = T))
#### Difference in temperature ####
par(mar=c(2,2,2,2))
Temp_diff <- LGM_temp - present_temp
plot(Temp_diff, frame=F, axes=F, xaxs="i", interpolate=T, col=rev(col_temp), box=F, legend=F)
mtext(side = 3,text = "Change in air temperature ºC", line = -3, cex=1.5)
plot(cont, add=T, lwd=2)
color.legend(-180, -120,180, -100, rect.col = rev(col_temp), legend = seq(round(min(Temp_diff[])),round(max(Temp_diff[])),by = 3 ) , align = "rb")
#### Difference in temperature percentile ####
percentile_temp <- quantile(Temp_diff, probs=c(seq(0,1,length.out = 21)))
Temp_diff_percentile <- Temp_diff
index <- seq_along(percentile_temp)
for (i in 1:(length(index)-2)){
                temp <- which(Temp_diff_percentile[] >= percentile_temp[i] & Temp_diff_percentile[] < percentile_temp[i+1])
                Temp_diff_percentile[temp] <- i*1000
}
temp_41 <- which(Temp_diff_percentile[] >= percentile_temp[20] & Temp_diff_percentile[] <= percentile_temp[21])
Temp_diff_percentile[temp_41] <- 20*1000
plot(Temp_diff_percentile, interpolate=T, frame=F, axes=F, xaxs="i", col=rev(col_temp), box=F, legend=F)
mtext(side = 3,text = "Change in air temperature ºC (percentile)", line = -3, cex=1.5)
plot(cont, add=T, lwd=2)
space_legend <- seq(2,20,2)
per_legend<- percentile_temp
per_legend[space_legend] <- NA
per_legend <- round(per_legend,digits =2 )
color.legend(-180, -120,180, -100, rect.col =rev(col_temp), legend= per_legend, align = "rb")
#### Difference in precipitation ####
Prec_diff <- LGM_prec - present_prec
plot(Prec_diff, interpolate=T, frame=F, axes=F, xaxs="i", col=rev(col_prec), box=F, legend=F, main=NULL)
mtext(side = 3,text = "Change in precipitation kg/m2/s", line = -3, cex=1.5)
plot(cont, add=T, lwd=2)
mtext(side = 3,text = "Change in precipitation kg/m2/s", line = -3, cex=1.5)
color.legend(-180, -120,180, -100, rect.col = rev(col_prec), legend = seq(round(min(Prec_diff[])),round(max(Prec_diff[])),by = 3 ) , align = "rb")
#### Difference in precipitation percentile ####
percentile_prec <- quantile(Prec_diff, probs=c(seq(0,1,length.out = 21)))
Prec_diff_percentile <- Prec_diff
index <- seq_along(percentile_prec)
for (i in 1:(length(index)-2)){
        temp <- which(Prec_diff_percentile[] >= percentile_prec[i] & Prec_diff_percentile[] < percentile_prec[i+1])
        Prec_diff_percentile[temp] <- i*1000
}
temp_41 <- which(Prec_diff_percentile[] >= percentile_prec[20] & Prec_diff_percentile[] <= percentile_prec[21])
Prec_diff_percentile[temp_41] <- 20*1000
plot(Prec_diff_percentile, interpolate=T, frame=F, axes=F, xaxs="i", col=rev(col_prec), box=F, legend=F)
mtext(side = 3,text = "Change in precipitation kg/m2/s (percentile)", line = -3, cex=1.5)
plot(cont, add=T, lwd=2)
space_legend <- seq(2,20,2)
per_legend <- percentile_prec
per_legend[space_legend] <- NA
per_legend <- round(per_legend,digits =2 )
color.legend(-180, -120,180, -100, rect.col =rev(col_prec), legend= per_legend, align = "rb")


