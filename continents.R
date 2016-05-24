library(rworldmap)
library(rgeos)
library(maptools)
sPDF <- getMap()
cont <-
        sapply(levels(sPDF$continent),
               FUN = function(i) {
                       ## Merge polygons within a continent
                       poly <- gUnionCascaded(subset(sPDF, continent==i))
                       ## Give each polygon a unique ID
                       poly <- spChFIDs(poly, i)
                       ## Make SPDF from SpatialPolygons object
                       SpatialPolygonsDataFrame(poly,
                                                data.frame(continent=i, row.names=i))
               },
               USE.NAMES=TRUE)

## Bind the 6 continent-level SPDFs into a single SPDF
cont <- Reduce(spRbind, cont)

## Plot to check that it worked
plot(cont)

