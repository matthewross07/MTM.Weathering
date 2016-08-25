#This is just a helper script to shrink and
#project shapefiles into .RData format and sparsify the timeseries data for quick display

#Script will not work as all data is stored locally until publication. 
# it is just here to keep a record of changes to data.

library(rgdal)
library(lubridate)
library(sp)
library(reshape2)
library(dygraphs)
library(xts)
library(ggplot2)
library(reshape2)
source('~/Dropbox/Shared Science/NSF_MTM_All/MTM_MudRiver/src_functions/qc_functions_mr.R')
mytimezone <- 'Etc/GMT-5'

#Read in shapefiles for watershed outlines
setwd("~/Dropbox/Shared Science/NSF_MTM_All/MTM_TEMP")
all.sheds <- readOGR('watershedoutlines', 'ALL_merged')

#Convert to leaflet projection wgs84
all.sheds <- spTransform(all.sheds, CRS('+init=epsg:4326'))
all.sheds$Site <- c('RB', 'RB_1', 'REM', 'LF', 'MR14', 'MR2', 'LB', 'MR')

#Subset to our primary sites
isco.sheds <- all.sheds[all.sheds$Site %in% c('RB', 'LF', 'MR', 'LB'), ]
# #Check that it makes sense
# plot(isco.sheds, col = 1:4)
# legend(
#   'topleft',
#   pch = 15,
#   col = palette(),
#   legend = (isco.sheds$Site)
# )




#Load flux data
load('~/Dropbox/Shared Science/NSF_MTM_All/MTM_MudRiver/tidy_data/Flux.Dat.RData')

#Remove NO3 data. 
flux.dat <- flux.dat[flux.dat$element != 'NO3.N',]
flux.dat$date <- as.Date(flux.dat$min10)

d.q <- dcast(flux.dat,date~site,value.var='Q.mm',mean) #mm/hr
d.sc <- dcast(flux.dat,date~site,value.var='sc',mean) #mm/hr
d.p <-  dcast(flux.dat,date~.,value.var='P',mean) 
names(d.p) <- c('date','Precip')


#For loop to sum flux over daily timesteps with min and max estimates for each site by element.
elements <- unique(flux.dat$element)
sites <- unique(flux.dat$site)

ct <- 0
f.l <- list()
c.l <- list()
fc.l <- list()
for(i in 1:length(elements)){
  #Subset element
  el <- flux.dat[flux.dat$element == elements[i],]
  for(j in 1:length(sites)){
    ct <- ct+1
    #Subset by site
    el.site <- el[el$site == sites[j],]
    #Melt to get lwr,fit, and uppper in right spots
    fl.melt <- melt(el.site, id.vars=c('min10','date'),measure.vars=c('area.flx','min.area.flx','max.area.flx'))
    fl.melt$value <- fl.melt$value * 1000 #(g/ha/10min)
    fl.melt$value[fl.melt$value < 0] <- 0 # Floor flux to zero
    c.melt <- melt(el.site,id.vars=c('min10','date'),measure.vars=c('min.conc','conc','max.conc'))
    c.melt$value[c.melt$value < 0] <- 0 
    #Cast to put data in vectors for xts. 
    fl.cast <- dcast(fl.melt,date~variable,sum) #g/ha/day
    c.cast <- dcast(c.melt,date~variable,mean)
    #remove first day
    fl.cast <- fl.cast[-1,]
    c.cast <- c.cast[-1,]
    #Convert to xts format for quicker plotting and storage
    flx.xts <- xts(cbind('Lwr.flux'=fl.cast$min.area.flx,'Fit.flux'=fl.cast$area.flx,'Upr.flux'=fl.cast$max.area.flx),order.by=fl.cast$date)
    c.xts <-  xts(cbind('Lwr.conc'=c.cast$min.conc,'Fit.conc'=c.cast$conc,'Upr.conc'=c.cast$max.conc),order.by=c.cast$date)
    f.l[[ct]] <- flx.xts
    c.l[[ct]] <- c.xts
    fc.l[[ct]] <- cbind(flx.xts,c.xts)
    names(f.l)[[ct]] <- paste(elements[i],sites[j],sep='.')
    names(c.l)[[ct]] <- paste(elements[i],sites[j],sep='.')
    names(fc.l)[[ct]] <- paste(elements[i],sites[j],sep='.')
  }
}


# save(d.q,d.sc,d.p,f.l,c.l,fc.l,isco.sheds,
#      file="~/Dropbox/Shared Science/NSF_MTM_All/MTM_Shiny/MTM.Weathering/Flux.Shine.RData")


dygraph(fc.l[['SO4.LB']]) %>% dyOptions(useDataTimezone=T) %>%
  dyAxis('y','Flux g/ha/day',independentTicks=T) %>%
  dyAxis('y2','Concentration (mg/l)',independentTicks=T) %>%
  dySeries(c("Lwr.flux", "Fit.flux", "Upr.flux"), label = "Flux (g/ha/day)",color='#01665e',strokeWidth=2) %>%
  dySeries(c('Lwr.conc','Fit.conc','Upr.conc'),label='Conc (mg/l)',color='#8c510a',axis='y2',strokeWidth=2) %>%
  dyShading(from='2014-10-1',to='2015-10-1',color='gray')






