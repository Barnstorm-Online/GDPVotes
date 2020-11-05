library(rgdal)
library(dplyr)
library(sf)
library(purrr)


unlink("./data/gdp_temp", recursive = TRUE)
dir.create("./data/gdp_temp")
# FUNKKKKKKK YEAAA
Louisiana <- getStateGDP("Louisiana")

Ysc <- getStateShortCode("Louisiana")
temp = list.files(path = "./data")







# purrr::map(names(All), function(n){
#   stateGDP<-getStateGDP(n)
#   dirPath<-paste("./data/gdp_temp/",getStateShortCode(n))
#   dir.create(dirPath)
#   save(stateGDP, file=paste(dirPath,"/",getStateShortCode(n),n,"-GDP.Rda"))
# })
#
# sfData <- st_read('./data/Counties/cb_2018_us_county_500k.shp')
#
# sfAk <- st_read('./data/2016_dataverse/ak_2016/ak_2016.shp')
#
# akData <- sfAk %>%
#   select(G16PRERTRU, G16PREDCLI) %>%
#   mutate(Balance = as.integer(G16PRERTRU)-as.integer(G16PREDCLI)) %>%
#   select(Balance)
#
# data <- sfData %>%
#   # select(STATEFP) %>%
#   filter(STATEFP=="02")
#   # filter(STATEFP!="15" & STATEFP!="02" & as.integer(STATEFP) <= 56)
#
# plot(data)
#
# plot(akData)
