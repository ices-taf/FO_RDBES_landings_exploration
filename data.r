
library(dplyr)
library(tidyr)
library(stringr)
library(icesFO)
library(icesVMS)
library(ggplot2)
library(rmarkdown)
library(mapplots)
library(RColorBrewer)
library(leaflet)
library(leaflet.minicharts)
library(htmlwidgets)


dir.create('data')
dir.create('model')
dir.create('report')
dir.create('output')

dat.wd  <- "data"
mod.wd  <- "model"
