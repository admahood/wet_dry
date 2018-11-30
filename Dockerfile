FROM rocker/geospatial:latest
MAINTAINER "Adam Mahood" adam.mahood@colorado.edu

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    awscli

RUN install2.r --error \ 
  assertthat \ 
  doParallel \ 
  foreach \
  gdalUtils \
  ggthemes \ 
  httr \ 
  randomForest \
  rasterVis \ 
  RCurl \ 
  snowfall \ 
  tidyverse \ 
  viridis


