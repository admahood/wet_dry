FROM rocker/geospatial:latest
MAINTAINER "Adam Mahood" adam.mahood@colorado.edu

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    awscli \
    cargo \
    htop 


    
RUN install2.r --error \
  assertthat \
  caret \
  caTools \
  doParallel \
  eseis \
  foreach \
  gdalUtils \
  ggthemes \
  httr \
  party \
  gganimate \
  gifski \
  picante \
  randomForest \
  ranger \
  rasterVis \
  rfUtilities \
  RCurl \
  ROCR \
  sf \
  snowfall \
  tidyverse \
  viridis

