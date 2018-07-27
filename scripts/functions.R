# functions

# functions --------------------------------------------------------------------
# TODO: make new functions for landsat 5

bright7 <- function(band1,band2,band3,band4,band5,band7) {
  x <- (band1 *0.3561) +
    (band2 * 0.3972) + 
    (band3 * 0.3904) +
    (band4 * 0.6966) +
    (band5 * 0.2286) +
    (band7 * 0.1596)
  return(x)
}

green7 <- function(band1,band2,band3,band4,band5,band7){
  x <- (band1 *-0.3344) +
    (band2 * -0.3544) + 
    (band3 * -0.4556) +
    (band4 * 0.6966) +
    (band5 * -0.0242) +
    (band7 * -0.2630)
  return(x)
}

wet7 <- function(band1,band2,band3,band4,band5,band7){
  x <- (band1 * 0.2626) +
    (band2 * 0.2141) + 
    (band3 * 0.0926) +
    (band4 * 0.0656) +
    (band5 * -0.7629) +
    (band7 * -0.5388)
  return(x)
}

bright5 <- function(band1,band2,band3,band4,band5,band7) {
  x <- (band1 *0.2043) +
    (band2 * 0.4158) + 
    (band3 * 0.5524) +
    (band4 * 0.5741) +
    (band5 * 0.3124) +
    (band7 * 0.2303)
  return(x)
}

green5 <- function(band1,band2,band3,band4,band5,band7){
  x <- (band1 *-0.1603) +
    (band2 * -0.2819) + 
    (band3 * -0.4934) +
    (band4 * 0.7940) +
    (band5 * 0.0002) +
    (band7 * -0.1446)
  return(x)
}

wet5 <- function(band1,band2,band3,band4,band5,band7){
  x <- (band1 * 0.0315) +
    (band2 * 0.2021) + 
    (band3 * 0.3102) +
    (band4 * 0.1594) +
    (band5 * 0.6806) +
    (band7 * -0.6109)
  return(x)
}

bright8 <- function(band2,band3,band4,band5,band6,band7) {
  x <- (band2 *0.3029) +
    (band3 * 0.2786) + 
    (band4 * 0.4733) +
    (band5 * 0.5599) +
    (band6 * 0.508) +
    (band7 * 0.1872)
  return(x)
}

green8 <- function(band2,band3,band4,band5,band6,band7){
  x <- (band2 *-0.2941) +
    (band3 * -0.243) + 
    (band4 * -0.5424) +
    (band5 * 0.7276) +
    (band6 * 0.0713) +
    (band7 * -0.1608)
  return(x)
}

wet8 <- function(band2,band3,band4,band5,band6,band7){
  x <- (band2 * 0.1511) +
    (band3 * 0.1973) + 
    (band4 * 0.3283) +
    (band5 * 0.3407) +
    (band6 * -0.7117) +
    (band7 * -0.4559)
  return(x)
}

get_ndvi <- function(band3, band4){
  return((band4 - band3)/ (band4 + band3))}
get_evi <- function(band1,band3,band4){
  return(2.5 * ((band4 - band3)/(band4 + (6 * band3) - (7.5 * band1) + 1)))
}
get_savi <- function(band3, band4){
  return(((band4 - band3) / (band4 + band3 + 0.5)) * 1.5)}
get_sr <- function(band3,band4){return(band4/band3)}

get_satvi <- function(band3, band5, band7, L = 0.5) {
  x= (band5 - band3) / (band5 + band3 + L)
  y= x  * (1 + L) 
  z= y - (band7 / 2)
  return(y)
}

get_ndsvi <- function(band3, band5) {
  x = (band5 - band3) / (band5 + band3)
  return(x)
}
