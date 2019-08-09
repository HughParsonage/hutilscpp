library(ggplot2)
library(magrittr)
library(data.table)
library(hutils)

euclid_distance <- function(x, y, w, z) sqrt((x - w)^2 + (y - z)^2)

DT <- CJ(Lat = -89:89, Lat2 = seq(-80, 80, by = 0.5), Lon = 0:179, Lon2 = seq(178, 0, length.out = 150))
DT[, haversine_lon1 := haversine_distance(Lat, Lon, Lat, Lon + 1)]
DT[, euclid_lon1 := euclid_distance(Lat, Lon, Lat, Lon + 1)]

DT[, haversine_lat1 := haversine_distance(Lat, Lon, Lat + 1, Lon)]
DT[, euclid_lat1 := euclid_distance(Lat, Lon, Lat + 1, Lon)]

DT[, haversine_latlon1 := haversine_distance(Lat, Lon, Lat + 1, Lon + 1)]
DT[, euclid_latlon1 := euclid_distance(Lat, Lon, Lat + 1, Lon + 1)]

DT[, haver_over_euclid_lon1 := haversine_lon1 / euclid_lon1]
DT[, haver_over_euclid_lat1 := haversine_lat1 / euclid_lat1]
DT[, haver_over_euclid_latlon1 := haversine_latlon1 / euclid_latlon1]

ggplot(DT[Lon == 0L],
       aes(Lat, haver_over_euclid_lon1)) +
  geom_line() +
  coord_flip()

ggplot(DT[Lon == 0L],
       aes(Lat, haver_over_euclid_latlon1)) +
  geom_line() +
  coord_flip()

ggplot(NULL) +
  geom_line(data = DT[Lon == 0L],
            aes(Lat, haver_over_euclid_latlon1),
            color = "red") +
  geom_line(data = DT[Lon == 0L],
            aes(Lat, haver_over_euclid_lon1),
            color = "blue") +
  geom_blank(aes(x = 0:1, y = c(0, 120))) +
  coord_flip()

ggplot(DT[Lat == 1L],
       aes(Lon, haver_over_euclid_lat1)) +
  geom_line()

ggplot(DT[Lat == 1L],
       aes(Lon, haver_over_euclid_latlon1)) +
  geom_line()

