#### Setup
# Load packages
library(blackmarbler)
library(geodata)
library(sf)
library(terra)
library(ggplot2)
library(tidyterra)
library(lubridate)


#### Define NASA bearer token
token <- "eyJ0eXAiOiJKV1QiLCJvcmlnaW4iOiJFYXJ0aGRhdGEgTG9naW4iLCJzaWciOiJlZGxqd3RwdWJrZXlfb3BzIiwiYWxnIjoiUlMyNTYifQ.eyJ0eXBlIjoiVXNlciIsInVpZCI6ImVtbWFsb2QiLCJleHAiOjE3MjUyODM3OTgsImlhdCI6MTcyMDA5OTc5OCwiaXNzIjoiRWFydGhkYXRhIExvZ2luIn0.K9pdOmDzXT1hK45KjlbDlgLrGqBBONzrFZ7wvq56ndmvP05UBhUyfRTLMBbymNAvYxPtfd0VjTdgvu8NT4HyYAz4DPnJ24uayuZykJIUu0uBzHaZbwNyspoU8OKi6a_4HigTzkU2gU-CzPuWQ5au9iiOZnkUtOOjn1D2KiU242a8lnLvvQihfCF5edTj7L2SGfFqEXnE2ncwvlGeBKqmFZNCiw9lztPkXLIlnUjOkz4TBRtkdAHVSkM323s0LKhgYG2eBQCRyCc1l8E0eawYX-vYHm9qOe2GeqKOu8WY9swwCOCbf9RuQzCn9dUVzr0KPmrEMxAobejQk0gkVnEDQg"
bearer <- token


####  ITALY
### ROI
# Define region of interest (roi). The roi must be (1) an sf polygon and (2)
# in the WGS84 (epsg:4326) coordinate reference system. Here, we use the
# getData function to load a polygon of Ghana
roi_sf <- gadm(country = "ITA", level=1, path = tempdir()) 

### Daily data
r_20190601 <- bm_raster(roi_sf = roi_sf,
                        product_id = "VNP46A2",
                        date = "2019-06-01",
                        bearer = bearer)

r_20240510 <- bm_raster(roi_sf = roi_sf,
                        product_id = "VNP46A2",
                        date = "2024-05-10",
                        bearer = bearer)

#### Prep data - Italy cropping
r_20190601_prep <- r_20190601 |> terra::mask(roi_sf)
r_20240510_prep <- r_20240510 |> terra::mask(roi_sf)

## Distribution is skewed, so log - img enhancement
r_20190601_prep[] <- log(r_20190601_prep[] + 1)
r_20240510_prep[] <- log(r_20240510_prep[] + 1)

##### Map
ggplot() +
  geom_spatraster(data = r_20190601_prep) +
  scale_fill_gradient2(low = "black",
                       mid = "yellow",
                       high = "red",
                       midpoint = 4.5,
                       na.value = "transparent") +
  labs(title = "Nighttime Lights: June 2019") +
  coord_sf() +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "none")

ggplot() +
  geom_spatraster(data = r_20240510_prep) +
  scale_fill_gradient2(low = "black",
                       mid = "yellow",
                       high = "red",
                       midpoint = 4.5,
                       na.value = "transparent") +
  labs(title = "Nighttime Lights: May 2024") +
  coord_sf() +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "none")

#### Extract annual data
ntl_df <- bm_extract(roi_sf = roi_sf,
                     product_id = "VNP46A4",
                     date = 2019:2024,
                     bearer = bearer)

#### Trends over time
ntl_df |>
  ggplot() +
  geom_col(aes(x = date,
               y = ntl_mean),
           fill = "darkorange") +
  facet_wrap(~NAME_1) +
  labs(x = NULL,
       y = "NTL Luminosity",
       title = "Italy Admin Level 1: Annual Average Nighttime Lights") +
  scale_x_continuous(labels = seq(2019, 2024, 1),
                     breaks = seq(2019, 2024, 1)) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold")
        )


####  LOMBARDY
### ROI
# Define region of interest (roi). The roi must be (1) an sf polygon and (2)
# in the WGS84 (epsg:4326) coordinate reference system. Here, we use the
# getData function to load a polygon of Ghana
roi_sf <- gadm(country = "ITA", level=2, path = tempdir()) 

r<-roi_sf$GID_1=="ITA.10_1"
prova = roi_sf[r]

### Monthly data
r_201904 <- bm_raster(roi_sf = prova,
                        product_id = "VNP46A3",
                        date = "2019-04-01",
                        bearer = bearer)

r_202004 <- bm_raster(roi_sf = prova,
                      product_id = "VNP46A3",
                      date = "2020-04-01",
                      bearer = bearer)

r_202104 <- bm_raster(roi_sf = prova,
                      product_id = "VNP46A3",
                      date = "2021-04-01",
                      bearer = bearer)

r_202204 <- bm_raster(roi_sf = prova,
                      product_id = "VNP46A3",
                      date = "2022-04-01",
                      bearer = bearer)

r_202304 <- bm_raster(roi_sf = prova,
                      product_id = "VNP46A3",
                      date = "2023-04-01",
                      bearer = bearer)

r_202404 <- bm_raster(roi_sf = prova,
                      product_id = "VNP46A3",
                      date = "2024-04-01",
                      bearer = bearer)

#### Prep data - Italy cropping
r_201904_prep <- r_201904 |> terra::mask(prova)
r_202004_prep <- r_202004 |> terra::mask(prova)
r_202104_prep <- r_202104 |> terra::mask(prova)
r_202204_prep <- r_202204 |> terra::mask(prova)
r_202304_prep <- r_202304 |> terra::mask(prova)
r_202404_prep <- r_202404 |> terra::mask(prova)

## Distribution is skewed, so log - img enhancement
r_201904_prep[] <- log(r_201904_prep[] + 1)
r_202004_prep[] <- log(r_202004_prep[] + 1)
r_202104_prep[] <- log(r_202104_prep[] + 1)
r_202204_prep[] <- log(r_202204_prep[] + 1)
r_202304_prep[] <- log(r_202304_prep[] + 1)
r_202404_prep[] <- log(r_202404_prep[] + 1)

##### Map
ggplot() +
  geom_spatraster(data = r_201904_prep) +
  scale_fill_gradient2(low = "black",
                       mid = "yellow",
                       high = "red",
                       midpoint = 4.5,
                       na.value = "transparent") +
  labs(title = "Nighttime Lights: April 2019") +
  coord_sf() +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "right")

ggplot() +
  geom_spatraster(data = r_202004_prep) +
  scale_fill_gradient2(low = "black",
                       mid = "yellow",
                       high = "red",
                       midpoint = 4.5,
                       na.value = "transparent") +
  labs(title = "Nighttime Lights: April 2020") +
  coord_sf() +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "none")

ggplot() +
  geom_spatraster(data = r_202104_prep) +
  scale_fill_gradient2(low = "black",
                       mid = "yellow",
                       high = "red",
                       midpoint = 4.5,
                       na.value = "transparent") +
  labs(title = "Nighttime Lights: April 2021") +
  coord_sf() +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "none")

ggplot() +
  geom_spatraster(data = r_202204_prep) +
  scale_fill_gradient2(low = "black",
                       mid = "yellow",
                       high = "red",
                       midpoint = 4.5,
                       na.value = "transparent") +
  labs(title = "Nighttime Lights: April 2022") +
  coord_sf() +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "none")

ggplot() +
  geom_spatraster(data = r_202304_prep) +
  scale_fill_gradient2(low = "black",
                       mid = "yellow",
                       high = "red",
                       midpoint = 4.5,
                       na.value = "transparent") +
  labs(title = "Nighttime Lights: April 2023") +
  coord_sf() +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "none")

ggplot() +
  geom_spatraster(data = r_202404_prep) +
  scale_fill_gradient2(low = "black",
                       mid = "yellow",
                       high = "red",
                       midpoint = 4.5,
                       na.value = "transparent") +
  labs(title = "Nighttime Lights: April 2024") +
  coord_sf() +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "right")


#### Extract annual data
ntl_df <- bm_extract(roi_sf = prova,
                     product_id = "VNP46A4",
                     date = 2019:2024,
                     bearer = bearer)

#### Trends over time
ntl_df |>
  ggplot() +
  geom_col(aes(x = date,
               y = ntl_mean),
           fill = "darkorange") +
  facet_wrap(~NAME_2) +
  labs(x = NULL,
       y = "NTL Luminosity",
       title = "Italy Admin Level 1: Annual Average Nighttime Lights") +
  scale_x_continuous(labels = seq(2019, 2024, 1),
                     breaks = seq(2019, 2024, 1)) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold")
  )

### Monthly data
ntl_df_201904 <- bm_extract(roi_sf = prova,
                      product_id = "VNP46A3",
                      date = "2019-04-01",
                      bearer = bearer)

ntl_df_202004 <- bm_extract(roi_sf = prova,
                      product_id = "VNP46A3",
                      date = "2020-04-01",
                      bearer = bearer)

ntl_df_202104 <- bm_extract(roi_sf = prova,
                      product_id = "VNP46A3",
                      date = "2021-04-01",
                      bearer = bearer)

ntl_df_202204 <- bm_extract(roi_sf = prova,
                      product_id = "VNP46A3",
                      date = "2022-04-01",
                      bearer = bearer)

ntl_df_202304 <- bm_extract(roi_sf = prova,
                      product_id = "VNP46A3",
                      date = "2023-04-01",
                      bearer = bearer)

ntl_df_202404 <- bm_extract(roi_sf = prova,
                      product_id = "VNP46A3",
                      date = "2024-04-01",
                      bearer = bearer)

merged_df <- rbind(ntl_df_201904,ntl_df_202004,ntl_df_202104,ntl_df_202204,ntl_df_202304,ntl_df_202404)
merged_df$date<-as.Date(merged_df$date)
merged_df$year<-format(merged_df$date,"%Y")
merged_df$year<-as.numeric(merged_df$year)

#### Trends over time
merged_df |>
  ggplot() +
  geom_col(aes(x = year,
               y = ntl_mean),
           fill = "darkorange") +
  facet_wrap(~NAME_2) +
  labs(x = NULL,
       y = "NTL Luminosity",
       title = "Lombardy: Annual Average Nighttime Lights") +
  scale_x_continuous(labels = seq(2019, 2024, 1),
                     breaks = seq(2019, 2024, 1)) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold")
  )


### Export

r <- r_202404_prep

# Create a temporary filename for the example
f <- file.path("C:\\Users\\emmal\\OneDrive - Politecnico di Milano\\LM\\Earth Observation\\Project B/202404.tif")

writeRaster(r, f, overwrite=TRUE)

#writeRaster(r, f, overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES"), datatype='INT1U')
