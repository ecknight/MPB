# ---
# title: MPB project - get some GIS
# author: Elly Knight & Emily Swerdfager
# created: Oct 25, 2024
# ---

#NOTES################################


#PREAMBLE############################

#1. Load packages----
library(tidyverse) #basic data wrangling
library(sf) #work with polygons
library(terra) #for the crs function

#2. Set GD root path----
root <- "G:/.shortcut-targets-by-id/1vozK1hbdXP5HCPjjn4rkaDt7Y2MqqQbM/MPB"

#3. Get the points----
dat <- read.csv(file.path(root, "All_MPB_Locations.csv"))

#4. Turn off sci not----
options(scipen=99999)

#WRANGLING###############

#1. Check----
ggplot(dat) + 
  geom_point(aes(x=longitude, y=latitude, colour = factor(year))) +
  facet_wrap(~factor(year))

#2. Make a sf object----
pts <- st_as_sf(dat, coords=c("longitude", "latitude"), crs=4326, remove=FALSE) |> 
  st_transform(3400)

#3. Buffer our object----
buff <- st_buffer(pts, 300) |> 
  mutate(area = as.numeric(st_area(geometry)))

#4. Take a sample for testing----
# buff <- buff |> 
#   sample_n(10)

#PINE#########

#1. Read it in----
pine <- read_sf(file.path(root, "purePine", "purePine_AB.shp"))

#2. Look at it----
str(pine)

#3. Get the pine----
pts.pine <- buff |> 
  st_transform(crs(pine)) |> #match the projections
  st_intersection() |> #get the unioned polygon
  mutate(area_purepine = as.numeric(st_area(geometry))) |> #get the area of each pine*buffer poly
  st_drop_geometry() |> 
  group_by(location, latitude, longitude, year, area) |> #pick the columsn you want to retain and group by them (location level)
  summarize(area_purepine = sum(area_purepine)) |> #sum the pine polys
  ungroup() |> 
  mutate(prop_purepine = area_purepine/area) #calculate proportion

#4. Plot----
ggplot(pts.pine) +
  geom_histogram(aes(x=prop_purepine))

#WRR###########

#1. Look at the layers----
st_layers(file.path(root, "WRR Areas_Emily Swerdfager_2024-01-19.gdb"))

#2. Read it in----
#get rid of the one weird polygon and then make the rest valid
wrr <- read_sf(file.path(root, "WRR Areas_Emily Swerdfager_2024-01-19.gdb"), "WRR_Complete") |> 
  dplyr::filter(!is.na(st_is_valid(read_sf(file.path(root, "WRR Areas_Emily Swerdfager_2024-01-19.gdb"), "WRR_Complete")))) |> 
  st_make_valid()

#3. Look at it----
str(wrr)

ggplot(wrr) +
  geom_sf(aes(fill=Treatment_Name))

#4. Get the patch size----
pts.wrr <- pts |> #use points not buffer
  st_transform(crs(wrr)) |> #match the projections
  st_join(wrr) |> #get the spatial join because don't care about the polygon union geometry and want to retain the columns from wrr
  st_drop_geometry() |> 
  rename(area_harvest = Shape_Area) |> 
  dplyr::select(location, latitude, longitude, area_harvest)
