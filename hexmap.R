#' Example of how to create a hexmap
#' 
#' 
#' 

library(sf)
library(dplyr)
library(ggplot2)

df <- read.csv("relative rates priority cohort.csv") # Data
talb_codes <- read.csv("talb_codes.csv")  # Regional codes
talb_sf <- st_read("territorial-authority-local-board-2025-clipped.shp") # Shapefile location type 2


df$val1 <- as.character(df$val1)
talb_codes$Location_Code <- as.character(talb_codes$Location_Code)

df <- talb_codes %>%
  left_join(df, talb_codes, by = c("Location_Code" = "val1"))


talb_map <- talb_sf %>%
  left_join(df, by = c("TALB2025_1" = "Location_Label"))


colours <- c("#c4161c",
             "#ed1c24",
             "#e8731b",
             "#26567f",
             "#2c86b4",
             "#3a9cae"
)

colours <- rev(colours)

# Hex map

talb_map <- st_transform(talb_map, 2193)

# create a point in each polygon 
pts <- talb_map %>%
  mutate(pt = st_point_on_surface(geometry)) %>%
  st_as_sf(sf_column_name = "pt")


if(st_is_longlat(talb_map)){
  talb_map <- st_transform(talb_map, 2193)
  pts <- st_transform(pts, 2193)
}

# create hex grid
hex <- st_make_grid(
  st_union(st_geometry(talb_map)),
  cellsize = 20000,
  square = FALSE
) %>%
  st_as_sf() %>%
  mutate(hex_id = row_number())

pt_hex <- st_join(pts, hex, join = st_intersects, left = TRUE)


# aggregate by hexes
hex_counts <- pt_hex %>%
  st_drop_geometry() %>%
  group_by(hex_id) %>%
  summarise(
    count = sum(Count, na.rm = TRUE),     # size driver
    rate  = mean(Relative.rate),  # colour driver
    .groups = "drop"
  )

hex_map <- hex %>%
  left_join(hex_counts, by = "hex_id")

#Normal one

ggplot(hex_map) +
  geom_sf(aes(fill = rate), color = "darkgrey", size = 0.3) +
  scale_fill_gradientn(
    colours = colours,
    na.value = "transparent",
    name = "Relative Rate"
  ) +
  theme(
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.background = element_rect(fill = "white")
  )







