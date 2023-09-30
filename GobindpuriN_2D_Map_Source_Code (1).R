library("tidyverse")
library("geojsonsf")
library("mapview")
library("rayshader")
library("magick")
library("biscale")
library("cowplot")

food_desert <- read.csv("Limited_Access_to_Healthy_Food.csv",
                            colClasses=c("Census.Tract"="character"))

food_desert2 <- food_desert %>%
  mutate(Census.Tract = as.double(Census.Tract))



census_tracts <- geojson_sf("https://services8.arcgis.com/rGGrs6HCnw87OFOT/arcgis/rest/services/People_of_Color_v2/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

census_tracts2 <- census_tracts %>%
  mutate(Census_Tract = as.double(Census_Tract))

joined_data <- census_tracts2 %>%
  left_join(food_desert2, by = c("Census_Tract" = "Census.Tract"))

Joined_clean <- joined_data %>%
  filter(is.na(Score.Limited.Access.to.Healthy.Food.Retailers) != TRUE & 
           Score.Limited.Access.to.Healthy.Food.Retailers != "--") %>%
  mutate(People_of_Color = as.numeric(Percent_People_of_Color),
         Access_Healthy_Foods = as.numeric(Score.Limited.Access.to.Healthy.Food.Retailers)) 

data <- bi_class(Joined_clean, x = People_of_Color, y = Access_Healthy_Foods, style = "equal", dim = 3)

map <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkViolet", dim = 3) +
  labs(
    title = "% People of Color and Limited Access To Healthy Foods in Washington State",
    subttitle = "Dark Violet (DkViolet) Palette"
  ) +
  bi_theme()

legend <- bi_legend(pal = "DkViolet",
                    dim = 3,
                    xlab = "% People of Color ",
                    ylab = "% Limited Access To Healthy Foods ",
                    size = 8)

finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.2, .65, 0.2, 0.2)

plot(finalPlot)

