library(tidyverse)
library(rnaturalearth)
library(sf)



seag_data <- source("https://raw.githubusercontent.com/CBMC-GCMP/seagrasses-goc-review/main/data/GC_Seagrasses_Redlist.csv")

spdf_mx <- st_transform(st_as_sf(ne_countries(scale = 'large', country = 'mexico')), crs = 4326)




seag_coord <- st_as_sf(seag_data, coords = c("Longitude", "Latitude"), crs = 4326)


ggplot(seag_coord) +
        geom_sf(data = spdf_mx, 
                fill = "gray90",
                col = NA) +
        geom_sf() +
        coord_sf(xlim = c(-116, -105), 
                 ylim = c(20, 32)) +
        facet_wrap(~Specie) +
        theme_bw() +
        theme(strip.text = element_text(face = "italic"), 
              strip.background = element_blank(), 
              axis.text.x = element_text(angle = 90))

ggsave("figs/distribution_map.png", dpi = 600)






