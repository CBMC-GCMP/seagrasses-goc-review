library(tidyverse)
library(rnaturalearth)
library(sf)
library(patchwork)


seag_data <- read_csv("https://raw.githubusercontent.com/CBMC-GCMP/seagrasses-goc-review/main/data/GC_Seagrasses_Redlist.csv")


spdf_mx <- st_transform(st_as_sf(ne_countries(scale = 'large', country = 'mexico')), crs = 4326)




seag_coord <- st_as_sf(seag_data, coords = c("Longitude", "Latitude"), crs = 4326)


(spp_map <- ggplot(seag_coord) +
                geom_sf(data = spdf_mx, 
                        fill = "gray90",
                        col = NA) +
                geom_sf(pch = 21, 
                        col = "black", 
                        aes(fill = Threat_index), 
                        size = 2, 
                        alpha = .6) +
                coord_sf(xlim = c(-116, -105), 
                         ylim = c(22, 32)) +
                facet_wrap(~Specie) +
                scale_fill_distiller(palette = "Spectral") +
                theme_bw() +
                theme(panel.grid = element_blank(), 
                      panel.border = element_rect(fill = NA, color = "gray90"),
                      strip.text = element_text(face = "italic"), 
                      strip.background = element_blank(), 
                      axis.text.x = element_text(angle = 90)))




seag_coord %>% 
        group_by(Red_red_list_category, Locality) %>% 
        count() %>% 
        arrange(-n) %>% 
        ggplot() +
        geom_sf() +
        facet_wrap(Red_red_list_category ~.,)


spp_map + inset_element(gen_map, 0.8, 0.8, 1, 1, align_to = 'full')


ggsave(filename = "figs/distribution_map.png", dpi = 600)

