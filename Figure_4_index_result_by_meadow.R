library(tidyverse)
library(rnaturalearth)
library(sf)
library(patchwork)


seag_data <- read_csv("https://raw.githubusercontent.com/CBMC-GCMP/seagrasses-goc-review/main/data/GC_Seagrasses_Redlist.csv")


spdf_mx <- st_transform(st_as_sf(ne_countries(scale = 'large', country = 'mexico')), crs = 4326)




seag_coord <- st_as_sf(seag_data, coords = c("Longitude", "Latitude"), crs = 4326)

seag_coord %>% 
        group_by(Zone, Specie) %>% 
        summarise(TI = mean(Threat_index)) %>% 
        ggplot(aes(x=TI)) +
        geom_density(aes(fill = Specie), 
                     alpha =.5) +
        xlim(0,1) +
        scale_fill_brewer(palette = "Set3") +
        theme_bw() +
        theme(panel.grid = element_blank(), 
              panel.border = element_rect(fill = NA, color = "gray90"),
              strip.text = element_text(face = "italic"), 
              strip.background = element_blank())

ggsave(filename = "figs/TI_density_fig.png", dpi = 600)

(spp_map <- seag_coord %>% 
                group_by(Zone, Specie) %>% 
                summarise(TI = mean(Threat_index)) %>% 
                ggplot() +
                geom_sf(data = spdf_mx, 
                        fill = "gray90",
                        col = NA) +
                geom_sf(pch = 21, 
                        col = "black", 
                        aes(fill = TI), 
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

ggsave(filename = "figs/TI_distribution_map.png", dpi = 600)


unique(seag_coord$Red_red_list_category)

(IUCN_map <- seag_coord %>% 
        group_by(Zone, Specie) %>% 
        summarise(TI = mean(Threat_index)) %>% 
        mutate(IUCN = cut(TI, 
                          breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
                          labels = c("LC", "NT", "VU", "EN", "CO"))) %>% 
        ggplot() +
        geom_sf(data = spdf_mx, 
                fill = "gray90",
                col = NA) +
        geom_sf(pch = 21, 
                col = "black", 
                aes(fill = IUCN), 
                size = 2, 
                alpha = .6) +
        coord_sf(xlim = c(-116, -105), 
                 ylim = c(22, 32)) +
        facet_wrap(~Specie) +
        scale_fill_manual(values = c("green", "yellow", "orange", "red", "black")) +
        theme_bw() +
        theme(panel.grid = element_blank(), 
              panel.border = element_rect(fill = NA, color = "gray90"),
              strip.text = element_text(face = "italic"), 
              strip.background = element_blank(), 
              axis.text.x = element_text(angle = 90)))

seag_coord %>% 
        filter(Zone == "Los Cabos") %>% View()
IUCN_barplot <- seag_coord %>% 
        group_by(Zone, Specie) %>% 
        summarise(TI = mean(Threat_index)) %>% 
        mutate(IUCN = cut(TI, 
                          breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
                          labels = c("LC", "NT", "VU", "EN", "CO"))) %>% 
        group_by(IUCN, Specie) %>% 
        count() %>% 
        ggplot(aes(x = reorder(Specie, n), y = n, fill = IUCN)) +
        geom_col(col = "black") +
        coord_flip() +
        scale_fill_manual(values = c("green", "yellow", "orange", "red", "black")) +
        labs(x = "", y = "# of locations") +
        theme_bw() +
        theme(panel.border = element_rect(fill = NA, color = "gray90"), 
        axis.text.y = element_text(face = "italic"), 
        legend.position = "")
(IUCN_barplot| (IUCN_map + inset_element(gen_map, 0.8, 0.8, 1, 1, align_to = 'full'))) 

ggsave(filename = "figs/fill_IUCN_map.png", dpi = 600)

