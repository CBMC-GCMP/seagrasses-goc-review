library(ggplot2)
library(tidyverse)
library(ggthemes)

df<-read.csv("data/Review_base - copia.csv")

df %>% 
        mutate(topic = tools::toTitleCase(topic)) %>% # Up until here it is all calculations to create the new dataframe
        ggplot(aes(x=decade, fill = topic, colour= topic)) +
        geom_histogram(breaks=seq(1910, 2020, by=10)) + 
        labs(title= "Seagrasses research in the Gulf of California", 
             x="Decade", 
             y="Number of publications", 
             fill="",
             col="") +
        scale_colour_viridis_d() +
        scale_fill_viridis_d() +
        xlim(c(1910, 2020)) +
        ylim(c(0,25)) +
        theme_bw() +
        theme(panel.grid = element_blank(),
              legend.position = "bottom")

ggsave("figs/research_figure.png", dpi = 600, height = 5, width = 9)

