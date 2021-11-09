library(tidyverse)


seag_data <- read_csv("https://raw.githubusercontent.com/CBMC-GCMP/seagrasses-goc-review/main/data/GC_Seagrasses_Redlist.csv")




seag_data %>% 
        group_by(Specie, Protection_level) %>% 
        count() %>% 
        mutate(PL = factor(Protection_level, 
                           levels = c("0", "0.5", "1"),
                           labels = c("Open area", "MPA", "NTZ"))) %>% 
        
        ggplot(aes(x = reorder(Specie, n), y = n, fill = PL)) +
        geom_col() +
        coord_flip() +
        labs(x = "", y = "# of records", fill = "Protection level") +
        theme_bw() +
        theme(panel.border = element_rect(fill = NA, color = "gray90"), 
              axis.text.y = element_text(face = "italic"), 
              legend.position = "bottom")


ggsave("figs/seagrasses_per_protection.png", dpi = 800)
