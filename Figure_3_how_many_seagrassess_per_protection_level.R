library(tidyverse)


seag_data <- read_csv("https://raw.githubusercontent.com/CBMC-GCMP/seagrasses-goc-review/main/data/GC_Seagrasses_Redlist.csv")


seag_data %>% 
        group_by(Specie, Zone, Protection_level) %>% 
        count() %>% 
        group_by(Specie, Protection_level) %>% 
        summarise(n = mean(n)) %>% 
        mutate(PL = factor(Protection_level, 
                           levels = c("0", "0.5", "1"),
                           labels = c("Open area", "Multi-use", "Core zone"))) %>% 
        group_by(Specie) %>% 
        mutate(tot = sum(n), perc = n) %>% 
        ggplot(aes(x = reorder(Specie, tot), y = perc, fill = PL)) +
        geom_col() +
        coord_flip() +
        labs(x = "", y = "# of locations", fill = "Protection level") +
        theme_bw() +
        theme(panel.border = element_rect(fill = NA, color = "gray90"), 
              axis.text.y = element_text(face = "italic"), 
              legend.position = "bottom")


ggsave("figs/seagrasses_per_protection_number.png", dpi = 800)

seag_data %>% 
        group_by(Specie, Zone, Protection_level) %>% 
        count() %>% 
        group_by(Specie, Protection_level) %>% 
        summarise(n = mean(n)) %>% 
        mutate(PL = factor(Protection_level, 
                           levels = c("0", "0.5", "1"),
                           labels = c("Open area", "Multi-use", "Core zone"))) %>% 
        group_by(Specie) %>% 
        mutate(tot = sum(n), perc = n/tot) %>% 
        ggplot(aes(x = reorder(Specie, tot), y = perc, fill = PL)) +
        geom_col() +
        coord_flip() +
        labs(x = "", y = "% of locations", fill = "Protection level") +
        theme_bw() +
        theme(panel.border = element_rect(fill = NA, color = "gray90"), 
              axis.text.y = element_text(face = "italic"), 
              legend.position = "bottom")


ggsave("figs/seagrasses_per_protection_percent.png", dpi = 800)
