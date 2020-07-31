library(here)
library(tidyverse)
library(waffle)
library(extrafont)
library(RColorBrewer)
library(ggtext)
library(glue)


# Load data

animal_outcomes <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_outcomes.csv')


rehomed_animals <- animal_outcomes %>%
  filter(outcome == "Rehomed" | outcome == "Reclaimed") %>%
  filter(year >= 2000 & year<= 2015) %>%
  count(animal_type,year, wt = Total) %>%
  mutate(n = n/1000,
         year = as_factor(year)
  )


#Waffle Plot

pal <- brewer.pal(12, "Set3")


plot <- rehomed_animals %>% 
  ggplot(aes(fill = animal_type, values = n)) +
  geom_waffle(n_rows = 10,size = 2, colour = pal[2], flip = TRUE) +
  facet_wrap(~year, nrow = 5, strip.position = "bottom") +
  scale_fill_manual(
    name = NULL,
    values = c(pal[4], pal[6], pal[5], pal[10]),
  ) +
  theme(plot.background = element_rect(fill = pal[2]),
        strip.background = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 100,
                                  lineheight = 0.5),                           
        plot.subtitle = element_markdown(margin = margin(0,0,10,0)),
        text = element_text(family = "Agency FB",
                            size = 50),
        plot.caption = element_textbox(
          lineheight = 1,  
          padding = margin(10, 10, 10, 10),  
          halign = 0.5,  
          valign = 0.5,
          hjust = 0.5)
  ) +
  labs(
    title = "Which Animals went home?",
    subtitle = glue("Number of ",
                    "<span style = color:{pal[4]}>Cats, </span>",
                    "<span style = color:{pal[6]}>Dogs, </span>",
                    "<span style = color:{pal[5]}>Livestock ,</span>",
                    "and ",
                    "<span style = color:{pal[10]}>Other Animals </span>",
                    "that were either reclaimed or rehomed by RSPCA Australia from 2000 to 2015",
                    "<br> <span style = font-size:40pt>",
                    "Each box represents 1000 animals and fewer than 1000 horses were homed in any year",
                    "</span>"
    ),
    caption = paste0(
      "**Data**: RSPCA - Australia |",
      " **Image**: @AndrewJamesMott |",
      "#TidyTuesday Week 30 2020"
    )
  )

ggsave("Week 30.png",plot = plot, path = here("2020","week 30"), device = "png",
       width = 1200, height = 600, units = "mm")
