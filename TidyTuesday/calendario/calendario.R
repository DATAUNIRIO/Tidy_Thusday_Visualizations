##================================================================================================
##                                                                                              
##    Nome:                                           
##                                                    
##    site: https://github.com/pabrodez/tidytuesday/blob/master/2019-07-16/script.R
##          https://twitter.com/pabrodez/status/1153042930535473157?s=03
##    Objetivo:
##    prof. Steven Dutt-Ross                          
##    UNIRIO           
##================================================================================================


library(tidyverse)
library(lubridate)
library(ggthemes)
library(grid)

r4ds_members <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-16/r4ds_members.csv")

df <-
  r4ds_members %>%  ## inspiration from https://github.com/andriy-gazin/geowaffle/blob/master/geowaffle.R
  select(date,
         messages_in_public_channels,
         messages_in_private_channels,
         messages_in_d_ms) %>%
  mutate(date = floor_date(date, "month")) %>%
  gather("type_msg", "n_msg", -date) %>%
  group_by(date, type_msg) %>%
  summarise(n_msg = sum(n_msg)) %>%
  mutate(total_msg = sum(n_msg)) %>%
  group_by(type_msg, add = TRUE) %>%
  summarise(prop_msg = floor(n_msg / total_msg * 100)) %>%
  group_by(type_msg, add = TRUE) %>%
  group_modify(.f = ~ slice(.x, rep(1, .x$prop_msg))) %>%
  select(-prop_msg) %>%
  arrange(date, type_msg) %>%
  ungroup() %>%
  group_by(date) %>%
  group_modify(.f = ~ {
    bind_cols(type_msg = .x$type_msg, head(expand.grid(x = 1:10, y = 1:10), nrow(.x)))
  }) %>%
  ungroup() %>%
  mutate(date = factor(strftime(date, "%b %y"),
                       levels = strftime(seq.Date(
                         min(.$date), max(.$date), "months"
                       ), "%b %y")))


plot_r4ds <- ggplot(df, aes(x = x, y = y, fill = type_msg)) +
  geom_tile(alpha = .5) +
  facet_wrap( ~ date) +
  coord_fixed(ratio = 1.5 / 1) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 12)) +
  scale_fill_solarized(
    label = c("Direct\nmessages", "Private\nchannels", "Public\nchannels"),
    guide = guide_legend(
      title = NULL,
      label.position = "bottom",
      label.hjust = 0
    )
  ) +
  labs(title = "How does the R4DS community message?",
       caption = "Data: R4DS tidytuesday | Graphic: @pabrodez") +
  theme_void() +
  theme(
    text = element_text(color = "#6b634e", family = "Ubuntu Mono"),
    panel.background = element_rect(fill = "#ffefbf"),
    plot.background = element_rect(fill = "#ffefbf"), 
    legend.direction = "horizontal",
    legend.spacing.x = unit(.1, "cm"),
    legend.key.height = unit(.1, "cm"),
    legend.position = c(.5, 1.1),
    strip.text = element_text(hjust = 0.1, vjust = 1, size = 10),
    plot.margin = margin(.5, 1, .5, 1, unit = "cm"),
    plot.title = element_text(margin = margin(t = 1, b = 4, unit = "cm"), hjust = .5, size = 16, face = "bold"),
    plot.caption = element_text(margin = margin(t = 2, unit = "cm"), size = 10)
  )

plot_r4ds <- ggplotGrob(plot_r4ds)  ## code from https://stackoverflow.com/questions/48199791/rounded-corners-in-ggplot2
bg <- plot_r4ds$grobs[[1]]
round_bg <- roundrectGrob(x=bg$x, y=bg$y, width=bg$width, height=bg$height,
                          r=unit(0.1, "snpc"),
                          just=bg$just, name=bg$name, gp=bg$gp, vp=bg$vp)
plot_r4ds$grobs[[1]] <- round_bg

ggsave("2019_07_16.png", plot_r4ds, height = 29, width = 21, units = "cm", dpi = "retina")
