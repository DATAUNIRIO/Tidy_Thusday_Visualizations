#https://github.com/thebioengineer/tidytuesdayR
#devtools::install_github("thebioengineer/tidytuesdayR")

#Import the Data

library(tidyverse)
library(gganimate)
library(waffle)

d <- tidytuesdayR::tt_load(2019, week = 50)

diseases <- d$diseases
murders <- d$international_murders
guns <- d$gun_murders
regents <- d$nyc_regents

#Part 1: Diseases
#The first plot was inspired by an interactive graphic from WSJ. The data for these visuals comes from the Tycho Project.
#To start, let’s add a column that provides a rate of disease incidence. The metrics will be in using of cases per 100,000 people per year.

disease_2 <- diseases %>% 
  group_by(year, state, disease) %>% 
  mutate(rate = count / population * 10000 * 52 / weeks_reporting) %>% 
  ungroup()

#Measles Plot with Vaccine
#The graphic from the blog post uses Measles. This is a particularly impactful graphic emphasizing the impact of the measles vaccine on the rate of measles occurence across US states. Rather than improve upon this already impressive graph, I chose to animate it instead. This uses the gganimate package. This animation only require an additional 3 lines of code. So fancy for such little effort!
  
jet.colors <- colorRampPalette(c("#F0FFFF", "cyan", "#007FFF", "yellow", 
                                   "#FFBF00", "orange", "red", "#7F0000"), bias = 2.25)


p_anim <- ggplot(disease_2 %>% filter(disease == "Measles"), aes(x = year, y = state, fill = rate)) + 
  geom_tile(color = "white", size = 0.35) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = jet.colors(16), na.value = 'white') +
  theme_minimal() +
  transition_time(year) + 
  shadow_mark() +
  annotate(geom = "text", x = 1963.5, y = 50.5, label = "Vaccine introduced", size = 5, hjust = 0) +
  ggtitle("Incidence of Meases (USA) \n1928-2011") +
  geom_vline(xintercept = 1963, col = "black") +
  labs(x = "Year", y = "State", fill = "Rate\n(Cases per 100,000)")

# uncomment to save animation
# anim_save(p_anim, width = 1000, height = 600, filename = "measles.gif")

animate(p_anim, width = 1000, height = 600)

#During the 2019-49 #TidyTuesday, I saw some waffle plots on Twitter. These had me intrigued, so I decided to play around with geom_waffle from the waffle library.
#For this visual, I filtered for a single state (Georgia) and limited the time frame from 1976 to 1995. With the waffle plot, you can see that there were upticks in measles cases in 1977, 1979, 1980, and 1990. From some quick web searching, there does appear to have been an outbreak of measles in GA in 1990.

disease_3 <- disease_2 %>% 
  filter(state == "Georgia" & year %in% c(1976:1995))

p_waffle <- ggplot(disease_3) +
  geom_waffle(aes(fill = disease, values = count), 
              color = "white", n_rows = 50, flip = TRUE) +
  facet_wrap(~year) +
  coord_equal() +
  ggtitle("Measles Outbreak in Geogia \n(1976-1995)") +
  theme_classic()

p_waffle

#As a comparison, I also made a line graph with the same data.

p_line <- ggplot(disease_3) +
  geom_line(aes(x = year,color = disease, y = count)) +
  theme_classic()

p_line


#Post Update

#geocacheR gave me some feedback on the animated plot. I like the suggestions, so I’d like to implement them. Key points were:
#Too many colors
#Difference between missing data and low vaccine incidence was unclear
#Change gridlines, horizonal ones aren’t helpful, but maybe adding vertical onces could help

disease_3 <- disease_2 %>%
  filter(state != "Hawaii" & state != "Alaska" & disease == "Measles") %>% 
  mutate(rate = case_when(is.nan(rate) ~ 0, TRUE ~ rate))

p_anim_2 <- ggplot(disease_3, aes(x = year, y = state, fill = rate)) +
  geom_tile(color = "white", size = 0.35) +
  scale_x_continuous(expand = c(0,0)) +
  # scale_fill_gradient2() +
  # scale_fill_gradientn(colors = jet.colors(16), na.value = 'white') +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  transition_time(year) +
  shadow_mark() +
  annotate(geom = "text", x = 1963.5, y = 48.5, label = "Vaccine introduced", size = 5, hjust = 0) +
  ggtitle("Incidence of Meases (USA) \n1928-2011") +
  geom_vline(xintercept = 1963, col = "black") +
  geom_vline(xintercept = seq(from = 1930, to = 2000, by = 10), col = "grey20", alpha = 0.2, linetype = "dashed") +
  labs(x = "Year", y = "State", fill = "Rate\n(Cases per 100,000)")

# new color scheme
p_anim_2a <- p_anim_2 + scale_fill_gradient2()
animate(p_anim_2a, width = 1000, height = 600, )

# I think I like the original colors, so here are the plot updates with those included.

# old color scheme
p_anim_2b <- p_anim_2 + scale_fill_gradientn(colors = jet.colors(16), na.value = 'white')
animate(p_anim_2b, width = 1000, height = 600)

