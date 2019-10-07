##================================================================================================
##                                                                                              
##    Nome: Bar Chart Races With gganimate                                          
##                                                    
##    site: https://emilykuehler.github.io/bar-chart-race/?source=post_page
##          https://github.com/emilykuehler/tidytuesday/blob/master/grand-slam-tennis/tennis.R
##
##    Objetivo:
##    prof. Steven Dutt-Ross                          
##    UNIRIO           
##================================================================================================
library(tidyverse); library(gganimate)

grand_slams <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slams.csv")


grand_slams_clean <- grand_slams %>% 
  mutate(tournament_order = case_when(grand_slam=='australian_open' ~ 1,
                                      grand_slam=='french_open' ~ 2,
                                      grand_slam=='wimbledon' ~ 3,
                                      grand_slam=='us_open' ~ 4)) %>%
  arrange(tournament_date)

head(grand_slams_clean)


# Next, we’ll go through a two step process to ultimately prepare our data for gganimate. 
# As noted before, we are going to start in 1975 because of the multiple ties that 
# existed in previous years. The grouping for 1975 will consist of the players with the 
# top 10 rolling wins from 1968 through 1975 (i.e. all the tournaments of 1975). 
# We will label this init_df. After this each grouping will be a point in time, which 
# is after a single grand slam tournament.

#get data from 1968-1975, helps avoid ties, incomplete bar chart at beginning
#basically just making this more visually appealling
init_df <- grand_slams_clean %>%
  filter(year <= 1975) %>%
  group_by(name) %>%
  filter(rolling_win_count==max(rolling_win_count)) %>%
  ungroup() %>%
  top_n(10, wt=rolling_win_count) %>%
  arrange(desc(rolling_win_count)) %>%
  select(name,gender, rolling_win_count) %>%
  mutate(curr_year = 1975,
         ordering = as.double(rev(seq(10:1))) * 1.0)

#outer loop gets year
for (i in 1976:2019) {
  #inner loop gets tournament
  for (j in 1:4) {
    tmp_df <- grand_slams_clean %>%
      #filter data up to correct point in time
      filter(year < i | (year==i & tournament_order <= j)) %>%
      #get each players max win count
      group_by(name) %>% 
      filter(rolling_win_count==max(rolling_win_count)) %>% 
      ungroup() %>% 
      top_n(10, wt=rolling_win_count) %>%
      select(name, gender, rolling_win_count) %>%
      arrange(desc(rolling_win_count)) %>%
      slice(1:10) %>%
      #add var for curr_year, ordering for easy bar chart (reverse it cuz we're gonna do horiz)
      mutate(curr_year = i,
             tournament_num = j,
             ordering = as.double(rev(seq(10:1))) * 1.0) 
    init_df <- init_df %>%
      bind_rows(tmp_df)
  }
}

head(init_df)


#So this is starting to look pretty good. However, when we put the data into gganimate, 
# we want each frame to transition after a grand slam tournament. 
#Right now, we have the variables curr_year, however there are four tournaments 
#in a year, so that transition is too long, and tournament_num, which identifies 
#tournaments correctly, but not uniquely. But, we can use these variables to create 
#a unique id and easily plug that into gganimate.

final_df <- init_df %>% 
  group_by(curr_year, tournament_num) %>% 
  mutate(frame_id = group_indices()) %>% 
  ungroup() %>% 
  head()

#Before diving into our plot, let’s take a look at player names, which we’re going to be using as labels on the bars.

unique(final_df$name)

final_df <- final_df %>% mutate(name = ifelse(name == 'Evonne Goolagong Cawley', 'Evonne Goolagong', name))

# The Fun Part!
  
#  Ok, now that we have our data in the right format, we can go ahead and make our plot. The first thing we’ll do is set our theme, palette and font.

my_font <- 'Quicksand'
my_background <- 'antiquewhite'
my_pal <- c('#F8AFA8','#74A089') #colors for bars (from wesanderson)
my_theme <- my_theme <- theme(text = element_text(family = my_font),
                              rect = element_rect(fill = my_background),
                              plot.background = element_rect(fill = my_background, color = NA),
                              panel.background = element_rect(fill = my_background, color = NA),
                              panel.border = element_blank(),
                              plot.title = element_text(face = 'bold', size = 20),
                              plot.subtitle = element_text(size = 14),
                              panel.grid.major.y = element_blank(),
                              panel.grid.minor.y = element_blank(),
                              panel.grid.major.x = element_line(color = 'grey75'),
                              panel.grid.minor.x = element_line(color = 'grey75'),
                              legend.position = 'none',
                              plot.caption = element_text(size = 8),
                              axis.ticks = element_blank(),
                              axis.text.y =  element_blank())

theme_set(theme_light() + my_theme)

#Ultimately we’re going to have two bar colors, one for male players and one for female players and then otherwise set a relatively minimal theme (doing my best to replicate John Burn-Murdoch’s design, though with a different color scheme.

#------------------------------------------------
#Bar Chart Race Attempt 1
#------------------------------------------------

barplot_race_blur <- ggplot(aes(ordering, group = name), data = final_df) +
  geom_tile(aes(y = rolling_win_count / 2, 
                height = rolling_win_count,
                width = 0.9, fill=gender), alpha = 0.9) +
  scale_fill_manual(values = my_pal) +
  geom_text(aes(y = rolling_win_count, label = name), family=my_font, nudge_y = -2, size = 3) +
  geom_text(aes(y = rolling_win_count, label = rolling_win_count), family=my_font, nudge_y = 0.5) +
  geom_text(aes(x=1,y=18.75, label=paste0(curr_year)), family=my_font, size=8, color = 'gray45') +
  coord_cartesian(clip = "off", expand = FALSE) +
  coord_flip() +
  labs(title = 'Most Grand Slam Singles Championships',
       subtitle = 'Open Era Only',
       caption = 'data source: Wikipedia | plot by @emilykuehler',
       x = '',
       y = '') +
  transition_states(frame_id, 
                    transition_length = 4, state_length = 3) +
  ease_aes('cubic-in-out')

# So this animation is generally doing what we want, with one huge exception. During the transitions, we see trailing decimals on the labels, which looks awful. My first attempt at fixing this was to use various rounding strategies, such as flooring the win count. This did not work. What we have to do is change the data type of the label to character in order to get rid of the trailing decimals.

#------------------------------------------------
# Final Plot
#------------------------------------------------

barplot_race_blur <- ggplot(aes(ordering, group = name), data = final_df) +
  geom_tile(aes(y = rolling_win_count / 2, 
                height = rolling_win_count,
                width = 0.9, fill=gender), alpha = 0.9) +
  scale_fill_manual(values = my_pal) +
  geom_text(aes(y = rolling_win_count, label = name), family=my_font, nudge_y = -2, size = 3) +
  #convert to character to get rid of blurry trailing decimals
  geom_text(aes(y = rolling_win_count, label = as.character(rolling_win_count)), family=my_font, nudge_y = 0.5) +
  geom_text(aes(x=1,y=18.75, label=paste0(curr_year)), family=my_font, size=8, color = 'gray45') +
  coord_cartesian(clip = "off", expand = FALSE) +
  coord_flip() +
  labs(title = 'Most Grand Slam Singles Championships',
       subtitle = 'Open Era Only',
       caption = 'data source: Wikipedia | plot by @emilykuehler',
       x = '',
       y = '') +
  transition_states(frame_id, 
                    transition_length = 4, state_length = 3) +
  ease_aes('cubic-in-out')

animate(barplot_race_blur, nframes = 1000, fps = 30, width = 600, height = 400, res=80, detail = 3)
#anim_save("barplot_race_blur.gif")
