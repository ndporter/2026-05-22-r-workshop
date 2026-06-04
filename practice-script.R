#test script
dir.create('data')
dir.create('data_output')
dir.create('fig_output')

# first half-day -----

#download data
#download.file("https://raw.githubusercontent.com/datacarpentry/r-socialsci/main/episodes/data/SAFI_clean.csv", "data/SAFI_clean.csv", mode = "wb")

#install.packages("tidyverse")
library(tidyverse)
library(here)

#create an object
area_hectare <- 1.0
area_acres <- 2.47 * area_hectare
area_hectare <- 2.5
area_acres #has not changed

a <- 9
b <- sqrt(a) #a must be assigned (created) before this works without error
b

round(3.14159, digits=3)

hh_members <- c(3, 7, 10, 6) #number of household members
resp_wall_type <- c("muddaub", "burntbricks", "sunbricks")



# Data!!! -----------------------------------------------------------------

library(tidyverse)

interviews <- read_csv("data/SAFI_clean.csv", na="NULL")

class(interviews)

dim(interviews)
head(interviews)
summary(interviews)
glimpse(interviews)

interviews[1, 1]
interviews[1, 2] #1st row, 2nd column
interviews[2,1]
interviews[4:6, 7]
interviews[3, ]
interviews[,7]
interviews["rooms"]
names(interviews)
interviews$rooms

villages <- factor(interviews$village)
str(villages)

# dplyr -----

#select "selcts" columns from a tibble/dataframe
select(interviews, village, no_membrs, months_lack_food, rooms)
select(interviews, village:respondent_wall_type)

#filter
filter(interviews, village=="Chirodzo", no_membrs>2)

#using the pipe to chain dplyr verbs
interviews |> 
  filter(village=="Chirodzo") |> 
  select(!village)

#won't work (removes column before using)
# interviews |> 
#   select(!village) |>
#   filter(village=="Chirodzo")

interviews |> 
  filter(!is.na(memb_assoc)) |> 
  mutate(people_per_room = no_membrs / rooms) |> 
  glimpse()

output <- interviews |> 
  filter(!is.na(memb_assoc)) |> 
  group_by(village, memb_assoc) |> 
  summarize(mean_no_membrs = mean(no_membrs),
            min_membrs = min(no_membrs),
            count = n()) |> 
  arrange(desc(min_membrs)) |> 
  ungroup()

str(output)

interviews |> 
  count(village)

# ggplot2 -----

library(tidyverse)
interviews_plotting <- read_csv("https://raw.githubusercontent.com/datacarpentry/r-socialsci/main/episodes/data/interviews_plotting.csv")

write_csv(interviews_plotting,
          file='data_output/interviews_plotting.csv')

plot(interviews_plotting$no_membrs,
     interviews_plotting$liv_count,
     main = "Base R Scatterplot",
     xlab = "Num of hh members",
     ylab = "Num of livestock owned"
     )

interviews_plotting |> 
  ggplot(aes(x=no_membrs, y=number_items)) +
  geom_point(alpha=0.5)

interviews_plotting |> 
  ggplot(aes(x=no_membrs, y=number_items)) +
  geom_jitter()

interviews_plotting |> 
  ggplot(aes(x=no_membrs, y=number_items)) +
  geom_jitter(alpha=0.5,
              color="maroon",
             width = 0.2,
             height=0.2)

interviews_plotting |> 
  ggplot(aes(x=no_membrs, y=number_items)) +
  geom_jitter(aes(color=village),
              alpha=0.5,
              width = 0.2,
              height=0.2)

interviews_plotting |> 
  ggplot(aes(x=no_membrs, y=number_items, color=village)) +
  geom_jitter(alpha=0.5,
              width = 0.2,
              height=0.2)

interviews_plotting |> 
  filter(respondent_wall_type!="cement") |> 
  ggplot(aes(x=respondent_wall_type, y=rooms)) +
  geom_boxplot(outliers=FALSE) +
  geom_jitter(alpha=0.5,
              color='tomato',
              width=0.2,
              height=0.2)

interviews_plotting |> 
  ggplot(aes(x=respondent_wall_type)) +
  geom_bar(aes(fill=village),
           position='dodge')

percent_wall_type <- interviews_plotting |> 
  filter(respondent_wall_type!="cement") |> 
  count(village, respondent_wall_type) |> 
  group_by(village) |> 
  mutate(percent = (n/sum(n))*100) |> 
  ungroup()
  
percent_wall_type |> 
  ggplot(aes(x=village, y=percent, fill=respondent_wall_type)) +
  geom_bar(stat='identity', position='dodge') +
  labs(title='Proportion of wall type by village',
       fill = "Type of wall in home",
       x="Village",
       y="Percent")

barplot <- percent_wall_type |> 
  ggplot(aes(y=respondent_wall_type, x=percent)) +
  geom_bar(stat='identity', position='dodge') +
  facet_wrap(~ village) +
  theme_bw() +
  theme(panel.grid=element_blank())

ggsave("fig_output/barplot.jpg", plot=barplot, width=15, height=10)
