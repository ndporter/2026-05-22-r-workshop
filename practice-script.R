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

