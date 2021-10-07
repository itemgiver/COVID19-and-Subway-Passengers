library(dplyr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(readxl)
library(tidyverse)
library(gridExtra)
library(ggpubr)

##### Part 1: Analyze by each station
# Load subway dataset
subway <- read.csv("seoul_subway_by_time.csv", header = T, fileEncoding = "euc-kr")

# Extract 2019, 2020 data
subway2019 <- subway %>%
  filter(month >= 201901, month < 202001)

subway2020 <- subway %>%
  filter(month >= 202001, month < 202101)

# Sum up by two time periods (7-10am, 5-8pm)
data2019 <- subway2019 %>%
  mutate(period1_2019 = Time07_1 + Time07_2 + Time08_1 + Time08_2 + Time09_1 + Time09_2, period2_2019 = Time17_1 + Time17_2 + Time18_1 + Time18_2 + Time19_1 + Time19_2) %>%
  group_by(station) %>%
  summarise(period1_2019 = sum(period1_2019), period2_2019 = sum(period2_2019)) %>%
  dplyr::select(station, period1_2019, period2_2019) 

data2020 <- subway2020 %>%
  mutate(period1_2020 = Time07_1 + Time07_2 + Time08_1 + Time08_2 + Time09_1 + Time09_2, period2_2020 = Time17_1 + Time17_2 + Time18_1 + Time18_2 + Time19_1 + Time19_2) %>%
  group_by(station) %>%
  summarise(period1_2020 = sum(period1_2020), period2_2020 = sum(period2_2020)) %>%
  dplyr::select(station, period1_2020, period2_2020) 

# Merge two periods
period_data <- merge(data2019, data2020, by="station")

# Calculate the decrease rate in subway passengers
subway_organized <- period_data %>%
  mutate(rate1 = period1_2020 / period1_2019,
         rate2 = period2_2020 / period2_2019) %>%
  dplyr::select("station", "rate1", "rate2")

subway_organized$station <- gsub("\\s*\\([^\\)]+\\)", "", as.character(subway_organized$station))

# Divide stations into 4 types
# 7-10am
subway_organized$type_v1 <- ifelse(subway_organized$rate1 >= 0.9, "Type 1: Over 0.9", ifelse(
  subway_organized$rate1 >= 0.8, "Type 2: 0.8 to 0.9", ifelse(
    subway_organized$rate1 >= 0.7, "Type 3: 0.7 to 0.8", "Type 4: Under 0.7"
  )))
# 5-8pm
subway_organized$type_v2 <- ifelse(subway_organized$rate2 >= 0.9, "Type 1: Over 0.9", ifelse(
  subway_organized$rate2 >= 0.8, "Type 2: 0.8 to 0.9", ifelse(
    subway_organized$rate2 >= 0.7, "Type 3: 0.7 to 0.8", "Type 4: Under 0.7"
  )))

# Map visualization for each station
map <- shapefile("TL_SCCO_SIG.shp")
map <- spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
new_map <- fortify(map, region = 'SIG_CD')

theme_set(theme_gray(base_family='NanumGothic'))

new_map$id <- as.numeric(new_map$id)
seoul_map <- new_map[new_map$id <= 11740,]

# Import station coordinates
station_coord <- read.csv("station_coordinate_info.csv", header = T, fileEncoding = "euc-kr")
subway_organized <- merge(subway_organized, station_coord, by="station")

# 7-10am
ggplot() +
  geom_polygon(data = seoul_map, aes(x=long, y=lat, group=group), fill = 'white', color='black') +
  geom_point(data=subway_organized, aes(x=lng, y=lat, color=type_v1), size=3) +
  labs(title="Subway stations in Seoul (7-10am)") +
  scale_color_manual(values=c("#ffd6a1", "#ff9595", "#d60a24", "#690c22"))
# 5-8pm
ggplot() +
  geom_polygon(data = seoul_map, aes(x=long, y=lat, group=group), fill = 'white', color='black') +
  geom_point(data=subway_organized, aes(x=lng, y=lat, color=type_v1), size=3) +
  labs(title="Subway stations in Seoul (5-8pm)") +
  scale_color_manual(values=c("#ffd6a1", "#ff9595", "#d60a24", "#690c22"))


##### Part 2: Analyze by each district
### Import organized dataset of district information
district_info <- read.csv("seoul_district_info.csv", header = T, fileEncoding = "euc-kr")

### Linear regression for Time Period 1 (7-10am)
# For Time Period 2, just change y=rate1 to y=rate2

# 1) Population
ggplot(data=district_info, aes(x=population, y=rate1)) +
  labs(title = "Correlation between Population & Subway users (7-10am)",
       x = "Population in each district", y = "Ratio of subway users (2020/2019)") +
  ylim(0.6, 0.9) +
  geom_point(size = 3) +
  geom_smooth(method="lm") +
  stat_cor(size = 5) +
  stat_regline_equation(label.y = 0.88, size = 5)

# 2) Number of companies
ggplot(data=district_info, aes(x=num_of_company, y=rate1)) +
  labs(title = "Correlation between Number of companies & Subway users (7-10am)",
       x = "Population in each district", y = "Ratio of subway users (2020/2019)") +
  ylim(0.6, 0.9) +
  geom_point(size = 3) +
  geom_smooth(method="lm") +
  stat_cor(size = 5) +
  stat_regline_equation(label.y = 0.88, size = 5)

# 3) COVID-19 patients
ggplot(data=district_info, aes(x=covid_patients, y=rate1)) +
  labs(title = "Correlation between COVID-19 patients & Subway users (7-10am)",
       x = "Population in each district", y = "Ratio of subway users (2020/2019)") +
  ylim(0.6, 0.9) +
  geom_point(size = 3) +
  geom_smooth(method="lm") +
  stat_cor(size = 5) +
  stat_regline_equation(label.y = 0.88, size = 5)

# 4) Number of colleger students
ggplot(data=district_info, aes(x=univ_students, y=rate1)) +
  labs(title = "Correlation between College students & Subway users (7-10am)",
       x = "Population in each district", y = "Ratio of subway users (2020/2019)") +
  ylim(0.6, 0.9) +
  geom_point(size = 3) +
  geom_smooth(method="lm") +
  stat_cor(size = 5) +
  stat_regline_equation(label.y = 0.88, size = 5)


### Map visualization by each district (7-10 am)
# For 5-8 pm, just change rate1 to rate2
map <- shapefile("TL_SCCO_SIG.shp")
map <- spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
new_map <- fortify(map, region = 'SIG_CD')

theme_set(theme_gray(base_family='NanumGothic'))

new_map$id <- as.numeric(new_map$id)
seoul_map <- new_map[new_map$id <= 11740,]
seoul_merge <- merge(seoul_map, district_info, by='id')

# 1) Population
ggplot() +
  geom_polygon(data = seoul_merge, aes(x=long, y=lat, group=group, fill=population)) +
  scale_fill_gradient(low = "#00b3ff", high = "#2600ff", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "Population by district") + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) +
  geom_text(data = district_info, aes(x=center_long, y=center_lat, label=address, family = 'AppleGothic', size = 5))

# 2) Number of company
ggplot() +
  geom_polygon(data = seoul_merge, aes(x=long, y=lat, group=group, fill=population)) +
  scale_fill_gradient(low = "#00ff61", high = "#008532", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "Number of companies by district") + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) +
  geom_text(data = district_info, aes(x=center_long, y=center_lat, label=address, family = 'AppleGothic', size = 5))

# 3) COVID-19 patients
ggplot() +
  geom_polygon(data = seoul_merge, aes(x=long, y=lat, group=group, fill=covid_patients)) +
  scale_fill_gradient(low = "#ffa8a8", high = "#910000", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "COVID-19 patients by district") + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) +
  geom_text(data = district_info, aes(x=center_long, y=center_lat, label=address, family = 'AppleGothic', size = 5))

# 4) College students
ggplot() +
  geom_polygon(data = seoul_merge, aes(x=long, y=lat, group=group, fill=covid_patients)) +
  scale_fill_gradient(low = "#96beff", high = "#012154", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "College students by district") + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) +
  geom_text(data = district_info, aes(x=center_long, y=center_lat, label=address, family = 'AppleGothic', size = 5))



