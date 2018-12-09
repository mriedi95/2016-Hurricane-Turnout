#LOAD LIBRARIES
library(tidyverse)
library(rgdal)
library(devtools)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(RColorBrewer)

#DESIGNATE DISASTER COUNTIES, EARLY VOTING, AND REGISTRATION EXTENTION
disaster_counties_list = c("NAS", "DUV", "STJ", "PUT", "FLA", "VOL", "SEM", "BRE", "IND")
early_voted = c(1, 3)
reg_day = c("12oct2016", "13oct2016", "14oct2016", "15oct2016", "16oct2016", "17oct2016", "18oct2016")
voted = c(1, 3, 4, 5)



#READ IN FLORIDA VOTER FILE
fl_voter <- read.csv("C:/Users/Matthew/Desktop/FL Voter File/flvoter.csv")  %>% 
  mutate(disaster_counties = as.character(v1) %in% disaster_counties_list)  %>%
  mutate(voted_early = as.character(v5Nov2016type) %in% early_voted) %>%
  mutate(regis_ext = as.character(regdate) %in% reg_day) %>%
  mutate(voted_2016 = as.character(v5Nov2016type) %in% voted)

#CREATE ONLY VOTED SUBSET AND ONLY REGISTERED IN OCTOBER SUBSET
voted_subset <- subset(fl_voter, !(voted_2016 == FALSE))
reg_subset <- subset(fl_voter, regyear == 2016 & regmonth == 10 & regday <= 18)


#COMPARE COUNTIES

#VOTED
fl_voter %>% 
  group_by(v1) %>% 
  summarise(percentage = mean(voted_2016) * 100, disaster = unique(disaster_counties), sample_size = n()) %>% 
  ggplot(., aes(x = v1, y = percentage, color = disaster, size = sample_size)) + geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Turnout by County", x = "County", y = "Percentage", size = "Sample Size") +
  scale_color_manual(name = c("County Type"), labels = c("Non-Disaster", "Disaster"), values = c("blue", "red"))

#EARLY VOTE
fl_voter %>% 
  group_by(v1) %>% 
  summarise(percentage = mean(voted_early) * 100, disaster = unique(disaster_counties), sample_size = n()) %>% 
  ggplot(., aes(x = v1, y = percentage, color = disaster, size = sample_size)) + geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Early Turnout by County", x = "County", y = "Percentage", size = "Sample Size") +
  scale_color_manual(name = c("County Type"), labels = c("Non-Disaster", "Disaster"), values = c("blue", "red"))

#EARLY VOTE AS A PERCENTAGE OF TOTAL VOTE
voted_subset %>% 
  group_by(v1) %>% 
  summarise(percentage = mean(voted_early) *100, disaster = unique(disaster_counties), sample_size = n()) %>% 
  ggplot(., aes(x = v1, y = percentage, color = disaster, size = sample_size)) + geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Early Turnout as a Percentage of Total Vote", x = "County", y = "Percentage", size = "Sample Size") +
  scale_color_manual(name = c("County Type"), labels = c("Non-Disaster", "Disaster"), values = c("blue", "red"))

#REGISTERED DURING EXTENSION
reg_subset %>% 
  group_by(v1) %>% 
  summarise(percentage = mean(regis_ext) *100, disaster = unique(disaster_counties), sample_size = n()) %>% 
  ggplot(., aes(x = v1, y = percentage, color = disaster, size = sample_size)) + geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Registration During Extension by County", x = "County", y = "Percentage", size = "Sample Size") +
  scale_color_manual(name = c("County Type"), labels = c("Non-Disaster", "Disaster"), values = c("blue", "red"))


#STATISTICS
t.test(voted_2016 ~ disaster_counties, data = fl_voter)
t.test(voted_early ~ disaster_counties, data = fl_voter)
t.test(voted_early ~ disaster_counties, data = voted_subset)
t.test(regis_ext ~ disaster_counties, data = reg_subset)


#BY RACE


#SUMMARISE MEANS BASED ON RACE AND DISASTER COUNTIES
fl_voter %>% 
  group_by(v21, disaster_counties) %>%
  summarise(raceperc = mean(voted_2016)*100) %>%
  as.data.frame()
voted_subset %>% 
  group_by(v21, disaster_counties) %>%
  summarise(racepercearly = mean(voted_early)*100) %>%
  as.data.frame()
reg_subset %>% 
  group_by(v21, disaster_counties) %>%
  summarise(racereg = mean(regis_ext)*100) %>%
  as.data.frame()

#CREATE NEW DATA FRAME WITH RACES, COUNTY TYPE, AND MEANS
race <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8)
disastertype <- c("nondis", "dis", "nondis", "dis", "nondis", "dis", "nondis", "dis", "nondis", "dis", "nondis", "dis", "nondis", "dis", "nondis", "dis")
voteperc <- c(64.22911, 66.98828, 64.14175, 65.73389, 64.53731, 65.38885, 64.83088, 64.43621, 72.52942, 74.76208, 62.01775, 65.36801, 60.91857, 62.18115, 58.21689, 57.47091)
voteearlyperc <- c(65.23971, 66.10757, 72.11891, 71.76785, 72.75060, 69.95592, 72.01368, 66.36135, 67.61053, 68.85160, 68.56459, 68.09899, 66.44929, 65.32942, 66.63695, 65.47637)
regperc <- c(40.69767, 55.86207, 42.07780, 43.36794, 42.55867, 48.82985, 42.84311, 44.42503, 43.25851, 48.63618, 43.72222, 45.16129, 40.00548, 42.85714, 37.42670, 40.86412)
racedata <- data.frame(race, disastertype, voteperc, voteearlyperc, regperc)

#CREATE GRAPHS
racevotegraph <- ggplot(data=racedata, aes(x=race, y=voteperc, fill=disastertype)) +
  geom_bar(stat="identity", position=position_dodge())
raceearlygraph <- ggplot(data=racedata, aes(x=race, y=voteearlyperc, fill=disastertype)) +
  geom_bar(stat="identity", position=position_dodge())
racereggraph <- ggplot(data=racedata, aes(x=race, y=regperc, fill=disastertype)) +
  geom_bar(stat="identity", position=position_dodge())

#DISPLAY GRAPHS
racevotegraph + 
  scale_x_discrete(limit = c("American Indian/Alaskan Native", "Asian/Pacific Islander", "Black, Not Hispanic", "Hispanic", "White, Not Hispanic", "Other", "Multi-racial", "Unknown")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "Turnout by Race", x = "Race", y = "Percentage Voted") +
  scale_fill_manual(name = c("County Type"), labels = c("Disaster", "Non-Disaster"), values = c("red", "blue"))

raceearlygraph + 
  scale_x_discrete(limit = c("American Indian/Alaskan Native", "Asian/Pacific Islander", "Black, Not Hispanic", "Hispanic", "White, Not Hispanic", "Other", "Multi-racial", "Unknown")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "Early Turnout by Race", x = "Race", y = "Percentage Voted Early of Total Vote") +
  scale_fill_manual(name = c("County Type"), labels = c("Disaster", "Non-Disaster"), values = c("red", "blue"))

racereggraph + 
  scale_x_discrete(limit = c("American Indian/Alaskan Native", "Asian/Pacific Islander", "Black, Not Hispanic", "Hispanic", "White, Not Hispanic", "Other", "Multi-racial", "Unknown")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "Registration by Race", x = "Race", y = "Percentage Registered During Extension") +
  scale_fill_manual(name = c("County Type"), labels = c("Disaster", "Non-Disaster"), values = c("red", "blue"))



#CREATE MAPS

#CREATE OUTLINE
states <- map_data("state")
fl_df <- subset(states, region == "florida")
counties <- map_data("county")
fl_county <- subset(counties, region == "florida")
fl_base <- ggplot(data = fl_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "grey")

#MERGE DATA FRAMES
grouped_fl_voter <- fl_voter %>% 
  group_by(v1) %>% 
  summarise(mean_voted_early = mean(voted_early), 
            mean_voted = mean(Voted2016),
            mean_dis = mean(disaster_counties))
grouped_fl_voter2 <- voted_subset %>%
  group_by(v1) %>%
  summarise(mean_early_vote = mean(voted_early))
grouped_fl_voter3 <- reg_subset %>%
  group_by(v1) %>%
  summarise(regis_late = mean(regis_ext))


matching_data = data.frame(abbrev = c("ALA","BAK", "BAY", "BRA", "BRE", "BRO", "CAL", "CHA", "CIT", "CLA", "CLL", "CLM", "DAD", "DES", "DIX", "DUV", "ESC", "FLA", "FRA", "GAD", "GIL", "GLA", "GUL", "HAM", "HAR", "HEN", "HER", "HIG", "HIL", "HOL", "IND", "JAC", "JEF", "LAF", "LAK", "LEE", "LEO", "LEV", "LIB", "MAD", "MAD", "MRN", "MRT", "MON", "NAS", "OKA", "OKE", "ORA", "OSC", "PAL", "PAS", "PIN", "POL", "PUT", "SAN", "SAR", "SEM", "STJ", "STL", "SUM", "SUW", "TAY", "UNI", "VOL", "WAK", "WAL", "WAS"), full_n = c("alachua", "baker", "bay", "bradford", "brevard", "broward", "calhoun", "charlotte", "citrus", "clay", "collier", "columbia", "miami-dade", "de soto", "dixie", "duval", "escambia", "flagler", "franklin", "gadsden", "gilchrist", "glades", "gulf", "hamilton", "hardee", "hendry", "hernando", "highlands", "hillsborough", "holmes", "indian river", "jackson", "jefferson", "lafayette", "lake", "lee", "leon", "levy", "liberty", "madison", "manatee", "marion", "martin", "monroe", "nassau", "okaloosa", "okeechobee", "orange", "osceola", "palm beach", "pasco", "pinellas", "polk", "putnam", "santa rosa", "sarasota", "seminole", "st johns", "st lucie", "sumter", "suwannee", "taylor", "union", "volusia", "wakulla", "walton", "washington"))

grouped_fl_voter <- left_join(grouped_fl_voter, matching_data, by = c("v1" = "abbrev"))
grouped_fl_voter2 <- left_join(grouped_fl_voter2, matching_data, by = c("v1" = "abbrev"))
grouped_fl_voter3 <- left_join(grouped_fl_voter3, matching_data, by = c("v1" = "abbrev"))

fl_grouped_final_data <- left_join(fl_county, grouped_fl_voter, by = c("subregion" = "full_n"))
fl_grouped_final_data2 <- left_join(fl_county, grouped_fl_voter2, by = c("subregion" = "full_n"))
fl_grouped_final_data3 <- left_join(fl_county, grouped_fl_voter3, by = c("subregion" = "full_n"))


my.palette <- brewer.pal(n = 11, name = "RdBu")
my.palette2 <- brewer.pal(n = 3, name = "Set1")
Theme1 <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)


mapvoted <- fl_base + 
  geom_polygon(data = fl_grouped_final_data, aes(fill = mean_voted), color = "grey") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  Theme1 +
  scale_fill_gradientn(colors = my.palette, name = "Voted")
  

mapvoted_early <- fl_base + 
  geom_polygon(data = fl_grouped_final_data, aes(fill = mean_voted_early), color = "grey") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  Theme1 +
  scale_fill_gradientn(colors = my.palette, name = "Voted Early")

mapearlypct <- fl_base + 
  geom_polygon(data = fl_grouped_final_data2, aes(fill = mean_early_vote), color = "grey") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  Theme1 +
  scale_fill_gradientn(colors = my.palette, name = "Early Vote as % of Total Turnout")


mapregis <- fl_base + 
  geom_polygon(data = fl_grouped_final_data3, aes(fill = regis_late), color = "grey") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  Theme1 +
  scale_fill_gradientn(colors = my.palette, name = "Registered During Extension")


#DISPLAY MAPS
mapvoted
mapvoted + coord_fixed(xlim = c(-85, -80),  ylim = c(27, 31), ratio = 1.3)

mapvoted_early
mapvoted_early + coord_fixed(xlim = c(-85, -80),  ylim = c(27, 31), ratio = 1.3)

mapearlypct
mapearlypct + coord_fixed(xlim = c(-85, -80),  ylim = c(27, 31), ratio = 1.3)

mapregis
mapregis + coord_fixed(xlim = c(-85, -80),  ylim = c(27, 31), ratio = 1.3)


#MAP SHOWING DISASTER COUNTIES
FLDisasterMap <- fl_base + 
  geom_polygon(data = fl_grouped_final_data, aes(fill = mean_dis), color = "grey") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  Theme1 +
  scale_fill_gradientn(colors = rev(my.palette2)) +
  guides(fill=FALSE) +
  labs(caption = "Disaster Counties in Red")
