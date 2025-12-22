#Catherine dell'Olio
# This code allows me to select a stratified sample from the ILM adult butternuts based on health (both % girdle and % live canopy, in a 3x3 subgroup stratification for high, medium, and low health in both metrics independently.)
#After I generated a sample from this code, I then tested its representativeness of the pool of eligible adult butternuts in terms of spatial extent and crown class.
#I call the package dpylr to filter my data with its syntax
#I call one datasheet: ILM_assoc_trees, which is just a datasheet of all the ILM trees.  Alternatively, this can be run with the full datasheet.csv filtered by site == "ILM".

#setting working directory
setwd("~/dataviz_cfd")
df <- read.csv("ILM_assoc_trees.csv")
library(dplyr)

#filter to eligible butternuts: adults with DBH greater than 4.5 cm and less than 50 cm
adults_df <- df %>% filter(seedling_or_adult == "Adult")
adults_df <- adults_df %>% filter(dbh_cm >= 4.5)
adults_df <- adults_df %>% filter(dbh_cm <=50)

#creating the subgroups:
#health is represented by two metrics: girdle percentage and percent live canopy
# girdle is considered high if it is over 60%, low if it is less than or equal to 30%, and medium if in between
# percent live canopy is considered high if it is over 75%, low if it is less than or equal to 30%, and medium if in between
# these are independent metrics so they create a 3x3 grid of possible subgroups
cat1 <- adults_df %>%  filter(adult_percent_live_canopy <= 30 & live_adult_girdle <= 30)
cat2 <- adults_df %>%  filter(adult_percent_live_canopy <= 75 & adult_percent_live_canopy > 30 & live_adult_girdle <= 30)
cat3 <- adults_df %>%  filter(adult_percent_live_canopy > 75 & live_adult_girdle <= 30)
cat4 <- adults_df %>%  filter(adult_percent_live_canopy <= 30 & 30 < live_adult_girdle  & live_adult_girdle <= 60)
cat5 <- adults_df %>%  filter(30 < adult_percent_live_canopy & adult_percent_live_canopy <= 75 & 30 < live_adult_girdle  & live_adult_girdle <= 60)
cat6 <- adults_df %>%  filter(75 < adult_percent_live_canopy & 30 < live_adult_girdle  & live_adult_girdle <= 60)
cat7 <- adults_df %>%  filter(adult_percent_live_canopy <= 30 & 60 < live_adult_girdle)
cat8 <- adults_df %>%  filter(30 < adult_percent_live_canopy & adult_percent_live_canopy <= 75 & 60 < live_adult_girdle)
cat9 <- adults_df %>%  filter(75 < adult_percent_live_canopy & 60 < live_adult_girdle)

#retrieving the list of tree IDs in each subgroup
cat1$number
cat2$number
cat3$number
cat4$number
cat5$number
cat6$number
cat7$number
cat8$number
cat9$number

#a function to sample a certain size from each category.  the reason I don't have it sample 6 from every category is because category 1 is too small--only 2 trees.  So those will both be sampled.
randomsample <- function(cat, size){
  s <-  sample(1:nrow({{cat}}), size, replace = FALSE)
  return(cat[s,])
}
r1 <- randomsample(cat1, 2)
r2 <- randomsample(cat2, 6)
r3 <- randomsample(cat3, 6)
r4 <- randomsample(cat4, 6)
r5 <- randomsample(cat5, 6)
r6 <- randomsample(cat6, 6)
r7 <- randomsample(cat7, 6)
r8 <- randomsample(cat8, 6)
r9 <- randomsample(cat9, 6)
#combine every selection from the subgroups to make the sample
totalsample <- rbind(r1, r2,r3, r4, r5, r6, r7, r8, r9)
#retrieve tree IDs for the proposed sample
totalsample$number
#This gives me the list of trees to sample at ILM for the associate trees analysis!

#TESTING FOR REPRESENTATIVENESS
#does this sample consist of mostly similar proportions of trees in each crown class as the eligible population?  Trees of different crown classes receive different amounts of light which might really impact our analysis.
barplot <- function(data, variable, title){
  ggplot(data, aes({{variable}})) +
    geom_bar()+
    ggtitle(title)
}
barplot(totalsample, adult_crown_class, "crown class of proposed sample")
barplot(adults_df, adult_crown_class, "crown class of total pool of eligible adults")
#the answer is mostly yes (though if you re-run this code, you will get a different sample!)
barplot(r1, adult_crown_class, "r1 crown classes")
barplot(r2, adult_crown_class, "r2 crown classes")
barplot(r3, adult_crown_class, "r3 crown classes")
barplot(r4, adult_crown_class, "r4 crown classes")
barplot(r5, adult_crown_class, "r5 crown classes")
barplot(r6, adult_crown_class, "r6 crown classes")
barplot(r7, adult_crown_class, "r7 crown classes")
barplot(r8, adult_crown_class, "r8 crown classes")
barplot(r9, adult_crown_class, "r9 crown classes")
#does the sample appear to cover the spatial extent of eligible trees?
ggplot() +
  geom_point(data = adults_df, aes(gps_w, gps_n), colour="green2") +
  geom_point(data = totalsample, aes(gps_w, gps_n), colour="red") +
  ggtitle("Map of all eligible adults (green) with proposed sample (red) overlaid")

#produce a csv of which trees to sample
write.csv(totalsample, "proposed_sample_ILM_assoc_trees.csv")
