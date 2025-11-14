install.packages("dplyr")
library(dplyr)
setwd("~/dataAnalysisCfd")
data <- read.csv("datasheet.csv")
data <- data[c(1:47, 49:430),]
data$dbh_cm <- as.numeric(data$dbh_cm)

data_seedlings <- data %>% filter(seedling_or_adult == "Seedling")
average_densi <- numeric(nrow(data_seedlings))
mowing <- logical(nrow(data_seedlings))
vole_nibbling <- logical(nrow(data_seedlings))
deer_browse <- logical(nrow(data_seedlings))
deer_rub <- logical(nrow(data_seedlings))
rabbit_browse <- logical(nrow(data_seedlings))
data_seedlings <- cbind(data_seedlings, average_densi, mowing, vole_nibbling, deer_browse, deer_rub, rabbit_browse)
for (i in 1:nrow(data_seedlings)) {
  data_seedlings$average_densi[i] <- 1.04 * mean(c(data_seedlings$densi_n[i], data_seedlings$densi_s[i], data_seedlings$densi_e[i], data_seedlings$densi_w[i]))
}
data_seedlings[grep('Having been mowed in the past', data_seedlings$seedling_damage), "mowing"] <- TRUE
data_seedlings[grep("Vole (nibbling on the bark)", data_seedlings$seedling_damage), "vole_nibbling"] <- TRUE
data_seedlings[grep("Deer browse", data_seedlings$seedling_damage), "deer_browse"] <- TRUE
data_seedlings[grep("Deer rub", data_seedlings$seedling_damage), "deer_rub"] <- TRUE
data_seedlings[grep("Rabbit browse", data_seedlings$seedling_damage), "rabbit_browse"] <- TRUE

data_adults <- data %>% filter(seedling_or_adult == "Adult")

data_liveseedlings <- data_seedlings %>% filter(seedling_dead_or_alive == "Alive")
for (i in 2017:2025){
  nam <- paste("data_seedlings", i, sep = "")
  assign(nam, data_seedlings %>% filter(seedling_germ_yr == {{i}}))
}

data_liveadults <- data_adults %>% filter(adult_dead_or_alive == "Alive")

na.zero <- function (x) {
  x[is.na(x)] <- 0
  x
}
convert.na <- function(y){
  y <- na.zero(y)
}

data_liveadults$live_adult_canker_base <- na.zero(data_liveadults$live_adult_canker_base)
data_liveadults$live_adult_canker_1stlivebranch <- na.zero(data_liveadults$live_adult_canker_1stlivebranch)
data_liveadults$live_adult_girdle <- na.zero(data_liveadults$live_adult_girdle)
data_liveseedlings$seedling_canker_base <- na.zero(data_liveseedlings$seedling_canker_base)
data_liveseedlings$seedling_canker_stem <- na.zero(data_liveseedlings$seedling_canker_stem)
data_liveseedlings$seedling_girdle <- na.zero(data_liveseedlings$seedling_girdle)
for (i in 1:nrow(data_liveadults)){
  if (data_liveadults$live_adult_canker_presence[i] == "N" && is.na(data_liveadults$live_adult_canker_1stlivebranch[i]))
    data_liveadults$live_adult_canker_1stlivebranch[i] <- 0
}

retention_strategy <- numeric(nrow(data_liveadults))
data_liveadults <- cbind(data_liveadults, retention_strategy)

for(i in 1:nrow(data_liveadults)){
  if ((data_liveadults$adult_percent_live_canopy[i] > 70  &
      data_liveadults$live_adult_canker_base[i] < 20) |
      (data_liveadults$adult_percent_live_canopy[i] >= 50 &
      data_liveadults$live_adult_canker_base[i] == 0))
      data_liveadults$retention_strategy[i] <- "healthy"
      else data_liveadults$retention_strategy[i] <- "unhealthy"
}

data_healthyadults <- data_liveadults %>% filter(retention_strategy == "healthy")

data_ILM <- data %>% filter(site == "ILM")
data_ILMhybridvis <- data_ILM %>% filter(hybrid_visibility == "Y")

data_CPVT <- data %>% filter(site == "CPVT")
data_CPVThybridvis <- data_CPVT %>% filter(hybrid_visibility == "Y")

data_CPVTadults <- data_adults %>% filter(site == "CPVT")

data_ILMadults <- data_adults %>% filter(site == "ILM")

data_CPVTseedlings <- data_seedlings %>% filter(site == "CPVT")
data_CPVTliveseedlings <- data_liveseedlings %>% filter(site == "CPVT")

data_ILMseedlings <- data_seedlings %>% filter(site == "ILM")
data_ILMliveseedlings <- data_liveseedlings %>% filter(site == "ILM")

data_CPVTliveadults <- data_liveadults %>% filter(site == "CPVT")

data_ILMliveadults <- data_liveadults %>% filter(site == "ILM")