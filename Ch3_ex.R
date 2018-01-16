# Setup -------------------------------------------------------------------

library(mlbench)
library(AppliedPredictiveModeling)
library(tidyverse)
library(corrplot)
library(GGally)
library(caret)

# 3.1 ---------------------------------------------------------------------

data(Glass)
str(Glass)

#Tíðnirit
Glass %>%
  gather(-Type, key = "efni", value = "value") %>%
  ggplot(mapping = aes(x = value )) +
    geom_density() +
    facet_wrap(~efni, scales = "free")

#Boxplott
Glass %>%
  gather(-Type, key = "efni", value = "value") %>%
  ggplot(mapping = aes(x = efni, y = value)) +
  geom_boxplot()

#Scatterplot matrix
Glass %>%
  ggpairs(mapping = aes(colour = Type, alpha = 0.6))

#Skilgreinum hvaða dálkar innihalda skýribreytur svo hægt sé að vísa til þeirra með nafni
skyribreytur <- names(Glass)[1:9]

#Til að laga corrplot er merkingum ása leyft að fara yfir spássíur með eftirfarandi breytingu
par(xpd = TRUE)
#Fylgniplott
fylgni <- cor(Glass[, skyribreytur])
corrplot(fylgni, method = "square", mar = c(1, 1, 1, 1))

center_scale <- preProcess(Glass[, skyribreytur], method = c("center", "scale"))
cs_gogn <- predict(center_scale, newdata = Glass[, skyribreytur])
ss_gogn <- spatialSign(cs_gogn) %>% as_data_frame()
ss_gogn$Type <- Glass$Type

#Scatterplott matrix fyrir centered, scaled og spatialsigned gögn
ss_gogn %>%
  ggpairs(mapping = aes(colour = Type))



# 3.2 ---------------------------------------------------------------------

library(mlbench)
data(Soybean)

























