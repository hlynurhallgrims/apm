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

#T��nirit
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

#Skilgreinum hva�a d�lkar innihalda sk�ribreytur svo h�gt s� a� v�sa til �eirra me� nafni
skyribreytur <- names(Glass)[1:9]

#Til a� laga corrplot er merkingum �sa leyft a� fara yfir sp�ss�ur me� eftirfarandi breytingu
par(xpd = TRUE)
#Fylgniplott
fylgni <- cor(Glass[, skyribreytur])
corrplot(fylgni, method = "square", mar = c(1, 1, 1, 1))

center_scale <- preProcess(Glass[, skyribreytur], method = c("center", "scale"))
cs_gogn <- predict(center_scale, newdata = Glass[, skyribreytur])
ss_gogn <- spatialSign(cs_gogn) %>% as_data_frame()
ss_gogn$Type <- Glass$Type

#Scatterplott matrix fyrir centered, scaled og spatialsigned g�gn
ss_gogn %>%
  ggpairs(mapping = aes(colour = Type))



# 3.2 ---------------------------------------------------------------------

library(mlbench)
data(Soybean)

























