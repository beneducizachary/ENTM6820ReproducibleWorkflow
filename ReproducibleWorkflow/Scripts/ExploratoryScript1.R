#### Exploratory script for workflow

### Load data set
datum1 <- read.csv("https://raw.githubusercontent.com/beneducizachary/ENTM6820/main/ReproducibleWorkflow/Data/BMPvegDataRawZB7.csv")
str(datum1)

### Load packages
install.packages("tidyverse")
library(tidyverse)

### Format for analysis of planted wildflower richness and density.
datum2 <- datum1 %>%
  select(Date, Round, Site, Subplot, Quadrat, ASTU, COTI3, CHFA2, DRAM, GAPU, HEAN2, MOPU, RUHI2) %>% # Select
  # relevant grouping variables and our planted species.
  filter(Round == "c"|Round == "d"|Round == "e"|Round == "f") %>% # This filters out sampling rounds a and b,
  # which did not have any planted wildflowers recorded.
  mutate_at(c(6:13), as.numeric) %>% # Making sure the counts are considered numbers.
  mutate_at(c(6:13), ~replace_na(.,0)) %>% # Empty cells are replaced with NAs when imported into R,
  # so need to replace those with zeros.
  pivot_longer(cols = ASTU:RUHI2, names_to = "Species", values_to = "Count") %>% # Switch to long format for
  # easier summarizing.
  group_by(Site, Round, Subplot, Species) %>% # Telling R what factors you want to group by. Will only sum
  # values of rows containing the same unique combinations of the specified factors. Essentially summing
  # counts for the 10 quadrats in each subplot x sampling occasion x species.
  summarise(Count=sum(Count)) %>% # See last sentence.
  pivot_wider(names_from = Species, values_from = Count) %>% # Changing back to wide format to aid in calculating
  # subplot level richness and density.
  rowwise() %>% # Specify that we want to calculate within rows.
  mutate(
    TotalDens = sum(c_across(ASTU:RUHI2)), # Sums all counts across columns.
    Richness = sum(c_across(ASTU:RUHI2)!=0) # Counts the frequency of values >0.
    ) %>%
  mutate(Row = 
           case_when(between(Subplot, 1, 4)~1,
                     between(Subplot, 5, 8)~2,
                     between(Subplot, 8, 12)~3,
                     between(Subplot, 13, 16)~4)) %>%
  mutate(Col = 
           case_when(Subplot == 4 | Subplot == 8 | Subplot == 12 | Subplot == 16 ~ 1,
                     Subplot == 3 | Subplot == 7 | Subplot == 11 | Subplot == 15 ~ 2,
                     Subplot == 2 | Subplot == 6 | Subplot == 10 | Subplot == 14 ~ 3,
                     Subplot == 1 | Subplot == 5 | Subplot == 9 | Subplot == 13 ~ 4)) %>%
 # Finally, we'll add the treatment information. However, the treatments were
  # not applied at this stage, so we are looking for no treatment effects!
  mutate(Treatment =
           case_when(Subplot == 1 & Site == "FCU1" ~ "Disking",
                     Subplot == 2 & Site == "FCU1" ~ "Mowing",
                     Subplot == 3 & Site == "FCU1" ~ "Combo",
                     Subplot == 4 & Site == "FCU1" ~ "Control",
                     Subplot == 5 & Site == "FCU1" ~ "Mowing",
                     Subplot == 6 & Site == "FCU1" ~ "Control",
                     Subplot == 7 & Site == "FCU1" ~ "Disking",
                     Subplot == 8 & Site == "FCU1" ~ "Combo",
                     Subplot == 9 & Site == "FCU1" ~ "Combo",
                     Subplot == 10 & Site == "FCU1" ~ "Disking",
                     Subplot == 11 & Site == "FCU1" ~ "Mowing",
                     Subplot == 12 & Site == "FCU1" ~ "Control",
                     Subplot == 13 & Site == "FCU1" ~ "Control",
                     Subplot == 14 & Site == "FCU1" ~ "Mowing",
                     Subplot == 15 & Site == "FCU1" ~ "Combo",
                     Subplot == 16 & Site == "FCU1" ~ "Disking",
                     Subplot == 1 & Site == "BEU1" ~ "Combo",
                     Subplot == 2 & Site == "BEU1" ~ "Disking",
                     Subplot == 3 & Site == "BEU1" ~ "Mowing",
                     Subplot == 4 & Site == "BEU1" ~ "Control",
                     Subplot == 5 & Site == "BEU1" ~ "Mowing",
                     Subplot == 6 & Site == "BEU1" ~ "Control",
                     Subplot == 7 & Site == "BEU1" ~ "Combo",
                     Subplot == 8 & Site == "BEU1" ~ "Disking",
                     Subplot == 9 & Site == "BEU1" ~ "Disking",
                     Subplot == 10 & Site == "BEU1" ~ "Mowing",
                     Subplot == 11 & Site == "BEU1" ~ "Control",
                     Subplot == 12 & Site == "BEU1" ~ "Combo",
                     Subplot == 13 & Site == "BEU1" ~ "Control",
                     Subplot == 14 & Site == "BEU1" ~ "Combo",
                     Subplot == 15 & Site == "BEU1" ~ "Disking",
                     Subplot == 16 & Site == "BEU1" ~ "Mowing",
                     Subplot == 1 & Site == "PBU1" ~ "Mowing",
                     Subplot == 2 & Site == "PBU1" ~ "Disking",
                     Subplot == 3 & Site == "PBU1" ~ "Combo",
                     Subplot == 4 & Site == "PBU1" ~ "Control",
                     Subplot == 5 & Site == "PBU1" ~ "Control",
                     Subplot == 6 & Site == "PBU1" ~ "Combo",
                     Subplot == 7 & Site == "PBU1" ~ "Mowing",
                     Subplot == 8 & Site == "PBU1" ~ "Disking",
                     Subplot == 9 & Site == "PBU1" ~ "Disking",
                     Subplot == 10 & Site == "PBU1" ~ "Mowing",
                     Subplot == 11 & Site == "PBU1" ~ "Combo",
                     Subplot == 12 & Site == "PBU1" ~ "Control",
                     Subplot == 13 & Site == "PBU1" ~ "Disking",
                     Subplot == 14 & Site == "PBU1" ~ "Combo",
                     Subplot == 15 & Site == "PBU1" ~ "Control",
                     Subplot == 16 & Site == "PBU1" ~ "Mowing")) %>%
  mutate_at(c(16), as.factor) %>%
  mutate(Treatment = relevel(Treatment, "Control"))
### ANALYSIS

# This dataset represents pre-treatment data collected during the 2022 field season
# before treatments were applied in fall 2022/winter 2023. Therefore,
# there should not be any treatment level effects. However, there may be effects of location
# (spatial autocorrelation) in this dataset that may erroneously produce treatment effects.

## Let's look at the data:

ggplot(datum2, aes(x = Richness)) + geom_histogram()

ggplot(datum2, aes(x = TotalDens)) + geom_histogram()

# We can see that the distribution of the raw data, especially for density, is not normal.
# There is also likely underdispersion in the richness response.

## Let's try a general linear model anyway:
lmRich <- lm(Richness~Treatment, datum2)
lmTotalDens <- lm(TotalDens~Treatment, datum2)

summary(lmRich) # No treatment effects here, so doesn't look like treatments allocation was not skewed.
summary(lmTotalDens) # No significance, but there is weak evidence suggesting that the mowing treatments
                     # have 1.21 more flowers on average. This is a pretty small effect, however, considering
                     # a subplot can have hundreds of flowers.
plot(residuals(lmRich))
plot(residuals(lmTotalDens))
hist(residuals(lmRich))
hist(residuals(lmTotalDens))

# Messy residuals plots... showing that there's clearly clustering of values indicating autocorrelation.
# Of course, knowing that each subplot was sampled four times throughout the season, we have pseudoreplication
# for the variable of interest (Treatment). Additionally, there are three Sites, each with 16 subplots
# each (4 of each Treatment group). Therefore, we also (probably) have spatial autocorrelation.

## Let's fix the pseudoreplication problem.
library(lme4)

lmmRich <- lmer(Richness ~ Treatment + (1|Site/Subplot), datum2)
summary(lmmRich)
plot(residuals(lmmRich))

lmmTotalDens <- lmer(TotalDens ~ Treatment + (1|Site/Subplot), datum2)
summary(lmmTotalDens)
plot(residuals(lmmTotalDens))

# The nested random effect explicitly specifies that there are multiple measurements per subplot,
# and so should apply the correct degrees of freedom to test for Treatment differences. However,
# we still have messy residuals. The density model is also having trouble converging (singular fit).

## Switch to generalized linear mixed models for a better error distribution:
# Try a poisson error family first:
glmmRich <- glmer(Richness~Treatment + (1|Site/Subplot), family = poisson, datum2)
summary(glmmRich)
plot(residuals(glmmRich)) # Having another singular fit issue. This time for richness. 

glmmTotalDens <- glmer(TotalDens ~ Treatment + (1|Site/Subplot), family = poisson, datum2)
summary(glmmTotalDens)
plot(residuals(glmmTotalDens)) # This improved a bit. Residuals at lower values are still iffy
                               # and there is a perponderance of residuals below zero with a portion
                               # of high values.
                               # A negative binomial error family may help, as it can handle some 
                               # overdispersion and excess zeros compared to poisson.

glmmRich2 <- glmer.nb(Richness ~ Treatment + (1|Site/Subplot), datum2)
summary(glmmRich2)

glmmTotalDens2 <- glmer.nb(TotalDens ~ Treatment + (1|Site/Subplot), datum2)
plot(residuals(glmmTotalDens2))

### glmmTMB


## Try a multivariable model with Site and Round:

lmRich2 <- lm(Richness~Treatment+Site+Round, datum2)
lmTotalDens2 <- lm(TotalDens~Treatment+Site+Subplot, datum2)

summary(lmRich2)
summary(lmTotalDens2)

plot(residuals(lmRich2))
plot(residuals(lmTotalDens2))

# Didn't fix anything. Really the issue here is that the error distributrion appears non-Gaussian,
# pseudoreplication is present, and spatial autocorrelation is present.

# Let's run a generalized linear model testing poisson error family:

glmRich <- glm(Richness~Treatment, family = poisson, datum2)
glmTotalDens <- glm(TotalDens~Treatment, family = poisson, datum2)

summary(glmRich)
summary(glmTotalDens)

plot(residuals.glm(glmRich))
plot(residuals.glm(glmTotalDens))

# Run the same for negative binomial error family:

glmRich2 <- glm(Richness~Treatment, family = negbin, datum2)

## Tile plot of planted richness across subplots
ggplot(datum2, aes(x = Col, y = Row, fill = Richness)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  geom_text(aes(label = Richness), color = "black", size = 4) +
  coord_fixed() +
  facet_wrap(~Site*Round)

## Tile plot of flower density/10m^2
ggplot(datum2, aes(x = Col, y = Row, fill = TotalDens)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  geom_text(aes(label = TotalDens), color = "black", size = 4) +
  coord_fixed() +
  facet_wrap(~Site*Round)

ggplot(datum2, aes(x = as.factor(Subplot), y = Richness)) +
  geom_boxplot() +
  facet_wrap(~Site)
  
ggplot(datum2, aes(x = as.factor(Subplot), y = TotalDens)) +
  geom_boxplot() +
  facet_wrap(~Site)

## There's definitely evidence of clustering going on. 
