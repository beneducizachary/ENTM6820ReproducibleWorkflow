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
  mutate(Treatment = relevel(Treatment, "Control")) %>%
  mutate_at(c(3), as.factor)

#### Plot density vs treatments:

## Overall
ggplot(datum2, aes(x = Treatment, y = TotalDens)) +
  geom_boxplot()

## by Site:
ggplot(datum2, aes(x = Treatment, y = TotalDens)) +
  geom_boxplot() +
  facet_wrap(~Site)

## by Round
ggplot(datum2, aes(x = Treatment, y = TotalDens)) +
  geom_boxplot() +
  facet_wrap(~Round)

## by Site*Round

ggplot(datum2, aes(x = Treatment, y = TotalDens)) +
  geom_boxplot() +
  facet_wrap(~Site*Round)

# Looks like there is an effect of Site and Round. Of note is that boxplots
# are pulled towards zero, suggesting zero inflation.

ggplot(datum2, aes(x = TotalDens)) +
  geom_histogram(bins = 140)

# Our response is at least non-Gaussian.
# Hard to tell if the residuals are without modeling.

# Of note is that subplots were sampled four times throughout the 2022 season.
# For the Treatment effects (of interest here), we would commit
# pseudoreplication if each time point is treated as an independent measurement.
# Additionally, subplots at the same site are not independent, so we'll need to
# account for this as well.

### General linear model:

lm1 <- lm(TotalDens ~ Treatment, datum2)
summary(lm1)

# Looking at the model estimates, we can see slight differences in effect size
# between treatment groups. However, variation is high and none of these effects
# are considered significant. While we are not seeing the consequence here,
# running such a model could lead to inflated type I error rates, as samples
# from the sample plots were taken throughout the year.

plot(residuals(lm1))
# The residuals are quite messy. More points are concentrated below zero,
# and those above zero are typcically higher. Additionally, the residuals
# are heteroscedastic.

# Several assumptions of the general linear model have been violated.

# Correct pseudoreplication with a mixed model:
library(lme4)

lmm <- lmer(TotalDens~Treatment + (1|Site/Subplot), datum2, REML = TRUE)
summary(lmm)
plot(residuals(lmm))
# Didn't help the residuals much. However, it is correctly identifying that
# There are 3 sites and 48 subplots. The previous lm assumed that there were
# 192 subplots. We do have singular fit, which is expected when estimating
# random effects with 3 levels for Site and 4 levels for Subplot. Here, I
# opt to keep it to avoid pseudoreplication. Moreover, a recent paper
# showed that including random effects with few levels to specify nesting is
# can only help hypothesis tests.


## Clearly the residuals are off, so let's find a better error family:
library(glmmTMB) # A useful package for fitting generalized linear mixed models.
library(DHARMa) # A package of diagnostic functions for such models.

# Poisson:
glmm1 <- glmmTMB(TotalDens ~ Treatment + (1|Site/Subplot), datum2,
                family = poisson)
summary(glmm1)
plot(simulateResiduals(fittedModel = glmm1))

# Negative binomial:
glmm2 <- glmmTMB(TotalDens ~ Treatment + (1|Site/Subplot), datum2,
                 family = nbinom2)
summary(glmm2)
plot(simulateResiduals(fittedModel = glmm2))

# Zero-inflated poisson:
zip1 <- glmmTMB(TotalDens ~ Treatment + (1|Site/Subplot), datum2,
                family = poisson, ziformula = ~1)
summary(zip1)
plot(simulateResiduals(fittedModel = zip1))

# Zero-inflated negative binomial:
zinb1<- glmmTMB(TotalDens ~ Treatment + (1|Site/Subplot), datum2,
                family = nbinom2, ziformula = ~1)
summary(zinb1)
plot(simulateResiduals(fittedModel = zinb1))

# Inspecting the DHARMa residual plots shows that the negative binomial and
# its zero-inflated counterpart fit far better than both poisson models.
# The regular negative binomial looks best, as the model complexity added by
# the zero inflated portion doesn't provide much improvement.

# Can also inspect the AIC score, a measure of model fit.

anova(glmm1, glmm2)
# The negative binomial (glmm2) has a much lower AIC, indicating better fit.

anova(glmm2, zinb1)
# Comparing negative binomial to it's zero-inflated counterpart,
# the delta AIC = 2. This is usually the maximum value to declare models 
# that fit the data equally well, as adding an additional parameter
# will always raise the AIC due to the penalty applied for model complexity.
# Sticking to the less complex model is still probably best. This is also
# supported by the the lack of change in estimates between the two.

## Model summaries:
#install.packages("broom.mixed")
library(broom.mixed)

# Raw output:
glmm2 %>%
  broom.mixed::tidy()

# Marginal means:
#install.packages("emmeans")
library(emmeans)

glmm2 %>%
  emmeans("Treatment", type = "response")

# There is high variation for these faux-effects, leading to low confidence
# in estimates. I'm not too surprised, as there is a lot of variation across
# subplots. This preliminary analysis suggests that there would need to be
# quite a large difference in effect size between treatments and control
# to detect an effect, which is slightly worrisome. I'm flirting with trying a
# spatial autocorrelation model (possibly from package spaMM) or trying a random
# slopes model once my post-treatment data come in after this field season. Not
# sure about the latter, as this can be quite data hungry.

glmm3 <- glmmTMB(TotalDens ~ Treatment + Round + (1|Site/Subplot), datum2, family = nbinom2)
summary(glmm3)
plot(simulateResiduals(glmm3))

anova(glmm2, glmm3)

emmeans(glmm3, specs = c("Treatment"), type = "response")


