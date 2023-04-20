
### Load data set
datum1 <- read.csv("https://raw.githubusercontent.com/beneducizachary/ENTM6820/main/ReproducibleWorkflow/Data/BMPvegDataRawZB7.csv")
str(datum1)

### Load packages
install.packages("tidyverse")
library(tidyverse)

### Format for analysis of planted wildflower richness and density.
datum2 <- datum1 %>%
  # Select relevant grouping variables and the planted species with counts > 0.
  select(Date, Round, Site, Subplot, Quadrat, ASTU, COTI3, CHFA2, DRAM, GAPU, HEAN2, MOPU, RUHI2) %>%
  # Filter out sampling rounds 'a' and 'b'. We did not detect any planted wildflowers in these rounds.
  filter(Round == "c"|Round == "d"|Round == "e"|Round == "f") %>%
  # Making sure counts are considered numbers.
  mutate_at(c(6:13), as.numeric) %>%
  # Replacing the empty cells filled with NAs by R with zeros.
  mutate_at(c(6:13), ~tidyr::replace_na(.,0)) %>%
  # Switch to long format for calculations.
  pivot_longer(cols = ASTU:RUHI2, names_to = "Species", values_to = "Count") %>%
  # Specify variables to group by for calculations.
  group_by(Site, Round, Subplot, Species) %>%
  # Summing counts for wildflowers across the 10 quadrats per subplot.
  summarise(Count=sum(Count)) %>%
  # Switch back to wide.
  pivot_wider(names_from = Species, values_from = Count) %>%
  # Specify we want next calculations applied across columns.
  rowwise() %>%
  mutate(
    TotalDens = sum(c_across(ASTU:RUHI2)), # Sums all counts across columns.
    Richness = sum(c_across(ASTU:RUHI2)!=0) # Counts the frequency of values >0.
  ) %>%
  # Assign subplots to columns and rows within fields for spatial visualization.
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
  # We'll add the treatment information.
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
  # Set treatment as a factor.
  mutate_at(c(16), as.factor) %>%
  # Relevel the Treatment variable so Control is the reference group.
  mutate(Treatment = relevel(Treatment, "Control")) %>%
  # Set Subplot variable as a factor for random effects.
  mutate_at(c(3), as.factor)

### Data visualization

## Overall density
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

# Check the distribution of the response:
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
# and those above zero are typically higher. Additionally, the residuals
# are heteroscedastic.

# Correct pseudoreplication with a mixed model:

#install.packages("lme4)
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


# It's also important to note that hierarchical models make the interpretation
# of raw residual plots problematic. We'll have to turn to DHARMa package for 
# diagnostic residual plots from here on out.

library(glmmTMB) # A useful package for fitting generalized linear mixed models.
library(DHARMa) # A package of diagnostic functions for such models.

# Poisson:
pois <- glmmTMB(TotalDens ~ Treatment + (1|Site/Subplot), datum2,
                family = poisson)
summary(pois)

plot(simulateResiduals(fittedModel = pois))

# Negative binomial:
nb <- glmmTMB(TotalDens ~ Treatment + (1|Site/Subplot), datum2,
              family = nbinom2)
summary(nb)

plot(simulateResiduals(fittedModel = nb))

# Zero-inflated poisson:
zip <- glmmTMB(TotalDens ~ Treatment + (1|Site/Subplot), datum2,
               family = poisson, ziformula = ~1)
summary(zip)

plot(simulateResiduals(fittedModel = zip))

# Zero-inflated negative binomial:
zinb <- glmmTMB(TotalDens ~ Treatment + (1|Site/Subplot), datum2,
                family = nbinom2, ziformula = ~1)
summary(zinb)

plot(simulateResiduals(fittedModel = zinb))

# Inspecting the DHARMa residual plots shows that the negative binomial and
# its zero-inflated counterpart fit far better than both poisson models.
# The regular negative binomial looks best, as the model complexity added by
# the zero inflated portion doesn't provide much improvement.

# Can also inspect the AIC score for a measure of model fit.

anova(pois, nb, zip, zinb)

# The classic negative binomial has the lowest AIC, although the delta AIC 
# between the nb and zinb is exact 2. This would warrant considering both models
# as well supported, as a penalty is added to the AIC score for number of
# parameters, so the AIC score will rise slightly by estimating the intercept
# of the zero-inflation side of the model.

# I'll opt for the more parsimonious model, the nb, rather than the more complex
# zinb. Additionally, the estimates don't change between the two.

# We can also compare the nb with and without random effects.

# First fit a model without random effects:

nb2 <- glmmTMB(TotalDens ~ Treatment, datum2, family = nbinom2)
summary(nb2)

plot(simulateResiduals(fittedModel = nb2))

# Now compare:

anova(nb2, nb)

# And we get a better fit with the random effects.

# Remember that our quick plots of the data show that Round is interacting with 
# counts, which is expected. Therefore, we should consider it's inclusion:

# Consider Treatment effects consistent across Rounds:

nb3 <- glmmTMB(TotalDens ~ Treatment + Round + (1|Site/Subplot), datum2,
               family = nbinom2)
plot(simulateResiduals(nb3))

# And now Round modifies the effect of Treatments via an interaction:

nb4 <- glmmTMB(TotalDens ~ Treatment*Round + (1|Site/Subplot), datum2,
               family = nbinom2)
plot(simulateResiduals(fittedModel = nb4))

# And let's compare them to the model without Round:

anova(nb, nb3, nb4)

# AIC shows that nb3, the model with + Round, fits better than the models 
# without Round and a Treatment*Round interaction, respectively. Additionally, 
# DHARMa diagnotics do not suggest any problems with model fit.

# We may expect there to be an interaction of Treatment and Round in the future,
# but this dataset (where treatment effects should actually not be present)
# supports the simpler model.

# Create a model selection table for all models:

#install.packages("AICcmodavg")
#install.packages("kimisc")
library(AICcmodavg)
library(kimisc)

DensAICtab <- tibble::lst(pois, nb, zip, zinb, nb2, nb3, nb4) %>%
  aictab(second.ord = TRUE) %>%
  as.data.frame() %>%
  mutate(Formula = 
           case_when(Modnames == "nb3" ~ "Density ~ Treatment + Round + (1|Plot/Subplot)",
                     Modnames == "nb4" ~ "Density ~ Treatment*Round + (1|Plot/Subplot)",
                     Modnames == "nb" ~ "Density ~ Treatment + (1|Plot/Subplot)",
                     Modnames == "zinb" ~ "Density ~ Treatment + (1|Plot/Subplot)",
                     Modnames == "nb2" ~ "Density ~ Treatment",
                     Modnames == "zip" ~ "Density ~ Treatment + (1|Plot/Subplot",
                     Modnames == "pois" ~ "Density ~ Treatment + (1|Plot/Subplot")) %>%
  mutate(Family =
           case_when(Modnames == "nb3" ~ "neg.binom",
                     Modnames == "nb4" ~ "neg.binom",
                     Modnames == "nb" ~ "neg.binom",
                     Modnames == "zinb" ~ "zero.inf.neg.binom",
                     Modnames == "nb2" ~ "neg.binom",
                     Modnames == "zip" ~ "zero.inf.pois",
                     Modnames == "pois" ~ "pois")) %>%
  select(Formula, Family, AICc, Delta_AICc, K, AICcWt)

DensAICtab

# For exporting only:
#write.csv(DensAICtab, file = 
#"C:/Classes/ENTM6280/Workflow/ENTM6820ReproducibleWorkflow/Tables/DensAICtab.csv")

##### Results/Summary

# Let's actually look at the results:

#install.packages("ggeffects")
#install.packages("gridExtra")
#install.packages("MuMIn")
#install.packages("broom.mixed")
library(ggeffects)
library(gridExtra)
library(MuMIn)
library(broom.mixed)

# Looking at emmeans from the model. 
nb3emmeans <- ggpredict(nb3)
nb3emmeans

cbp1 <- c("#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

nb3TreatOut <- as.data.frame(nb3emmeans$Treatment) %>%
  ggplot(aes(x = x, y = predicted, color = x, shape = x)) +
  geom_hline(yintercept = mean(nb3emmeans$Treatment$predicted), linetype = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, width = 0.15)) +
  geom_point(aes(size = 3)) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = .5),
        axis.text = element_text(color = "black")) +
  guides(size = FALSE, color = "none", shape = "none") +
  scale_x_discrete(name = NULL, limits = c("Control", "Mowing", "Disking", "Combo")) +
  scale_y_continuous(name = bquote('Wildflower density per 10'~m^2)) +
  scale_color_manual(values = cbp1) +
  scale_shape_manual(values = c(15, 16, 17, 18))

nb3RoundOut <- as.data.frame(nb3emmeans$Round) %>%
  ggplot(aes(x = x, y = predicted, color = x, shape = x)) +
  geom_hline(yintercept = mean(nb3emmeans$Round$predicted), linetype = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, width = 0.15)) +
  geom_point(aes(size = 3)) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = .5),
        axis.text = element_text(color = "black")) +
  guides(size = FALSE, color = "none", shape = "none") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = bquote('Wildflower density per 10'~m^2)) +
  scale_color_manual(values = cbp1) +
  scale_shape_manual(values = c(15, 16, 17, 18))

grid.arrange(nb3TreatOut, nb3RoundOut, nrow = 1)
```

Results as rate ratios (my preference):
  
  ```{r}
# Tidy estimates for the Treatment plot.
densEstTrt <- broom.mixed::tidy(nb3, conf.int = TRUE) %>%
  mutate(expEstimate = exp(estimate)) %>%
  mutate(expConf.low = exp(conf.low)) %>%
  mutate(expConf.high = exp(conf.high)) %>%
  filter(term == 'TreatmentCombo'|term == 'TreatmentDisking'|term == 'TreatmentMowing')

# Tidy estimates for the Round plot.
densEstRnd <- broom.mixed::tidy(nb3, conf.int = TRUE) %>%
  mutate(expEstimate = exp(estimate)) %>%
  mutate(expConf.low = exp(conf.low)) %>%
  mutate(expConf.high = exp(conf.high)) %>%
  filter(term == 'Roundd'|term == 'Rounde'|term == 'Roundf')

# Plot for Treatment effects.
densTrtplot <- densEstTrt %>%
  mutate(term = 
           case_when(term == "TreatmentMowing" ~ "Mowing",
                     term == "TreatmentDisking" ~ "Disking",
                     term == "TreatmentCombo" ~ "Combo")) %>%
  ggplot(aes(x = expEstimate, y = term, color = term, shape = term)) +
  geom_errorbar(aes(xmin = expConf.low, xmax = expConf.high, width = 0.15)) +
  geom_vline(xintercept = 1, linetype = 3, size = 1, color = "red") +
  geom_vline(xintercept = mean(densEstTrt$expEstimate), linetype = 3) +
  geom_point(aes(size = 2)) +
  geom_text(aes(label = paste(round(expEstimate, digits = 2),
                              "^(", round(p.value, digits = 3), ")", sep = ""),
                color = NULL, vjust = -1, hjust = -0.000001),
            parse = TRUE) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = .5),
        axis.text = element_text(color = "black"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.title = element_text(size = 12)) +
  guides(size = FALSE, color = "none", shape = "none") +
  scale_y_discrete(name = NULL, limits = c("Combo", "Disking", "Mowing")) +
  scale_x_continuous(limits = c(.5,2)) +
  xlab(label = 'Rate ratio of wildflower density per 10' ~m^2) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  scale_shape_manual(values = c(16, 17, 18))

# Plot for Round effects
densRndplot <- densEstRnd %>%
  mutate(term = 
           case_when(term == "Roundd" ~ "July/Aug",
                     term == "Rounde" ~ "Aug/Sept",
                     term == "Roundf" ~ "Oct")) %>%
  ggplot(aes(x = expEstimate, y = term, color = term, shape = term)) +
  geom_errorbar(aes(xmin = expConf.low, xmax = expConf.high, width = 0.15)) +
  geom_vline(xintercept = 1, linetype = 3, size = 1, color = "red") +
  geom_vline(xintercept = mean(densEstRnd$expEstimate), linetype = 3) +
  geom_point(aes(size = 2)) +
  geom_text(aes(label = paste(round(expEstimate, digits = 2),
                              "^(", round(p.value, digits = 3), ")", sep = ""),
                color = NULL, vjust = -1, hjust = -0.000001),
            parse = TRUE) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = .5),
        axis.text = element_text(color = "black"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.title = element_text(size = 12)) +
  guides(size = FALSE, color = "none", shape = "none") +
  scale_y_discrete(name = NULL, limits = c("Oct", "Aug/Sept", "July/Aug")) +
  scale_x_continuous(limits = c(0,1)) +
  xlab(label = 'Rate ratio of wildflower density per 10' ~m^2) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#CC79A7")) +
  scale_shape_manual(values = c(16, 17, 18))

# Both plots arranged.
grid.arrange(densTrtplot, densRndplot, nrow = 1)

# Only need this if exporting:
#g <- arrangeGrob(densTrtplot, densRndplot, nrow = 1)
#ggsave(path = "Figures", filename = "DensFig.png", plot = g, width = 8, height = 4)

# Note: p-values for the comparison of treatments to control the control
# group (left) and survey rounds to the June group (right) are supplied in the
# superscript. The the average of beta estimates and a rate ratio of 1
# (denoting no difference) are presented as grey dotted and red dotted lines,
# respectively.


