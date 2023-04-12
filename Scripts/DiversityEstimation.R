### Load data set
datum1 <- read.csv("https://raw.githubusercontent.com/beneducizachary/ENTM6820/main/ReproducibleWorkflow/Data/BMPvegDataRawZB7.csv")
str(datum1)

### Load packages
install.packages("tidyverse")
library(tidyverse)
library(gridExtra)
library(sjmisc)

### Format data for iNext
datum2 <- datum1 %>%
  select(Date, Round, Site, Subplot, Quadrat, ASTU, COTI3, CHFA2, DRAM, GAPU, HEAN2, MOPU, RUHI2) %>%
  filter(Round == "c"|Round == "d"|Round == "e"|Round == "f") %>%
  mutate_at(c(6:13), as.numeric) %>%
  mutate_at(c(6:13), ~tidyr::replace_na(.,0)) %>%
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
  pivot_longer(cols = ASTU:RUHI2, names_to = "Species", values_to = "Count") %>%
  group_by(Treatment, Species) %>%
  summarise(Count=sum(Count)) %>%
  pivot_wider(names_from = Species, values_from = Count) %>%
  rotate_df(cn = TRUE) %>%
  filter(!row_number() %in% c(1)) %>%
  mutate_at(c(1:4), as.numeric)

# Install/load the package iNEXT.
install.packages("devtools")
install.packages("rlang")
install_github('AnneChao/iNEXT')
library(devtools)
library(rlang)
library(iNEXT)

# Create a color blindness friendly palette:
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Rarefaction of hill numbers between treatments:

iNEXT(datum2, q = c(0, 1, 2), datatype = "abundance", nboot = 50, knots = 500) %>%
  ggiNEXT(facet.var = "Order.q") +
  theme_classic() +
  scale_color_manual(values = cbp1)

iNEXT(datum2, q = c(0, 1, 2), datatype = "abundance") %>%
  ggiNEXT(facet.var = "Assemblage", type = 2) +
  theme_classic()

flwrdiv$AsyEst

flwrdiv <- iNEXT(datum2, q = c(0, 1, 2), datatype = "abundance", nboot = 50, knots = 500)

## Plot the asymptotic estimates for hill numbers of orders q = 0, 1, 2

# Create new data frames subset by diversity order from the iNEXT object:

flwrasyrich <- flwrdiv$AsyEst %>%
  as.data.frame() %>%
  subset(Diversity == "Species richness")

flwrasyshan <- flwrdiv$AsyEst %>%
  as.data.frame() %>%
  subset(Diversity == "Shannon diversity")

flwrasysimp <- flwrdiv$AsyEst %>%
  as.data.frame() %>%
  subset(Diversity == "Simpson diversity")

# Plot the estimates

richplot <- flwrasyrich %>%
  mutate_at(c(3:7), as.numeric) %>%
  mutate_at(c(1:2), as.factor) %>%
  ggplot(aes(x = Assemblage, y = Estimator, color = Assemblage, shape = Assemblage)) +
  geom_hline(yintercept = mean(flwrasyrich$Estimator), linetype = 3) +
  geom_errorbar(aes(ymin = LCL, ymax = UCL, width = 0.15)) +
  geom_point(aes(size = 3)) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = .5),
        axis.text = element_text(color = "black")) +
  guides(size = FALSE, color = "none", shape = "none") +
  scale_y_continuous(name = "Hill Richness", limits = c(5, 10)) +
  scale_color_manual(values = cbp1) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  xlab(NULL)
  
shanplot <- flwrasyshan %>%
  mutate_at(c(3:7), as.numeric) %>%
  mutate_at(c(1:2), as.factor) %>%
  ggplot(aes(x = Assemblage, y = Estimator, color = Assemblage, shape = Assemblage)) +
  geom_hline(yintercept = mean(flwrasyshan$Estimator), linetype = 3) +
  geom_errorbar(aes(ymin = LCL, ymax = UCL, width = 0.15)) +
  geom_point(aes(size = 3)) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = .5),
        axis.text = element_text(color = "black")) +
  guides(size = FALSE, color = "none", shape = "none") +
  scale_y_continuous(name = "Hill Shannon", limits = c(3.5, 5)) +
  scale_color_manual(values = cbp1) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  xlab(NULL)

simpplot <- flwrasysimp %>%
  mutate_at(c(3:7), as.numeric) %>%
  mutate_at(c(1:2), as.factor) %>%
  ggplot(aes(x = Assemblage, y = Estimator, color = Assemblage, shape = Assemblage)) +
  geom_hline(yintercept = mean(flwrasysimp$Estimator), linetype = 3) +
  geom_errorbar(aes(ymin = LCL, ymax = UCL, width = 0.15)) +
  geom_point(aes(size = 3)) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = .5),
        axis.text = element_text(color = "black")) +
  guides(size = FALSE, color = "none", shape = "none") +
  scale_y_continuous(name = "Hill Simpson", limits = c(3, 4.5)) +
  scale_color_manual(values = cbp1) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  xlab(NULL)

grid.arrange(richplot, shanplot, simpplot, nrow = 2)

