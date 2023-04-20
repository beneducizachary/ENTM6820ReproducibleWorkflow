### Load data set
datum1 <- read.csv("https://raw.githubusercontent.com/beneducizachary/ENTM6820/main/ReproducibleWorkflow/Data/BMPvegDataRawZB7.csv")

### Load packages
#install.packages("tidyverse")
#install.packages("gridExtra")
#install.packages("sjmisc")
library(tidyverse)
library(gridExtra)
library(sjmisc)

### Format data for iNext by treatment:
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

### Install/load the package iNEXT.
#install.packages("devtools")
#install.packages("rlang")
#install_github('AnneChao/iNEXT')
library(devtools)
library(rlang)
library(iNEXT)

# Create a color blindness friendly palette:
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## Rarefaction of hill numbers between treatments:

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
  geom_errorbar(aes(ymin = Estimator, ymax = UCL, width = 0.15)) +
  geom_point(aes(size = 3)) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = .5),
        axis.text = element_text(color = "black")) +
  guides(size = FALSE, color = "none", shape = "none") +
  scale_y_continuous(name = "Hill Richness", limits = c(0, 10)) +
  scale_x_discrete(name = NULL, limits = c("Control", "Mowing", "Disking", "Combo")) +
  scale_color_manual(values = c("#E69F00", "#999999", "#56B4E9", "#009E73")) +
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
  scale_y_continuous(name = "Hill Shannon", limits = c(0, 10)) +
  scale_x_discrete(name = NULL, limits = c("Control", "Mowing", "Disking", "Combo")) +
  scale_color_manual(values = c("#E69F00", "#999999", "#56B4E9", "#009E73")) +
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
  scale_y_continuous(name = "Hill Simpson", limits = c(0, 10)) +
  scale_x_discrete(name = NULL, limits = c("Control", "Mowing", "Disking", "Combo")) +
  scale_color_manual(values = c("#E69F00", "#999999", "#56B4E9", "#009E73")) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  xlab(NULL)

grid.arrange(richplot, shanplot, simpplot, nrow = 1)

# Only need this if exporting:
g1 <- arrangeGrob(richplot, shanplot, simpplot, nrow = 1)
ggsave(path = "Figures", filename = "TrtDivFig.png", plot = g1, width = 8, height = 4)

## Run rarefaction of hill numbers between sites:

# Summarize by site:

datum3 <- datum1 %>%
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
  group_by(Site, Species) %>%
  summarise(Count=sum(Count)) %>%
  pivot_wider(names_from = Species, values_from = Count) %>%
  rotate_df(cn = TRUE) %>%
  filter(!row_number() %in% c(1)) %>%
  mutate_at(c(1:3), as.numeric)

# Run iNEXT:

flwrdivSite <- iNEXT(datum3, q = c(0, 1, 2), datatype = "abundance", nboot = 50,
                     knots = 500)

# Subset the diversity orders:

flwrasyrichSite <- flwrdivSite$AsyEst %>%
  as.data.frame() %>%
  subset(Diversity == "Species richness")

flwrasyshanSite <- flwrdivSite$AsyEst %>%
  as.data.frame() %>%
  subset(Diversity == "Shannon diversity")

flwrasysimpSite <- flwrdivSite$AsyEst %>%
  as.data.frame() %>%
  subset(Diversity == "Simpson diversity")

# And plot these site estimates:

richplotSite <- flwrasyrichSite %>%
  mutate_at(c(3:7), as.numeric) %>%
  mutate_at(c(1:2), as.factor) %>%
  ggplot(aes(x = Assemblage, y = Estimator, color = Assemblage, shape = Assemblage)) +
  geom_hline(yintercept = mean(flwrasyrichSite$Estimator), linetype = 3) +
  geom_errorbar(aes(ymin = Estimator, ymax = UCL, width = 0.15)) +
  geom_point(aes(size = 3)) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = .5),
        axis.text = element_text(color = "black")) +
  guides(size = FALSE, color = "none", shape = "none") +
  scale_y_continuous(name = "Hill Richness", limits = c(0,10)) +
  scale_x_discrete(name = NULL, limits = c("PBU1", "FCU1", "BEU1")) +
  scale_color_manual(values = c("#490092", "#000000", "#924900")) +
  scale_shape_manual(values = c(16, 17, 18)) +
  xlab(NULL)

shanplotSite <- flwrasyshanSite %>%
  mutate_at(c(3:7), as.numeric) %>%
  mutate_at(c(1:2), as.factor) %>%
  ggplot(aes(x = Assemblage, y = Estimator, color = Assemblage, shape = Assemblage)) +
  geom_hline(yintercept = mean(flwrasyshanSite$Estimator), linetype = 3) +
  geom_errorbar(aes(ymin = LCL, ymax = UCL, width = 0.15)) +
  geom_point(aes(size = 3)) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = .5),
        axis.text = element_text(color = "black")) +
  guides(size = FALSE, color = "none", shape = "none") +
  scale_y_continuous(name = "Hill Shannon", limits = c(0,10)) +
  scale_x_discrete(name = NULL, limits = c("PBU1", "FCU1", "BEU1")) +
  scale_color_manual(values = c("#490092", "#000000", "#924900")) +
  scale_shape_manual(values = c(16, 17, 18)) +
  xlab(NULL)

simpplotSite <- flwrasysimpSite %>%
  mutate_at(c(3:7), as.numeric) %>%
  mutate_at(c(1:2), as.factor) %>%
  ggplot(aes(x = Assemblage, y = Estimator, color = Assemblage, shape = Assemblage)) +
  geom_hline(yintercept = mean(flwrasysimpSite$Estimator), linetype = 3) +
  geom_errorbar(aes(ymin = LCL, ymax = UCL, width = 0.15)) +
  geom_point(aes(size = 3)) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = .5),
        axis.text = element_text(color = "black")) +
  guides(size = FALSE, color = "none", shape = "none") +
  scale_y_continuous(name = "Hill Simpson", limits = c(0,10)) +
  scale_x_discrete(name = NULL, limits = c("PBU1", "FCU1", "BEU1")) +
  scale_color_manual(values = c("#490092", "#000000", "#924900")) +
  scale_shape_manual(values = c(16, 17, 18)) +
  xlab(NULL)

grid.arrange(richplotSite, shanplotSite, simpplotSite, nrow = 1)

# Only need this if exporting:
g2 <- arrangeGrob(richplotSite, shanplotSite, simpplotSite, nrow = 1)
ggsave(path = "Figures", filename = "SiteDivFig.png", plot = g2, width = 8, height = 4)

## Run rarefaction of hill numbers between rounds:

# Summarize by round:

datum4 <- datum1 %>%
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
  mutate(Round = 
           case_when(Round == "c" ~ "June/July",
                     Round == "d" ~ "July/Aug",
                     Round == "e" ~ "Aug/Sept",
                     Round == "f" ~ "Oct")) %>%
  group_by(Round, Species) %>%
  summarise(Count=sum(Count)) %>%
  pivot_wider(names_from = Species, values_from = Count) %>%
  rotate_df(cn = TRUE) %>%
  filter(!row_number() %in% c(1)) %>%
  mutate_at(c(1:4), as.numeric)

# Run iNEXT:

flwrdivRound <- iNEXT(datum4, q = c(0, 1, 2), datatype = "abundance", nboot = 50,
                      knots = 500)

# Subset the diversity orders:

flwrasyrichRound <- flwrdivRound$AsyEst %>%
  as.data.frame() %>%
  subset(Diversity == "Species richness")

flwrasyshanRound <- flwrdivRound$AsyEst %>%
  as.data.frame() %>%
  subset(Diversity == "Shannon diversity")

flwrasysimpRound <- flwrdivRound$AsyEst %>%
  as.data.frame() %>%
  subset(Diversity == "Simpson diversity")

# And plot these site estimates:

richplotRound <- flwrasyrichRound %>%
  mutate_at(c(3:7), as.numeric) %>%
  mutate_at(c(1:2), as.factor) %>%
  ggplot(aes(x = Assemblage, y = Estimator, color = Assemblage, shape = Assemblage)) +
  geom_hline(yintercept = mean(flwrasyrichRound$Estimator), linetype = 3) +
  geom_errorbar(aes(ymin = Estimator, ymax = UCL, width = 0.15)) +
  geom_point(aes(size = 3)) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = .5),
        axis.text = element_text(color = "black")) +
  guides(size = FALSE, color = "none", shape = "none") +
  scale_y_continuous(name = "Hill Richness", limits = c(1,9)) +
  scale_x_discrete(name = NULL, limits = c("June/July", "July/Aug", "Aug/Sept", "Oct")) +
  scale_color_manual(values = c("#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  xlab(NULL)

shanplotRound <- flwrasyshanRound %>%
  mutate_at(c(3:7), as.numeric) %>%
  mutate_at(c(1:2), as.factor) %>%
  ggplot(aes(x = Assemblage, y = Estimator, color = Assemblage, shape = Assemblage)) +
  geom_hline(yintercept = mean(flwrasyshanRound$Estimator), linetype = 3) +
  geom_errorbar(aes(ymin = LCL, ymax = UCL, width = 0.15)) +
  geom_point(aes(size = 3)) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = .5),
        axis.text = element_text(color = "black")) +
  guides(size = FALSE, color = "none", shape = "none") +
  scale_y_continuous(name = "Hill Shannon", limits = c(1,9)) +
  scale_x_discrete(name = NULL, limits = c("June/July", "July/Aug", "Aug/Sept", "Oct")) +
  scale_color_manual(values = c("#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  xlab(NULL)

simpplotRound <- flwrasysimpRound %>%
  mutate_at(c(3:7), as.numeric) %>%
  mutate_at(c(1:2), as.factor) %>%
  ggplot(aes(x = Assemblage, y = Estimator, color = Assemblage, shape = Assemblage)) +
  geom_hline(yintercept = mean(flwrasysimpRound$Estimator), linetype = 3) +
  geom_errorbar(aes(ymin = LCL, ymax = UCL, width = 0.15)) +
  geom_point(aes(size = 3)) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = .5),
        axis.text = element_text(color = "black")) +
  guides(size = FALSE, color = "none", shape = "none") +
  scale_y_continuous(name = "Hill Simpson", limits = c(1,9)) +
  scale_x_discrete(name = NULL, limits = c("June/July", "July/Aug", "Aug/Sept", "Oct")) +
  scale_color_manual(values = c("#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  xlab(NULL)

grid.arrange(richplotRound, shanplotRound, simpplotRound, nrow = 1)

# Only need this if exporting:
g3 <- arrangeGrob(richplotRound, shanplotRound, simpplotRound, nrow = 1)
ggsave(path = "Figures", filename = "RoundDivFig.png", plot = g3, width = 9, height = 4)
