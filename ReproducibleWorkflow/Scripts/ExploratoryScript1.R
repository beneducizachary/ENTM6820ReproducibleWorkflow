#### Exploratory script for workflow

### Load data set
datum1 <- read.csv("https://raw.githubusercontent.com/beneducizachary/ENTM6820/main/ReproducibleWorkflow/Data/BMPvegDataRawZB7.csv")
str(datum1)

### Load packages
install.packages("tidyverse")
library(tidyverse)
library(dplyr)

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
    )

ggplot(datum2, aes(x = as.factor(Subplot), y = Richness)) +
  geom_boxplot() +
  facet_wrap(~Site)
  
ggplot(datum2, aes(x = as.factor(Subplot), y = TotalDens)) +
  geom_boxplot() +
  facet_wrap(~Site)
