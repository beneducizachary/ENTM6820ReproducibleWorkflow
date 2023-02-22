
##### Question 2 #####

# Load the libraries
library(ggplot2)

# Load the data
datum <- read.csv("https://raw.githubusercontent.com/noelzach/EndophyteBiocontrol/main/Data/MycotoxinData.csv")

head(datum)

## Construct a boxplot

# For some reason there's an issue with DON being auto classified as a character
summary(datum$DON)

datum$DON = as.numeric(datum$DON) # Change to a numeric type

ggplot(datum, aes(x=Treatment, y = DON, color = Cultivar))+
  geom_boxplot()+
  ylab("DON (ppm)")+
  xlab("")

##### Question 3 #####
## Construct a barplot

# Create a vector containing the color palette to be used

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# I didn't end up calling it, actually

ggplot(datum, aes(x=Treatment, y = DON, fill = Cultivar, group = Cultivar))+
  stat_summary(fun=mean, geom = "bar", position = "dodge")+
  ylab("DON (ppm)")+
  xlab("")+
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge")

##### Question 4 #####

# Add points to the boxplot

ggplot(datum, aes(x=Treatment, y = DON, fill = Cultivar))+
  geom_boxplot(outlier.size = 0)+ # Removed outliers since they are also plotted with geom_point. 
  ylab("DON (ppm)")+
  xlab("")+
  geom_point(position = position_jitterdodge(dodge.width = 0.9), shape =21, color = "black")

# Add points and change the color of the barplot

ggplot(datum, aes(x=Treatment, y = DON, fill = Cultivar, group = Cultivar))+
  stat_summary(fun=mean, geom = "bar", position = "dodge")+
  ylab("DON (ppm)")+
  xlab("")+
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge")+
  geom_point(position = position_jitterdodge(dodge.width = 0.9), shape = 21, colour = "black")

##### Question 5 #####

# Add colorblind-friendly colors to the boxplot

ggplot(datum, aes(x=Treatment, y = DON, fill = Cultivar))+
  geom_boxplot(outlier.size = 0)+ # Removed outliers since they are also plotted with geom_point. 
  ylab("DON (ppm)")+
  xlab("")+
  geom_point(position = position_jitterdodge(dodge.width = 0.9), shape =21, color = "black")+
  scale_fill_manual(values = c("#0072B2", "#D55E00"))

# Add those colors to the barplot

ggplot(datum, aes(x=Treatment, y = DON, fill = Cultivar, group = Cultivar))+
  stat_summary(fun=mean, geom = "bar", position = "dodge")+
  ylab("DON (ppm)")+
  xlab("")+
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge")+
  geom_point(position = position_jitterdodge(dodge.width = 0.9), shape = 21, colour = "black")+
  scale_fill_manual(values = c("#0072B2", "#D55E00"))

# Facet wrap the boxplot

ggplot(datum, aes(x=Treatment, y = DON, fill = Cultivar))+
  geom_boxplot(outlier.size = 0)+ # Removed outliers since they are also plotted with geom_point. 
  ylab("DON (ppm)")+
  xlab("")+
  geom_point(position = position_jitterdodge(dodge.width = 0.9), shape =21, color = "black")+
  scale_fill_manual(values = c("#0072B2", "#D55E00"))+
  facet_wrap(~Cultivar)

# Facet wrap the barplot

ggplot(datum, aes(x=Treatment, y = DON, fill = Cultivar, group = Cultivar))+
  stat_summary(fun=mean, geom = "bar", position = "dodge")+
  ylab("DON (ppm)")+
  xlab("")+
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge")+
  geom_point(position = position_jitterdodge(dodge.width = 0.9), shape = 21, colour = "black")+
  scale_fill_manual(values = c("#0072B2", "#D55E00"))+
  facet_wrap(~Cultivar)

# Change the boxplot to classic theme

ggplot(datum, aes(x=Treatment, y = DON, fill = Cultivar))+
  geom_boxplot(outlier.size = 0)+ # Removed outliers since they are also plotted with geom_point. 
  ylab("DON (ppm)")+
  xlab("")+
  geom_point(position = position_jitterdodge(dodge.width = 0.9), shape =21, color = "black")+
  scale_fill_manual(values = c("#0072B2", "#D55E00"))+
  facet_wrap(~Cultivar)+
  theme_classic()

# Change the barplot to minimal theme

ggplot(datum, aes(x=Treatment, y = DON, fill = Cultivar, group = Cultivar))+
  stat_summary(fun=mean, geom = "bar", position = "dodge")+
  ylab("DON (ppm)")+
  xlab("")+
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge")+
  geom_point(position = position_jitterdodge(dodge.width = 0.9), shape = 21, colour = "black")+
  scale_fill_manual(values = c("#0072B2", "#D55E00"))+
  facet_wrap(~Cultivar)+
  theme_minimal()

# Add transparency to the boxplot points

ggplot(datum, aes(x=Treatment, y = DON, fill = Cultivar))+
  geom_boxplot(outlier.size = 0)+ # Removed outliers since they are also plotted with geom_point. 
  ylab("DON (ppm)")+
  xlab("")+
  geom_point(position = position_jitterdodge(dodge.width = 0.9), shape =21, color = "black",
             alpha = 0.5)+
  scale_fill_manual(values = c("#0072B2", "#D55E00"))+
  facet_wrap(~Cultivar)+
  theme_classic()

# Add transparency to the barplot points

ggplot(datum, aes(x=Treatment, y = DON, fill = Cultivar, group = Cultivar))+
  stat_summary(fun=mean, geom = "bar", position = "dodge")+
  ylab("DON (ppm)")+
  xlab("")+
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge")+
  geom_point(position = position_jitterdodge(dodge.width = 0.9), shape = 21, colour = "black",
             alpha = 0.5)+
  scale_fill_manual(values = c("#0072B2", "#D55E00"))+
  facet_wrap(~Cultivar)+
  theme_minimal()

##### Question 9 #####

# Both the boxlpots and barplots are showing the central tendency of groupings of data.
# The middle line of the boxplot showing the median and the barplot showing the
# mean (in this case). However, the distribution of the data is hidden behind the base barplot,
# while the boxplot shows the 25% quartile ranges and highlights any outlier data points.
# The barplot is far more obscure about the distribution of the data compared to the boxplot.
# Additionally, the mean can be a biased measure of central tendency when data are drawn from
# a non-gaussian process, so the standard barplot can be misleading. The boxplot is better
# for visualizing and presenting differences between groups due to its automatic plotting
# of central tendency and data distribution.

##### Question 10 #####

ggplot(datum, aes(Treatment, DON, fill = Cultivar))+
  geom_violin()

