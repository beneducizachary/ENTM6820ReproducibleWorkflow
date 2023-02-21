# This is a comment - annotate code with ###
2+2

x=3 # Equal sign works the same way, but usually reserved to be used within a function
y<-2

x+y

name<-"Zach"

seven<-"7"

seven+x # Parentheses denote character values

class(seven)

class(x)

vec <- c(1,2,3,4,5,6,7) # numeric vector
vec <- c(1:7)
vec <- 1:7

vec2 <- c("Zach", "Jie", "Mitch")
vec3 <- c(TRUE, FALSE, TRUE) # Logical vector

vec2[2]

vec+x # Adds three to overy value of the vector

mean(vec)
sd(vec)
sd(vec)/sqrt(7)

se <- function(x){
  y <- sd(x)/sqrt(length(x))
  return(y)
}

sum(vec)
median(vec)
min(vec)
max(vec)
summary(vec)

exp(vec)
#> # greater than
 # < # less than
  #| # or
  #& # and
  #>= 
  #!= # not equal to
  
t <- 1:10
t[(t > 8) | (t < 5)]
t[t != 2]

1 %in% t


mat1 <- matrix(data = c(1,2,3), nrow = 3, ncol = 3) # matrices only contain one
# data class. Dataframes can do multiple.
mat1

mat2 <- matrix(data = c("Zach", "Jie", "Tom"), nrow = 3, ncol = 3)

mat1[5] # If we wanted the middle number of mat1 we can use the number, moving down columns and across rows

mat1[2,2] # Or give the x and y of the coolumns in the matrix

df <- data.frame(mat1[,1], mat2[,1]) # If we want two colums, one from each table

colnames(df) <- c("value", "name")
df  

# The following produce the same outcomes
df[,1]
df$value

# Subsetting dataframes
df[df$name == "Jie",]
df$value[df$name == "Jie"]

# easier way

subset(df, name == "Jie")

#### Installing packages ####

# Functions not installed in base R are available in packages

install.packages("ggplot2")
install.packages("ggpubr")

# Dependencies are needed for many packages

# Need to load package into your environment with library()

library(ggplot2)
library(ggpubr)

#### Importing data into R ####

# Can copy and provide a file path OR make sure your file is in the working directory
datum <- read.csv("BMP_Veg_Data_2022_1_19_2023.csv")
head(datum)

#### Data visualization with the preloaded mtcars in R ####
data("mtcars")

# Call the object to see. Can also use the function head() to call the first few observations
mtcars

# Can make a basic plot
plot(x = mtcars$wt, y = mtcars$mpg)

# CHange the labels of the axes, font style, and point shape
plot(x = mtcars$wt, y = mtcars$mpg,
     xlab = "Car weight",
     ylab = "Miles per gallon",
     font.lab = 6, # Changes the font style
     pch = 20, #pch changes the shape of the points
     )
# Ggplot layers are built upon by adding "geomes" or layers
# Convention is to tstart a new geome on the next line
# Ggplot works with layers. The layers towards the bottom are at the front
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(alpha = 0.5, aes(size = wt, color = wt)) + # Alpha adjusts the transparency
  geom_smooth(method = lm, se = FALSE, color = "blue") + # can easily use the options method to avoid this smoothing and apply a linear model
  xlab("Weight") +
  ylab("Miles per gallon") +
  theme_classic()+ # Can apply themes with built in visual styles
  scale_color_gradient(low = "forestgreen", high = "black")+
  scale_y_log10() # Auto transform the axis

# If you put size into the main portion of function, then it will try to apply size to everything

getwd()

bullrichdat<-read.csv("./Data/Bull_richness.csv", na.strings = NA) # ../ mean one folder up
# If you include "./" and hit tab, it will give options of folders to select.

head(bullrichdat)

# Subset the dataset where Soybean Within No-till treatments are shown
bull.richness.soy.no.till<-bullrichdat[bullrichdat$Crop == "Soy" &
                                           bullrichdat$Treatment == "No-till",]

library(ggplot2)

ggplot(bull.richness.soy.no.till, aes(x = GrowthStage, y = richness, color = Fungicide))+
  geom_boxplot()+
  theme_classic()+
  xlab("Growth Stage")+
  ylab("Richness")+
  geom_point(position = position_jitterdodge(dodge.width = 0.9))# Shows the distribution of all the points, identifying outliers

# Can also import data straight from a web-hosted csv file
bull.richness<- read.csv('https://raw.githubusercontent.com/noelzach/Reproducibility/main/07_ggplot/Bull_richness.csv')

##### Feb 21, 2023 #####

## Need to use fun.data for the mean standard error
ggplot(bull.richness.soy.no.till, aes(x = GrowthStage, y = richness, fill = Fungicide))+
  xlab("Growth Stage")+
  ylab("Richness")+
  stat_summary(fun=mean, geom = "bar", position = "dodge")+
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge")+
  geom_point(position = position_jitterdodge(dodge.width = 0.9), shape = 21, color = "black")+ #Need to change the shape to one with dots and outlines
  scale_color_manual(values = c("blue", "green"))+
  scale_fill_manual(values = c("blue", "green"))

# How to represent these data as a timeseries
# Need to give a group argument in the main ggplot function 
ggplot(bull.richness.soy.no.till, aes(x = GrowthStage, y = richness, color = Fungicide, group = Fungicide))+
  xlab("Growth Stage")+
  ylab("Bulleribasidiaceae \n Richness")+ # \n adds a line break for the label
  stat_summary(fun=mean, geom = "line")+
  stat_summary(fun=mean, geom = "point")+
  stat_summary(fun.data = mean_se, geom = "errorbar")

# Demonstration of facetting

# Need to reorder the levels of your factor

bullrichdat$GrowthStage <- factor(bullrichdat$GrowthStage, levels = c("V6", "V8", "V15", "R3", "R4", "R6"))

palette <- c("Green", "Black")

ggplot(bullrichdat, aes(x = GrowthStage, y = richness, color = Fungicide, group = Fungicide))+
  xlab("Growth Stage")+
  ylab("Bulleribasidiaceae \n Richness")+
  stat_summary(fun=mean, geom = "line")+
  stat_summary(fun=mean, geom = "point")+
  stat_summary(fun.data = mean_se, geom = "errorbar")+
  facet_wrap(~Crop*Treatment, scales = "free")+ # Adding an interaction by crop facets by treamtment and crop
  # can change the order of the interaction to change the headings
  # Can also change the scales for each facet via the "scales = "free'" argument
  scale_color_manual(values = palette)+
  theme_classic()
  # Should recolor for color blindness and add jittered points to show the distribution

