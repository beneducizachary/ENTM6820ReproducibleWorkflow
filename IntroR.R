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
  geom_point(alpha = 0.5) + # Alpha adjusts the transparency
  geom_smooth(method = lm, color = "black") + # can easily use the options method to avoid this smoothing and apply a linear model
  xlab("Weight") +
  ylab("Miles per gallon") +
  theme_classic() # Can apply themes with buil in visual styles
