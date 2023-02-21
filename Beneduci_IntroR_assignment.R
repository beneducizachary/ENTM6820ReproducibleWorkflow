##### Introduction to R assignment #####

### Question 1 ###
# Code can be entered in the console (not saved)
# or in a script (to be saved).
# The output of the code is typically found in the console,
# although outputs in the form of objects will be stored
# in the global environment.
# Plots show up in the "Plots" tab found at the bottom
# right of the RStudio window.
# The global environment is the area where R objects
# are stored.
# You can go to the console and type "help()" and
# insert the function of interest into the parentheses.
# An R package is a collection of pre-made functions
# that can be downloaded and used.
# A function is a collection of code used to manipulate objects.
# You navigate to the "Packages" tab in the lower right
# window to see all available and loaded functions.
# The working directory is the directory where the R project
# is saved, and we can locate it by running the "getwd()" function.

### Question 2 ###
# In RStudio, go to file and select "New Project".
# Select "Version Control" under "Create a Project".
# Select "Git" as the version control type. 
# If you do not have a Git repository,
# navigate to your browser and login to GitHub.
# Navigate to the "Repositories" tab and click the "New" icon.
# Give the repository a name and select your desired license.
# Inside your new repositoru, select the green "Code" icon
# and copy the url of your repository.
# Paste this url into the Git project wizard, name the local copy of your
# repository, and specify the file path where you would like
# it to be locally stored.
# Your R project should now be linked with Git.

### Questions 3 ###

# A vector is an object of multiple scalar elements (i.e. numbers). It can have
# multiple rows but only one column.
# A matrix is an object that contains only one data class, such as numeric or
# character. A matrix could be considered (at least visually) as being made of
# multiple vectors. A matrix can have multiple rows AND columns.
# A dataframe is an object containing multiple data classes. It can be thought
# of as a matrix with the possibility of different data classes between columns.

### Question 4 ###

z <- 1:200

mean(z)
sd(z)

t <- z > 1

df <- data.frame(z, t)

colnames(df) <- c("z", "zlog")

df$zsquared <- (df$z)^2

df_subset <- subset(df, (zsquared > 10) & (zsquared < 100))

### Question 5 ###

install.packages("ggplot2")
install.packages("dplyr")
install.packages("purrr")
install.packages("lme4")
install.packages("emmeans")

library(ggplot2)
library(dplyr)
library(purrr)
library(lme4)
library(emmeans)

data(iris) # Load the iris dataset for manipulation via packages
head(iris)

## ggplot2
p <- ggplot(data=iris, aes(Sepal.Length, Sepal.Width))
p + geom_point() # scatterplot of Sepal.Width ~ Sepal.Length

## dplyr

iris <- as_tibble(iris) # Convert to tibble

iris %>% arrange(Sepal.Width) # Observations in ascending order by Sepal.Width

## purr

means <- 1:3

randsamp <- map(means, rnorm, n = 3, sd = .5) # Use map function to generate 
# three random samples of 3 subsamples

str(randsamp)

## lme4

data("sleepstudy") # Actually, the iris dataset doesn't have an adequate number of levels
# in "Species" to run a random effect, so I'll use the preloaded lme4 dataset

head(sleepstudy)

sleepstudy$Subject = as.factor(sleepstudy$Subject)

m1 <- lmer(Reaction ~ Days + (1|Subject), sleepstudy, REML = TRUE)

summary(m1)

## emmeans

emmeans(m1, ~Days)

tips_data <- read.csv("https://raw.githubusercontent.com/noelzach/Reproducibility/c29492f2d6e6b36544ee42dfe7a630814ac137cb/06_IntroR/Assignment/TipsR.csv",
                      na.strings = ".")

tips_data # Note NA values under "Gender"
