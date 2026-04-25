################################################################################
# Student Name: Nikita Mishra
# Date: 25-04-2026
# Course: Intermediate Analytics
# Section: 31
# ALY6015 - Module 2: 
################################################################################


################################################################################
#Load Required Libraries
################################################################################

required_packages <- c("tidyverse", "dplyr", "ggplot2")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

lapply(required_packages, install_if_missing)


#-------------------------------------------------------------------------------
# SECTION 11.1
#A- (Blood Types)
#-------------------------------------------------------------------------------

observed <- c(12,8,24,6)
expected <- c(0.20,0.28,0.36,0.16)

chisq.test(x = observed, p = expected)


barplot(observed,
        names.arg = c("A", "B", "O", "AB"),
        main = "Blood Type Distribution",
        xlab = "Blood Type",
        ylab = "Frequency",
        col = "lightblue",
        border = "black")


#Explain
#This test evaluates whether the observed blood type distribution significantly differs from the expected proportions.

#-------------------------------------------------------------------------------
# SECTION 11.1 
# B-(Airlines)
#-------------------------------------------------------------------------------

observed <- c(125, 10, 18, 40)
expected <- c(0.708, 0.082, 0.09, 0.12)

chisq.test(x = observed, p = expected)


barplot(observed,
        names.arg = c("On Time", "NAS Delay", "Late Aircraft", "Other"),
        main = "Flight Performance",
        xlab = "Flight Status",
        ylab = "Number of Flights",
        col = "orange",
        border = "black")


#Explain
#This test checks whether the observed flight delay categories follow the expected distribution.


#-------------------------------------------------------------------------------
# SECTION 11.2 
# A- (Ethnicity vs Year)
#-------------------------------------------------------------------------------

data <- matrix(c(724,335,174,107,
                 370,292,152,140),
               nrow = 2, byrow = TRUE)

chisq.test(data)


#Explain
#This test checks whether the observed ethnicity distribution across groups follow the expected distribution.


#-------------------------------------------------------------------------------
# SECTION 11.2 
# B- (Women in Military)
#-------------------------------------------------------------------------------

Data1 <- matrix(c(10791,62491,
                  7816,42750,
                  932,9525,
                  11819,54344),
                nrow = 4, byrow = TRUE)


chisq.test(Data1)

#Explain
#This test examines whether gender distribution differs significantly across military categories.


#-------------------------------------------------------------------------------
# SECTION 12.1 
# A- (Sodium ANOVA)
#-------------------------------------------------------------------------------

condiments <- c(270,130,230,180,80,70,200)
cereals <- c(260,220,290,290,200,320,140)
desserts <- c(100,180,250,250,300,360,300,160)

group <- factor(c(rep("Condiments",7),
                  rep("Cereals",7),
                  rep("Desserts",8)))

values <- c(condiments, cereals, desserts)

anova_model <- aov(values ~ group)

summary(anova_model)


#Boxplot

boxplot(values ~ group,
        col = c("lightgreen", "lightblue", "lightpink"),
        main = "Sodium Levels by Food Category",
        xlab = "Food Category",
        ylab = "Sodium (mg)",
        border = "black")

means <- tapply(values, group, mean)
points(1:3, means, col = "red", pch = 19)

#Explain
#This analysis tests whether mean sodium levels differ significantly across food categories.


#-------------------------------------------------------------------------------
# SECTION 12.2 
# A- (Sales ANOVA + Tukey)
#-------------------------------------------------------------------------------

cereal <- c(578,320,264,249,237)
candy <- c(311,106,109,125,173)
coffee <- c(261,185,302,689)

group <- factor(c(rep("Cereal",5),
                  rep("Candy",5),
                  rep("Coffee",4)))

values <- c(cereal, candy, coffee)

sales_model <- aov(values ~ group)
summary(sales_model)

TukeyHSD(sales_model)

means <- tapply(values, group, mean)
points(1:3, means, col = "red", pch = 19)

#Boxplot

means <- tapply(values, group, mean)

bp <- barplot(means,
              col = c("lightblue", "lightpink", "lightgreen"),
              main = "Mean Sales by Category",
              xlab = "Category",
              ylab = "Mean Sales",
              border = "black")

#Explain
#This analysis evaluates whether average sales differ significantly among product categories.


#-------------------------------------------------------------------------------
#SECTION 12.2 
#B- Per-Pupil Expenditures
#-------------------------------------------------------------------------------

# Per-Pupil Expenditure
eastern <- c(4946,5953,6202,7243,6113)
middle <- c(6149,7451,6000,6479)
western <- c(5282,8605,6528,6911)

values <- c(eastern, middle, western)

group <- factor(c(rep("Eastern",5),
                  rep("Middle",4),
                  rep("Western",4)))


model3 <- aov(values ~ group)
summary(model3)

TukeyHSD(model3)

boxplot(values ~ group,
        col = c("lightblue","lightgreen","lightpink"),
        main = "Per-Pupil Expenditure",
        xlab = "Region",
        ylab = "Expenditure ($)",
        border = "black")


#Explain
#This analysis checks whether mean expenditure differs significantly across regions.


#-------------------------------------------------------------------------------
# SECTION 12.3- Increase Plant Growth (Two-Way ANOVA) 
#-------------------------------------------------------------------------------

growth <- c(9.2,9.4,8.9,8.5,9.2,8.9,
            7.1,7.2,8.5,5.5,5.8,7.6)

light <- factor(rep(c("Light1","Light2"), each=6))
food <- factor(rep(c("A","B"), each=3, times=2))

data <- data.frame(growth, light, food)

model <- aov(growth ~ light * food, data=data)
summary(model)

interaction.plot(light, food, growth)


#Explain
#This analysis examines both individual and interaction effects of light and food on growth.


#-------------------------------------------------------------------------------
# 12.4- BASEBALL DATA (Chi-Square)
#-------------------------------------------------------------------------------

bb <- read.csv("baseball.csv")

bb$Decade <- bb$Year - (bb$Year %% 10)

wins <- bb %>%
  group_by(Decade) %>%
  summarise(wins = sum(W))

chisq.test(wins$wins)

#Box plot
ggplot(bb, aes(x = factor(Decade), y = W)) +
  geom_boxplot(fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of MLB Team Wins by Decade",
    x = "Decade",
    y = "Number of Wins"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#Explain
#This test assesses whether wins are distributed equally across decades.


#-------------------------------------------------------------------------------
# 12.5- CROP DATA (Two-Way ANOVA)
#-------------------------------------------------------------------------------

crop <- read.csv("crop_data.csv")

crop$fertilizer <- as.factor(crop$fertilizer)
crop$density <- as.factor(crop$density)
crop$block <- as.factor(crop$block)

model5 <- aov(yield ~ fertilizer * density + block, data=crop)
summary(model5)


#Boxplot


ggplot(crop, aes(x = interaction(fertilizer, density), y = yield, fill = fertilizer)) +
  geom_boxplot(color = "black") +
  labs(
    title = "Crop Yield by Fertilizer and Planting Density",
    x = "Fertilizer × Density",
    y = "Yield"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#Explain
#This analysis evaluates the effects of fertilizer, planting density, and their interaction on crop yield.

