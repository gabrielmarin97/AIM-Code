#==========================================================#
# Lecture 5: Uncertainty in the Basic Model
# Gabriel Marin, Anahuac University
# gabrielmarinmu.97@gmail.com
# Note: In this R-script we will model uncertainty in the
# basic model of a small open economy. 
#==========================================================#

#Clear environment
rm(list=ls())

# Required packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

# Parameters (Let us continue assuming beta*(1+r) = 1)
p <- 0.3 # probability of the high-output state of the economy
r <- 0.03 # 3% risk-free rate

t <- 2 # number of periods

# Result vectors
y <- c(NA, NA)
c <- c(NA, NA)
TB <- c(NA,NA)
# Output under uncertainty
y[1] <- 0.1 # output period 1
y2_l <- 0.05 # low-output realization
y2_h <- 0.2 # high-output realization
E_y2 <- p*y2_h + (1-p)*y2_l # expected value of period 2 output

# Consumption under uncertainty, we will use the trade balance for solving it
TB[1] <- (1/(2+r))*(y[1] - E_y2) # CA_1 = S_1 = y_1 - c_1. This is the size of the precautionary saving
c[1] <- y[1] - TB[1] # Rearranging

# Depending on which output materializes, consumption in period 2 will adjust to keep trade
# balance closed. 
set.seed(123)

y[2] <- if_else(runif(1, 0, 1) <= p, y2_h, y2_l)

c[2] <- TB[1]*(1+r) + y[2]
TB[2] <- y[2] - c[2]

# Bind all into a dataset
data <- cbind(index = seq(1,2),y,c,TB)
data <- data.frame(data)
# Plot
# Setup theme
theme_set(theme_bw() +
            theme(title = element_text(face = "bold", size = 16),
                  axis.text.x = element_text(size = 14, 
                                             angle = 90, vjust = 0.5, hjust=1), # vertical x text
                  axis.text.y = element_text(size = 14))) 


ggplot(data = data, aes(x = index)) +
  geom_line(aes(y=y, color ="Output")) +
  geom_line(aes(y=c, color ="Consumption")) +
  geom_line(aes(y=TB, color ="Trade Balance")) +
  labs(title = "Our Basic Model Under Uncertainty",
       x = "Period",
       y = "", caption = "Source: Author's elaboration.") +
  labs(color = "Variable", linetype = "Variable")  # Set the title for the legend
  



