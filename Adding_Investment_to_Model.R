#==========================================================#
# Lecture 2: Introducing Investment to the Basic Model
# Gabriel Marin, Anahuac University
# gabrielmarinmu.97@gmail.com
# Note: In this R-script we use R to simulate the behavior of 
# our theoretical model with investment 
#==========================================================#

#Clear environment
rm(list=ls())

# Required packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(mFilter)
library(wbstats)
library(readxl)
library(plotrix)
library(stargazer)
library(ggpubr)

# Setup working directory
setwd("/Users/gmm/Dropbox/Anahuac Economics Department/Applied International Macroeconomics/Github/aim/Code/Data")

# Parameter selection
alpha <- 0.2 
r <- 0.03

# Create our functions of interest
k <- function(A){
  ((alpha*A)/(1+r)) ^ (1/(1-alpha))
}

I <- function(k1,k2) {
  return(k2 - k1)
}

y <- function(k,A) {
  return((k ^ alpha))
}

S1 <- function(A2){
  return(
          (
            A1 * y(k1,A1) - 
          ( A2*y(k(A2),A2)  -(1+r)*(k(A2) - k(A_bar)))
          )/(2+r)  #end equation
          
          ) # end return
}

CA1 <- function(S1,I){
  return(S1 - I)
}

#--------------------------------------------------------
## Savings and Investment as a function of A2
#--------------------------------------------------------
# Using Example III
A1 = 0.08; A_bar = 0.03
# Obtain each given variable
k1 <- k(A_bar); y1 <- y(k1,A1) 

# Compute the relevant variables
A2 <- seq(0.03,0.09,by = 0.0025)

savings_ex3 <- S1(A2)
inv_ex3 <- I(k(A1),k(A2))
ca_ex3 <- CA1(savings_ex3,inv_ex3)

# Plot savings and investment as functions of A2
plot_db <- cbind(A2,savings_ex3,inv_ex3,ca_ex3)
plot_db <- fortify(data.frame(plot_db))

# GGplot (Savings and Investment as functions of A2)

# Setup a theme for all your plots
theme_set(theme_bw() +
            theme(title = element_text(face = "bold", size = 14),
                  axis.text.x = element_text(size = 12),
                  axis.text.y = element_text(size = 12),
                  plot.caption = element_text(hjust = 0)))

# Make the plot more aesthetic when we combine it
y_limits <- c(-0.004,0.01)

# Investment and savings as function of TFP of period 2 (A2)
s_i_plot <- ggplot(data = plot_db, aes(x = A2)) +
  geom_line(color = "steelblue", aes(y = savings_ex3)) +
  geom_line(color = "darkred", aes(y = inv_ex3)) +
  labs(title = "Figure 1. Investment and Savings",
       x = "A2",
       y = "") +
  geom_text(label = "Savings", color = "steelblue",
            x = 0.04, y = 0.0035, size = 4) +
  geom_text(label = "Investment", color = "darkred",
            x = 0.04, y = -0.0015, size = 4) +
  ylim(y_limits) + geom_hline(yintercept = 0, linetype = "dotted", color = "black")

s_i_plot

# Current Account as function of TFP of period 2 (A2)
ca_plot <- ggplot(data = plot_db, aes(x = A2)) +
  geom_line(color = "steelblue", aes(y = ca_ex3)) +
  labs(title = "Figure 2. Current Account",
       x = "A2",
       y = "") +
  ylim(y_limits) + geom_hline(yintercept = 0, linetype = "dotted", color = "black")

ca_plot

# Combine both plots
combined_plot <- ggarrange(s_i_plot, ca_plot, ncol = 2) 
combined_plot1 <- annotate_figure(combined_plot,
bottom = text_grob("Source: Author's elaboration. Note: Model assumes r = 0.03, A1 = 0.08, and A_bar = 0.03.", 
                   size = 12, hjust = 1))


# Show the final plot
combined_plot1




