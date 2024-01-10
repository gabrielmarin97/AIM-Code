#==========================================================#
# Lecture 4: The Balance of Payments in Practice
# Gabriel Marin, Anahuac University
# gabrielmarinmu.97@gmail.com
# Note: In this R-script we will download IMF data
# and analyze some BoP variables
#==========================================================#

#Clear environment
rm(list=ls())

# Required packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(readxl)
library(stargazer)
library(ggpubr)
library(stringr)
library(devtools)

# To install imfr API package
#devtools::install_github("christophergandrud/imfr")
library(imfr)
# Setup working directory
setwd("/Users/gmm/Dropbox/Anahuac Economics Department/Applied International Macroeconomics/Github/aim/Code/Data")

# Now, we want to be able to replicate the graphs I showed you
# in today's lecture

# First, let us obtain the database we need
imf_db_codes <- imf_databases() # we want BOP - Balance of Payments
# Which parameters does BOP has?
param_ids <- imf_parameter_defs("BOP")

# Now, which indicators have which code?
ind_ids <- imf_parameters("BOP")
ind_bop <- ind_ids$indicator

# We want to analyze the current account, the trade balance, the financial account,
# and, out of curiosity, how have capital flows behaved in LAC.

# Current account - BCA_BP6_USD
# CA (primary income) - BIP_BP6_USD
# CA (secondary income) - BIS_BP6_USD
# Trade balance - BGS_BP6_USD
# Financial account - BF_BP6_USD
# Direct Investment (Net) - BFD_BP6_USD
# Direct Investment (assets) - BFDA_BP6_USD
# Direct Investment (liabilites) - BFDL_BP6_USD

# For simplicity, let us group our codes
ind_codes <- c("BCA_BP6_USD", "BIP_BP6_USD", "BIS_BP6_USD",
               "BGS_BP6_USD", "BF_BP6_USD", "BFD_BP6_USD",
               "BFDA_BP6_USD", "BFDL_BP6_USD")

ind_clean <- ind_bop %>% 
              filter(input_code %in% ind_codes)

# Now, we only want to analyze Latin America, let's check how they
# label such countries in the database
cntry_ids <- ind_ids$ref_area

# 26 IDB member countries
cntry_codes <- c("AR", "BS", "BB", "BZ", "BO",
                 "BR", "CL", "CO", "CR", "DO",
                 "EC", "SV", "GT", "GY", "HT",
                 "HN", "JM", "MX", "NI", "PA",
                 "PY", "PE", "SR", "TT", "UY",
                 "VE")

cntry_clean <- cntry_ids %>% 
            filter(input_code %in% cntry_codes) 

# Which frequency suits our questions? Today, we will use annual data
freq_bop <- ind_ids$freq

freq_clean <- freq_bop %>% 
              filter(input_code == "Q")

# Now, download the dataset with our codes
imf_bop_db_q <- imf_dataset(database_id = "BOP",
                      start_year = "2010", 
                      end_year = "2023",
                      parameters = list(freq = freq_clean,
                                        indicator = ind_clean,
                                        ref_area = cntry_clean))

freq_clean <- freq_bop %>% 
  filter(input_code == "A")

imf_bop_db_a <- imf_dataset(database_id = "BOP",
                            start_year = "2010", 
                            end_year = "2023",
                            parameters = list(freq = freq_clean,
                                              indicator = ind_clean,
                                              ref_area = cntry_clean))
# Add labels to the database
imf_bop_w_labels <- imf_bop_db %>% 
  mutate(name = case_when(
    indicator == "BCA_BP6_USD" ~ "Current Account",
    indicator == "BIP_BP6_USD" ~ "Primary Income",
    indicator == "BIS_BP6_USD" ~ "Secondary Income",
    indicator == "BGS_BP6_USD" ~ "Trade Balance",
    indicator == "BF_BP6_USD" ~ "Financial Account",
    indicator == "BFD_BP6_USD" ~ "Direct Investment, Net",
    indicator == "BFDA_BP6_USD" ~ "FA, Assets",
    indicator == "BFDL_BP6_USD" ~ "FA, Liabilities",
    TRUE ~ "Other"
  )) 

imf_bop_w_labels_a <- imf_bop_db_a %>% 
  mutate(name = case_when(
    indicator == "BCA_BP6_USD" ~ "Current Account",
    indicator == "BIP_BP6_USD" ~ "Primary Income",
    indicator == "BIS_BP6_USD" ~ "Secondary Income",
    indicator == "BGS_BP6_USD" ~ "Trade Balance",
    indicator == "BF_BP6_USD" ~ "Financial Account",
    indicator == "BFD_BP6_USD" ~ "Direct Investment, Net",
    indicator == "BFDA_BP6_USD" ~ "FA, Assets",
    indicator == "BFDL_BP6_USD" ~ "FA, Liabilities",
    TRUE ~ "Other"
  )) 

# Transform to wide
imf_bop_db_wide <- imf_bop_w_labels %>%
                    pivot_wider(
                      id_cols = c(date,ref_area),
                      names_from = indicator,
                      values_from = value
                    )


# As a means to comparison, we will express each item in percentage of GDP,
# This requires us to also download GDP data. We will use the IFS database
# We need the GDP indicator
ifs_ind <- imf_parameters("IFS")
imf_ifs <- ifs_ind$indicator

# Not Seasonally Adjusted Nominal GDP - NGDP_SA_XDC
# Nominal exchange rate (LCU per USD) Period average - ENDA_XDC_USD_RATE 

ifs_codes <- c("NGDP_SA_XDC", "ENDA_XDC_USD_RATE")

ifs_clean <- imf_ifs %>% filter(input_code %in% ifs_codes)

# Quick note: you can also just use IFS to download BOP data, but it has mostly
# net variables, not the assets and liabilities of each sub item of each account.

imf_ifs_db <- imf_dataset(database_id = "IFS",
                          start_year = "2010", 
                          end_year = "2023",
                          parameters = list(freq = freq_clean,
                                            indicator = ifs_clean,
                                            ref_area = cntry_clean))

# Now compute GDP in million USD
imf_ifs_db_wide <- imf_ifs_db %>%
                  pivot_wider(
                    id_cols = c(date,ref_area),
                    names_from = indicator,
                    values_from = value
                  ) %>% 
                  mutate(gdp_usd = as.numeric(NGDP_SA_XDC) / 
                                  as.numeric(ENDA_XDC_USD_RATE))

imf_ifs_db_wide <- imf_ifs_db_wide %>% 
                   select(date, ref_area, gdp_usd)

# Merge with the BoP database
names(imf_ifs_db_wide)
names(imf_bop_db_wide)

# It is doing a one to many merge
imf_db_merged <- merge(imf_ifs_db_wide, imf_bop_db_wide, 
                       by = c("date","ref_area"))

# Since BoP data is in quarterly flows, we need to adjust to compare to GDP
# Compute a 4 quarter rolling sum

# Convert all columns to numeric (excluding 'date' and 'variable')
imf_db_merged <- imf_db_merged %>%
  mutate(across(
    .cols = -c(date, ref_area),
    .fns = ~as.numeric(.x)
  ))

# Assuming your dataset is named 'your_data' and 'date' is your date column
imf_db_merged <- imf_db_merged %>%
  arrange(ref_area,date)  # Make sure the data is sorted by date

# Define the number of periods for the rolling sum
n_periods <- 4

# Apply the rolling sum to all numeric columns except 'date' (assuming 'date' is not numeric)
imf_db_rs <- imf_db_merged %>%
  mutate(across(where(is.numeric), 
                ~rollapply(.x, width = n_periods, FUN = sum, 
                           fill = NA, align = "right", na.rm = TRUE), 
                .names = "roll_sum_{.col}"))

# Now create rolling sum as % of GDP
imf_db_perc_gdp <- imf_db_rs %>% 
                   select(date,ref_area,starts_with("roll_sum_")) %>% 
                   mutate(across(starts_with("roll_sum_"), ~ . / roll_sum_gdp_usd*100,
                               .names = "perc_gdp_{.col}"))

# Select only relevant variables
imf_db_perc_gdp <- imf_db_perc_gdp %>% 
                   select(date,ref_area,starts_with("perc_gdp_"))

#It's more convenient to transform the dataset to long again for GGplot
# Use the indicator and then remove the long part
imf_db_long <- imf_db_perc_gdp %>%
  pivot_longer(
    cols = starts_with("perc_gdp_roll_sum_"),
    names_to = "indicator",
    values_to = "value"
  ) %>% 
    mutate(indicator = str_replace(indicator, "perc_gdp_roll_sum_", ""))

# Use better names,
imf_db_long <- imf_db_long %>% 
  mutate(name = case_when(
    indicator == "BCA_BP6_USD" ~ "Current Account",
    indicator == "BIP_BP6_USD" ~ "Primary Income",
    indicator == "BIS_BP6_USD" ~ "Secondary Income",
    indicator == "BGS_BP6_USD" ~ "Trade Balance",
    indicator == "BF_BP6_USD" ~ "Financial Account",
    indicator == "BFD_BP6_USD" ~ "Direct Investment, Net",
    indicator == "BFDA_BP6_USD" ~ "FA, Assets",
    indicator == "BFDL_BP6_USD" ~ "FA, Liabilities",
    TRUE ~ "GDP"
  )) %>% mutate(year = substr(date, 1,4))

# Now, collapse and obtain some summary statistics
collapsed_db <- imf_db_long %>% 
                mutate(value = as.numeric(value)) %>% 
                filter(is.finite(value)) %>%  # Filter out non-finite values
                group_by(date, indicator, name, year) %>% 
                summarize(mean = mean(value, na.rm = TRUE),
                          median = median(value, na.rm = TRUE),
                          p25 = quantile(value, 0.25, na.rm = TRUE),
                          p75 = quantile(value, 0.75, na.rm = TRUE),
                          sum = sum(value, na.rm = TRUE))

collapsed_db <- collapsed_db %>% filter(year > 2010) %>% 
                filter(date != "2023-Q3")

# Now plot each component of the BoP
# Setup theme
theme_set(theme_bw() +
            theme(title = element_text(face = "bold", size = 16),
                  axis.text.x = element_text(size = 14, 
                                             angle = 90, vjust = 0.5, hjust=1), # vertical x text
                  axis.text.y = element_text(size = 14))) 

# Figure 1. Current Account for LAC
ca_plot <- collapsed_db %>%
  mutate(date = as.yearqtr(date, format = "%Y-Q%q")) %>% 
  arrange(date) %>%
  ggplot(aes(x = date, group = 1)) +
  geom_line(data = filter(collapsed_db, name == "Current Account"), 
            aes(y = mean, color = 'Mean'), linetype="solid") +
  geom_line(data = filter(collapsed_db, name == "Current Account"), 
            aes(y = median, color = 'Median'), linetype="solid") +
  geom_line(data = filter(collapsed_db, name == "Current Account"), 
            aes(y = p25, color = '25th Percentile'), linetype="dashed", size=1) +
  geom_line(data = filter(collapsed_db, name == "Current Account"), 
            aes(y = p75, color = '75th Percentile'), linetype="dashed", size=1) +
  scale_color_manual(values = c("Mean" = 'darkgreen', 
                                "Median" = 'purple', 
                                "25th Percentile" = 'orange', 
                                "75th Percentile" = 'darkblue')) +
  labs(color = "Indicator", linetype = "Indicator") + # Set the title for the legend
  labs(title = "Current Account Balance (% GDP) ",
       x = "",
       y = "", caption = "Source: Author's elaboration using data from IMF IFS.")+
  theme(legend.position = c(0.1, 0.85), # Move legend inside the plot
  legend.background = element_rect(fill = "white", colour = "black")) # Optional: Add background to legend


ca_plot

## Figure 2. Trade Balance (% GDP)
tb_plot <- collapsed_db %>%
  mutate(date = as.yearqtr(date, format = "%Y-Q%q")) %>% 
  arrange(date) %>%
  ggplot(aes(x = date, group = 1)) +
  geom_line(data = filter(collapsed_db, name == "Trade Balance"), 
            aes(y = mean, color = 'Mean'), linetype="solid") +
  geom_line(data = filter(collapsed_db, name == "Trade Balance"), 
            aes(y = median, color = 'Median'), linetype="solid") +
  geom_line(data = filter(collapsed_db, name == "Trade Balance"), 
            aes(y = p25, color = '25th Percentile'), linetype="dashed", size=1) +
  geom_line(data = filter(collapsed_db, name == "Trade Balance"), 
            aes(y = p75, color = '75th Percentile'), linetype="dashed", size=1) +
  scale_color_manual(values = c("Mean" = 'darkgreen', 
                                "Median" = 'purple', 
                                "25th Percentile" = 'orange', 
                                "75th Percentile" = 'darkblue')) +
  labs(color = "Indicator", linetype = "Indicator") + # Set the title for the legend
  labs(title = "Trade Balance (% GDP) ",
       x = "",
       y = "", caption = "Source: Author's elaboration using data from IMF IFS.")+
  theme(legend.position = c(0.15, 0.85), # Move legend inside the plot
        legend.background = element_rect(fill = "white", colour = "black")) # Optional: Add background to legend

tb_plot

## Figure 3. Financial Account Balance (% GDP)
fa_plot <- collapsed_db %>%
  mutate(date = as.yearqtr(date, format = "%Y-Q%q")) %>% 
  arrange(date) %>%
  ggplot(aes(x = date, group = 1)) +
  geom_line(data = filter(collapsed_db, name == "Financial Account"), 
            aes(y = -mean, color = 'Mean'), linetype="solid") +
  geom_line(data = filter(collapsed_db, name == "Financial Account"), 
            aes(y = -median, color = 'Median'), linetype="solid") +
  geom_line(data = filter(collapsed_db, name == "Financial Account"), 
            aes(y = -p25, color = '25th Percentile'), linetype="dashed", size=1) +
  geom_line(data = filter(collapsed_db, name == "Financial Account"), 
            aes(y = -p75, color = '75th Percentile'), linetype="dashed", size=1) +
  scale_color_manual(values = c("Mean" = 'darkgreen', 
                                "Median" = 'purple', 
                                "25th Percentile" = 'orange', 
                                "75th Percentile" = 'darkblue')) +
  labs(color = "Indicator", linetype = "Indicator") + # Set the title for the legend
  labs(title = "Financial Account Balance (% GDP) ",
       x = "",
       y = "", caption = "Source: Author's elaboration using data from IMF IFS.")+
  theme(legend.position = c(0.05, 0.2), # Move legend inside the plot
        legend.background = element_rect(fill = "white", colour = "black")) # Optional: Add background to legend

fa_plot

# Figure 4. Primary Income
pi_plot <- collapsed_db %>%
  mutate(date = as.yearqtr(date, format = "%Y-Q%q"),
         year = as.numeric(as.character(year))) %>% 
  filter(year > 2015) %>% 
  arrange(date) %>%
  ggplot(aes(x = date, group = 1)) +
  geom_line(data = filter(collapsed_db, name == "Primary Income"), 
            aes(y = mean, color = 'Mean'), linetype="solid") +
  geom_line(data = filter(collapsed_db, name == "Primary Income"), 
            aes(y = median, color = 'Median'), linetype="solid") +
  geom_line(data = filter(collapsed_db, name == "Primary Income"), 
            aes(y = p25, color = '25th Percentile'), linetype="dashed", size=1) +
  geom_line(data = filter(collapsed_db, name == "Primary Income"), 
            aes(y = p75, color = '75th Percentile'), linetype="dashed", size=1) +
  scale_color_manual(values = c("Mean" = 'darkgreen', 
                                "Median" = 'purple', 
                                "25th Percentile" = 'orange', 
                                "75th Percentile" = 'darkblue')) +
  labs(color = "Indicator", linetype = "Indicator") + # Set the title for the legend
  labs(title = "Primary Income (% GDP) ",
       x = "",
       y = "", caption = "Source: Author's elaboration using data from IMF IFS.")+
  theme(legend.position = c(0.1, 0.15), # Move legend inside the plot
        legend.background = element_rect(fill = "white", colour = "black")) # Optional: Add background to legend


pi_plot

# Figure 5. Secondary Income
si_plot <- collapsed_db %>%
  mutate(date = as.yearqtr(date, format = "%Y-Q%q"),
         year = as.numeric(as.character(year))) %>% 
  filter(year > 2015) %>% 
  arrange(date) %>%
  ggplot(aes(x = date, group = 1)) +
  geom_line(data = filter(collapsed_db, name == "Secondary Income"), 
            aes(y = mean, color = 'Mean'), linetype="solid") +
  geom_line(data = filter(collapsed_db, name == "Secondary Income"), 
            aes(y = median, color = 'Median'), linetype="solid") +
  geom_line(data = filter(collapsed_db, name == "Secondary Income"), 
            aes(y = p25, color = '25th Percentile'), linetype="dashed", size=1) +
  geom_line(data = filter(collapsed_db, name == "Secondary Income"), 
            aes(y = p75, color = '75th Percentile'), linetype="dashed", size=1) +
  scale_color_manual(values = c("Mean" = 'darkgreen', 
                                "Median" = 'purple', 
                                "25th Percentile" = 'orange', 
                                "75th Percentile" = 'darkblue')) +
  labs(color = "Indicator", linetype = "Indicator") + # Set the title for the legend
  labs(title = "Secondary Income (% GDP) ",
       x = "",
       y = "", caption = "Source: Author's elaboration using data from IMF IFS.")+
  theme(legend.position = c(0.1, 0.85), # Move legend inside the plot
        legend.background = element_rect(fill = "white", colour = "black")) # Optional: Add background to legend


si_plot

# These odd mean values must be from outliers, which is why it's best to use the
# median in this type of analysis. 


