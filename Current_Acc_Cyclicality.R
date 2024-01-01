#==========================================================#
# Lecture 1: Current Account Procyclicality?
# Gabriel Marin, Anahuac University
# gabrielmarinmu.97@gmail.com
# Note: In this R-script we will make use of World Bank
# data to identify the relationship between the current account and output.
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

# Setup working directory
setwd("/Users/gmm/Dropbox/Anahuac Economics Department/Applied International Macroeconomics/Github/aim/Code/Data")

# Let us search the WB API to obtain our relevant variables
curacc_inds<- wb_search("current account") # the indicator we need is BN.CAB.XOKA.CD
gdp_inds <- wb_search("GDP") #GDP code is NY.GDP.MKTP.CD
nx_inds <- wb_search("import|export") #GDP code is NY.GDP.MKTP.CD

# Obtain current account data from the API
ca_db <- wb_data(indicator = "BN.CAB.XOKA.CD", start_date = 1980, end_date = 2022)
ca_db <- ca_db %>% select(iso2c, iso3c, country, date, BN.CAB.XOKA.CD) # select only relev variables

# Obtain GDP data from the API
gdp_db <- wb_data(indicator = "NY.GDP.MKTP.CD", start_date = 1980, end_date = 2022)
gdp_db <- gdp_db %>% select(iso2c, iso3c, country, date, NY.GDP.MKTP.CD) # select only relev variables

# Obtain import and export data from the API
nx_db <- wb_data(indicator = c("NE.IMP.GNFS.CD", "NE.EXP.GNFS.CD"), start_date = 1980, end_date = 2022)
nx_db <- nx_db %>% select(iso2c, iso3c, country, date,
                          NE.IMP.GNFS.CD, NE.EXP.GNFS.CD) # select only relev variables

# Now merge the databases together
names(gdp_db)
names(ca_db)
names(nx_db)
db <- merge(ca_db, gdp_db, by = c("iso2c","iso3c", "country","date"))
db <- merge(db,nx_db, by = c("iso2c","iso3c", "country","date"))

# Rename variables
db <- rename(db, ca = "BN.CAB.XOKA.CD" , gdp_usd = "NY.GDP.MKTP.CD",
             imports = "NE.IMP.GNFS.CD", exports = "NE.EXP.GNFS.CD")

# Create net exports variable and exclude post-COVID era
db <- db %>% mutate(nx = exports-imports) %>% filter(date < 2020)

# For the correlations, we want to isolate the structural component of output and 
# our variables of interest. One econometric tool that we can use is the standard
# Hodrick-Prescott filter. It uses a smoothing parameter (lambda) of 1600 for 
# quarterly data. With annual data, the filter requires a lambda of 6.25, but most
# researchers use a lambda of 100. 
lambda = 6.25

# Using a HP filter in R when your data has gaps is a pain, we will use just omit
# the countries with not a full sample (1980 to 2019)

hp_db <- db %>% group_by(iso3c) %>% 
         mutate(trend_gdp = hpfilter(gdp_usd, freq = lambda)$trend,
                trend_ca = hpfilter(ca, freq = lambda)$trend,
                trend_nx = hpfilter(nx, freq = lambda)$trend) %>%  
         ungroup()

#=====================================================================
# Extra credit: How would our results change if we use a lambda of 100?
#=====================================================================

# Now, we will compute the correlation for each country
db1 <- hp_db # It's always recommended to create other versions of your database, in case you make a mistake
          # when performing calculations
db1 <- db1 %>% group_by(iso3c) %>% mutate(corr_ca = cor(trend_ca,trend_gdp, use = "na.or.complete"),
                                          corr_tb = cor(trend_nx,trend_gdp, use = "na.or.complete"))

# Let us use the IMF country income classification, its a matter of taste, but I prefer it
# over the World Bank's country income classification

country_id <- read_xlsx("country_classifications.xlsx", sheet = 1)
country_id <- country_id %>% select(isocode, advanced, emerging, low_income, idb_lac) %>%
              rename(iso3c = isocode)

# Now merge country ids with our correlation database
db2 <- merge(db1, country_id, by = c("iso3c"))
# Create economy type identifier for easier summarizing
db2 <- db2 %>% mutate(economy_type = ifelse(advanced == 1,"Advanced",
                                            ifelse(emerging==1,"Emerging","Low-Income")))

# Finally, let us summarize in a table our correlations by income group
results <- db2 %>% filter(date == 2019) %>% 
  group_by(economy_type) %>%
  summarise(average_corr_ca = round(mean(corr_ca, na.rm = TRUE),4), # Average correlation by group
            pval_corr_ca = round(pt(average_corr_ca/std.error(corr_ca),
                                 length(economy_type) - 1),4),  # p-value function from a t-statistic
            average_corr_tb = round(mean(corr_tb, na.rm = TRUE),4), # Average correlation by group
            pval_corr_tb = round(pt(average_corr_tb/std.error(corr_tb),
                                    length(economy_type) - 1),4),  # p-value function from a t-statistic
            )

# Select only the variables we need 
results1 <- results %>% select(economy_type, average_corr_ca, average_corr_tb)

# Make the table more elegant
# Stargazer is meant to be used more for regression analysis, hence why we needed to manually
# format some parts of the table like the column names.

stargazer(results1, 
          title = "Table 1. Average Correlation of Current Account and Trade Balance with Output by Economy Type",
          type = "text",
          summary = FALSE,
          covariate.labels = c("","Economy Type", "Current Account", "Trade Balance"),
          notes =c("Source: Author's elaboration using data from WB WDI.",
                    "Note: Correlations use a HP filter with smoothing parameter of 6.25." ))


# Finally, let us get to know more of current account dynamics in our country: Mexico

rolling_db <- db2 %>% filter(country == "Mexico") %>% select(date, trend_gdp, trend_ca)

window_size <- 8 # 8-year rolling window

# Rolling correlation
rolling_correlation <- rollapply(data = rolling_db[,c("trend_gdp","trend_ca")],
                                 width = window_size,
                                 FUN = function(z) cor(z[,1], z[,2], use = "pairwise.complete.obs"),
                                 by.column = FALSE,
                                 align = 'right')

# Dates
dates <- seq(from = 1987, to = 2019, by =1 )
plot_db <-  cbind(dates,rolling_correlation)
plot_db <- tbl_df(plot_db)

# GG plot
# Setup a theme for all your plots
theme_set(theme_bw() +
          theme(title = element_text(face = "bold", size = 16),
                axis.text.x = element_text(size = 14),
                axis.text.y = element_text(size = 14)))

ggplot(data = plot_db, aes(x = dates, y = rolling_correlation)) +
  geom_line(color = "steelblue") +
  labs(title = "8-Year Rolling Correlation of Current Account and Output - Mexico ",
       x = "",
       y = "Correlation", caption = "Source: Author's elaboration using data from WB WDI.") 










