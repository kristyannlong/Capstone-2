# For this project, rather than writing a report, you will analyze a new data set (not the same as Capstone I) of your choice from Kaggle, then present your analysis via a pre-recorded Loom video. You will need a webcam for this assignment.
# 
# Similar to the previous capstone, you will be analyzing data and presenting, just in a different format, and with a new data set.
# 
# With pre-recorded video:
#   Using Loom or similar, you will be showing your screen and your analysis - your graphs, charts, results, and explanations of the data - while your video is somewhere on the screen. Part of this job is presenting information to other people.
# 
# Either way:
#   Present your findings, hypotheses, analysis, graphs, charts, methodologies, and anything else relevant in video format. 
# 
# Below are links for Loom, as well as several sites where various data sets can be found.
# 
# When submitted please schedule a time to present here: https://calendly.com/d/48z-p4c-5kb/data-capstone-presentation



library(tidyverse)
library(tidyselect)
library(tidyr)
library(dplyr)
library(tidyselect)
library(dtplyr)
library(dbplyr)
library(janitor)
library(shiny)
library(readr)
library(tools)
# Install VIM to visualize missing data
# install.packages("VIM")
library(VIM)
library(ggplot2)

# first, i did notice when i was looking at the data that the two df were named incorrectly.  the rates should be the ages df.  i didn't mess with the labeling as i understood the issue.  

# Null Hypothesis:  The rates of suicide do not change based on age or country.
# Alternative Hypothesis:  The rates of suicide are lower with the younger age groups in lower income countries.

suicide_ages_1990_2022 <- read.csv("suicide data/age_std_suicide_rates_1990-2022.csv")
suicide_rates_1990_2022 <- read.csv("suicide data/suicide_rates_1990-2022.csv")

view(suicide_ages_1990_2022)
view(suicide_rates_1990_2022)

glimpse(suicide_ages_1990_2022)   # Rows: 5,928
glimpse(suicide_rates_1990_2022)  # Rows: 118,560.  this dataset has duplicated lines

summary(suicide_ages_1990_2022)
summary(suicide_rates_1990_2022)

names(suicide_ages_1990_2022)
names(suicide_rates_1990_2022)

# checking for the percent of complete cases
comp <- complete.cases(suicide_ages_1990_2022)
mean(comp) # [1] 0.7925101
comp1 <- complete.cases(suicide_rates_1990_2022)
mean(comp1) # [1] 0.7506748

# using VIM to plot the missing data
VIM::aggr(suicide_rates_1990_2022)
VIM::aggr(suicide_ages_1990_2022)

# excluding the NAs
suicide_ages_1990_2022NoNA <- na.exclude(suicide_ages_1990_2022)
suicide_rates_1990_2022NoNA <- na.exclude(suicide_rates_1990_2022)

# checking for duplicates
duplicated(suicide_ages_1990_2022NoNA)
sum(duplicated(suicide_ages_1990_2022NoNA)) # no dups

duplicated(suicide_rates_1990_2022NoNA)
sum(duplicated(suicide_rates_1990_2022NoNA)) # 15778 dups

# removing duplicates from suicide_rates_1990_2022NoNA
suicide_rates_1990_2022_noNA_noDup <- unique(suicide_rates_1990_2022NoNA)
summary(suicide_rates_1990_2022_noNA_noDup)

# comparing the 2 datasets to see if they are compatible.
compare_df_cols(suicide_ages_1990_2022NoNA, suicide_rates_1990_2022_noNA_noDup)

# two columns have the same data but different names.  
# changing the name of one to match the other before joining.
n_suicide_rates_1990_2022 <- suicide_rates_1990_2022_noNA_noDup |>
  rename(GNI = GrossNationalIncome)
names(n_suicide_rates_1990_2022)

# recompare to make sure it matches now
compare_df_cols(suicide_ages_1990_2022, n_suicide_rates_1990_2022)

# join the two datasets into one
suicide_all_1990_2022 <- left_join(
  n_suicide_rates_1990_2022, 
  suicide_ages_1990_2022
)

summary(suicide_all_1990_2022)


# time to dive in!  
# grouping

# by year
suicides_by_year <- suicide_all_1990_2022 |>
  group_by(Year) 
  
suicides_by_year  # 32 groupings

# by country
suicides_by_country <- suicide_all_1990_2022 |>
  group_by(CountryName)

suicides_by_country  # 101 groupings

# by age group
suicides_by_age_group <- suicide_all_1990_2022 |>
  group_by(AgeGroup)

suicides_by_age_group  # 6 groupings

# checking to see if more than 2 genders are listed
suicides_by_gender <- suicide_all_1990_2022 |>
  group_by(Sex)

suicides_by_gender # 2 groupings

# total suicides per country broken down into age groups
age_group_country_sum <- suicide_all_1990_2022 |>
  group_by(CountryCode, CountryName, AgeGroup) |>
  summarize(total_suicides = sum(SuicideCount)
  )
age_group_country_sum


#  I really like this one, but it's too big.  splitting into 2 to see if that helps.
ggplot(age_group_country_sum, aes(
  x = CountryCode, y = total_suicides, size = total_suicides)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), alpha = 0.7) +
  scale_size_continuous(name = "Total Suicides", 
                        labels = function(x) format(x, scientific = FALSE)) +
  facet_grid(rows = vars(AgeGroup)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# two charts based on A to M and N to Z for the countries.
AtoM_age_group_country_sum <- subset(age_group_country_sum, grepl("^[A-M]", CountryCode))

NtoZ_age_group_country_sum <- subset(age_group_country_sum, grepl("^[N-Z]", CountryCode))

# Countries A to M
ggplot(AtoM_age_group_country_sum, aes(
  x = CountryCode, y = total_suicides, size = total_suicides)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), alpha = 0.7) +
  scale_size_continuous(name = "Total Suicides", 
                        labels = function(x) format(x, scientific = FALSE)) +
  facet_grid(rows = vars(AgeGroup)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# Countries N to Z
ggplot(NtoZ_age_group_country_sum, aes(
  x = CountryCode, y = total_suicides, size = total_suicides)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), alpha = 0.7) +
  scale_size_continuous(name = "Total Suicides", 
                        labels = function(x) format(x, scientific = FALSE)) +
  facet_grid(rows = vars(AgeGroup)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# The chart is HUGE.  splitting the countries alpha into 3 charts.  2 is still too huge
AtoG_age_group_country_sum <- subset(age_group_country_sum, grepl("^[A-G]", CountryCode))

HtoM_age_group_country_sum <- subset(age_group_country_sum, grepl("^[H-M]", CountryCode))

NtoZ_age_group_country_sum <- subset(age_group_country_sum, grepl("^[N-Z]", CountryCode))

# Countries A to G
ggplot(AtoG_age_group_country_sum, aes(
  x = CountryCode, y = total_suicides, size = total_suicides)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), alpha = 0.7) +
  scale_size_continuous(name = "Total Suicides", 
                        labels = function(x) format(x, scientific = FALSE)) +
  facet_grid(rows = vars(AgeGroup)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme(strip.text = element_text(size = 5, hjust = 0.5))

# Countries H to M
ggplot(HtoM_age_group_country_sum, aes(
  x = CountryCode, y = total_suicides, size = total_suicides)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), alpha = 0.7) +
  scale_size_continuous(name = "Total Suicides", 
                        labels = function(x) format(x, scientific = FALSE)) +
  facet_grid(rows = vars(AgeGroup)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme(strip.text = element_text(size = 5, hjust = 0.5))

# Countries N to Z
ggplot(NtoZ_age_group_country_sum, aes(
  x = CountryCode, y = total_suicides, size = total_suicides)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), alpha = 0.7) +
  scale_size_continuous(name = "Total Suicides", 
                        labels = function(x) format(x, scientific = FALSE)) +
  facet_grid(rows = vars(AgeGroup)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme(strip.text = element_text(size = 5, hjust = 0.5))

# total suicides broken down by country without regard to age, including GNI
mean_suicides_country <- suicide_all_1990_2022 |>
  group_by(CountryCode, CountryName) |>
  summarize(mean_suicides = mean(SuicideCount),
            GNI = first(GNIPerCapita)
            )

mean_suicides_country

# breaking the countries into groups based on GNI as using the country names gets too big
# charting for total suicides by country including GNI
# defining the GNI groups
categorize_gni <- function(gni_value) {
  if (gni_value <= 5000) {
    return("0-5000")
  } else if (gni_value <= 10000) {
    return("5001-10000")
  } else if (gni_value <= 15000) {
    return("10001-15000")
  } else if (gni_value <= 30000) {
    return("15001-30000")
  }  else {
    return(">30000")
  }
}

mean_suicides_country <- mean_suicides_country %>%
  mutate(GNI_Group = sapply(GNI, categorize_gni))

view(mean_suicides_country)

# arrange the data frame based on the mean total suicides within each GNI group
mean_suicides_country <- mean_suicides_country %>%
  mutate(GNI_Group = reorder(GNI_Group, mean_suicides))

# specify the desired order of levels for GNI_Group
desired_order <- c("0-5000", "5001-10000", "10001-15000", "15001-30000", ">30000")

# change GNI_Group to factor with the levels for GNI in order
mean_suicides_country$GNI_Group <- factor(mean_suicides_country$GNI_Group, levels = desired_order)

# scatter plotting for total suicides by GNI groups
ggplot(mean_suicides_country, aes(x = GNI_Group, y = mean_suicides, size = mean_suicides)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), alpha = 0.7) +
  scale_size_continuous(name = "Mean Suicides") +
  labs(x = "GNI Group", y = "Mean Suicides", title = "Total Suicides by GNI Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# bar plotting
ggplot(mean_suicides_country, aes(x = GNI_Group, y = mean_suicides)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "GNI Group", y = "Mean Suicides", title = "Total Suicides by GNI Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# box plot
ggplot(mean_suicides_country, aes(x = GNI_Group, y = mean_suicides)) +
  geom_boxplot(fill = "skyblue") +
  labs(x = "GNI Group", y = "Mean Suicides", title = "Total Suicides by GNI Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# total suicides broken down into age group 
age_group_suicides_by_year <- suicide_all_1990_2022 |>
  group_by(Year, AgeGroup) |>
  summarize(total_suicides = sum(SuicideCount))

age_group_suicides_by_year
# this data shows that the majority of suicides are from ages 35-54

# playing with charts other than a scatter. 

# Histogram.  It's ok, just very congested and not really easy to read
ggplot(age_group_suicides_by_year, aes(x = Year, y = total_suicides, fill = AgeGroup)) +
  geom_histogram(stat = "identity", position = "dodge", alpha = 0.7) +
  labs(title = "Histogram of Suicides by Age Group and Year", x = "Year", y = "Suicides") +
  theme_minimal()

# I do really like the boxplot though.  very easy to read.  
ggplot(age_group_suicides_by_year, aes(x = Year, y = total_suicides, fill = AgeGroup)) +
  geom_boxplot() +
  labs(title = "Boxplot of Suicides by Age Group and Year", x = "Year", y = "Suicides") +
  theme_minimal()

# of course, going back to a scatter, i love it.
ggplot(age_group_suicides_by_year, aes(x = Year, y = total_suicides, color = AgeGroup)) +
  geom_point()

# revisited this after doing the gni groups.  made more sense then.
# age group suicide mean per country and what is the country's gni
age_group_gni <- suicide_all_1990_2022 %>%
  mutate(GNI_Group = sapply(GNIPerCapita, categorize_gni)) %>%
  group_by(AgeGroup, GNI_Group) %>%
  summarize(mean_suicides = mean(SuicideCount))

age_group_gni

desired_order <- c("0-5000", "5001-10000", "10001-15000", "15001-30000", ">30000")

age_group_gni$GNI_Group <- factor(age_group_gni$GNI_Group, levels = desired_order)

# bar plot for mean suicides by AgeGroup and GNI_Group
ggplot(age_group_gni, aes(x = AgeGroup, y = mean_suicides, fill = GNI_Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Age Group", y = "Mean Suicides", title = "Mean Suicides by Age Group and GNI Group") +
  scale_fill_brewer(palette = "Set3") +  # Change the color palette if needed
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# This was too congested.  Switched focus.

# # Plot by age group, country, gni, and mean suicide rates
# ggplot(age_group_country_gni, aes(
#   x = CountryCode, y = GNI, color = AgeGroup, size = mean_suicides)
#   ) +
#   geom_point(position = position_jitter(width = 0.2, height = 0), alpha = 0.7) +
#   scale_size_continuous(name = "Mean Suicides") +
#   labs(x = "Country", y = "GNI", color = "Age Group") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# scatterplot is very cluttered.  the jitter helped, but not enough.  
# i think i need to break it down further?



total_suicides_country <- suicide_all_1990_2022 |>
  group_by(CountryCode, CountryName) |>
  summarize(total_suicides = sum(SuicideCount),
            GNI = first(GNIPerCapita)
  )

total_suicides_country
