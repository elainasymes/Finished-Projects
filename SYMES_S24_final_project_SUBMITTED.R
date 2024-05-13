# Loading Packages
library(tidyverse)
library(readxl)
library(gt)
library(ggrepel)
library(janitor)
library(scales)
library(patchwork)

# Loading data
MMR_by_country <- read_excel("~/Desktop/R Final Project/MMR-By-Country.xlsx")

Mistreatment_by_race <- read_excel("~/Desktop/R Final Project/Intersection between mistreatment and race.xlsx")

COD <- read_excel("~/Desktop/R Final Project/Maternal Causes of Death.xlsx")

Social_determinants <- read_excel("~/Desktop/R Final Project/Medical and Health Characteristics.xlsx")

SES <- read_excel("~/Desktop/R Final Project/SES.xlsx")

Death_trends <- read_excel("~/Desktop/R Final Project/Maternal Death Trends.xlsx")

Mortality_by_race <- read.csv("~/Desktop/R Final Project/VSRR_Provisional_Maternal_Death_Counts_and_Rates.csv")

GDP <- read.csv("gdp-per-capita-worldbank.csv")

# Comparing Maternal Mortality in the US with Other Countries
  
  # Cleaning and tidying data
MMR_by_country_clean <- MMR_by_country[-c(1,2,3),] |> 
  rename(UNICEF_Region = `Estimates of country-level maternal mortality ratio (MMR; maternal deaths per 100,000 live births) 2000-2020`) |> 
  rename(country_code = '...2') |> 
  rename(country = '...3') |> 
  rename('2000' = '...4') |> 
  rename('2005' = '...5') |> 
  rename('2010' = '...6') |> 
  rename('2015' = '...7') |> 
  rename('2020' = '...8') 
 
MMR_by_country_tidy <- MMR_by_country_clean |> 
  pivot_longer(
    cols = starts_with("20"),
    names_to = "year",
    values_to = "deaths") |> 
  mutate(deaths = as.numeric(deaths))

  # Filtering high income countries
HI_countries <- MMR_by_country_tidy |> 
  filter(country == 'Australia'| country == "United States"|country == "Canada"|
           country == "Norway"| country == "United Kingdom"| country == "Germany"|
           country == "Netherlands"|country == "New Zealand")
  # Plot
ggplot(HI_countries, aes(x = deaths, y = country, fill = factor(year))) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#7A306C", "#225560", "#A9E4EF", "#EDF060","#F0803C"))+
  labs(title = "Maternal Mortality in High Income Countries", y = "Country", x = "Maternal Deaths per 100,000 Live Births",fill = "Year")+
  theme_bw()

# Countries with least MMR

  # Finding average MMR of all countries across timepoints given in dataset
average_deaths <- MMR_by_country_tidy |> 
  group_by(country) |> 
  summarise(avg_deaths = mean(deaths))

  # Slicing for 10 countries with least MMR
top_10 <- average_deaths |> 
  slice_min(avg_deaths, n = 10)

  # Creating a gt table
top_10_table <- gt(top_10)
top_10_table |> 
  cols_label(country = "Country") |> 
  cols_label(avg_deaths = "Average Deaths per 100,000 Live Births") |> 
  tab_header(
    title = "Countries with Lowest Maternal Mortality Rates")

# GDP vs MMR Analysis 

  # Finding GDP of high income countries
GDP_of_interest <- GDP |> 
  rename(gdp_per_capita = GDP.per.capita..PPP..constant.2017.international...) |> 
  rename(country = Entity) |> 
  select(country, Year, gdp_per_capita) |> 
  filter(Year == 2000|Year== 2005| Year== 2010| Year==2015|Year==2020) |> 
  filter(country == 'United States'| country== 'United Kingdom'| 
           country== 'Norway'|country== 'New Zealand'|country== 'Netherlands'| 
           country== 'Germany'|country == 'Canada'| country == 'Australia')

  # Average GDP of HI countries from timeperiods of interest
avg_GDP <- GDP_of_interest |> 
  group_by(country) |> 
  summarise(avg_GDP = mean(gdp_per_capita))

  # Average MMR of HI countries
avg_HI <- HI_countries |> 
  group_by(country) |> 
  summarise(avg_deaths = mean(deaths))

  # Joing GDP and MMR datasets
GDP_MMR <- inner_join(avg_HI,avg_GDP, by = "country")

  #Plotting GDP vs MMR and labeling countries
ggplot(GDP_MMR, aes(x = avg_deaths, y = avg_GDP, label = country))+
  geom_point()+
  geom_label_repel(aes(label= country)) +
  theme_bw()+
  labs(
    title = "Maternal Mortality in High Income Contries in Relation to GDP per Capita",
    y = "Average GDP per Capita (USD)",
    x = "Average Maternal Deaths per 100,000 Live Births")


# Maternal Mortality in the US by Race

  #Cleaning data
clean_Mortality_by_race <- Mortality_by_race|> 
  drop_na() |> 
  filter(Group == "Race and Hispanic origin") |> 
  rename(Race = Subgroup) |> 
  select(Race, Year.of.Death,Month.of.Death,Maternal.Deaths,Live.Births,Maternal.Mortality.Rate) |> 
  filter(Race != "American Indian or Alaska Native, Non-Hispanic")

  #Yearly MMR average by race  
average_mmr <- clean_Mortality_by_race |> 
  group_by(Race, Year.of.Death) |> 
  summarise(avg_mmr = mean(Maternal.Mortality.Rate)) |>
  mutate(Race = ifelse(Race == "White, Non-Hispanic","White",
                       ifelse(Race == "Black, Non-Hispanic", "Black", 
                              ifelse(Race == "Asian, Non-Hispanic", "Asian", Race))))

  # Plotting average MMR by race to show trend over time
average_mmr |> 
  ggplot(aes(x = Year.of.Death, y = avg_mmr,color = Race))+
  geom_point(size = 3) +
  geom_line(linewidth = 1)+
  scale_color_manual(values = c("White" = "#7A306C",
                                "Black" = "#225560",
                                "Asian" = "#A9E4EF",
                                "Hispanic" = "#EDF060")) +
  labs(title = "Maternal Mortality Ratio from 2000 - 2020 by Race",
       x = "Year",
       y = "Average Maternal Moratlity Ratio")+
  theme_bw()

# Social Determinants of Health

  # Onset of pre-natal treatment 

    # Cleaning and tidying data
Social_determinants_clean <- Social_determinants |> 
  select(Characteristic,Total,White,Black, Hispanic)

Social_determinants_tidy <- Social_determinants_clean |> 
  pivot_longer(
    cols = 3:5,
    names_to = "race",
    values_to = "Percent"
  ) |> 
  mutate(Total = as.numeric(Total),
         Percent = as.numeric(Percent))

    #Filtering characteristics
prenatal <- Social_determinants_tidy |> 
  filter(Characteristic == "First Trimester"| Characteristic == "Late or none")

    # Plotting onset of pre natal treatment by race
prenatal_plot <- ggplot(prenatal, aes(x =  Characteristic, y = Percent, fill = race))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(limits = c(0, 100))+
  labs(
    title = "Onset of Pre-Natal Care (2020)",
    x = "Timepoint",
    y = "Percent")+
  scale_fill_manual(name = "Race", values = c("White" = "#7A306C", "Black" = "#225560", "Hispanic" = "#EDF060"))+
  theme_bw()
prenatal_plot

  # Delivery method of payment 
    
    # Filtering by payment types
payment <- Social_determinants_tidy |> 
  filter(Characteristic == "Medicaid" |Characteristic == "Private"|Characteristic == "Self Pay"|Characteristic == "Other"|Characteristic == "WIC")

    # Grouping by factors 
payment$Characteristic <- factor(payment$Characteristic, 
                                 levels = c("WIC", "Medicaid", "Private", "Self Pay", "Other"))

    # Plotting payment type by race 
pay_plot <- ggplot(payment, aes(x =  Characteristic, y = Percent, fill = race))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(limits = c(0, 100))+
  labs(
    title = "Method of Payment for Delivery (2020)", 
    x = "Method")+
  scale_fill_manual(name = "Race",values = c("White" = "#7A306C", "Black" = "#225560", "Hispanic" = "#EDF060"))+
  theme_bw()
pay_plot

  # Educational attainment

    # Cleaning and tidying data
SES_clean <- SES[-c(1),] |> 
  rename(variable = "2013-2017",
         total = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7792750/",
         White = "...3",
         Black = "...4",
         Hispanic = "...5"
  )

SES_tidy <- SES_clean |> 
  pivot_longer(
    cols = 3:5,
    names_to = "race",
    values_to = "MMR") |> 
  mutate(MMR = as.numeric(MMR))

    # Filtering and grouping by factors for education statuses
edu <- SES_tidy |> 
  filter(variable %in% c("Less than High School", "High School", "Some College", "College or more"))

edu$variable <- factor(edu$variable, levels = c("Less than High School", "High School", "Some College", "College or more"))

    # Plotting educational attainment and MMR by race
edu_plot <- ggplot(edu, aes(x = variable, y = MMR, fill = race))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(
    title = "Maternal Mortality in Relation to Educational Attainment (2013-2017)",
    x = "Level of Education",
    y = "Maternal Deaths per 100,000 Live Births"
  )+
  scale_fill_manual(name = "Race", values = c("White" = "#7A306C", "Black" = "#225560", "Hispanic" = "#EDF060"))+
  theme_bw()
edu_plot

  # Class

    # Filtering and grouping by factors for class statuses
class <- SES_tidy |> 
  filter(variable %in% c("Low SES","Middle SES","High SES"))

class$variable <- factor(class$variable, levels = c("Low SES","Middle SES","High SES"))

    # Plotting class status and MMR by race
ses_plot <- ggplot(class, aes(x = variable, y = MMR, fill = race))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(
    title = "Maternal Mortality in Relation to Class (2013-2017)",
    x = "Class",
    y = "Maternal Deaths per 100,000 Live Births"
  )+
  scale_fill_manual(name = "Race", values = c("White" = "#7A306C", "Black" = "#225560", "Hispanic" = "#EDF060"))+
  theme_bw()
ses_plot

# Causes of Death
  # Cleaning and tidying data
COD <- COD |> 
  select(Cause, White, Black, Hispanic)

COD$Hispanic <- as.numeric(COD$Hispanic)

COD_tidy <- COD |> 
  na.omit() |> 
  pivot_longer(
    cols = 2:4,
    names_to = "Race",
    values_to = "MMR"
  ) |> 
  mutate(MMR = as.numeric(MMR))

  # Plotting COD by race 
ggplot(COD_tidy, aes(x = Cause, y = MMR, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(name = "Race", 
                    values = c("White" = "#7A306C", 
                               "Black" = "#225560", 
                               "Hispanic" = "#EDF060")) +
  scale_y_continuous(breaks = seq(0, max(COD_tidy$MMR), by = 5)) +
  labs(
    title = "Maternal Causes of Death by Race (2013-2017)",
    x = "Cause of Death",
    y = "Maternal Deaths per 100,000 Live Births"
  )+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 15, hjust = 1))