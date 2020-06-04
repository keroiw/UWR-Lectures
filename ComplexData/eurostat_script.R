library(eurostat)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(nlme)
library(ggplot2)

trim_geo <- function(df) df %>% filter(geo != "France (metropolitan)") %>% mutate(geo=str_replace(geo, "Germany.*", "Germany"))

filter_eu <- function(df) df %>% filter(geo %in% eu_countries$name)

postprocess <- function (df) df %>% trim_geo() %>% filter_eu %>% mutate(geo=toupper(geo),
                                                                        geo=factor(geo))

get_stat <- function(id) get_eurostat(id, select_time = "Y", type="label") %>% postprocess()

reduce_columns <- function(df) { 
  df %>% 
    mutate(year = as.numeric(format(time,'%Y')),
           year_fct = as.ordered(format(time,'%Y'))) %>% 
    filter(year >= 2005 & year <= 2018) %>%
    select(c(geo, year_fct, values))
}

population <- get_eurostat("tps00001", type="label") %>% postprocess()
employment <- get_stat("t2020_10")
hicp <- get_stat("tec00118")
debt <- get_stat("teina225")
poverty <- get_stat("t2020_50")
gdp_per_cap <- get_stat("tec00114")

base_cols <- c("country", "year_fct")
population.clean <- population %>% reduce_columns()
colnames(population.clean) <- c(base_cols, "population")

employment.clean <- employment %>%
  filter(sex == "Total") %>% 
  reduce_columns()
colnames(employment.clean) <- c(base_cols, "emp_pct")

hicp.clean <- hicp %>% reduce_columns()
colnames(hicp.clean) <- c(base_cols, "hicp")

debt.clean <- debt %>% 
  filter(grepl("^Percentage", unit)) %>%
  reduce_columns()
colnames(debt.clean) <- c(base_cols, "debt_pct_gdp")

poverty.clean <- poverty %>% 
  filter(sex == "Total" & age == "Total", unit == "Percentage") %>%
  reduce_columns()
colnames(poverty.clean) <- c(base_cols, "risk_poverty_pct")

gdp_per_cap.clean <- gdp_per_cap %>% reduce_columns()
colnames(gdp_per_cap.clean) <- c(base_cols, "gdp_per_cap")

years <- seq(from=2005, to=2018, by=1)
measurment_years <- rep(years, each=length(eu_countries$name))
countries <- rep(unique(population.clean$country), each=length(years))
dataset <- data.frame(year_fct = as.ordered(years),
                      country = toupper(countries))

dfs <- list(dataset, population.clean, employment.clean, hicp.clean, debt.clean, poverty.clean, gdp_per_cap.clean)
dataset <- reduce(dfs, function(df_l, df_r) merge(df_l, df_r, all.x=T)) 

hdi <- read.csv('/home/wiorek/Desktop/UWR-Lectures/ComplexData/HDI.csv', skip=1)
hdi <- hdi %>% 
  dplyr::select("Country", matches("^X20")) %>%
  tidyr::pivot_longer(cols = matches("^X20")) %>%
  dplyr::mutate(name = str_remove(name, 'X'),
                Country = toupper(Country)) %>%
  dplyr::rename(country=Country) %>%
  tidyr::pivot_wider(names_from=c("name")) %>%
  dplyr::filter(country %in% toupper(eurostat::eu_countries$name)) %>%
  dplyr::arrange(country)

hdi.long <- hdi %>% 
  tidyr::pivot_longer(cols = -c("country"), names_to="year", values_to="hdi") %>%
  filter(year >= 2005) %>%
  mutate(year_fct = as.ordered(year))

leadership <- toupper(c("germany", "france", "spain", "netherlands", "austria", "belgium", "luxembourg", "italy"))
eurozone <- toupper(c("finland", "greece", "portugal", "slovenia", "cyprus", "malta", "slovakia", "estonia", "latvia", "lithuania", "ireland"))
outsiders <- toupper(c("bulgaria", "croatia", "czechia", "hungary", "poland", "romania", "sweden", "united kingdom", "denmark"))

dataset.full <- dataset %>% mutate(country=toupper(country))
dataset.full <- merge(hdi.long, dataset.full) %>% 
  select("hdi", everything()) %>%
  mutate(state = case_when(country %in% leadership ~ 1,
                           country %in% eurozone ~ 2,
                           country %in% outsiders ~ 3),
         state = as.factor(state),
         year = as.numeric(year),
         hdi = as.numeric(levels(hdi))[hdi]) %>%
  select(-c("year_fct"))

fit.rand_slp <- lme(hdi ~ year*state + risk_poverty_pct + emp_pct, 
                    random=~1+year|country,
                    data=dataset.full,
                    na.action=na.omit)
summary(fit.rand_slp)

dataset.full %>% 
  mutate(country = country,
         year=year) %>%
  ggplot(aes(x=year, y=hdi)) + 
  geom_line(aes(color=country)) + 
  facet_grid(. ~ state) + 
  theme() 

dataset.full %>%
  mutate(country = country,
         year=year) %>%
  ggplot(aes(x=year, y=hdi)) +
  geom_line(aes(color=country)) 
