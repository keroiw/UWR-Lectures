library(eurostat)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(nlme)
library(ggplot2)
library(lubridate)
library(mice)
library(bigstep)
library(doBy)

trim_geo <- function(df) df %>%
  filter(geo != "France (metropolitan)",
         geo != "Germany including former GDR") %>% 
  mutate(geo=str_replace(geo, "Germany.*", "Germany"))

filter_eu <- function(df) df %>% filter(geo %in% eu_countries$name)

postprocess <- function (df) df %>% trim_geo() %>% filter_eu() %>% mutate(geo=toupper(geo),
                                                                          geo=factor(geo))

get_stat <- function(id) get_eurostat(id, select_time = "Y", type="label") %>% postprocess()

reduce_columns <- function(df) { 
  df %>% 
    mutate(year = as.numeric(format(time,'%Y')),
           year_fct = as.ordered(format(time,'%Y'))) %>% 
    filter(year >= 2005 & year <= 2018) %>%
    select(c(geo, year_fct, values))
}

population <- get_eurostat("demo_pjan", type="label") %>% postprocess()
employment <- get_stat("t2020_10")
debt <- get_stat("teina225")
poverty <- get_stat("t2020_50")
ind_growth_rates <- get_eurostat("ei_isir_m", type="label", select_time = "M") %>% postprocess() 
immigration <- get_stat("migr_imm8")
emmigration <- get_stat("migr_emi2")

base_cols <- c("country", "year_fct")
  
### Population

population.clean <- population %>% 
  filter(unit=="Number", age=="Total", sex=="Total") %>%
  reduce_columns()
colnames(population.clean) <- c(base_cols, "population")

### Employment

employment.clean <- employment %>%
  filter(sex == "Total") %>% 
  reduce_columns()
colnames(employment.clean) <- c(base_cols, "emp_pct")

### Debt

debt.clean <- debt %>% 
  filter(grepl("^Percentage", unit)) %>%
  reduce_columns()
colnames(debt.clean) <- c(base_cols, "debt_pct_gdp")

### Poverty

poverty.clean <- poverty %>% 
  filter(sex == "Total" & age == "Total", unit == "Percentage") %>%
  reduce_columns()
colnames(poverty.clean) <- c(base_cols, "risk_poverty_pct")

### HICP

hicp <- read.csv('/home/wiorek/Desktop/UWR-Lectures/ComplexData/hicp_full_europe.csv', skip=2)
hicp.clean <- hicp %>% 
  filter(grepl("20", X)) %>% 
  separate(col=X, into=c("year", "month"), sep=4) %>% 
  filter(year>=2005 & year <= 2018) %>% 
    pivot_longer(-c("year", "month"), names_to="country", values_to="hicp") %>%
  mutate(hicp = as.numeric(levels(hicp))[hicp],
         country = recode(country, `Czech.Republic`="czechia", `United.Kingdom`="united kingdom"),
         country = factor(toupper(country)),
         year_fct = ordered(as.numeric(year))) %>%
  select(-year) %>%
  group_by(year_fct, country) %>%
  summarise(hicp=mean(hicp), .groups="drop")

### Industry growth rates

ind_growth_rates.clean <- ind_growth_rates %>% 
  filter(grepl("Industry", nace_r2)) %>%
  filter(grepl("Growth rate on previous", unit)) %>%
  mutate(month = as.numeric(format(time,'%m')), 
         year = as.numeric(format(time,'%Y'))) %>%
  select(geo, values, month, year) %>%
  group_by(geo, year) %>%
  summarise(an_ind_rate=sum(values), .groups="drop") %>%
  filter(year >= 2005 & year <= 2018) %>%
  mutate(year_fct = as.ordered(year)) %>%
  rename(country=geo) %>%
  select(-year)

### Immigration 

immigration.clean <- immigration %>% 
  filter(unit=="Number", sex=="Total", age=="Total", agedef=="Age in completed years") %>%
  reduce_columns()
colnames(immigration.clean) <- c(base_cols, "immigration")

### Emmigration

emmigration.clean <- emmigration %>% 
  filter(unit=="Number", sex=="Total", age=="Total", agedef=="Age in completed years") %>%
  reduce_columns()
colnames(emmigration.clean) <- c(base_cols, "emmigration") 


### Gdp-Per-Cap

gdp_per_cap <- read.csv('/home/wiorek/Desktop/UWR-Lectures/ComplexData/gdp_per_cap.csv', skip=4)
gdp_per_cap.clean <- gdp_per_cap %>%
  mutate(country = recode(Country.Name, `Czech Republic`="Czechia", `Slovak Republic`="Slovakia")) %>%
  filter(country %in% eu_countries$name) %>%
  select(country, everything(), -c("Country.Code", "Indicator.Name", "Indicator.Code", "Country.Name")) %>%
  pivot_longer(-c("country"), names_to="year", values_to="gdp_per_cap") %>%
  mutate(year = as.numeric(str_remove(year, 'X'))) %>%
  filter(year >= 2005 & year <= 2018) %>%
  mutate(country = as.factor(toupper(country)), 
         year_fct = as.ordered(year)) %>%
  select(-c("year"))

## Merge datasets

years <- seq(from=2005, to=2018, by=1)
measurment_years <- rep(years, each=length(eu_countries$name))
countries <- rep(unique(population.clean$country), each=length(years))
dataset <- data.frame(year_fct = as.ordered(years),
                      country = toupper(countries))

dfs <- list(dataset, 
            population.clean, 
            employment.clean, 
            debt.clean, 
            hicp.clean, 
            poverty.clean, 
            gdp_per_cap.clean, 
            ind_growth_rates.clean, 
            immigration.clean,
            emmigration.clean)

dataset <- reduce(dfs, function(df_l, df_r) merge(df_l, df_r, all.x=T)) 

### HDI

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

#### Merging hdi with full

leadership <- toupper(c("germany", "france", "spain", "netherlands", "austria", "belgium", "luxembourg", "italy"))
eurozone <- toupper(c("finland", "greece", "portugal", "slovenia", "cyprus", "malta", "slovakia", "estonia", 
                      "latvia", "lithuania", "ireland"))
outsiders <- toupper(c("bulgaria", "croatia", "czechia", "hungary", "poland", "romania", "sweden", "united kingdom", "denmark"))

#western <- toupper(c("ireland", "united kingdom", "france", "luxembourg", "belgium", "netherlands", "portugal", "spain"))
#central <- toupper(c("germany", "austria", "czechia", "slovakia", "slovenia", "hungary", "poland", "greece", "malta", "cyprus"))
#northern <- toupper(c("denmark", "finland", "sweden"))
#estern <- toupper(c("croatia", "romania", "bulgaria", "estonia", "latvia", "lithuania", "italy"))

dataset.full <- dataset %>% mutate(country=toupper(country))
dataset.full <- merge(hdi.long, dataset.full) %>% 
  select("hdi", everything()) %>%
  mutate(state = case_when(country %in% leadership ~ "leadership",
                           country %in% eurozone ~ "eurozone",
                           country %in% outsiders ~ "outsiders"),
         #region = case_when(country %in% western ~ "western",
          #                  country %in% southern ~ "southern",
          #                  country %in% central ~ "cental",
          #                  country %in% northern ~ "northern",
          #                  country %in% southestern ~ "southern",
          #                  country %in% eastern ~ "eastern"),
         state = as.factor(state),
         #region = as.factor(region), 
         year = as.numeric(year),
         hdi = as.numeric(levels(hdi))[hdi]) %>%
  select(-c("year_fct"))


## Imputation

# dataset.full %>% filter(is.na(an_ind_rate)) # - the only missing value for an_ind_rate
dataset.full <- dataset.full %>% mutate(an_ind_rate = replace_na(an_ind_rate, 0))

get_closest <- function(dataset, col_name, country_name, n=3) {
  X <- dataset %>% filter(country != country_name) %>% select(c("country",  col_name, "year_fct"))
  Y <- dataset %>% filter(country == country_name) %>% select(c(col_name, "year_fct"))
  na_idx <- is.na(Y[, 1])
  X_wide <- X %>% spread(key="country", value=col_name)
  X_wide <- X_wide[!na_idx, ]
  Y <- Y[!na_idx, ]
  euclid_diff <- (X_wide %>% select(-("year_fct"))) - Y[, 1]
  euclid_diff <- apply(euclid_diff, 2, function(x) sqrt(sum(x^2)))
  countries <- colnames(X_wide)
  countries <- countries[2:length(countries)]
  countries[which.minn(euclid_diff, 3)]
}

fill_missing <- function(data, country_name, closest_countries, col_name) {
  Y <- data %>% filter(country == country_name) %>% select(c(col_name, "year_fct"))
  na_years <- Y[is.na(Y[,1]), 2] 
  X <- data %>% 
    subset(country %in% unlist(closest_countries)) %>%
    subset(year_fct %in% unlist(na_years)) %>%
    select(c("country", col_name, "year_fct")) %>%
    spread(key="country", value=col_name)
  mean_vals <- apply(X %>% select(-year_fct), 1, 'mean')
  Y[which(Y[,2] %in% unlist(na_years)), 1] <- mean_vals
  data[data$country == country_name, col_name] <- Y[, 1]
  data
}

#before_imput <- dataset %>% filter(country=="CROATIA") %>% ggplot(aes(x=year_fct, y=risk_poverty_pct)) + geom_point()
to_impute <- dataset %>% filter(country=="CROATIA", is.na(risk_poverty_pct)) %>% select(year_fct)

croatia_closest <- get_closest(dataset, "risk_poverty_pct", "CROATIA")
dataset <- fill_missing(dataset, "CROATIA", croatia_closest, "risk_poverty_pct")

dataset %>% filter(country=="CROATIA") %>% ggplot(aes(x=year_fct, y=risk_poverty_pct)) + geom_point()

after_imput <- dataset %>% filter(country=="CROATIA")
after_imput$imputed = "No"
after_imput[after_imput$year_fct %in% unlist(to_impute), "imputed"] = "Yes" 

after_imput %>%
  ggplot(aes(x=year_fct, y=risk_poverty_pct, color=imputed)) + 
  geom_point()

#### Cohorts

dataset.cohort <- dataset.full %>% filter(year==min(dataset.full$year))
X <- dataset.cohort %>% select(-c("hdi", "debt_pct_gdp", "immigration", "emmigration", "risk_poverty"))
fit.cohort_lm <- lm(hdi~., data=dataset.cohort)

dataset.cohort %>%
ggplot(aes(x=region, y=hdi)) + 
geom_boxplot()

#### Longitudinal
        

#### Mixed effects

fit.rand_slp <- lme(hdi ~ year*state + emp_pct + risk_poverty_pct, 
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
