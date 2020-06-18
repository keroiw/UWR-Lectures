library(eurostat)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(lubridate)


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

base_cols <- c("country", "year_fct")

### Population

get_population <- function() {
  population <- get_eurostat("demo_pjan", type="label") %>% postprocess()
  population.clean <- population %>% 
    filter(unit=="Number", age=="Total", sex=="Total") %>%
    reduce_columns()
  colnames(population.clean) <- c(base_cols, "population")
  population.clean
}

### Employment

get_employment <- function() {
  employment <- get_stat("t2020_10")
  employment.clean <- employment %>%
    filter(sex == "Total") %>% 
    reduce_columns()
  colnames(employment.clean) <- c(base_cols, "emp_pct")
  employment.clean
}

### Debt

get_debt <- function() {
  debt <- get_stat("teina225")
  debt.clean <- debt %>% 
    filter(grepl("^Percentage", unit)) %>%
    reduce_columns()
  colnames(debt.clean) <- c(base_cols, "debt_pct_gdp")
  debt.clean
}

### Poverty

get_poverty <- function() {
  poverty <- get_stat("ilc_peps01")
  poverty.clean <- poverty %>% 
    filter(sex == "Total" & age == "Total", unit == "Percentage") %>%
    reduce_columns()
  colnames(poverty.clean) <- c(base_cols, "risk_poverty_pct")
  poverty.clean
}


### Industry growth rates

get_ind_rates <- function() {
  ind_growth_rates <- get_eurostat("ei_isir_m", type="label", select_time = "M") %>% 
    postprocess()
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
  ind_growth_rates.clean
}

### Immigration 

get_immigration <- function() {
  immigration <- get_stat("migr_imm8")
  immigration.clean <- immigration %>% 
    filter(unit=="Number", sex=="Total", age=="Total", agedef=="Age in completed years") %>%
    reduce_columns()
  colnames(immigration.clean) <- c(base_cols, "immigration")
  immigration.clean
}

### Emmigration

get_emmigration <- function() {
  emmigration <- get_stat("migr_emi2")
  emmigration.clean <- emmigration %>% 
    filter(unit=="Number", sex=="Total", age=="Total", agedef=="Age in completed years") %>%
    reduce_columns()
  colnames(emmigration.clean) <- c(base_cols, "emmigration") 
  emmigration.clean
}

### HICP

get_hicp <- function() {
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
  hicp.clean
}

### Gdp-Per-Cap

get_gdp <- function() {
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
}

## Merge datasets

merge_tables <- function() {
  population.clean <- get_population()
  employment.clean <- get_employment()
  debt.clean <- get_debt()
  hicp.clean <- get_hicp()
  poverty.clean <- get_poverty()
  gdp_per_cap.clean <- get_gdp()
  ind_growth_rates.clean <- get_ind_rates()
  immigration.clean <- get_immigration()
  emmigration.clean <- get_emmigration()
  
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
  dataset
}

dataset.merged <- merge_tables()
write.table(dataset.merged, "/home/wiorek/Desktop/UWR-Lectures/ComplexData/Eurostat_Files/dataset_merged.csv", row.names=T)

## Imputation

dataset.merged <- read.table("/home/wiorek/Desktop/UWR-Lectures/ComplexData/Eurostat_Files/dataset_merged.csv", row.names=1)
dataset.merged <- dataset.merged %>% mutate(year_fct = as.ordered(year_fct))
dataset.filled <- dataset.merged %>% mutate(an_ind_rate = replace_na(an_ind_rate, 0))

get_closest <- function(dataset, col_name, country_name, n=3) {
  X <- dataset %>% filter(country != country_name) %>% select(c("country",  col_name, "year_fct"))
  Y <- dataset %>% filter(country == country_name) %>% select(c(col_name, "year_fct"))
  na_idx <- is.na(Y[, 1])
  na_years <- Y[is.na(Y[,1]), 2]
  X_wide <- X %>% spread(key="country", value=col_name)
  X_wide <- X_wide[!na_idx, ]
  Y <- Y[!na_idx, ]
  euclid_diff <- (X_wide %>% select(-("year_fct"))) - Y[, 1]
  euclid_diff <- apply(euclid_diff, 2, function(x) sqrt(sum(x^2)))
  countries <- colnames(X_wide)
  countries <- countries[2:length(countries)]
  list(dist=euclid_diff[doBy::which.minn(euclid_diff, n)], na_years=na_years)
}

fill_missing <- function(data, country_name, closest_observ, na_years, col_name) {
  Y <- data %>% filter(country == country_name) %>% select(c(col_name, "year_fct"))
  closest_countries <- names(closest_observ)
  X <- data %>% 
    subset(country %in% unlist(closest_countries)) %>%
    subset(year_fct %in% unlist(na_years)) %>%
    select(c("country", col_name, "year_fct")) %>%
    spread(key="country", value=col_name)
  weights <- 1/closest_observ
  partial_mean <- function(x) weighted.mean(x, weights, na.rm = T)
  mean_vals <- apply(X %>% select(-year_fct), 1, partial_mean)
  Y[which(Y[,2] %in% unlist(na_years)), 1] <- mean_vals
  Y$imputed <- "No"
  Y[which(Y[,2] %in% unlist(na_years)), "imputed"] <- "Yes"
  data[data$country == country_name, col_name] <- Y[, 1]
  list(data=data, Y=Y)
}

impute <- function(dataset, col_name, country, n=3) {
  croatia_closest <- get_closest(dataset, col_name, country, n)
  dataset.filled <- fill_missing(dataset, country, croatia_closest$dist, croatia_closest$na_years, col_name)
  list(imputed=dataset.filled$data, imputed_vals=dataset.filled$Y)
}

dataset.filled <- impute(dataset.filled, "risk_poverty_pct", "CROATIA")
dataset.filled$imputed_vals %>% ggplot(aes(x=year_fct, y=risk_poverty_pct, color=imputed)) + geom_point()

dataset.filled <- impute(dataset.filled$imputed, "risk_poverty_pct", "ROMANIA", n=2)
dataset.filled$imputed_vals %>% ggplot(aes(x=year_fct, y=risk_poverty_pct, color=imputed)) + geom_point()

dataset.filled <- dataset.filled$imputed %>% mutate(risk_poverty_pct = replace_na(risk_poverty_pct, 57))
dataset.filled %>% filter(country=="BULGARIA") %>% ggplot(aes(x=year_fct, y=immigration)) + geom_point()

## Immigration

get_missing <- function(data, country_name, col_name) {
  missing <- data %>% 
    filter(country==country_name) %>% 
    mutate(year = as.numeric(levels(year_fct))[year_fct]) %>%
    select("year", col_name)
  missing_years_pred <- is.na(missing[, 2])
  missing_years_idx <- which(missing_years_pred)
  missing_years <- missing[missing_years_idx, 1]
  list(missing=missing, pred=missing_years_pred,  idx=missing_years_idx, years=missing_years)
}

impute_ols <- function(data, country_name, col_name) {
  missing_info <- get_missing(data, country_name, col_name)
  missing <- missing_info$missing
  pred <- missing_info$pred
  idx <- missing_info$idx
  years <- missing_info$years
  
  lm_fit <- lm(paste(col_name, "year", sep=' ~ '), data=missing[which(!pred),])
  lm_pred <- predict(lm_fit, newdata = data.frame(year=years))
  lm_pred <- pmax(lm_pred, 0)
  missing[idx, 2] <- lm_pred
  data[data$country == country_name, col_name] <- missing[, 2]
  missing$imputed="No"
  missing[pred, "imputed"] = "Yes"
  list(data=data, imputed=missing)
}

impute_const <- function(data, country_name, col_name, const_val) {
  missing_info <- get_missing(data, country_name, col_name)
  missing <- missing_info$missing
  pred <- missing_info$pred
  idx <- missing_info$idx
  
  missing[idx, 2] <- const_val
  data[data$country == country_name, col_name] <- missing[, 2]
  missing$imputed="No"
  missing[pred, "imputed"] = "Yes"
  list(data=data, imputed=missing)
}

dataset.filled <- impute_ols(dataset.filled, "BULGARIA", "immigration")
dataset.filled$imputed %>% ggplot(aes(x=year, y=immigration, color=imputed)) + geom_point()
dataset.filled$data %>% filter(country=="BULGARIA") %>% ggplot(aes(x=year_fct, y=immigration)) + geom_point()
dataset.filled <- dataset.filled$data

dataset.filled <- impute_const(dataset.filled, "ROMANIA", "immigration", 140000)
dataset.filled$imputed %>% ggplot(aes(x=year, y=immigration, color=imputed)) + geom_point()
dataset.filled <- dataset.filled$data

dataset.filled <- impute_const(dataset.filled, "CROATIA", "immigration", 15000)
dataset.filled$imputed %>% ggplot(aes(x=year, y=immigration, color=imputed)) + geom_point()
dataset.filled <- dataset.filled$data

dataset.filled <- impute_const(dataset.filled, "BELGIUM", "immigration", 140000)
dataset.filled$imputed %>% ggplot(aes(x=year, y=immigration, color=imputed)) + geom_point()
dataset.filled <- dataset.filled$data

dataset.filled <- impute_ols(dataset.filled, "FRANCE", "immigration")
dataset.filled$imputed %>% ggplot(aes(x=year, y=immigration, color=imputed)) + geom_point()
dataset.filled <- dataset.filled$data

dataset.filled %>% filter(country=="UNITED KINGDOM") %>% ggplot(aes(x=year_fct, y=immigration)) + geom_point()
dataset.filled <- impute_const(dataset.filled, "UNITED KINGDOM", "immigration", 525000)
dataset.filled$imputed %>% ggplot(aes(x=year, y=immigration, color=imputed)) + geom_point()
dataset.filled <- dataset.filled$data

## Emmigration

dataset.filled %>% filter(country=="BULGARIA") %>% ggplot(aes(x=year_fct, y=emmigration)) + geom_point()
dataset.filled <- impute_ols(dataset.filled, "BULGARIA", "emmigration")
dataset.filled$imputed %>% ggplot(aes(x=year, y=emmigration, color=imputed)) + geom_point()
dataset.filled <- dataset.filled$data

dataset.filled %>% filter(country=="CROATIA") %>% ggplot(aes(x=year_fct, y=emmigration)) + geom_point()
dataset.filled <- impute_const(dataset.filled, "CROATIA", "emmigration", 8000)
dataset.filled$imputed %>% ggplot(aes(x=year, y=emmigration, color=imputed)) + geom_point()
dataset.filled <- dataset.filled$data

dataset.filled %>% filter(country=="FRANCE") %>% ggplot(aes(x=year_fct, y=emmigration)) + geom_point()
dataset.filled <- impute_const(dataset.filled, "FRANCE", "emmigration", 190000)
dataset.filled$imputed %>% ggplot(aes(x=year, y=emmigration, color=imputed)) + geom_point()
dataset.filled <- dataset.filled$data

dataset.filled %>% filter(country=="MALTA") %>% ggplot(aes(x=year_fct, y=emmigration)) + geom_point()
dataset.filled <- impute_const(dataset.filled, "MALTA", "emmigration", 4000)
dataset.filled$imputed %>% ggplot(aes(x=year, y=emmigration, color=imputed)) + geom_point()
dataset.filled <- dataset.filled$data

dataset.filled %>% filter(country=="ROMANIA") %>% ggplot(aes(x=year_fct, y=emmigration)) + geom_point()
dataset.filled <- impute_const(dataset.filled, "ROMANIA", "emmigration", 200000)
dataset.filled$imputed %>% ggplot(aes(x=year, y=emmigration, color=imputed)) + geom_point()
dataset.filled <- dataset.filled$data

dataset.filled %>% filter(country=="BELGIUM") %>% ggplot(aes(x=year_fct, y=emmigration)) + geom_point()
dataset.filled <- impute_const(dataset.filled, "BELGIUM", "emmigration", 90000)
dataset.filled$imputed %>% ggplot(aes(x=year, y=emmigration, color=imputed)) + geom_point()
dataset.filled <- dataset.filled$data

dataset.filled %>% filter(country=="CYPRUS") %>% ggplot(aes(x=year_fct, y=emmigration)) + geom_point()
dataset.filled <- impute_const(dataset.filled, "CYPRUS", "emmigration", 5000)
dataset.filled$imputed %>% ggplot(aes(x=year, y=emmigration, color=imputed)) + geom_point()
dataset.filled <- dataset.filled$data

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

## Merging hdi with full

leadership <- toupper(c("germany", "france", "spain", "netherlands", "austria", "belgium", "luxembourg", "italy"))
eurozone <- toupper(c("finland", "greece", "portugal", "slovenia", "cyprus", "malta", "slovakia", "estonia", 
                      "latvia", "lithuania", "ireland"))
outsiders <- toupper(c("bulgaria", "croatia", "czechia", "hungary", "poland", "romania", "sweden", "united kingdom", "denmark"))

dataset.full <- merge(hdi.long, dataset.filled) %>% 
  select("hdi", everything()) %>%
  mutate(state = case_when(country %in% leadership ~ "leadership",
                           country %in% eurozone ~ "eurozone",
                           country %in% outsiders ~ "outsiders"),
         state = as.factor(state),
         year = as.numeric(year),
         hdi = as.numeric(levels(hdi))[hdi]) %>%
  select(-c("year_fct", "debt_pct_gdp"))

dataset.normalized <- dataset.full
dataset.normalized$immigration <- (dataset.full$immigration / dataset.full$population) * 100
dataset.normalized$emmigration <- (dataset.full$emmigration / dataset.full$population) * 100
dataset.normalized$population <- dataset.full$population / 10000000
dataset.normalized$gdp_per_cap <- dataset.full$gdp_per_cap/ 10000

write.table(dataset.merged, "/home/wiorek/Desktop/UWR-Lectures/ComplexData/Eurostat_Files/dataset_normalized.csv", row.names=T)

#### Longitudinal

dataset.normalized <- read.table("/home/wiorek/Desktop/UWR-Lectures/ComplexData/Eurostat_Files/dataset_normalized.csv", row.names=1)

to_diff <- setdiff(colnames(dataset.normalized), c("country", "year", "state", "immigration", "emmigration"))
to_sum <- c("an_ind_rate")
make_longit <- function(country_data) {
  baseline <- country_data[country_data$year == 2005,]
  years <- sort(unique(country_data$year))
  longit <- country_data
  i = 1
  for (year in years[2:length(years)]) {
    df_row <-  country_data[country_data$year == year, ]
    longit[country_data$year == year, to_diff] <-  df_row[,to_diff] - baseline[,to_diff]
    longit[country_data$year == year, to_sum] <- tail(cumsum(country_data[1:i, to_sum]), n=1)
    i = i + 1
  }
  longit[-1, ]
}

country_data <- list()
for (country_name in unique(dataset.normalized$country)) {
  country_data[[country_name]] <- dataset.normalized %>% filter(country==country_name)
}

dataset.longit <- dplyr::bind_rows(lapply(country_data, make_longit))

write.table(dataset.longit, "/home/wiorek/Desktop/UWR-Lectures/ComplexData/Eurostat_Files/dataset_longit.csv", row.names=T)
