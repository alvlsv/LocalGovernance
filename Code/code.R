## Vlasov Alexander
##
## Project for the course
## Natural Resource Economics by Gerhard Toews
##

# Libraries ----------
library(tidyverse)
library(data.table)
library(tidytable)
library(tsibble)
library(fable)
library(forcats)

library(here)
i_am("code/code.R")
figure_path <- here("Figures")
extra_datasets_path <- "datasets/created datasets/"

library(scales)
library(ggridges)

library(stargazer)

# Dataset reading ----------

consolidated_budget_raw <-
  read_csv2("datasets/ConsolidatedBudgets/data.csv")



regional_gdp <-
  read_csv2("datasets/VVP/data_by_kind_of_activity.csv") |>
  mutate(
    year_date = ymd(year, truncated = 2),
    okato_region = substr(okato, start = 1, stop = 2)
  )


articles <-
  consolidated_budget |>
  select(NAME, CODE) |>
  group_by(NAME) |>
  summarise(n = length(CODE), alltypes = paste(CODE, collapse = ", "), )




municipal_model <-
  read_csv("datasets/BumoModels/data.csv") |>
  mutate(
    model_eng = str_replace_all(
      model,
      c(
        'Избираемый мэр' = 'Elected mayor',
        'Сити-менджер' = 'City manager',
        "Назначаемый мэр" = "Appointed mayor"
      )
    ),
    year_date = ymd(year, truncated = 2),
    mun_type_eng = str_replace_all(
      mun_type,
      c(
        "Городской округ, городской округ с внутригородским делением" = "City",
        "Муниципальный район" = "Municipal district"
      )
    ),
    region_code = substr(region, start = 3, stop = 4),
    region_name = substr(region, start = 6, stop = length(region)),
    region_code_oktmo =  substr(oktmo, start = 1, stop = 2),
  )




# Data Exploration -------

## Municipal Model  -----

### Ploting the model by year ----

model_by_year <-
  municipal_model |>
  group_by(model_eng, year_date, mun_type_eng) |>
  summarize(n = n())



model_by_year_plot <-
  ggplot(model_by_year, aes(x = year_date, y = n, fill = model_eng)) +
  geom_area() +
  labs(fill = "Governance model") +
  theme_bw() +
  scale_x_date(
    NULL,
    breaks = scales::breaks_width("2 years"),
    labels = scales::label_date("'%y")
  ) +  scale_y_continuous("Quantity of Municipalities", n.breaks = 10) +
  facet_wrap(~ mun_type_eng, scales = "free_y", dir = "h") +
  theme(legend.position = "bottom")



ggsave(
  "model_by_year_plot.pdf",
  model_by_year_plot,
  path = figure_path,
  width = 210 / 1.2,
  height = 148 / 1.2,
  units = "mm",
  device = cairo_pdf
)




model_shares_region <-
  municipal_model |>
  group_by(year_date, region_name) |>
  summarise(
    share_city_manager = mean(model_eng == "City manager", na.rm = T),
    share_elected = mean(model_eng == "Elected mayor", na.rm = T),
    share_appointed = mean(model_eng == "Appointed mayor", na.rm = T)
  )





model_shares_region |> stargazer(
  summary = T,
  summary.stat = c("n", "min", "p25", "median", "mean", "sd", "p75", "max")
)

### Ridge plot -----


ridgeplot_models <-
  ggplot(model_shares_region, aes(y = year_date |>
                                    year() |>
                                    factor())) +
  geom_density_ridges(
    aes(x = share_elected),
    stat = "binline",
    alpha = .7,
    panel_scaling = T
  ) +
  scale_x_continuous("Share of Elected Mayors in a Region", labels = scales::label_percent()) +
  theme_ridges(font_size = 11,
               grid = TRUE,
               center_axis_labels = TRUE) +
  labs(y = "") +
  coord_cartesian(clip = "off") +
  scale_y_discrete(expand = c(0, 0))




ggsave(
  "ridgeplot_models.pdf",
  ridgeplot_models,
  path = figure_path,
  width = 210 / 1.2,
  height = 148 / 1.2,
  units = "mm",
  device = cairo_pdf
)






## Regional GDP ----

resourse_extraction_gdp

gdp_pc <- regional_gdp |> filter(grepl("населения", indicator))

## Dependent Variables -------

### Resourse Extraction GDP -----
##


activities <- regional_gdp$kind_of_activity |> unique()
res_extraction_act <- activities[7]

indicators <- regional_gdp$indicator |> unique()



resourse_extraction_gdp <-
  regional_gdp |>
  filter(kind_of_activity == res_extraction_act,
         grepl("постоянных", units)) |>
  mutate(diff_resourse_gdp = as.numeric(value) / 100) |>
  select(-value, -indicator, -kind_of_activity, -okato, -id, -units) |>
  na.omit() |>
  group_by(territory) |>
  mutate(
    resourse_gdp = cumprod(diff_resourse_gdp),
    log_resourse_gdp = log(resourse_gdp),
    diff_log_resourse_gdp = difference(log_resourse_gdp)
  ) |> filter(okato_region != 45)


ridgeplot_resourse_extraction_gdp <-
  ggplot(resourse_extraction_gdp, aes(y = year_date |> year() |> factor())) +
  geom_density_ridges(
    aes(x = log_resourse_gdp),
    alpha = .7,
    panel_scaling = T,
    rel_min_height = 0.01,
    stat = "binline",
  ) +
  scale_x_continuous("Log GDP due to natural resourses (Index, 2004 prices)", n.breaks =
                       5) +
  theme_ridges(font_size = 11,
               grid = TRUE,
               center_axis_labels = TRUE) +
  labs(y = "") +
  coord_cartesian(clip = "off") +
  scale_y_discrete(expand = c(0, 0))



ggsave(
  "ridgeplot_resourse_extraction_gdp.pdf",
  ridgeplot_resourse_extraction_gdp,
  path = figure_path,
  width = 210 / 1.2,
  height = 148 / 1.2,
  units = "mm",
  device = cairo_pdf
)



### GDP per capita-----

gdp_pc <-
  regional_gdp |>
  filter(indicator == indicators[1]) |>
  select(-kind_of_activity,-id,-units,-okato_region,-indicator,-okato) |>
  mutate(gdp_pc = as.numeric(value), log_gdp_pc = log(gdp_pc), ) |> select(-value)

gdp_region <-
  regional_gdp |>
  filter(indicator == indicators[2]) |>
  select(-kind_of_activity,-id,-okato_region,-indicator,-okato,-units) |>
  mutate(gdp = as.numeric(value), log_gdp = log(gdp)) |> select(-value)

regional_gdp |>
  filter(indicator == indicators[2]) |>
  select(-kind_of_activity,-id,-okato_region,-indicator,-okato,-units) |>
  mutate(gdp = as.numeric(value)) |> select(-value)





military_gdp <-
  regional_gdp |>
  filter(kind_of_activity == activities[3], grepl("постоянных", units)) |>
  mutate(diff_military_gdp = as.numeric(value) / 100) |>
  select(-value, -indicator, -kind_of_activity, -okato, -id, -units) |>
  na.omit() |>
  group_by(territory) |>
  mutate(
    military_gdp = cumprod(diff_military_gdp),
    log_military_gdp = log(military_gdp)
  ) |> select(-diff_military_gdp, -military_gdp, -okato_region)



manufacturing_gdp <-
  regional_gdp |>
  filter(kind_of_activity == activities[9], grepl("постоянных", units)) |>
  mutate(diff_manufacturing_gdp = as.numeric(value) / 100) |>
  select(-value, -indicator, -kind_of_activity, -okato, -id, -units) |>
  na.omit() |>
  group_by(territory) |>
  mutate(
    manufacturing_gdp = cumprod(diff_manufacturing_gdp),
    log_manufacturing_gdp = log(manufacturing_gdp)
  ) |> select(-diff_manufacturing_gdp, -manufacturing_gdp, -okato_region)

healthcare_gdp <-
  regional_gdp |>
  filter(kind_of_activity == activities[4], grepl("постоянных", units)) |>
  mutate(diff_healthcare_gdp = as.numeric(value) / 100) |>
  select(-value, -indicator, -kind_of_activity, -okato, -id, -units) |>
  na.omit() |>
  group_by(territory) |>
  mutate(
    healthcare_gdp = cumprod(diff_healthcare_gdp),
    log_healthcare_gdp = log(healthcare_gdp)
  ) |> select(-diff_healthcare_gdp, -healthcare_gdp, -okato_region)


education_gdp <-
  regional_gdp |>
  filter(kind_of_activity == activities[10], grepl("постоянных", units)) |>
  mutate(diff_education_gdp = as.numeric(value) / 100) |>
  select(-value, -indicator, -kind_of_activity, -okato, -id, -units) |>
  na.omit() |>
  group_by(territory) |>
  mutate(
    education_gdp = cumprod(diff_education_gdp),
    log_education_gdp = log(education_gdp)
  ) |> select(-diff_education_gdp, -education_gdp, -okato_region)



energy_gdp <-
  regional_gdp |>
  filter(kind_of_activity == activities[8], grepl("постоянных", units)) |>
  mutate(diff_energy_gdp = as.numeric(value) / 100) |>
  select(-value, -indicator, -kind_of_activity, -okato, -id, -units) |>
  na.omit() |>
  group_by(territory) |>
  mutate(energy_gdp = cumprod(diff_energy_gdp),
         log_energy_gdp = log(energy_gdp)) |> select(-diff_energy_gdp, -energy_gdp, -okato_region)


home_prod_gdp <-
  regional_gdp |>
  filter(
    kind_of_activity == activities[17],
    grepl("постоянных", units)
  ) |>
  mutate(diff_home_prod_gdp = as.numeric(value) / 100 + 0.001) |>
  select(-value, -indicator, -kind_of_activity, -okato, -id, -units) |>
  na.omit() |>
  group_by(territory) |>
  mutate(
    home_prod_gdp = cumprod(diff_home_prod_gdp),
    log_home_prod_gdp = log(home_prod_gdp)
  ) |> select(-diff_home_prod_gdp, -home_prod_gdp, -okato_region)


aggriculture_gdp <-
  regional_gdp |>
  filter(kind_of_activity == activities[13], grepl("постоянных", units)) |>
  mutate(diff_aggriculture_gdp = as.numeric(value) / 100) |>
  select(-value, -indicator, -kind_of_activity, -okato, -id, -units) |>
  na.omit() |>
  group_by(territory) |>
  mutate(
    aggriculture_gdp = cumprod(diff_aggriculture_gdp),
    log_aggriculture_gdp = log(aggriculture_gdp)
  ) |> select(-diff_aggriculture_gdp, -aggriculture_gdp, -okato_region)



retail_gdp <-
  regional_gdp |>
  filter(kind_of_activity == activities[11], grepl("постоянных", units)) |>
  mutate(diff_retail_gdp = as.numeric(value) / 100) |>
  select(-value, -indicator, -kind_of_activity, -okato, -id, -units) |>
  na.omit() |>
  group_by(territory) |>
  mutate(retail_gdp = cumprod(diff_retail_gdp),
         log_retail_gdp = log(retail_gdp)) |>
  select(-diff_retail_gdp, -retail_gdp, -okato_region)




transport_gdp <-
  regional_gdp |>
  filter(kind_of_activity == activities[15], grepl("постоянных", units)) |>
  mutate(diff_transport_gdp = as.numeric(value) / 100) |>
  select(-value, -indicator, -kind_of_activity, -okato, -id, -units) |>
  na.omit() |>
  group_by(territory) |>
  mutate(
    transport_gdp = cumprod(diff_transport_gdp),
    log_transport_gdp = log(transport_gdp)
  ) |>
  select(-diff_transport_gdp, -transport_gdp, -okato_region)



hospitality_gdp <-
  regional_gdp |>
  filter(kind_of_activity == activities[5], grepl("постоянных", units)) |>
  mutate(diff_hospitality_gdp = as.numeric(value) / 100) |>
  select(-value, -indicator, -kind_of_activity, -okato, -id, -units) |>
  na.omit() |>
  group_by(territory) |>
  mutate(
    hospitality_gdp = cumprod(diff_hospitality_gdp),
    log_hospitality_gdp = log(hospitality_gdp)
  ) |>
  select(-diff_hospitality_gdp, -hospitality_gdp, -okato_region)



oil_supply_shocks_monthly <- 
  read_xlsx("datasets/Oil Supply News Shocks June 2023.xlsx", sheet=4)|> select(-2)


names(oil_supply_shocks_monthly)<- c("Date", "Oil_news_shock")


oil_supply_shocks<- oil_supply_shocks_monthly |> mutate(date=ym(Date), year=year(date))|> group_by(year) |> summarize(oil_shock=mean(Oil_news_shock)) 


model_resourses <-
  dplyr::inner_join(
    as_tibble(resourse_extraction_gdp) ,
    as_tibble(model_shares_region) ,
    by = join_by(territory == region_name, year_date)
  )  |>
  filter(!is.na(share_elected), !is.na(resourse_gdp)) |> distinct() |>
  dplyr::left_join(gdp_pc, by = join_by(territory == territory, year_date, year)) |>
  dplyr::left_join(military_gdp, by = join_by(territory == territory, year_date, year)) |>
  dplyr::left_join(manufacturing_gdp, by = join_by(territory == territory, year_date, year)) |>
  dplyr::left_join(healthcare_gdp, by = join_by(territory == territory, year_date, year)) |>
  dplyr::left_join(energy_gdp, by = join_by(territory == territory, year_date, year)) |>
  dplyr::left_join(education_gdp, by = join_by(territory == territory, year_date, year)) |>
  dplyr::left_join(construction_gdp, by = join_by(territory == territory, year_date, year)) |>
  dplyr::left_join(aggriculture_gdp, by = join_by(territory == territory, year_date, year)) |>
  dplyr::left_join(retail_gdp, by = join_by(territory == territory, year_date, year)) |>
  dplyr::left_join(transport_gdp, by = join_by(territory == territory, year_date, year)) |>
  dplyr::left_join(hospitality_gdp, by = join_by(territory == territory, year_date, year)) |>
  dplyr::left_join(gdp_region, by = join_by(territory == territory, year_date, year))|> 
  dplyr::left_join(oil_supply_shocks, by=join_by(year))












dupl <- model_resourses |> duplicates(index = year_date, key = territory)



model_resourses_ts <-
  model_resourses |>
  as_tsibble(index = year_date, key = territory) |>
  mutate(
    territory = factor(territory),
    share_appointed = 100 * share_appointed,
    share_city_manager = 100 * share_city_manager,
    share_elected = 100 * share_elected,
    population = gdp / gdp_pc
  )




ggplot(model_resourses_ts,
       aes(x = log_resourse_gdp, y = share_elected / 100, color = year)) + geom_point() +
  theme_light() + scale_y_continuous("Share of Municipalities with Elected Mayor in a Region",
                                     labels = scales::label_percent()) + labs(color = "Year", x = "Log GDP due to natural resourses (Index, 2004 prices)")



ggplot(model_resourses_ts,
       aes(x = log_gdp_pc, y = share_elected / 100, color = year)) + geom_point() +
  theme_light() + scale_y_continuous("Share of Municipalities with Elected Mayor in a Region",
                                     labels = scales::label_percent()) + labs(color = "Year", x = "Log GDP per capita")



# Panel Methods ----
#
library(plm)

library(lmtest)     # linear hypothesis tests
library(sandwich)   # Sandwich estimator
library(clubSandwich)   # Sandwich estimator

model_0 <-
  plm(
    share_elected ~ log_resourse_gdp + log_gdp_pc,
    pdata.frame(model_resourses_ts, index = c("year_date", "territory")),
    effect = "twoways"
  )
summary(model_0)


model_elected_1 <-
  plm(
    share_elected ~ log_resourse_gdp + log_gdp + log(population)++log_military_gdp + log_manufacturing_gdp +
      log_healthcare_gdp + log_energy_gdp + log_education_gdp  +
      log_construction_gdp + log_aggriculture_gdp  + log_retail_gdp + log_transport_gdp +
      log_hospitality_gdp ,
    pdata.frame(model_resourses_ts, index = c( "territory", "year_date")),
    effect = "twoways"
  )

summary(model_elected_1)

model_appointed_1 <-
  plm(
    share_appointed ~ log_resourse_gdp + log_gdp + log(population)+log_military_gdp + log_manufacturing_gdp +
      log_healthcare_gdp + log_energy_gdp + log_education_gdp  +
      log_construction_gdp + log_aggriculture_gdp  + log_retail_gdp + log_transport_gdp +
      log_hospitality_gdp ,
    pdata.frame(model_resourses_ts, index = c("territory", "year_date")),
    effect = "twoways"
  )
summary(model_appointed_1)




model_city_manager_1 <-
  plm(
    share_city_manager ~ log_resourse_gdp + log_gdp + log(population)+log_military_gdp + log_manufacturing_gdp +
      log_healthcare_gdp + log_energy_gdp + log_education_gdp  +
      log_construction_gdp + log_aggriculture_gdp  + log_retail_gdp + log_transport_gdp +
      log_hospitality_gdp ,
    pdata.frame(model_resourses_ts, index = c("territory", "year_date")),
    effect = "twoways"
  )
model_city_manager_1 |> summary()


stargazer(
  model_elected_1,
  model_city_manager_1,
  model_appointed_1,
  type = "text",
  se = list(
    summary(model_elected_1, vcov. = vcovDC(model_elected_1, type = "HC3"))$coefficients[, 2],
    summary(
      model_city_manager_1,
      vcov. = vcovDC(model_city_manager_1, type = "HC3")
    )$coefficients[, 2],
    summary(model_appointed_1, vcov. = vcovDC(model_appointed_1, type = "HC3"))$coefficients[, 2]
  ),
  df=F
)

stargazer(
  model_elected_1,
  model_city_manager_1,
  model_appointed_1,
  type = "latex",
  se = list(
    summary(model_elected_1, vcov. = vcovDC(model_elected_1, type = "HC3"))$coefficients[, 2],
    summary(
      model_city_manager_1,
      vcov. = vcovDC(model_city_manager_1, type = "HC3")
    )$coefficients[, 2],
    summary(model_appointed_1, vcov. = vcovDC(model_appointed_1, type = "HC3"))$coefficients[, 2]
  ), 
  df=F
)

summary(model_elected_1, vcov. = vcovDC(model_elected_1, type = "HC3"))
summary(model_elected_1)








model_elected_2 <-
  plm(
    share_elected ~ log_resourse_gdp + log_energy_gdp + log_gdp + log(population) +
      log_military_gdp + log_manufacturing_gdp +
      log_healthcare_gdp + log_education_gdp  +
      log_construction_gdp + log_aggriculture_gdp  + log_retail_gdp + log_transport_gdp +
      log_hospitality_gdp |
      oil_shock*lag(log_resourse_gdp,1) + oil_shock*lag(log_energy_gdp) + log_gdp + log(population) + log_military_gdp + log_manufacturing_gdp +
      log_healthcare_gdp + log_education_gdp  +
      log_construction_gdp + log_aggriculture_gdp  + log_retail_gdp + log_transport_gdp +
      log_hospitality_gdp,
    pdata.frame(model_resourses_ts, index = c("territory", "year_date")),
    effect = "twoways"
  )
 
