
library(tidyverse)
library(tidytuesdayR)
library(gh)
library(usethis)
library(janitor)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggiraph)
library(bslib)


#_______________________________________________________________________________
# API token

# # Need to activate a PAT (personal token)
# usethis::create_github_token()
# 
# # Replace (option 2) and copy in the token 
# gitcreds::gitcreds_set()

#_______________________________________________________________________________
# Tidy Tuesday calling

# # Select week of interest
# tt_datasets(year = 2025)
# 
# tidytuesdayR::last_tuesday()
# last_tuesday(date = today(tzone = "America/New_York"))
# 
# # Query API to pull data
# tuesdata <- tidytuesdayR::tt_load('2025-12-23')
# 
# # Create dfs for API query
# endangered_status <- tuesdata$endangered_status
# fam_lgs <- tuesdata$families
# lang_api <- tuesdata$languages
# 
# write_rds(endangered_status, file = "2025_12_23/data/2025_12_23_endangered_status.rds")
# write_rds(families, file = "2025_12_23/data/2025_12_23_families.rds")
# write_rds(languages, file = "2025_12_23/data/2025_12_23_languages.rds")

#_______________________________________________________________________________
#  Call downloaded files
endangered_status <- readRDS(file = "2025_12_23/data/2025_12_23_endangered_status.rds")
fam_api <- readRDS(file = "2025_12_23/data/2025_12_23_families.rds")
lang_api <- readRDS(file = "2025_12_23/data/2025_12_23_languages.rds")

#_______________________________________________________________________________
#  Filter and clean language data
languages <- 
  lang_api %>%
  mutate(match_id = id %in% endangered_status$id,
         match_fam = family_id %in% fam_api$id) %>%
  # left_join(y = endangered_status, by = "id") %>%
  # left_join(y = fam_api, by = c("family_id" = "id")) %>%
  rename(language = name)

#_______________________________________________________________________________
#  Exploring data structure

languages %>%
  count(is_isolate, is.na(family_id)) 
# Conclusion: All NA family ids are isolates (is_isolate) -- Perfect match here


endangered_status %>% 
  count(status_code, status_label, na.rm = F)
# Conclusion: status codes are directly matched to their labels;  no NAs

endangered_status %>% 
  mutate(matched = id %in% fam_api$id) %>%
  count(matched)  
# Conclusion: very few matches --- FOUND OUT WHY: endangered_status uses lang id, not family id


# What are isolates?

languages %>% filter(is_isolate) %>% count(macroarea)
# Macro areas aren't linked to the isolates... 132 NA

languages %>% filter(is_isolate) %>% 
  separate_longer_delim(countries, delim = ";") %>%
  count(countries) %>%
  print(n=100)
# Seems a lot of isolates in south american coutnries.. and US? (native languages?)


#_______________________________________________________________________________
# Bring in Rnaturalearth data / coordinates


geo_countries <- rnaturalearth::ne_countries(type = "countries", returnclass = "sf", scale = "medium")

plot(geo_countries)

sf::st_crs(geo_countries)
sf::st_bbox(geo_countries)
sf::st_geometry_type(geo_countries)

world_moll <- sf::st_transform(geo_countries, crs = "+proj=moll")
world_merc <- sf::st_transform(geo_countries, crs = 3857)
world_wg <- sf::st_transform(geo_countries, crs = 4326)

# ______________________________________________________________________________
# Join language with coordinate data
languages_rename <- 
  languages %>% 
  separate_longer_delim(countries, delim = ";") %>%
  rename_with(~ paste0("lang_", .))  # Prefix everything except join key

lang_geo <- 
  languages_rename %>% 
  left_join(
    x = .,
    y = world_moll,
    by = c("lang_countries" = "iso_a2")
  )


# ______________________________________________________________________________
# Exploration with geographic data

lang_geo %>%
  count(name) %>%
  arrange(desc(n)) %>% 
  head(20)
# Seems a lot of isolates in south american coutnries.. and US? (native languages?)as_con


lang_geo %>%
  count(continent) %>%
  head(50)
# Asia and Africa are way more language rich. Americas sits in between, Europe very few

lang_geo %>%
  count(region_un) %>%
  head(20)
# East Asia/Pacific and Sub Saharan Africa are clear lang rich... but central asia perhaps too

lang_geo %>%
  count(subregion) %>%
  arrange(desc(n)) %>%
  head(20)

lang_geo %>%
  group_by(name) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
# How many lang entries per country? most pop = papua new guinea... is it possible thera re 900 languages??

lang_geo %>%
  filter(name=="Papua New Guinea") %>%
  count(lang_status_code, lang_status_label)
# apparently... yes there are that many language, but almost a third shifting (parents use but not children) and half of that threatened (all gens use, but pop reducing)



lang_geo %>%
  count(name, subregion) %>%
  # group_by(name, subregion) %>%
  # summarize(n = n()) %>%
  filter(n == 1) %>%
  count(subregion)
# Islands are most likely to have 1 lang. First we counted languages per countries, then filtered only countries with 1 language, then counted the subregions in that group of 1-language countries

lang_geo %>%
  count(lang_language) %>%
  arrange(desc(n))
# English is the top language

lang_geo %>% tabyl(lang_language) %>% arrange(desc(n)) %>% head
# Anotehr way to compute with janitor package --> tabyl function

#why is english only 34 times?
lang_geo %>% filter(lang_language == "English") %>% count(name) %>% print(n=30)
# so this is representing where english in the official language. --- given list - liberia for example
# what about pidgin?
lang_geo %>% filter(name == "Nigeria",
                    str_detect(lang_language, "Pidgin"))
# wow so... differentiation within english isn't accounted for? Unlike 'dialects' for other languages
# So Pidgin English isn't included within English... puts into question what we are qualifying as English


lang_geo %>%
  count(continent, lang_status_label) %>% pivot_wider(names_from = lang_status_label, values_from = n)

lang_status_labels <- lang_geo %>% pull(lang_status_label) %>% unique %>% na.omit()
lang_status_codes <- lang_geo %>% pull(lang_status_code) %>% unique %>% na.omit()

factor_lang_status <- 
  tibble(label = lang_status_labels,
         code = lang_status_codes) %>%
  arrange(code) %>%
  left_join(tibble(
    code = c(1:6),
    definition = c("Is spoken by all generations and intergenerational transmission is uninterrupted.",
                   "Is spoken by most children but may be restricted to specific social domains, such as the home.",
                   "Children no longer learn the language as a mother tongue in the home.",
                   "The language is spoken by grandparents and older generations. While the parent generation may understand it, they do not speak it to children or among themselves.",
                   "The youngest speakers are grandparents and older, and they speak the language partially and infrequently.",
                   "There are no speakers left.")),
    by = "code") %>%
  mutate(col_name = str_replace(label, " ", "_"))

lang_geo %>%
  tabyl(continent, lang_status_label) %>%
  relocate(continent, factor_lang_status$label[1:6])


#_______________________________________________________________________________
# Exploratory visualization
# Join lang data to the geo coords (as opposed to joining geo coords to the lang data which was useful for exploring the data)
languages_rename %>% names
geo_lang <- 
  languages_rename %>%
  group_by(lang_countries) %>%
  summarize(
    across(c(lang_id, lang_latitude, lang_longitude), first),
    n_languages      = n_distinct(lang_language),
    n_families       = n_distinct(lang_family),
    n_not_extinct    = sum(lang_status_code==1, na.rm = T),
    n_threatened     = sum(lang_status_code==2, na.rm = T),
    n_shifting       = sum(lang_status_code==3, na.rm = T),
    n_moribund       = sum(lang_status_code==4, na.rm = T),
    n_almost_extinct = sum(lang_status_code==5, na.rm = T),
    n_extinct        = sum(lang_status_code==6, na.rm = T)
  ) %>%
  left_join(
    x = world_moll,
    y = .,
    by = c("iso_a2" = "lang_countries")
  ) %>%
  mutate(
    humans_per_lang = pop_est / n_languages,
    humans_per_fam = pop_est / n_families,
    languages_per_family = n_languages / n_families,
  )

geo_lang %>% count(region_wb)

#_______________________________________________________________________________
# Check aggregation happened correctly
lang_geo %>% count(name) %>% arrange(desc(n))
geo_lang %>% filter(name == "Papua New Guinea")
# Yes, Papua New Guinea still with 899 languages


#_______________________________________________________________________________
# Select continent for mapping
selected_continent <- 
  geo_lang %>%
  filter(str_detect(continent, "Africa"), name!= "Russia")

map_selected <- 
  selected_continent %>%
  ggplot() +
  geom_sf_interactive(aes(fill=n_languages,
                          tooltip =  paste(name, "\nPop:", pop_est, "\n# of Languages:", n_languages),
                          data_id = iso_a2)) +
  scale_fill_viridis_c(trans = "sqrt", name = "title") +
  theme_void()

# Use girafe without explicitly setting svg_height
# girafe will automatically calculate height based on the plot's intrinsic aspect ratio and the provided width.
map_selected_gir <- 
  girafe(
    ggobj = map_selected,
    width = 8 # Set only the width (in inches), ggiraph handles the rest
  )

map_selected_gir %>% class

map_selected_gir


#_______________________________________________________________________________
# Export relevant dfs for App.r

# write_rds(lang_geo, file = "2025_12_23/data/language_with_geo_coords.rds")
write_rds(languages_rename, file = "2025_12_23/shiny_app/data/languages_long_format_no_geo_coords.rds")
write_rds(factor_lang_status, file = "2025_12_23/shiny_app/data/language_status_factor_df.rds")
write_rds(geo_countries, file = "2025_12_23/shiny_app/data/rnaturalearth_api_df.rds")



