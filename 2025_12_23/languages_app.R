
library(tidyverse)
library(tidytuesdayR)
library(gh)
library(usethis)
library(janitor)
library(rnaturalearth)
library(countrycode)
library(ggiraph)
library(bslib)

#_______________________________________________________________________________
# Theming

ipa_colors <- c(
  "#1f2937",   # 1 Dark blue-gray (good for backgrounds)
  "#093c2eff", # 2 IPA forest
  "#27447E",   # 3 IPA navy
  "#374151",   # 4 Medium gray
  "#43a551ff", # 5 IPA green (good for primary)
  "#10b981",   # 6 IPA off-green
  "#84d0d4ff", # 7 IPA lt blue
  "#f59e0b",   # 8 IPA orange (good for warning)
  "#f8fafc"    # 9 Light gray (good for light backgrounds)
)

custom_theme <- 
  bs_theme(
    preset = "bootstrap",  # Start with clean bootstrap instead of darkly
    version = 5,
    primary = ipa_colors[5],    # IPA green for primary actions
    secondary = ipa_colors[2],  # Medium gray for secondary
    success = ipa_colors[7],    # Green for success
    warning = ipa_colors[8],    # Orange for warnings
    # Let bslib handle light/dark mode automatically  --> fg and bg
    base_font = font_google("Inter"),      # Modern, readable font
    heading_font = font_google("Inter"),   # Consistent headings
    code_font = font_google("JetBrains Mono") # Good code font
  )

#_______________________________________________________________________________
#  Call downloaded files
languages_rename <- readRDS(file = "data/languages_long_format_no_geo_coords.rds")
factor_lang_status <- readRDS(file = "data/language_status_factor_df.rds")
geo_countries <- readRDS(file = "data/rnaturalearth_api_df.rds")

#_______________________________________________________________________________
# Exploratory visualization

# Join lang data to the geo coords (as opposed to joining geo coords to the lang data which was useful for exploring the data)


#_______________________________________________________________________________
# Download diff projections from rnaturalearthdata
region_asia <- sf::st_transform(geo_countries, crs = "+proj=moll +lon_0=100")
region_europe <- sf::st_transform(geo_countries, crs = "+proj=moll +lon_0=10")
region_america <- sf::st_transform(geo_countries, crs = "+proj=moll +lon_0=-90")
region_africa <- sf::st_transform(geo_countries, crs = "+proj=moll +lon_0=20")
region_oceania <- sf::st_transform(geo_countries, crs = "+proj=moll +lon_0=150")
region_default <- sf::st_transform(geo_countries, crs = "+proj=moll +lon_0=0")


#_______________________________________________________________________________
# Wbstats population data
library(wbstats)

wb_pop_data <-
  wb_data(
  indicator = c(
    "SP.POP.TOTL",      # Population  
    "NY.GDP.PCAP.CD"),   # GDP per capita
  end_date = 2024) %>%
  rename(population = `SP.POP.TOTL`,
         gdp_capita = `NY.GDP.PCAP.CD`)


#_______________________________________________________________________________
# Modify languages data to have correct cols + add in wbstats population data

summary_lang <-
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
  left_join(y = wb_pop_data,
            by = c("lang_countries" = "iso2c"))  %>%
  mutate(humans_per_lang = population / n_languages,
         humans_per_fam = population / n_families,
         languages_per_family = n_languages / n_families)


#_______________________________________________________________________________
# Shiny application
library(shiny)
library(shinythemes)


# UI
ui <- fluidPage(
  title = "World Languages",
  theme = custom_theme,
  sidebarLayout(
    
    # Left sidebar
    sidebarPanel(
      
      h3("Plot Controls"),
      
      radioButtons(inputId = "region", 
                   label = "Select Region",
                   choices = setdiff(c("World", unique(geo_countries$region_un)), "Antarctica")
      ),
      
      radioButtons(inputId = "variable", 
                   label = "Select variable",
                   choices = c("No. of Languages",
                               "No. of Language Families",
                               "Humans per Language (average)",
                               "Humans per Language family (average)",
                               "Languages per family",
                               "Population",
                               "GDP per capita")),
      
      checkboxGroupInput(
        inputId = "lang_status",
        label = "Select language status:",
        choiceNames = paste0(factor_lang_status$code, ") ", str_to_title(factor_lang_status$label)),
        choiceValues = factor_lang_status$code,
        selected = factor_lang_status$code
      )
    ),
    
    # Main panel
    mainPanel(
      h2(textOutput("title_text")),
      h4(textOutput("subtitle_text")),
      girafeOutput("lang_plot"),
    )
  )
)

# Server
server <- function(input, output) {
  
  output$title_text <- renderText({
    input$variable
  })
  
  output$subtitle_text <- renderText({
    case_when(input$variable == "Population" ~ "World population",
              TRUE ~ "No subtitle entered, yet")
  })
  
  #_________________________________________ PLOTTING INTERACTIVITY ____________
  selected_region <- 
    reactive({
      if(input$region == "Africa"){
        left_join(
          x= region_africa, y = summary_lang, by = c("iso_a2"="lang_countries")
        ) %>%
          filter(region_un == "Africa")
      }else if(input$region == "Americas"){
        left_join(
          x= region_america, y = summary_lang, by = c("iso_a2"="lang_countries")
        ) %>%
          filter(region_un == "Americas")
      }else if(input$region == "Asia"){
        left_join(
          x= region_asia, y = summary_lang, by = c("iso_a2"="lang_countries")
        ) %>%
          filter(region_un == "Asia")
      }else if(input$region == "Europe"){
        left_join(
          x= region_europe, y = summary_lang, by = c("iso_a2"="lang_countries")
        ) %>%
          filter(region_un == "Europe")
      }else if(input$region == "Oceania"){
        left_join(
          x= region_oceania, y = summary_lang, by = c("iso_a2"="lang_countries")
        ) %>%
          filter(region_un == "Oceania")
      }else {
        left_join(
          x= region_default, y = summary_lang, by = c("iso_a2"="lang_countries")
        )}
    })
  
  
  variable <- 
    reactive({
      case_when(input$variable == "No. of Languages" ~ "n_languages",
                input$variable == "No. of Language Families" ~ "n_families",
                input$variable == "Humans per Language (average)" ~ "humans_per_lang",
                input$variable == "Humans per Language family (average)" ~ "humans_per_fam",
                input$variable == "Languages per family" ~ "languages_per_family",
                input$variable == "Population" ~ "population",
                input$variable == "GDP per capita" ~ "gdp_capita"
      )
    })
  
  output$lang_plot <- renderGirafe({
    
    map_selected <- 
      selected_region() %>%
      ggplot() +
      geom_sf_interactive(aes(fill = .data[[variable()]],
                              tooltip =  paste(name, 
                                               "\nPop:", scales::comma(population), 
                                               "\n# of Languages:", n_languages,
                                               "\n# of Lang Families:", n_families,
                                               "\nLanguages / Family:", round(languages_per_family, 2),
                                               "\nGDP per capita (USD):", scales::comma(round(gdp_capita, 0))),
                              data_id = iso_a2)) +
      scale_fill_viridis_c(trans = "sqrt",
                           name = str_wrap(input$variable,15),
                           labels = scales::comma) +
      theme_void()
    
    if(input$region == "Europe") {
      map_selected <- map_selected + coord_sf(xlim = c(-3500000, 2500000),
                                              ylim = c(3500000, 9000000))
    }
    
    if(input$region == "Africa") {
      map_selected <- map_selected + coord_sf(xlim = c(-4500000, 4500000),   # Senegal to Ethiopia
                                              ylim = c(-4500000, 4500000))   # Cape Town to Morocco
    }
          
    girafe_object <-
      girafe(
        ggobj = map_selected,
        width = 6 # Set only the width (in inches), ggiraph handles the rest
      )
    
    girafe_object
  })

  
}

# Run app
shinyApp(ui, server)






