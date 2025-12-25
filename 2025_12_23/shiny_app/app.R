
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

lang_data <-
  languages_rename %>%
  group_by(lang_countries) %>%
  count(lang_family, lang_status_label) %>%
  pivot_wider(names_from = lang_status_label, values_from = n, 
              values_fill = 0,
              names_repair = ~ str_replace_all(.x, " ", "_")) %>%
  group_by(lang_countries) %>%
  summarize(across(not_endangered:nearly_extinct, ~sum(.x)),
            total_languages = not_endangered + threatened + shifting + `NA` + moribund + nearly_extinct + extinct,
            total_families= n_distinct(lang_family)) 

summary_lang <-
  full_join(wb_pop_data, lang_data, by = c("iso2c" = "lang_countries")) %>%
  mutate(languages_per_family = total_languages / total_families,
         humans_per_language = population / (total_languages - `NA` - extinct),
         humans_per_family = population / total_families
         )

#_______________________________________________________________________________
# Shiny application
library(shiny)
library(shinythemes)


# UI
ui <- fluidPage(
  theme = shinytheme("darkly"),
  sidebarLayout(
    
    # Left sidebar
    sidebarPanel(
      width = 3,
      
      div(style = "margin-bottom: 40px;",
      
      h4("Plot Controls"),
      
      radioButtons(inputId = "region", 
                   label = "Select Region",
                   choices = setdiff(c("World", unique(geo_countries$region_un)), "Antarctica")
      ),
      
      radioButtons(inputId = "variable", 
                   label = "Select variable",
                   choices = c("No. of Languages" = "number_of_languages",
                               "No. of Language Families" = "total_families",
                               "Humans per Language (average)" = "humans_per_language",
                               "Humans per Language family (average)" = "humans_per_family",
                               "Languages per family" = "languages_per_family",
                               "Population" = "population"
                               ))
      ),  # close div
      
      conditionalPanel(
        condition = "input.variable == 'number_of_languages'",
        
        # Indent/style to show it's nested
        div(style = "margin-left: 30px",
            checkboxGroupInput(
              inputId = "lang_status",
              label = "Select language status:",
              choices = c("Not endangered" = "not_endangered",
                          "Threatened" = "threatened",
                          "Shifting" = "shifting",
                          "Moribund" = "moribund",
                          "Nearly extinct" = "nearly_extinct",
                          "Extinct" = "extinct"),
              selected = c("not_endangered","shifting","threatened", 
                           "moribund","nearly_extinct","extinct")
            )
        )
      )
    ),
    
    # Main panel
    mainPanel(
      width = 9,
      h1("World Languages", style = "color: #43a551ff;"),
      girafeOutput("lang_plot"),
      girafeOutput("histo", height = "200px"),
      a("Data from Glottolog 5.2", href = "https://glottolog.org/")
    )
  )
)

# Server
server <- function(input, output) {
  
  selected_region <- 
    reactive({
      if(input$region == "Africa"){
        left_join(
          x= region_africa, y = summary_lang, by = c("iso_a2"="iso2c")
        ) %>%
          filter(region_un == "Africa")
      }else if(input$region == "Americas"){
        left_join(
          x= region_america, y = summary_lang, by = c("iso_a2"="iso2c")
        ) %>%
          filter(region_un == "Americas")
      }else if(input$region == "Asia"){
        left_join(
          x= region_asia, y = summary_lang, by = c("iso_a2"="iso2c")
        ) %>%
          filter(region_un == "Asia")
      }else if(input$region == "Europe"){
        left_join(
          x= region_europe, y = summary_lang, by = c("iso_a2"="iso2c")
        ) %>%
          filter(region_un == "Europe")
      }else if(input$region == "Oceania"){
        left_join(
          x= region_oceania, y = summary_lang, by = c("iso_a2"="iso2c")
        ) %>%
          filter(region_un == "Oceania")
      }else {
        left_join(
          x= region_default, y = summary_lang, by = c("iso_a2"="iso2c")
        )}
    })
  
  plot_data <-
    reactive({
      data <- selected_region()
      
      data %>%
        mutate(number_of_languages = rowSums(across(all_of(input$lang_status))))
    })
  
  #                                                  _____    GGPLOT     _____
  output$lang_plot <- renderGirafe({
    
    
    variable <- input$variable
    
    map_selected <- 
      plot_data() %>%
      ggplot() +
      geom_sf_interactive(aes(fill = .data[[variable]],
                              tooltip =  paste(name, 
                                               "\nSelected variable:", .data[[variable]],
                                               "\n","Total Languages:", total_languages,
                                               "\nTotal Lang Families:", total_families,
                                               "\nLanguages / Family:", round(languages_per_family, 2),
                                               "\nPopulation:", scales::comma(population)),
                              data_id = iso_a2)) +
      scale_fill_viridis_c(trans = "sqrt",
                           name = "Unit",
                           labels = scales::comma) +
      theme_void() +
      labs(title = str_replace_all(input$variable, "_", " ") %>% str_to_title)
    
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
        width_svg = 10,     # Same fixed width
        height_svg = 6,     # Short height - aspect ratio ignored
        options = list(
          opts_sizing(rescale = FALSE, width = 1)  # Same settings
        ))
    
    girafe_object
  })
  
  output$histo <- renderGirafe({
    
    variable <- input$variable
    
    histogram <-
      ggplot(data = plot_data(),
           aes(x = .data[[variable]],
               fill = after_scale(..x..))) +
      geom_histogram(bins = 20, show.legend = F) +
      theme_bw() +
      scale_x_sqrt() +
      scale_fill_viridis_c(trans = "sqrt",
                           name = "Unit",
                           labels = scales::comma) +
      labs(y = "Countries",
           title = "Spread of countries",
           x = str_replace_all(input$variable, "_", " ") %>% str_to_title)
    
    girafe(ggobj = histogram,
           width_svg = 10,     # Same fixed width
           height_svg = 3,     # Short height - aspect ratio ignored
           options = list(
             opts_sizing(rescale = FALSE, width = 1)  # Same settings
           )
           )
    
  })

  
}

# Run app
shinyApp(ui, server)






