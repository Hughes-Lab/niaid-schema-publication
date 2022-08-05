library(tidyverse)
library(httr)
library(jsonlite)
library(extrafont)
library(ggthemes)
extrafont::loadfonts()


# Fetch the metadata from the Data Discovery Engine -----------------------
url_nde = "https://api.data.niaid.nih.gov/v1/query?q=includedInDataCatalog.name:%22Data%20Discovery%20Engine%22&size=1000"
url = "https://discovery.biothings.io/api/dataset/query?q=_meta.guide:(%22/guide/niaid%22%20OR%20%22/guide/niaid/ComputationalTool%22)&size=1000"

resp = httr::GET(url)
resp_nde = httr::GET(url_nde)

if(resp$status_code == 200) {
  results = jsonlite::fromJSON(content(resp, as="text"))
  df = results$hits
}

# The NIAID Data Ecosystem harvests metadata from the DDE and does some minor cleanup of measurementTechniques/pathogens
if(resp_nde$status_code == 200) {
  results_nde = jsonlite::fromJSON(content(resp_nde, as="text"))
  nde = results_nde$hits
}

# Clean up outdated type info
df = df %>% mutate(category = str_replace_all(`@type`, "niaid:", ""),
                   category = str_replace_all(category, "Niaid", ""))

# Donut chart -------------------------------------------------------------
donutColors = c("#e05e8f", "#32142d")

df %>%
  count(category) %>%
  mutate(ymax = sum(n),
         ymin = n  ) %>%
  ggplot(aes(x = 2.7, y = n, fill = category)) +
  geom_col() +
  annotate(geom = "text", x = 1, y = 0, label = str_c(df %>% count(), " total"), family = "Lato", size = 10) +
  annotate(geom = "text", x = 3.5, y = 0, label = str_c(df %>% filter(`@type` == "ComputationalTool") %>% count(), " computational tools"), family = "Lato", size = 6, colour = donutColors[1], hjust = 0) +
  annotate(geom = "text", x = 3.25, y = 100, label = str_c(df %>% filter(`@type` == "Dataset") %>% count(), " datasets"), family = "Lato", size = 6, colour = donutColors[2], hjust = 1) +
  coord_polar(theta="y", direction = -1) +
  xlim(c(1, 4)) +
  scale_fill_manual(values = donutColors) +
  theme_void() +
  theme(legend.position = "none")


# sdPublisher -------------------------------------------------------------
getName = function(x) {
  if(length(x) > 0 && x != "NULL") {
    if("name" %in% names(x)) {
      source_names = x$name
    } else {
      source_names = x
    }

    if(length(source_names) > 1) {
      return(paste(source_names, collapse = "#"))
    } else {
      return(source_names)
    }
  } else {
    return("not specified")
  }
}

sources = df %>%
  mutate(source_str = sapply(sdPublisher, function(x) getName(x)),
         source_str = str_replace_all(source_str, " and ", "#"),
         source = str_split(source_str, "#"))


sources2 = sources %>%
  unnest(cols = source) %>%
  mutate(source = ifelse(str_trim(source) == "GEO (pprivate)", "GEO", source)) %>%
  count(category, source) %>%
  arrange(desc(n))

sources2$source = factor(sources2$source, sources2 %>% pull(source) %>% unique() %>% rev())

ggplot(sources2, aes(x = source, y = n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~category, ncol=1) +
  ggtitle("Original metadata repository") +
  theme_minimal() +
  theme(text = element_text(family = "Lato", size = 18),
        axis.title = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "none")


# pathogens ---------------------------------------------------------
pathogens = nde %>% mutate(pathogens_str = sapply(infectiousAgent, function(x) getName(x))) %>%
  unnest(cols = pathogens_str) %>%
  mutate(pathogens = str_split(pathogens_str, "#")) %>%
  unnest(cols = pathogens) %>%
  mutate(pathogens = str_trim(pathogens))


pathogen_count = pathogens %>%
  # lump into other if not frequent
  mutate(pathogens = ifelse(pathogens == "OTHER", "Other",
                            ifelse(pathogens == "Severe acute respiratory syndrome coronavirus 2", "SARS coronavirus 2", pathogens)),
    pathogen_grp = fct_lump_n(pathogens, 21)) %>%
  count(pathogen_grp) %>%
  arrange(desc(n))


# refactor to sort by frequency
pathogen_count$pathogen_grp = factor(pathogen_count$pathogen_grp, pathogen_count$pathogen_grp %>%  rev())

# Pull the Tableau20 palette
colorPalette = ggthemes_data[["tableau"]][["color-palettes"]][["regular"]][["Tableau 20"]] %>% mutate(rank = row_number(), isOdd = (rank-1)%%2) %>% arrange(isOdd, rank) %>% pull(value)
colorPalette = c("#DDDDDD", colorPalette) %>% rev()

ggplot(pathogen_count, aes(x = pathogen_grp, y = n, fill = pathogen_grp)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = colorPalette) +
  ggtitle("Pathogens") +
  theme_minimal() +
  theme(text = element_text(family = "Lato", size = 18, color = "#EFEFEF"),
        axis.text = element_text(family = "Lato", size = 18, color = "#EFEFEF"),
        axis.title = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "none")

# measurement techniques ---------------------------------------------------------
techniques = nde %>% mutate(
  techniques_str = sapply(measurementTechnique, function(x) getName(x)),
  techniques = str_split(techniques_str, "#")) %>%
  unnest(cols = techniques) %>%
  mutate(techniques = str_trim(techniques))


# Count number of measurement techniques. A dataset or tool can have multiple techiques
techniques_count = techniques %>%
  # lump into other if not frequent
  mutate(techniques_grp = fct_lump_n(techniques, 20)) %>%
  count(techniques_grp) %>%
  arrange(desc(n))

# refactor to sort by frequency
techniques_count$techniques_grp = factor(techniques_count$techniques_grp, techniques_count$techniques_grp %>%  rev())

ggplot(techniques_count, aes(x = techniques_grp, y = n)) +
  geom_col(fill = "#FFFFFF") +
  coord_flip() +
  ggtitle("Measurement technique in catalogued datasets and computational tools") +
  theme_minimal() +
  theme(text = element_text(family = "Lato", size = 18, color = "#EFEFEF"),
        axis.text = element_text(family = "Lato", size = 18, color = "#EFEFEF"),
        axis.title = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "none")
