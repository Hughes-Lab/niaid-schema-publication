# Function to pull the percent each property is used within several data repositories
# Metadata was harvested using the Metadata Crawler (https://crawler.biothings.io/)
# and joined with datasets from the Data Discovery Engine (https://discovery.biothings.io/guide/niaid)
# -- metadata registered according to the NIAID Dataset schema


# imports -----------------------------------------------------------------
library(tidyverse)
library(httr)
library(data.table)
library(jsonlite)


# Get DDE dataset metadata ------------------------------------------------

# Pull the records from the DDE
dde_url = "https://discovery.biothings.io/api/dataset/query?size=1000&q=_meta.guide:%22/guide/niaid%22"

getData = function(url = dde_url) {
  resp = httr::GET(url)
  if(resp$status_code == 200){
    res = jsonlite::fromJSON(content(resp, as = "text"))
    df = res$hits
    return(df)
  }
}

dde = getData()

# remove internal tracking props
dde = dde %>% select(-`_id`, -`@context`, -`_score`, `_meta`, -`_ts`, -`_meta`, -`@type`)


props_dde = colnames(dde) 

# count frequency of each property in the DDE ----------------------------------------
countNA = function(df, property, nameListProps = c("temporalCoverage")) {
  print(property)
  # properties like temporalCoverage are lists with a value "name"
  if(property %in% nameListProps) {
    counts = df %>% count(is.na(.data[[property]]["name"]))
  } else {
    counts = df %>% count(.data[[property]] == "NULL" | is.na(.data[[property]]))  
  }
  
  colnames(counts) = c("name", "n")
  
  total = counts %>% summarise(total = sum(n)) %>% pull(total)
  available = counts %>% filter(name == FALSE) %>% pull(n)
  
  results = tribble(~property, ~num_available, ~total,
                    property, available, total
  )
  return(results)
}


counts_dde = map_df(props_dde, function(x) countNA(dde, x)) %>% 
  mutate(# infectiousDisease is a deprecated property which has been replaced by healthCondition
    property = ifelse(property == "infectiousDisease", "healthCondition", property)
  ) %>% 
  group_by(property) %>% 
  summarise(num_available = sum(num_available),
            total = mean(total)) %>% 
  mutate(source = "Data Discovery Engine",
         pct_available = num_available / total) %>%
  arrange(desc(pct_available), property)

# Pull data from Metadata Crawler -----------------------------------------
md_url = "https://crawler.biothings.io/api/query?facets=_index&size=0&q=_exists_:"

# get all the schema.org Dataset properties.
schema_url = "https://raw.githubusercontent.com/schemaorg/schemaorg/main/data/releases/14.0/schemaorg-current-https-properties.csv"
schemaorg = read_csv(schema_url)

ds_properties = schemaorg %>% filter(domainIncludes %like% "https://schema.org/Dataset" |
                                       domainIncludes == "https://schema.org/CreativeWork" | 
                                       domainIncludes %like% "https://schema.org/CreativeWork," | # get rid of CreativeWorkSeries, etc.
                                       domainIncludes == "https://schema.org/Thing") %>% 
  pull(label)

# loop over the properties in the dataset to count how frequently they occur
countExists = function(property, url = md_url) {
  resp = httr::GET(paste0(url, property))
  if(resp$status_code == 200){
    res = jsonlite::fromJSON(content(resp, as = "text"))
    df = res$facets$`_index`$terms
    
    if(length(df) > 0){
      # bit of cleanup: save property, coalesce the index names
      df = df %>% 
        mutate(property = property,
               source = case_when(
                 term %like% "omicsdi" ~ "OmicsDI",
                 term %like% "ncbi_geo" ~ "NCBI GEO",
                 term %like% "harvard_dataverse" ~ "Harvard Dataverse",
                 term %like% "immport" ~ "ImmPort",
                 term %like% "zenodo" ~ "Zenodo",
                 term %like% "nyu" ~ "NYU Data Catalog",
                 TRUE ~ term
               ))
      
      df = df %>% group_by(source, property) %>% 
        summarise(count = sum(count)) %>% 
        ungroup()
      return(df)}
  }
}

# get total per index
total_per_idx = countExists("_id") %>% 
  select(-property) %>% 
  rename(total = count)

counts_md = map_df(ds_properties, function(x) countExists(x))

# merge totals to get a percent available.
counts_md = counts_md %>% 
  left_join(total_per_idx, by = "source") %>% 
  mutate(pct_available = count / total) %>% 
  rename(num_available = count)

# Also drop the DDE -- outdated info.
counts_md = counts_md %>% filter(source != "indexed_discovery")

# plot heatmap -----------------------------------------------------------------
# Group them together; calculate average
counts = bind_rows(counts_md, counts_dde)

# calculate the total number of entries
total_all_index = dde %>% nrow() + total_per_idx %>% summarise(total = sum(total)) %>% pull(total)
num_sources = counts %>% pull(source) %>% unique() %>% length()

total_counts = counts %>% 
  group_by(property) %>% 
  summarise(total  = total_all_index,
            num_available = sum(num_available),
            pct_available = sum(pct_available, na.rm = TRUE)/num_sources,
            pct_available_by_record = num_available / total) %>% 
  arrange(desc(pct_available), property) %>% 
  mutate(source = "AVERAGE",
         property_num = row_number())

property_names =  total_counts %>% pull(property) %>% unique()

# bind the average total to the per source totals
# sort on the average value
counts = counts %>% left_join(total_counts %>% select(property, property_num), by = "property")

counts = bind_rows(counts, total_counts)
# hand sort the properties
counts$source = factor(counts$source, rev(c("ImmPort", "Harvard Dataverse","NCBI GEO", "Zenodo", "Data Discovery Engine", "OmicsDI", "NYU Data Catalog", "AVERAGE")))

ggplot(counts, aes(x = property_num, y = source, fill = pct_available)) +
  geom_tile() +
  scale_x_continuous(sec.axis = dup_axis(), labels = property_names, breaks = 1:length(property_names)) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "YlGnBu"), limits = c(0, 1)) +
  coord_fixed() +
  theme_minimal() +
  theme(text = element_text(family = "Lato"),
        panel.grid = element_blank(),
        axis.text.x.top = element_text(angle = 45, hjust = 0),
        axis.text.x.bottom = element_text(angle = -45, hjust = 0),
        axis.title = element_blank(),
        legend.position = "none")