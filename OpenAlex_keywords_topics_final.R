#remotes::install_github("ropensci/openalexR")
library(openalexR)
library(tidyverse)
library(readr)
options(openalexR.mailto = "natsumi.solange.shokida@umontreal.ca")

# Keyword search in titles and abstracts ---------------------------------------

list_keywords <- c("sex as a biological variable", "sex difference", 
                   "sex based biology", "sex dependent gene expression",
                   "sex specific heritability", "social genomics",
                   "Developmental Origins of Health and Disease",
                   "maternal fetal effects", "genomic")

# Topics -----------------------------------------------------------------------

# Sex and Gender in Healthcare (https://api.openalex.org/topics/T13252)
# Sex Determination and Differentiation in Organisms (11077)
# Birth, Development, and Health (11629)
# Maternal and fetal healthcare (11389)

list_topics <- c("t13252", "t11077", "t11629", "t11389")

# keywords ---------------------------------------------------------------------

keywords_df <- read_csv("data/OpenAlex/keywords_df.csv")

# Pivot keywords table to long format
keywords_df <- keywords_df %>%
  rename(Year = publication_year, Domain = domain) %>%
  pivot_longer(
    cols = -c(Year, Domain),
    names_to = "Keyword",
    values_to = "Count"
  )

# topics -----------------------------------------------------------------------

topics_df <- read_csv("data/OpenAlex/topics_df.csv")

topics_df <- topics_df %>%
  rename(Year = publication_year, Domain = domain) %>%
  pivot_longer(
    cols     = -c(Year, Domain),
    names_to = "Topic",
    values_to= "Count"
  ) %>%
  # Replace topic codes with descriptive labels
  mutate(Topic = recode(Topic,
    t13252 = "Sexo y género en la atención en salud (t13252)",
    t11077 = "Determinación y diferenciación sexual en organismos (t11077)",
    t11629 = "Nacimiento, desarrollo y salud (t11629)",
    t11389 = "Atención en salud maternal y fetal (t11389)"
  ))

# PLOTS ------------------------------------------------------------------------

keyword_year <- keywords_df %>%
  filter(Year >= 1975,
         Year <= 2022) |> 
  group_by(Year, Keyword) %>%
  summarize(Count = sum(Count, na.rm = TRUE), .groups = "drop") |> 
  mutate(Keyword = case_when(Keyword == "genomic" ~ "genómica",
                             Keyword == "sex difference" ~ "diferencia sexual",
                             Keyword == "maternal fetal effects" ~ "efectos materno-fetales",
                             Keyword == "social genomics" ~ "genómica social",
                             Keyword == "sex as a biological variable" ~ "sexo como variable biológica",
                             Keyword == "sex dependent gene expression" ~ "expresión génica \ndependiente del sexo",
                             Keyword == "sex based biology" ~ "biología basada en el sexo",
                             Keyword == "Developmental Origins of Health and Disease" ~ "orígenes del desarrollo de \nla salud y la enfermedad",
                             Keyword == "sex specific heritability" ~ "herencia sexo específica")) |> 
  filter(!is.na(Keyword)) |> 
  mutate(Keyword = fct_reorder(Keyword, Count, .fun = sum, .desc = TRUE))

# Plot evolution of keyword usage over time, faceted by keyword with free scales
keyword_year_plot <- ggplot(keyword_year, aes(x = Year, y = Count)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Evolución del uso de palabras clave",
    x = "Año",
    y = "Cantidad de documentos"
  ) +
  facet_wrap(~Keyword, scales = "free", nrow = 2) +
  theme(legend.position = "none")

# Display the plot
print(keyword_year_plot)

ggsave("keyword_year_plot.png", width = 15, height = 5, scale = 0.8)

# ------------------------------------------------------------------------------

# Summarize total counts per year and topic
# topic_year <- topics_df %>%
#   filter(Year >= 1975,
#          Year <= 2022) |> 
#   group_by(Year, Topic) %>%
#   summarize(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
#   mutate(Topic = case_when(Topic == "Sex Determination and Differentiation in Organisms (t11077)" ~
#                              "Sex Determination and Differentiation \nin Organisms (t11077)",
#                            TRUE ~ Topic)) |> 
#   mutate(Topic = fct_reorder(Topic, Count, .fun = sum, .desc = TRUE))

topic_year <- topics_df %>%
  filter(Year >= 1975,
         Year <= 2022) |> 
  group_by(Year, Topic) %>%
  summarize(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  mutate(Topic = case_when(Topic == "Determinación y diferenciación sexual en organismos (t11077)" ~
                             "Determinación y diferenciación sexual \nen organismos (t11077)",
                           Topic == "Sexo y género en la atención en salud (t13252)" ~
                             "Sexo y género en la atención \nen salud (t13252)",
                           TRUE ~ Topic)) |> 
  mutate(Topic = fct_reorder(Topic, Count, .fun = sum, .desc = TRUE))

# Plot evolution of topic usage over time, faceted by topic
topic_year_plot <- ggplot(topic_year, aes(x = Year, y = Count)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Evolución del uso de tópicos",
    x = "Año",
    y = "Cantidad de documentos"
  ) +
  facet_wrap(~Topic, scales = "free", nrow = 1) +
  theme(legend.position = "none")

# Display the plot
print(topic_year_plot)

ggsave("topic_year_plot.png", width = 15, height = 5, scale = 0.8)

# ------------------------------------------------------------------------------

# Evolution of keyword usage by domain
# keyword_domain <- keywords_df %>%
#   filter(Year >= 1975,
#          Year <= 2022) |> 
#   filter(!is.na(Domain)) |> 
#   mutate(Keyword = case_when(Keyword == "Developmental Origins of Health and Disease" ~ 
#                                "Developmental Origins \nof Health and Disease",
#                              Keyword == "sex dependent gene expression" ~
#                                "sex dependent \ngene expression",
#                              Keyword == "sex as a biological variable" ~
#                                "sex as a biological \nvariable",
#                              TRUE ~ Keyword)) |> 
#   mutate(Campo = case_when(Domain == "Health Sciences" ~ "Ciencias de la Salud",
#                            Domain == "Life Sciences" ~ "Ciencias de la Vida",
#                            Domain == "Physical Sciences" ~ "Ciencias Físicas",
#                            Domain == "Social Sciences" ~ "Ciencias Sociales")) |> 
#   mutate(Keyword = fct_reorder(Keyword, Count, .fun = sum, .na_rm = TRUE, .desc = TRUE)) 
# 
# keyword_domain_plot <- ggplot(keyword_domain, aes(x = Year, y = Count, color = Campo)) +
#   geom_line(linewidth = 0.5) +
#   theme_minimal() +
#   labs(
#     title = "Evolución del uso de palabras clave según área del conocimiento",
#     x = "Año",
#     y = "Cantidad de documentos"
#   ) +
#   facet_wrap(~Keyword, scales = "free_y", nrow = 2) +
#   theme(legend.position = "bottom")

keyword_domain <- keywords_df %>%
  filter(Year >= 1975,
         Year <= 2022) |> 
  filter(!is.na(Domain)) |> 
  mutate(Keyword = case_when(Keyword == "genomic" ~ "genómica",
                             Keyword == "sex difference" ~ "diferencia sexual",
                             Keyword == "maternal fetal effects" ~ "efectos materno-fetales",
                             Keyword == "social genomics" ~ "genómica social",
                             Keyword == "sex as a biological variable" ~ "sexo como variable biológica",
                             Keyword == "sex dependent gene expression" ~ "expresión génica \ndependiente del sexo",
                             Keyword == "sex based biology" ~ "biología basada en el sexo",
                             Keyword == "Developmental Origins of Health and Disease" ~ "orígenes del desarrollo de \nla salud y la enfermedad",
                             Keyword == "sex specific heritability" ~ "herencia sexo específica")) |> 
  mutate(Campo = case_when(Domain == "Health Sciences" ~ "Ciencias de la Salud",
                           Domain == "Life Sciences" ~ "Ciencias de la Vida",
                           Domain == "Physical Sciences" ~ "Ciencias Físicas",
                           Domain == "Social Sciences" ~ "Ciencias Sociales")) |> 
  filter(!is.na(Keyword)) |> 
  mutate(Keyword = fct_reorder(Keyword, Count, .fun = sum, .na_rm = TRUE, .desc = TRUE)) 

keyword_domain_plot <- ggplot(keyword_domain, aes(x = Year, y = Count, color = Campo)) +
  geom_line(linewidth = 0.5) +
  theme_minimal() +
  labs(
    title = "Evolución del uso de palabras clave según área del conocimiento",
    x = "Año",
    y = "Cantidad de documentos"
  ) +
  facet_wrap(~Keyword, scales = "free_y", nrow = 2) +
  theme(legend.position = "bottom")

# Display the keyword-domain plot
print(keyword_domain_plot)

ggsave("keyword_domain_plot.png", width = 15, height = 5, scale = 0.8)
# ------------------------------------------------------------------------------

# Evolution of topic usage by domain
# topic_domain <- topics_df %>%
#   filter(Year >= 1975,
#          Year <= 2022) |> 
#   filter(!is.na(Domain)) |> 
#   mutate(Topic = case_when(Topic == "Sex Determination and Differentiation in Organisms (t11077)" ~
#                              "Sex Determination and Differentiation \nin Organisms (t11077)",
#                            TRUE ~ Topic)) |> 
#   mutate(Campo = case_when(Domain == "Health Sciences" ~ "Ciencias de la Salud",
#                            Domain == "Life Sciences" ~ "Ciencias de la Vida",
#                            Domain == "Physical Sciences" ~ "Ciencias Físicas",
#                            Domain == "Social Sciences" ~ "Ciencias Sociales")) |> 
#   mutate(Topic = fct_reorder(Topic, Count, .fun = sum, .na_rm = TRUE, .desc = TRUE))

topic_domain <- topics_df %>%
  filter(Year >= 1975,
         Year <= 2022) |> 
  filter(!is.na(Domain)) |> 
  mutate(Topic = case_when(Topic == "Determinación y diferenciación sexual en organismos (t11077)" ~
                             "Determinación y diferenciación sexual \nen organismos (t11077)",
                           Topic == "Sexo y género en la atención en salud (t13252)" ~
                             "Sexo y género en la atención \nen salud (t13252)",
                           TRUE ~ Topic)) |> 
  mutate(Campo = case_when(Domain == "Health Sciences" ~ "Ciencias de la Salud",
                           Domain == "Life Sciences" ~ "Ciencias de la Vida",
                           Domain == "Physical Sciences" ~ "Ciencias Físicas",
                           Domain == "Social Sciences" ~ "Ciencias Sociales")) |> 
  mutate(Topic = fct_reorder(Topic, Count, .fun = sum, .na_rm = TRUE, .desc = TRUE))

topic_domain_plot <- ggplot(topic_domain, aes(x = Year, y = Count, color = Campo)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Evolución del uso de tópicos según área del conocimiento",
    x = "Año",
    y = "Cantidad de documentos"
  ) +
  facet_wrap(~Topic, scales = "free_y", nrow = 1) +
  theme(legend.position = "bottom")

# Display the topic-domain plot
print(topic_domain_plot)

ggsave("topic_domain_plot.png", width = 15, height = 5, scale = 0.8)

# ------------------------------------------------------------------------------

# Faceted bar plot: distribution of each keyword by domain, ordered by total count
domain_keywords <- keywords_df %>%
  group_by(Keyword, Domain) %>%
  summarize(Count = sum(Count, na.rm = TRUE), .groups = "drop")

# Get keyword order by total count
domain_order <- domain_keywords %>%
  group_by(Keyword) %>%
  summarize(Total = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Total))

# Set factor levels for correct facet order
domain_keywords <- domain_keywords %>%
  mutate(Keyword = factor(Keyword, levels = domain_order$Keyword))

keyword_domain_tot <- ggplot(domain_keywords, aes(x = reorder(Domain, -Count), y = Count, fill = Domain)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Keyword, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Distribution of Each Keyword by Domain",
    x = "Domain",
    y = "Count"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(keyword_domain_tot)
