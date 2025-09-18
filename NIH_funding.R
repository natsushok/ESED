library(tidyverse)
library(ggplot2)
library(patchwork)

genomic_total <- read_csv("data/NIH RePORT/genomic_total.csv")
genomic_social <- read_csv("data/NIH RePORT/genomic_social.csv")

sex_difference_total <- read_csv("data/NIH RePORT/sex_difference_total.csv")
sex_difference_social <- read_csv("data/NIH RePORT/sex_difference_social.csv")

maternal_fetal_total <- read_csv("data/NIH RePORT/maternal_fetal_total.csv")
maternal_fetal_social <- read_csv("data/NIH RePORT/maternal_fetal_social.csv")

social_genomic_total <- read_csv("data/NIH RePORT/social_genomic_total.csv")
social_genomic_social <- read_csv("data/NIH RePORT/social_genomic_social.csv")

sabv_total <- read_csv("data/NIH RePORT/SABV_total.csv")
sabv_social <- read_csv("data/NIH RePORT/SABV_social.csv")

genomic_total <- genomic_total %>%
  rename(Year = `Fiscal Year`, Funding = `Total Funding`) %>%
  select(Year, Projects, Funding) %>%
  mutate(Keyword = "genomic", Group = "Total") |> 
  filter(!is.na(Year))

genomic_social <- genomic_social %>%
  rename(Year = `Fiscal Year`, Funding = `Total Funding`) %>%
  select(Year, Projects, Funding) %>%
  mutate(Keyword = "genomic", Group = "Social") |> 
  filter(!is.na(Year))

sex_difference_total <- sex_difference_total %>%
  rename(Year = `Fiscal Year`, Funding = `Total Funding`) %>%
  select(Year, Projects, Funding) %>%
  mutate(Keyword = "sex difference", Group = "Total") |> 
  filter(!is.na(Year))

sex_difference_social <- sex_difference_social %>%
  rename(Year = `Fiscal Year`, Funding = `Total Funding`) %>%
  select(Year, Projects, Funding) %>%
  mutate(Keyword = "sex difference", Group = "Social") |> 
  filter(!is.na(Year))

maternal_fetal_total <- maternal_fetal_total %>%
  rename(Year = `Fiscal Year`, Funding = `Total Funding`) %>%
  select(Year, Projects, Funding) %>%
  mutate(Keyword = "maternal fetal", Group = "Total") |> 
  filter(!is.na(Year))

maternal_fetal_social <- maternal_fetal_social %>%
  rename(Year = `Fiscal Year`, Funding = `Total Funding`) %>%
  select(Year, Projects, Funding) %>%
  mutate(Keyword = "maternal fetal", Group = "Social") |> 
  filter(!is.na(Year))

social_genomic_total <- social_genomic_total %>%
  rename(Year = `Fiscal Year`, Funding = `Total Funding`) %>%
  select(Year, Projects, Funding) %>%
  mutate(Keyword = "social genomic", Group = "Total") |> 
  filter(!is.na(Year))

social_genomic_social <- social_genomic_social %>%
  rename(Year = `Fiscal Year`, Funding = `Total Funding`) %>%
  select(Year, Projects, Funding) %>%
  mutate(Keyword = "social genomic", Group = "Social") |> 
  filter(!is.na(Year))

sabv_total <- sabv_total %>%
  rename(Year = `Fiscal Year`, Funding = `Total Funding`) %>%
  select(Year, Projects, Funding) %>%
  mutate(Keyword = "SABV", Group = "Total") |> 
  filter(!is.na(Year))

sabv_social <- sabv_social %>%
  rename(Year = `Fiscal Year`, Funding = `Total Funding`) %>%
  select(Year, Projects, Funding) %>%
  mutate(Keyword = "SABV", Group = "Social") |> 
  filter(!is.na(Year))

summary <- bind_rows(
  genomic_total,
  genomic_social,
  sex_difference_total,
  sex_difference_social,
  maternal_fetal_total,
  maternal_fetal_social,
  social_genomic_total,
  social_genomic_social,
  sabv_total,
  sabv_social
)

summary <- summary %>%
  mutate(Funding_USD = as.numeric(gsub("[$,]", "", Funding)))

summary <- summary |> 
  mutate(Keyword = case_when(Keyword == "genomic" ~ "genómica",
                   Keyword == "sex difference" ~ "diferencia sexual",
                   Keyword == "maternal fetal" ~ "materno fetal",
                   Keyword == "social genomic" ~ "sociogenómica",
                   Keyword == "SABV" ~ "SABV"))

# Ordenar las keywords por el total de funding (Year == "Total") en orden decreciente
keyword_order <- summary %>%
  filter(Year == "Total", Group == "Total") %>%
  arrange(desc(Funding_USD)) %>%
  pull(Keyword)

summary <- summary %>%
  mutate(Keyword = factor(Keyword, levels = keyword_order))

# Plot 1: Evolution of funding (bars, 2008-2024)
plot_evolution <- summary %>%
  filter(Year != "Total", 
         Year >= 2008, 
         Year <= 2024) %>%
  ggplot(aes(x = as.numeric(Year), y = Funding_USD / 1e6, fill = Group, group = Group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~ Keyword, scales = "free_y", nrow = 1) +
  labs(
    title = "Evolución del presupuesto de NIH por año",
    subtitle = "Categorías vinculadas a ciencias sociales vs. Total (2008-2024)",
    x = "Año",
    y = "Presupuesto (Millones de dólares)",
    fill = "Grupo"
  ) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10))

# Plot 2: Comparison of totals
plot_totals <- summary %>%
  filter(Year == "Total") %>%
  ggplot(aes(x = Group, y = Funding_USD / 1e6, fill = Group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  facet_wrap(~ Keyword, scales = "free_y", nrow = 1) +
  labs(
    title = "Comparación de presupuesto agregado de NIH por grupo (1985-2025)",
    x = "",
    y = "",
    fill = "Grupo"
  ) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10),
        legend.position = "none")

# Combine plots: evolution on top, totals below
combined_plot <- plot_evolution / plot_totals + plot_layout(heights = c(2, 1))

print(combined_plot)

ggsave("funding_keywords_total_social.png", width = 10, height = 5, scale = 1)

# Calcular el porcentaje promedio que Social representa del Total Funding para cada keyword (2008-2024)
social_share <- summary %>%
  filter(Year != "Total", Year >= 2008, Year <= 2024, Group %in% c("Total", "Social")) %>%
  select(Keyword, Year, Group, Funding_USD) %>%
  pivot_wider(names_from = Group, values_from = Funding_USD) %>%
  mutate(perc_social = 100 * Social / Total) %>%
  group_by(Keyword) %>%
  summarise(avg_perc_social = mean(perc_social, na.rm = TRUE))

print(social_share)





