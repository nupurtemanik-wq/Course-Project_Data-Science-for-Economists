# --------------------------------------------------
# Germany Trade Dependency Analysis
# Author: Nupur Temani K 
# --------------------------------------------------

# Install packages if missing
if(!require(readxl)) install.packages("readxl", repos="https://cloud.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos="https://cloud.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos="https://cloud.r-project.org")
if(!require(readr)) install.packages("readr", repos="https://cloud.r-project.org")

library(readxl)
library(dplyr)
library(ggplot2)
library(readr)

# --------------------------------------------------
# 1 Load Trade Data
# --------------------------------------------------

trade <- read_excel("data/TradeData.xlsx")

print(trade)
print("Data Loaded Successfully")

# --------------------------------------------------
# 2 Filter Germany Import Data
# --------------------------------------------------

trade_clean <- trade %>%
  filter(reporterDesc == "Germany",
         flowDesc == "Import") %>%
  select(
    partner = partnerDesc,
    product_code = cmdCode,
    product = cmdDesc,
    value = primaryValue
  ) %>%
  filter(!is.na(value))

# --------------------------------------------------
# 3 Calculate China Import Share
# --------------------------------------------------

china_share <- trade_clean %>%
  group_by(product) %>%
  summarise(
    china_imports = sum(value[partner == "China"], na.rm = TRUE),
    total_imports = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(china_ratio = china_imports / total_imports)

# --------------------------------------------------
# 4 Supplier Market Shares by Product
# --------------------------------------------------

shares <- trade_clean %>%
  group_by(product, partner) %>%
  summarise(imports = sum(value), .groups = "drop") %>%
  group_by(product) %>%
  mutate(share = imports / sum(imports))


# --------------------------------------------------
# 5 Calculate Supplier Concentration (HHI)
# --------------------------------------------------

hhi <- shares %>%
  mutate(hhi_component = share^2) %>%
  group_by(product) %>%
  summarise(HHI = sum(hhi_component), .groups = "drop")

# --------------------------------------------------
# 6 Combine China Import Share with HHI
# --------------------------------------------------

results <- china_share %>%
  left_join(hhi, by = "product") %>%
  mutate(vulnerability_score = china_ratio * HHI)

# --------------------------------------------------
# 7 Identify the Most Vulnerable Import Sectors
# --------------------------------------------------

vulnerable <- results %>%
  arrange(desc(vulnerability_score)) %>%
  slice_head(n = 10)

print("Top Vulnerable German Import Sectors")
print(vulnerable)

# --------------------------------------------------
# 8 Visualize Import Dependency
# Scatter Plot: China Import Share vs Supplier Concentration
# --------------------------------------------------

plot1 <- ggplot(results, aes(x = HHI, y = china_ratio)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "Germany Import Dependence on China",
    x = "Supplier Concentration (HHI)",
    y = "China Import Share"
  ) +
  theme_minimal()


# --------------------------------------------------
# 9 Export Visualization
# Save Scatter Plot as PDF
# --------------------------------------------------

ggsave(
  filename = "germany_china_dependency_plot.pdf",
  plot = plot1,
  width = 8,
  height = 6
)

# --------------------------------------------------
# 10 Export Analytical Results
# Save Tables for Further Analysis
# --------------------------------------------------

library(writexl)

write_xlsx(
  list(
    Dependency_Results = results,
    Top_Vulnerable_Sectors = vulnerable
  ),
  "germany_trade_analysis_tables.xlsx"
)

print("Analysis Completed")

