# ============================================================================
# NbS STOCKTAKE — SYSTEMATIC MAP FIGURES (R/ggplot2)
# ============================================================================
# Reads: NbS_peer_reviewed_harmonized.csv + NbS_peer_reviewed_benchmarked.csv
# Produces: 11 publication-ready figures
# ============================================================================
# CHANGELOG (v2 — Spatial Method Typology update):
#   - clean_method() updated from 9 → 7 categories:
#       REMOVED: "Spatial Statistics", "Bayesian DSS", "Land Change Models"
#       RENAMED: "Spatial optimization" → "Spatial Optimisation"
#       KEPT:    MCDA / MCE, ML Prediction, Process-based Models,
#                Rule-based GIS, Spatial Optimisation, Scenario & Land-Change,
#                Participatory DSS
#   - Method color palette added for consistent coloring across figures
#   - NA handling added to fig1/fig4 to exclude "NA", "Unclear", empty values
# ============================================================================

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(forcats)
library(glue)
library(readr)
library(patchwork)  # install.packages("patchwork") if needed

# ============================================================================
# LOAD DATA
# ============================================================================

harmonized <- read_csv("C:/Users/JNamita/OneDrive - CGIAR/Alliance - ClimateActionNetZero - Documents/ClimateActionNetZero/1_Projects/D591_Rural-Scan_NBS/2_Technical_&_Data/Stocktake Review/data/NbS_peer_reviewed_harmonized.csv")

benchmarked <- read_csv("C:/Users/JNamita/OneDrive - CGIAR/Alliance - ClimateActionNetZero - Documents/ClimateActionNetZero/1_Projects/D591_Rural-Scan_NBS/2_Technical_&_Data/Stocktake Review/data/NbS_peer_reviewed_benchmarked.csv")

# Short NbS labels for plotting
nbs_short <- c(
  "Water harvesting and conservation" = "Water harvesting",
  "Forest conservation and restoration" = "Forest conservation",
  "Agroforestry" = "Agroforestry",
  "Wetland management" = "Wetland management",
  "Reforestation and afforestation" = "Reforestation",
  "Riparian buffers" = "Riparian buffers",
  "Constructed Wetlands" = "Constructed Wetlands",
  "Integrated fire management" = "Fire management",
  "Agrosilvopastoralism" = "Agrosilvopastoralism",
  "Community based forest management" = "CBFM"
)

harmonized <- harmonized %>%
  mutate(NbS_Short = recode(NbS_Practice, !!!nbs_short))

benchmarked <- benchmarked %>%
  mutate(NbS_Short = recode(NbS_Practice, !!!nbs_short))

# Cluster colors
cluster_cols <- c("Forest & Tree Systems" = "#2d6a4f",
                  "Water & Wetland Systems" = "#1976a8",
                  "Landscape Management" = "#c26a2a")

# Benchmark colors
bench_cols <- c("High" = "#2d6a4f", "Medium" = "#c9a227",
                "Low" = "#993333", "Unscored" = "#bbbbbb")

# ── Method display labels + colors (7-category typology) ──────────────────
method_display <- c(
  "MCDA / MCE"        = "MCDA / MCE",
  "Rule-based GIS"    = "Rule-based GIS",
  "ML Prediction"     = "ML & Statistical Prediction",
  "Process-based Models" = "Process-based Models",
  "Spatial Optimisation" = "Spatial Optimisation",
  "Scenario & Land-Change" = "Scenario & Land-Change",
  "Participatory DSS" = "Participatory / Expert DSS"
)

method_cols <- c(
  "MCDA / MCE"           = "#2d6a4f",
  "Rule-based GIS"       = "#74a08a",
  "ML Prediction"        = "#1976a8",
  "Process-based Models" = "#5ba3cf",
  "Spatial Optimisation"  = "#c26a2a",
  "Scenario & Land-Change" = "#e8a838",
  "Participatory DSS"    = "#8b5e83"
)

# Common theme
theme_stocktake <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey50", size = 9),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

# ============================================================================
# HELPER: Explode semicolon-separated columns into long format
# ============================================================================

explode_col <- function(df, col) {
  df %>%
    separate_rows(!!sym(col), sep = ";") %>%
    mutate(!!sym(col) := str_trim(!!sym(col))) %>%
    distinct(File, NbS_Short, NbS_Cluster, !!sym(col))  # dedup per paper
}

# ============================================================================
# HELPER: Consolidate harmonized method names (7-CATEGORY TYPOLOGY)
# ============================================================================
# Maps the full harmonized category names to shorter display labels.
# Old 9-category items (Spatial Statistics, Bayesian DSS, Land Change Models)
# are no longer expected in the data. If encountered they map to the closest
# new category or to NA.
# ============================================================================

clean_method <- function(x) {
  case_when(
    str_starts(x, "MCDA")              ~ "MCDA / MCE",
    str_starts(x, "Machine learning")  ~ "ML Prediction",
    str_starts(x, "Process.based")     ~ "Process-based Models",
    str_starts(x, "Rule.based")        ~ "Rule-based GIS",
    str_starts(x, "Spatial optim")     ~ "Spatial Optimisation",
    str_starts(x, "Scenario")          ~ "Scenario & Land-Change",
    str_starts(x, "Participatory")     ~ "Participatory DSS",
    # Legacy categories (should no longer appear — safety net)
    str_starts(x, "Spatial statistics") ~ NA_character_,
    str_starts(x, "Bayesian")          ~ NA_character_,
    str_starts(x, "Land change")       ~ NA_character_,
    x %in% c("NA", "Unclear", "")      ~ NA_character_,
    is.na(x)                           ~ NA_character_,
    TRUE                               ~ x
  )
}

clean_input <- function(x) {
  case_when(
    x == "Elevation" ~ "Elevation and terrain",
    x == "Geology" ~ "Soil properties and geology",
    TRUE ~ x
  )
}

# ============================================================================
# FIGURE 1: NbS × Spatial Method Heatmap
# ============================================================================

fig1_data <- explode_col(harmonized, "Spatial_Method_Harmonized") %>%
  mutate(Method = clean_method(Spatial_Method_Harmonized)) %>%
  filter(!is.na(Method),
         Method != "Participatory DSS") %>%   # REMOVE participatory DSS
  count(NbS_Short, Method)

# Recalculate ordering after removal
nbs_order <- harmonized %>%
  count(NbS_Short) %>%
  arrange(desc(n)) %>%
  pull(NbS_Short)

method_order <- fig1_data %>%
  count(Method, wt = n) %>%
  arrange(desc(n)) %>%
  pull(Method)

fig1 <- ggplot(fig1_data, aes(x = factor(Method, levels = method_order),
                              y = factor(NbS_Short, levels = rev(nbs_order)),
                              fill = n)) +
  geom_tile(color = "white", linewidth = 0.6) +
  
  # Dynamic text color for contrast
  geom_text(aes(label = n,
                color = n > max(n) * 0.6),   # threshold for dark tiles
            size = 8, fontface = "bold") +
  
  scale_color_manual(values = c("grey15", "white"), guide = "none") +
  
  scale_fill_distiller(palette = "YlGnBu",
                       direction = 1,
                       name = " Paper Count") +
  
  labs(subtitle = glue("n = {nrow(harmonized)} papers  |  6-category spatial method typology"),
       x = NULL, y = NULL) +
  
  theme_stocktake +
  theme(
    plot.subtitle = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15)
  )

# Prevent clipping
fig1 <- fig1 +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(t = 15, r = 25, b = 20, l = 60)
  )

fig1 <- fig1 +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 16)) +
  theme(
    axis.text.x = element_text(size = 18),
    plot.margin = margin(t = 15, r = 25, b = 30, l = 60)
  )

ggsave(
  filename = "C:/Users/JNamita/OneDrive - CGIAR/Alliance - ClimateActionNetZero - Documents/ClimateActionNetZero/1_Projects/D591_Rural-Scan_NBS/2_Technical_&_Data/Stocktake Review/figures/fig01_nbs_x_method_heatmap_adjusted.png",
  plot = fig1,
  width = 20,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)

# ============================================================================
# FIGURE 2: NbS × Input Variable Category Heatmap
# ============================================================================

# ---- data ----
fig2_data <- explode_col(harmonized, "Input_Variables_Harmonized") %>%
  mutate(Input = clean_input(Input_Variables_Harmonized)) %>%
  filter(Input != "NA", !is.na(Input)) %>%
  count(NbS_Short, Input)

input_order <- fig2_data %>%
  count(Input, wt = n) %>%
  arrange(desc(n)) %>%
  pull(Input)

# choose a threshold for switching label colour (high counts = darker tiles)
# Using median is a solid default; you can change to quantile(., 0.6) if needed
label_thresh <- median(fig2_data$n, na.rm = TRUE)

# ---- compute proper contrast threshold ----
max_n <- max(fig2_data$n, na.rm = TRUE)

fig2_data <- fig2_data %>%
  mutate(
    text_col = ifelse(n >= 0.7 * max_n, "white", "black")  # only darkest tiles white
  )

# ---- plot ----
fig2 <- ggplot(
  fig2_data,
  aes(
    x = factor(Input, levels = input_order),
    y = factor(NbS_Short, levels = rev(nbs_order)),
    fill = n
  )
) +
  geom_tile(color = "white", linewidth = 0.7) +
  
  geom_text(
    aes(label = n, colour = text_col),
    size = 8,
    fontface = "bold"
  ) +
  scale_colour_identity() +
  
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    name = "Paper count"
  ) +
  
  scale_x_discrete(
    labels = function(x) str_wrap(x, width = 14)
  ) +
  
  labs(
    subtitle = "Data dependency patterns across NbS types",
    x = NULL,
    y = NULL
  ) +
  
  theme_stocktake +
  theme(
    plot.subtitle = element_text(size = 18, face = "bold"),
    
    axis.text.x = element_text(size = 13, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.key.height = unit(0.6, "cm"),
    legend.key.width  = unit(2.5, "cm"),
    legend.position = "bottom",
    
    plot.margin = margin(t = 15, r = 20, b = 20, l = 60)
  )

ggsave(
  filename = "C:/Users/JNamita/OneDrive - CGIAR/Alliance - ClimateActionNetZero - Documents/ClimateActionNetZero/1_Projects/D591_Rural-Scan_NBS/2_Technical_&_Data/Stocktake Review/figures/fig02_nbs_x_input_heatmap_adjusted.png",
  plot = fig2,
  width = 20,     # wider (many input categories)
  height = 9,     # slightly taller for wrapped labels
  units = "in",
  dpi = 300,
  bg = "white"
)

# ============================================================================
# FIGURE 3: Benchmark Flag by NbS (stacked bar)
# ============================================================================

fig3_data <- benchmarked %>%
  mutate(Final_Benchmark = replace_na(Final_Benchmark, "Unscored")) %>%
  count(NbS_Short, Final_Benchmark) %>%
  mutate(Final_Benchmark = factor(Final_Benchmark, levels = c("High", "Medium", "Low", "Unscored")))

fig3 <- ggplot(fig3_data, aes(x = fct_reorder(NbS_Short, n, .fun = sum),
                              y = n, fill = Final_Benchmark)) +
  geom_col(width = 0.75) +
  coord_flip() +
  scale_fill_manual(values = bench_cols, name = "Benchmark") +
  labs(title = "Fig 3. Benchmark Flag by NbS Practice",
       subtitle = "Scalability × Evidence Quality composite classification",
       x = NULL, y = "Number of papers") +
  theme_stocktake

# ============================================================================
# FIGURE 4: Method Frequency (horizontal bar with method colors)
# ============================================================================

fig4_data <- explode_col(harmonized, "Spatial_Method_Harmonized") %>%
  mutate(Method = clean_method(Spatial_Method_Harmonized)) %>%
  filter(!is.na(Method)) %>%
  count(Method) %>%
  arrange(desc(n)) %>%
  mutate(pct = round(100 * n / sum(n), 0),
         label = glue("{Method}\n{n} ({pct}%)"))

fig4 <- ggplot(fig4_data, aes(x = fct_reorder(Method, n), y = n, fill = Method)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  geom_text(aes(label = glue("{n} ({pct}%)")), hjust = -0.1, size = 3) +
  coord_flip() +
  scale_fill_manual(values = method_cols) +
  labs(title = "Fig 4. Spatial Method Frequency",
       subtitle = "7-category typology  |  papers can use multiple methods",
       x = NULL, y = "Number of papers") +
  theme_stocktake +
  theme(legend.position = "none") +
  expand_limits(y = max(fig4_data$n) * 1.15)

# ============================================================================
# FIGURE 5: Scalability Sub-score Distribution (scored papers only)
# ============================================================================

fig5_data <- benchmarked %>%
  filter(!is.na(Scalability_Score), Scalability_Score != "NA") %>%
  select(File, A = Scalability_A_Geographic, B = Scalability_B_Method, C = Scalability_C_Data) %>%
  pivot_longer(cols = c(A, B, C), names_to = "Dimension", values_to = "Score") %>%
  mutate(Score = as.integer(Score)) %>%
  filter(!is.na(Score)) %>%
  mutate(Dimension = recode(Dimension,
                            "A" = "A: Geographic Scope",
                            "B" = "B: Method Transferability",
                            "C" = "C: Data Generalizability"
  ))

fig5 <- ggplot(fig5_data, aes(x = factor(Score), fill = Dimension)) +
  geom_bar(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("A: Geographic Scope" = "#2d6a4f",
                               "B: Method Transferability" = "#1976a8",
                               "C: Data Generalizability" = "#c26a2a")) +
  labs(title = "Fig 5. Scalability Sub-score Distribution",
       subtitle = glue("n = {n_distinct(fig5_data$File)} scored papers"),
       x = "Score (1 = Low, 3 = High)", y = "Number of papers") +
  theme_stocktake

# ============================================================================
# FIGURE 6: Geographic Coverage (bar chart by continent + cluster)
# ============================================================================

fig6_data <- harmonized %>%
  filter(!is.na(Geographic_Scope_Harmonized), Geographic_Scope_Harmonized != "NA") %>%
  count(Geographic_Scope_Harmonized, NbS_Cluster)

fig6 <- ggplot(fig6_data, aes(x = fct_reorder(Geographic_Scope_Harmonized, n, .fun = sum),
                              y = n, fill = NbS_Cluster)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = cluster_cols, name = "NbS Cluster") +
  labs(title = "Fig 6. Geographic Coverage of Studies",
       subtitle = "Number of papers by continent, colored by NbS cluster",
       x = NULL, y = "Number of papers") +
  theme_stocktake

# ============================================================================
# FIGURE 7: Analytical Gap Matrix (% addressing each dimension)
# ============================================================================

fig7_data <- benchmarked %>%
  group_by(NbS_Short) %>%
  summarise(
    `Climate Benefit` = round(100 * mean(str_starts(Climate_Benefit, "Yes"), na.rm = TRUE)),
    `Climate Barrier` = round(100 * mean(str_starts(Climate_Barrier, "Yes"), na.rm = TRUE)),
    Economics = round(100 * mean(str_starts(Economics, "Yes"), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  pivot_longer(-NbS_Short, names_to = "Dimension", values_to = "Pct")

fig7 <- ggplot(fig7_data, aes(x = Dimension,
                              y = factor(NbS_Short, levels = rev(nbs_order)),
                              fill = Pct)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(Pct, "%")), size = 3.2, fontface = "bold") +
  scale_fill_gradient2(low = "#d73027", mid = "#fee08b", high = "#1a9850",
                       midpoint = 50, name = "% Yes") +
  labs(title = "Fig 7. Analytical Gap Matrix",
       subtitle = "% of papers addressing climate and economic dimensions",
       x = NULL, y = NULL) +
  theme_stocktake +
  theme(axis.text.x = element_text(size = 9))

# ============================================================================
# FIGURE 8: Scalability × Quality Cross-table
# ============================================================================

fig8_data <- benchmarked %>%
  filter(!is.na(Scalability_Score), Scalability_Score != "NA",
         !is.na(Quality_Score), Quality_Score != "NA") %>%
  mutate(
    Scalability_Score = as.integer(Scalability_Score),
    Quality_Score     = as.integer(Quality_Score)
  ) %>%
  count(Scalability_Score, Quality_Score) %>%
  mutate(
    # ✅ Benchmark logic (more intuitive “maturity” framing)
    # High only when BOTH are high (3×3)
    Benchmark = case_when(
      Scalability_Score == 3 & Quality_Score == 3 ~ "High",
      Scalability_Score == 1 | Quality_Score == 1 ~ "Low",
      TRUE ~ "Medium"
    ),
    # ✅ text colour per benchmark (matches tile meaning)
    TextCol = case_when(
      Benchmark == "High"   ~ "#2d6a4f",
      Benchmark == "Medium" ~ "#c9a227",
      TRUE                  ~ "#993333"
    )
  )

fig8 <- ggplot(
  fig8_data,
  aes(
    x    = factor(Quality_Score),
    y    = factor(Scalability_Score),
    fill = Benchmark
  )
) +
  geom_tile(color = "white", linewidth = 1.5, alpha = 0.3) +
  geom_text(aes(label = n), size = 8, fontface = "bold", color = fig8_data$TextCol) +
  geom_text(aes(label = Benchmark), vjust = 2.5, size = 6, color = fig8_data$TextCol) +
  scale_fill_manual(values = bench_cols, guide = "none") +
  labs(
    subtitle = glue("n = {sum(fig8_data$n)} scored papers"),
    x        = "Evidence Quality Score",
    y        = "Scalability Score"
  ) +
  theme_stocktake +
  theme(
    panel.grid = element_blank(),
    
    # Make titles larger
    plot.title = element_text(size = 22, face = "bold"),
    plot.subtitle = element_text(size = 16),
    
    # Axis titles
    axis.title.x = element_text(size = 18, margin = margin(t = 10)),
    axis.title.y = element_text(size = 18, margin = margin(r = 10)),
    
    # Axis tick labels (1,2,3)
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  )

ggsave(
  filename = "C:/Users/JNamita/OneDrive - CGIAR/Alliance - ClimateActionNetZero - Documents/ClimateActionNetZero/1_Projects/D591_Rural-Scan_NBS/2_Technical_&_Data/Stocktake Review/figures/fig08_scalability_quality_cross_adjusted.png",
  plot = fig8,
  width = 10,
  height = 9,
  units = "in",
  dpi = 300,
  bg = "white"
)

# ============================================================================
# FIGURE 9: Validation Methods (horizontal bar)
# ============================================================================

fig9_data <- explode_col(harmonized, "Method_Validation_Harmonized") %>%
  mutate(Validation = str_trim(Method_Validation_Harmonized)) %>%
  filter(!str_detect(Validation, "^\\d+$"), Validation != "NA", nchar(Validation) > 2) %>%
  count(Validation) %>%
  arrange(desc(n))

fig9 <- ggplot(fig9_data, aes(x = fct_reorder(Validation, n), y = n)) +
  geom_col(fill = "#1976a8", alpha = 0.75, width = 0.7) +
  geom_text(aes(label = n), hjust = -0.2, size = 3.2) +
  coord_flip() +
  labs(title = "Fig 9. Validation Methods Used",
       subtitle = "Frequency across the corpus (papers can use multiple methods)",
       x = NULL, y = "Number of papers") +
  theme_stocktake +
  expand_limits(y = max(fig9_data$n) * 1.1)

# ============================================================================
# FIGURE 10: Analysis Resolution Distribution (donut or bar)
# ============================================================================

fig10_data <- harmonized %>%
  count(Analysis_Resolution_Harmonized) %>%
  mutate(Resolution = ifelse(is.na(Analysis_Resolution_Harmonized) |
                               Analysis_Resolution_Harmonized == "NA",
                             "Not reported", Analysis_Resolution_Harmonized)) %>%
  group_by(Resolution) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  arrange(desc(n)) %>%
  mutate(pct = round(100 * n / sum(n)),
         label = glue("{Resolution}\n({n}, {pct}%)"))

fig10 <- ggplot(fig10_data, aes(x = fct_reorder(Resolution, n), y = n)) +
  geom_col(fill = "#2d6a4f", alpha = 0.7, width = 0.7) +
  geom_text(aes(label = glue("{n} ({pct}%)")), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(title = "Fig 10. Analysis Resolution Distribution",
       subtitle = glue("n = {nrow(harmonized)} papers"),
       x = NULL, y = "Number of papers") +
  theme_stocktake +
  expand_limits(y = max(fig10_data$n) * 1.15)

# ============================================================================
# FIGURE 11: NbS × Output Type Heatmap
# ============================================================================

# Remove last 3 output types (adjust names if needed)
drop_outputs <- c(
  "Conservation network / selection solutions",
  "Conservation network / selection solutions: Spatial optimization outputs",
  "Scenarios comparisons"
)

fig11_data <- explode_col(harmonized, "Output_Harmonized") %>%
  filter(Output_Harmonized != "NA", !is.na(Output_Harmonized)) %>%
  filter(!Output_Harmonized %in% drop_outputs) %>%
  count(NbS_Short, Output_Harmonized, name = "n")

output_order <- fig11_data %>%
  count(Output_Harmonized, wt = n) %>%
  arrange(desc(n)) %>%
  pull(Output_Harmonized)

# Contrast threshold
max_n <- max(fig11_data$n, na.rm = TRUE)

fig11_data <- fig11_data %>%
  mutate(text_col = ifelse(n >= 0.7 * max_n, "white", "black"))

fig11 <- ggplot(
  fig11_data,
  aes(
    x = factor(Output_Harmonized, levels = output_order),
    y = factor(NbS_Short, levels = rev(nbs_order)),
    fill = n
  )
) +
  geom_tile(color = "white", linewidth = 0.7) +
  geom_text(
    aes(label = n, colour = text_col),
    size = 6,
    fontface = "bold"
  ) +
  scale_colour_identity() +
  scale_fill_distiller(
    palette = "BuPu",
    direction = 1,
    name = "Paper count"
  ) +
  scale_x_discrete(
    labels = function(x) str_wrap(x, width = 18)
  ) +
  labs(
    subtitle = "Types of geospatial outputs produced by NbS practice",
    x = NULL, y = NULL
  ) +
  theme_stocktake +
  theme(
    plot.subtitle = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 14),
    legend.key.height = unit(0.6, "cm"),
    legend.key.width  = unit(2.5, "cm"),
    legend.position = "bottom",
    plot.margin = margin(t = 15, r = 20, b = 20, l = 60)
  )

# Save directly (same style as previous figures)
ggsave(
  filename = "C:/Users/JNamita/OneDrive - CGIAR/Alliance - ClimateActionNetZero - Documents/ClimateActionNetZero/1_Projects/D591_Rural-Scan_NBS/2_Technical_&_Data/Stocktake Review/figures/fig11_nbs_x_output_heatmap_adjusted3.png",
  plot = fig11,
  width = 18,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)


# ============================================================================
# DISPLAY ALL FIGURES
# ============================================================================

fig1
fig2
fig3
fig4
fig5
fig6
fig7
fig8
fig9
fig10
fig11

# ============================================================================
# SAVE ALL FIGURES
# ============================================================================

OUT_DIR <- "C:/PDFs_extracted_text/extraction_results/figures"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

ggsave(file.path(OUT_DIR, "fig01_nbs_x_method_heatmap.png"), fig1, width = 12, height = 7, dpi = 300)
ggsave(file.path(OUT_DIR, "fig02_nbs_x_input_heatmap.png"), fig2, width = 13, height = 7, dpi = 300)
ggsave(file.path(OUT_DIR, "fig03_benchmark_by_nbs.png"), fig3, width = 10, height = 6, dpi = 300)
ggsave(file.path(OUT_DIR, "fig04_method_frequency.png"), fig4, width = 10, height = 5, dpi = 300)
ggsave(file.path(OUT_DIR, "fig05_scalability_subscores.png"), fig5, width = 8, height = 5, dpi = 300)
ggsave(file.path(OUT_DIR, "fig06_geographic_coverage.png"), fig6, width = 9, height = 5, dpi = 300)
ggsave(file.path(OUT_DIR, "fig07_gap_matrix.png"), fig7, width = 8, height = 6, dpi = 300)
ggsave(file.path(OUT_DIR, "fig08_scalability_quality_cross.png"), fig8, width = 7, height = 6, dpi = 300)
ggsave(file.path(OUT_DIR, "fig09_validation_methods.png"), fig9, width = 10, height = 5, dpi = 300)
ggsave(file.path(OUT_DIR, "fig10_resolution_distribution.png"), fig10, width = 10, height = 5, dpi = 300)
ggsave(file.path(OUT_DIR, "fig11_nbs_x_output_heatmap.png"), fig11, width = 12, height = 7, dpi = 300)

message(glue("\n✓ All 11 figures saved to: {OUT_DIR}"))
