#============================#
#       Initial Setup       #
#============================#

## Load custom packages and functions
safely_source <- function(file) {
  if (file.exists(file)) {
    source(file)
  } else {
    warning(glue::glue("File not found: {file}"))
  }
}

custom_paths <- here::here("03 Simulations", "Week4", "Custom_scripts")
safely_source(file.path(custom_paths, "custom_packages.R"))
safely_source(file.path(custom_paths, "custom_functions.R"))

## Set font and theme
if (!"chivo" %in% sysfonts::font_families()) {
  tryCatch(
    font_add_google("Chivo", "chivo"),
    error = function(e) message("Failed to load Chivo font from Google.")
  )
}
showtext_auto()
theme_set(theme_minimal(base_family = "chivo"))

## Define color palette
my_colours <- colorspace::qualitative_hcl(12, palette = "Dark 3")

## Set and create figure directory
fig_dir <- here::here("03 Simulations", "Week4", "Figures")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

## Set and create data directory
data_dir <- here::here("03 Simulations", "Week4", "Data")
if(!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

#============================#
#       Data Import         #
#============================#

message("Starting data import...")

safe_fread <- function(path) {
  tryCatch(
    fread(path),
    error = function(e) {
      warning(glue::glue("Failed to read: {path}"))
      NULL
    }
  )
}

safe_read_xlsx <- function(url) {
  tryCatch(
    openxlsx::read.xlsx(url, colNames = TRUE, skipEmptyRows = TRUE, detectDates = TRUE) |> as.data.table(),
    error = function(e) {
      warning(glue::glue("Failed to read Excel file from: {url}"))
      NULL
    }
  )
}

safe_read_csv <- function(url) {
  tryCatch(
    readr::read_csv(url) |> as.data.table(),
    error = function(e) {
      warning(glue::glue("Failed to read CSV from: {url}"))
      NULL
    }
  )
}

# Local file
esbg <- safe_fread(here::here("03 Simulations", "Week4", "es_data-v5_0.csv"))

# Web sources
cpds <- safe_read_xlsx("https://cpds-data.org/wp-content/uploads/2024/11/cpds-1960-2022-update-2024-2.xlsx")
lijphart <- safe_fread("https://andy.egge.rs/data/L.csv")
cabinets <- safe_read_csv("https://www.parlgov.org/data/parlgov-development_csv-utf-8/view_cabinet.csv")

# Confirm successful reads
datasets <- list(esbg = esbg, cpds = cpds, lijphart = lijphart, cabinets = cabinets)
invisible(lapply(names(datasets), function(name) {
  if (is.null(datasets[[name]])) {
    stop(glue::glue("Dataset `{name}` failed to load. Halting execution."))
  } else {
    message(glue::glue("Loaded `{name}` ({nrow(datasets[[name]])} rows)"))
  }
}))


#============================#
# Optional: Save Datasets    #
#============================#

save_imported_data <- TRUE       # Toggle to FALSE to skip saving
default_save_type  <- "rds"      # Options: "rds", "csv", "parquet", etc.

if (isTRUE(save_imported_data)) {
  message("Saving imported datasets locally...")
  
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
  
  # Named list of datasets and (optionally) custom file types
  saved_raw_data <- list(
    esbg     = list(data = esbg,     type = default_save_type),
    cpds     = list(data = cpds,     type = default_save_type),
    lijphart = list(data = lijphart, type = default_save_type),
    cabinets = list(data = cabinets, type = default_save_type)
  )
  
  # Save each dataset
  invisible(Map(function(name, item) {
    file_path <- file.path(data_dir, name)
    save_data_simple(item$data, file_name = file_path, file_type = item$type)
  }, names(saved_raw_data), saved_raw_data))
  
  message("All datasets saved.")
}


labels <- list(
  leg_type = c("Majoritarian", "Proportional", "Mixed"),
  
  elecrule = c(
    "SMDP", "TRS", "AV", "BC", "BV", "PBV", "LV", "SNTV",
    "List PR", "STV", "Mixed Dependent", "Mixed Independent"
  ),
  
  region1 = c(
    "Sub-Saharan Africa", "South Asia", "East Asia", "South East Asia",
    "Pacific Islands/Oceania", "Middle East/North Africa",
    "Latin America", "Caribbean and non-Iberic America",
    "Eastern Europe/post-Soviet states", "Industrialized Countries (OECD)",
    "Oil Countries"
  ),
  
  region2 = c(
    "Sub-Saharan Africa", "South Asia", "East Asia", "South East Asia",
    "Pacific Islands/Oceania", "Middle East/North Africa",
    "Latin America", "Caribbean and non-Iberic America",
    "Eastern Europe/post-Soviet states", "Western Europe"
  ),
  
  gini_lookup = c(
    postfisc_gini    = "Post-Fiscal Gini",
    prefisc_gini     = "Pre-Fiscal Gini",
    pretran_gini     = "Pre-Transfer Gini",
    perc_change_gini = "% Change in Gini",
    receipts         = "Gov. Revenue (% GDP)",
    outlays          = "Gov. Outlays (% GDP)",
    debt             = "Gov. Debt (% GDP)"
  ),
  
  dispro_lookup = c(
    dis_gall       = "Gallagher Index",
    dis_gall_lag1  = "Gallagher Index (Lag 1)",
    dis_gall_lag2  = "Gallagher Index (Lag 2)",
    dis_gall_lag3  = "Gallagher Index (Lag 3)",
    dis_gall_lag4  = "Gallagher Index (Lag 4)"
  )
)


#============================#
#     Data Preparation       #
#============================#

message("Preparing and transforming data...")

## Helper: Create factor safely
create_labeled_factor <- function(vec, labels) {
  factor(vec, levels = seq_along(labels), labels = labels)
}

## Add factor columns
esbg[, `:=`(
  region1_factor = create_labeled_factor(region1, labels$region1),
  region2_factor = create_labeled_factor(region2, labels$region2)
)]

## Expand to country-year panel
years_range <- esbg[, .(min_year = min(year), max_year = max(year)), by = country]
full_grid <- years_range[, .(year = min_year:max_year), by = country]
esbg_full <- merge(full_grid, esbg, by = c("country", "year"), all.x = TRUE)[order(country, year)]

## Forward fill numeric columns (excluding 'year')
cols_to_fill <- setdiff(names(esbg_full)[sapply(esbg_full, is.numeric)], "year")
esbg_full[, (cols_to_fill) := lapply(.SD, \(x) nafill(x, type = "locf")), by = country, .SDcols = cols_to_fill]
# Fill iso with forward fill, bearing in mind that it is not numeric 

## Re-create factor columns post-fill
esbg_full[, `:=`(
  legislative_type_factor = create_labeled_factor(legislative_type, labels$leg_type),
  elecrule_factor         = create_labeled_factor(elecrule, labels$elecrule),
  region1_factor          = create_labeled_factor(region1, labels$region1),
  region2_factor          = create_labeled_factor(region2, labels$region2)
)]

## Create iso3c column
esbg_full[, iso := countrycode::countrycode(country, "country.name", "iso3c")]
esbg_full <- esbg_full[!is.na(iso)]

#============================#
#     Dataset Subsets       #
#============================#

## Helper: Filter dataset safely and name clearly
subset_dataset <- function(data, democracy = TRUE, non_presidential = FALSE, name = "dataset") {
  filtered <- copy(data)
  
  if (democracy) {
    filtered <- filtered[bmr_democracy == 1]
  }
  
  if (non_presidential) {
    filtered <- filtered[presidential == 0]
  }
  
  if (nrow(filtered) == 0) {
    warning(glue::glue("Filtered `{name}` resulted in 0 rows."))
  } else {
    message(glue::glue("`{name}` subset created with {nrow(filtered)} rows."))
  }
  
  return(filtered)
}

## Create subset variants
esbg_dem            <- subset_dataset(esbg, name = "esbg_dem")
esbg_dem_full       <- subset_dataset(esbg_full, name = "esbg_dem_full")
esbg_dem_leg        <- subset_dataset(esbg, non_presidential = TRUE, name = "esbg_dem_leg")
esbg_dem_leg_full   <- subset_dataset(esbg_full, non_presidential = TRUE, name = "esbg_dem_leg_full")

## Create proper iso variable in cpds
cpds <- cpds[, iso := countrycode::countrycode(country, "country.name", "iso3c")]

## Create a variable for region 
cpds <- cpds[, region23 := countrycode::countrycode(country, "country.name", "region23")]

## Create a variable for decades
cpds[, decade := floor(year / 10) * 10]
cpds[, decade := paste0(decade, "s")]

## Merge esbg_dem_leg_full with cpds
cpds_esbg <- merge.data.table(
  cpds[, .(country, year, decade, iso, region23, poco, eu, emu, gov_right1, gov_cent1, gov_left1, gov_chan, gov_gap, prop, rae_ele, rae_leg, effpar_ele, effpar_leg, dis_gall, outlays, debt, inflation, receipts, realgdpgr, prefisc_gini, postfisc_gini, pretran_gini, sstran, pop65)],
  esbg_dem_leg_full[, .(country, year, iso, elec_id, date, presidential, legislative_type, legislative_type_factor, elecrule, elecrule_factor, enep, enpp, region1, region2, region1_factor, region2_factor, bmr_democracy)],
  by = c("iso", "year"),
  all.x = TRUE
)

## Clean up outlays variable 
cpds_esbg[, outlays := as.numeric(outlays)]

#============================#
#   Plotting Configuration   #
#============================#

## Centralise size control for accessibility/scaling
theme_sizes <- list(
  title     = 48,
  axis_title = 45,
  axis_text  = 38,
  legend_title = 30,
  legend_text  = 25,
  strip_text   = 35
)

## Define reusable base plot theme
plot_theme_base <- function(font_family = "chivo") {
  theme_minimal(base_family = font_family) +
    theme(
      plot.title      = element_text(hjust = 0.5, size = theme_sizes$title, face = "bold"),
      axis.title      = element_text(size = theme_sizes$axis_title),
      # axis.title.x    = element_blank(),
      axis.text       = element_text(size = theme_sizes$axis_text),
      axis.text.x     = element_text(angle = 0, vjust = 0.5),
      legend.title    = element_text(size = theme_sizes$legend_title),
      legend.text     = element_text(size = theme_sizes$legend_text),
      legend.position = "bottom",
      legend.key.height = unit(1, "cm"),
      panel.grid.major.y = element_line(color = "grey95", linewidth = 0.6),
      panel.grid.minor   = element_blank()
    )
}

## Set as global theme
theme_set(plot_theme_base())


#============================#
#   Plot 1: leg_share        #
#============================#

message("Creating plot: Electoral Systems in Most Recent Legislative Elections")

## Create dataset: most recent legislative election per country
esbg_bmr_dem_leg_recent <- esbg_dem_leg[, .SD[which.max(year)], by = country]

## Count combinations
leg_share <- esbg_bmr_dem_leg_recent[, .N, by = .(legislative_type, elecrule)]
setnames(leg_share, "N", "count")

## Label factors (preserve original label mappings)
leg_share[, legislative_type := create_labeled_factor(legislative_type, labels$leg_type)]
leg_share[, elecrule := create_labeled_factor(elecrule, labels$elecrule)]

## Define color palette for electoral rules
elec_colours <- qualitative_hcl(length(unique(leg_share$elecrule)), palette = "Dark 3")

## Create barplot
leg_share_plot <- ggplot(leg_share, aes(x = factor(legislative_type), y = count / sum(count),   # Proportional height
                                        fill = elecrule)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  scale_x_discrete(labels = labels$leg_type) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 0.5)   # Retain your tuned upper bound
  ) +
  scale_fill_manual(values = elec_colours) +
  labs(
    y = "Share",
    fill = "Electoral rule",
    title = "Electoral Systems in Most Recent Legislative Elections"
  ) +
  plot_theme_base() +  # Apply global base theme
  theme(
    legend.position = "right",
    axis.title.x = element_blank(),  
    legend.title = element_blank()  
  )

## Save the plot
save_plot(
  plot = leg_share_plot,
  path = file.path(fig_dir, "leg_share.png")
)

#============================#
# Plot 2: ENEP (Reversed Color)
#============================#

message("Creating plot: ENEP with reversed year color")

## Prepare data
esbg_enep_clean <- copy(esbg_dem_leg)
esbg_enep_clean[, enep := ifelse(enep < 0, NA, enep)]
esbg_enep_clean[, year_rev := max(year, na.rm = TRUE) - year]

## Plot with reversed year coloring
enep_plot <- ggplot(esbg_enep_clean, aes(x = factor(legislative_type), y = enep, color = year_rev)) +
  geom_boxplot(outlier.size = 0.5, outlier.colour = "grey") +
  geom_jitter(size = 1, alpha = 0.5, width = 0.42) +
  scale_x_discrete(labels = labels$leg_type) +
  scale_y_continuous(
    labels = scales::comma_format(accuracy = 1),
    limits = c(0, 15)
  ) +
  scale_color_gradient(
    low = "red", high = "blue",  # early = red, late = blue
    name = "Year",
    breaks = scales::pretty_breaks(5),
    labels = function(x) max(esbg_enep_clean$year, na.rm = TRUE) - x
  ) +
  labs(
    y = "ENEP",
    title = "Effective Number of Electoral Parties by Electoral System"
  ) +
  guides(color = guide_colorbar(barheight = unit(12, "lines"))) +
  plot_theme_base() +
  theme(legend.position = "right", 
        axis.title.x = element_blank())

## Save plot
save_plot(
  plot = enep_plot,
  path = file.path(fig_dir, "enep_leg.png")
)


#============================#
#   Plot 3: ENEP / ENPP Ratio
#============================#

message("Creating plot: Ratio of ENEP to ENPP by Legislative Type")

## Prepare dataset
dame <- esbg_dem_leg[, .(country, year, legislative_type, enep, enpp)]
dame[, enep := ifelse(enep < 0, NA, enep)]
dame[, enpp := ifelse(enpp < 0, NA, enpp)]
dame[, diff := enep / enpp]
# dame[, diff := ifelse(diff < 1, NA, diff)]

## Reverse year for color mapping
dame[, year_rev := max(year, na.rm = TRUE) - year]

## Create plot
dame_plot <- ggplot(dame, aes(x = factor(legislative_type),  y = diff,  color = year_rev)) +
  geom_boxplot(outlier.size = 0.5, outlier.colour = "grey") +
  geom_jitter(size = 1, alpha = 0.5, width = 0.42) +
  scale_x_discrete(labels = labels$leg_type) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  scale_color_gradient(
    low = "red", high = "blue",
    name = "Year",
    breaks = scales::pretty_breaks(5),
    labels = function(x) max(dame$year, na.rm = TRUE) - x
  ) +
  expand_limits(y = 0) +
  labs(
    y = "Ratio between ENEP and ENPP",
    title = "Difference in Effective # of Electoral and Legislative Parties by Electoral System"
  ) +
  guides(color = guide_colorbar(barheight = unit(12, "lines"))) +
  plot_theme_base() +
  theme(legend.position = "right", 
        axis.title.x = element_blank())

## Save plot
save_plot(
  plot = dame_plot,
  path = file.path(fig_dir, "diff_enep_enpp_gb.png")
)


#============================#
#   Plot 4: CPDS ENEP Ratio  #
#============================#

message("Creating plot: CPDS ENEP / ENPP Ratio by Electoral System")

## Prepare dataset
cpds_elec <- cpds[, .(country, year, prop, rae_ele, rae_leg, effpar_ele, effpar_leg, dis_gall, dis_rel, dis_abso)]
cpds_elec[, effpar_ratio := effpar_ele / effpar_leg]
cpds_elec[, prop := factor(prop, levels = c("0", "1", "2"), labels = c("SMSP", "Modified PR", "PR"))]

## Add reversed year color variable
cpds_elec[, year_rev := max(year, na.rm = TRUE) - year]

## Plot
cpds_ratio_plot <- cpds_elec %>%
  filter(!is.na(prop)) %>%
  ggplot(aes(x = prop, y = effpar_ratio, color = year_rev)) +
  geom_boxplot(outlier.size = 0.5, outlier.colour = "grey") +
  geom_jitter(size = 1, alpha = 0.5, width = 0.42) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  scale_color_gradient(
    low = "red", high = "blue",
    name = "Year",
    breaks = scales::pretty_breaks(5),
    labels = function(x) max(cpds_elec$year, na.rm = TRUE) - x
  ) +
  labs(
    y = "Ratio between ENEP and ENPP",
    title = "Difference in Effective # of Electoral and Legislative Parties by Electoral System"
  ) +
  guides(color = guide_colorbar(barheight = unit(12, "lines"))) +
  plot_theme_base() +
  theme(legend.position = "right", 
        axis.title.x = element_blank())

## Save the plot
save_plot(
  plot = cpds_ratio_plot,
  path = file.path(fig_dir, "diff_enep_enpp_cpds.png")
)


#============================#
# Plot 5: dis_gall Over Time
#============================#

message("Creating plot: Gallagher Index by Region (1960–2020)")

## Prepare data
cpds_disg <- copy(cpds)
cpds_disg[, region := countrycode(country, "country.name", "region23")]

## Aggregate
disg_summary <- cpds_disg[, .(dis_gall = mean(dis_gall, na.rm = TRUE)), by = .(region, year)]

## Plot
disg_plot <- ggplot(disg_summary, aes(x = year, y = dis_gall, color = region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(1945, 2020, by = 5)) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  expand_limits(y = c(0, 30)) + 
  labs(
    y = "Gallagher Index",
    title = "Gallagher Index by Region, 1960 - 2020"
  ) +
  plot_theme_base() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank())

## Save plot
save_plot(
  plot = disg_plot,
  path = file.path(fig_dir, "dis_gall_by_region.png")
)


#============================#
# Plot 6: Lijphart ENEP vs. MWC
#============================#

message("Creating plot: Minimal Winning Cabinets vs. Effective Number of Parties")

## Prepare data
lijphart_clean <- copy(lijphart)

## Plot
lijphart_enep_plot <- ggplot(
  lijphart_clean,
  aes(x = eff_num_parl_parties_1945_2010, y = pct_minimal_winning_one_party_cabinet_1945_2010)) +
  geom_point(size = 3) +
  ggrepel::geom_text_repel(aes(label = country), size = 10, max.overlaps = 20) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
  expand_limits(x = 1) +
  labs(
    x = "Effective Number of Parties (Parliament)",
    y = "Percentage of Minimal Winning One Party Cabinets",
    title = "Minimal Winning Cabinets vs. Effective # of Parties"
  ) +
  plot_theme_base()

## Save the plot
save_plot(
  plot = lijphart_enep_plot,
  path = file.path(fig_dir, "lijphart_enlep_mwc.png")
)


#============================#
# Plot 7: Disproportionality vs ENEP
#============================#

message("Creating plot: Effective # of Parties vs. Electoral Disproportionality")

## Prepare data
lijphart_clean <- copy(lijphart)

## Plot
lijphart_disp_plot <- ggplot(
  lijphart_clean, aes(x = index_of_disproportionality_1945_2010, y = eff_num_parl_parties_1945_2010)) +
  geom_point(size = 3) +
  ggrepel::geom_text_repel(aes(label = country), size = 10, max.overlaps = 20) +
  geom_smooth(method = "lm", se = FALSE) +
  expand_limits(x = 0, y = 0) +
  labs(
    x = "Electoral Disproportionality",
    y = "Effective Number of Parties (Parliament)",
    title = "Effective # of Parties vs. Electoral Disproportionality"
  ) +
  plot_theme_base()

## Save the plot
save_plot(
  plot = lijphart_disp_plot,
  path = file.path(fig_dir, "lijphart_disproportionality_enlp.png")
)


#============================#
# Preparing Data for Gini & Fiscal Plots
#============================#

message("Preparing data for Gini and fiscal outcome plots...")

# 1. Redistributive effect variable
cpds[, perc_change_gini := (postfisc_gini - prefisc_gini) / prefisc_gini]

# 2. Lag Gallagher Index
cpds <- cpds[, `:=`(
  dis_gall_lag1 = shift(dis_gall, 1),
  dis_gall_lag2 = shift(dis_gall, 2),
  dis_gall_lag3 = shift(dis_gall, 3),
  dis_gall_lag4 = shift(dis_gall, 4)
), by = country]

# 3. Outcome + lag variables
cpds_outcomes <- c("postfisc_gini", "prefisc_gini", "pretran_gini", 
                   "perc_change_gini", "receipts", "outlays", "debt")
lag_vars <- c("dis_gall_lag1", "dis_gall_lag2", "dis_gall_lag3", "dis_gall_lag4")

# 4. Subset relevant columns
cols_interest <- c("country", "year", "dis_gall", "prop", lag_vars, cpds_outcomes)
cpds_outcomes_df <- cpds[, ..cols_interest]

# 5. Region variable
cpds_outcomes_df[, region := countrycode(country, "country.name", "region23")]

# 6. Label electoral system
cpds_outcomes_df[, prop_labelled := factor(prop, levels = 0:2, 
                                           labels = c("SMSP", "Modified PR", "PR"))]

# 7. Create decade variable
min_year <- floor(min(cpds_outcomes_df$year, na.rm = TRUE) / 10) * 10
max_year <- ceiling(max(cpds_outcomes_df$year, na.rm = TRUE) / 10) * 10

cpds_outcomes_df[, decade := cut(
  year,
  breaks = seq(min_year, max_year + 10, by = 10),
  labels = paste0(seq(min_year, max_year, by = 10), "s"),
  right = FALSE
)]

#============================#
# Plot 8: Gini Lollipop by Decade
#============================#

message("Creating fixed lollipop plot with all decade labels shown")

# Prepare data
gini_summary <- cpds_outcomes_df[, .(
  prefisc_gini = mean(prefisc_gini, na.rm = TRUE),
  postfisc_gini = mean(postfisc_gini, na.rm = TRUE)
), by = .(decade, prop_labelled)]

gini_long <- melt(gini_summary,
                  id.vars = c("decade", "prop_labelled"),
                  variable.name = "gini_type", value.name = "mean_gini"
)

gini_wide <- dcast(gini_long, decade + prop_labelled ~ gini_type, value.var = "mean_gini")

# One numeric y value per decade (not per system)
gini_wide[, decade_num := as.numeric(as.factor(decade))]

# Create jitter offset only for plotting
jitter_width <- 0.25
gini_wide[, jitter_offset := as.numeric(factor(prop_labelled)) * jitter_width - jitter_width]
gini_wide <- gini_wide[!is.na(prop_labelled)]

## Plot
gini_lollipop_plot <- ggplot(gini_wide) +
  geom_segment(aes(
    x = prefisc_gini,
    xend = postfisc_gini,
    y = decade_num + jitter_offset,
    yend = decade_num + jitter_offset
  ), color = "#E7E7E7", linewidth = 2) +
  
  geom_point(aes(
    x = prefisc_gini,
    y = decade_num + jitter_offset,
    shape = prop_labelled,
    color = "Pre-fiscal Gini"
  ), size = 3) +
  
  geom_point(aes(
    x = postfisc_gini,
    y = decade_num + jitter_offset,
    shape = prop_labelled,
    color = "Post-fiscal Gini"
  ), size = 3) +
  
  scale_y_continuous(
    trans = "reverse",
    breaks = unique(gini_wide$decade_num),
    labels = gini_wide[, .SD[1], by = decade][order(decade_num)]$decade
  ) +
  
  scale_color_manual(
    name = "Gini Type",
    values = c("Pre-fiscal Gini" = "#436685", "Post-fiscal Gini" = "#BF2F24")
  ) +
  scale_shape("Electoral System") +
  expand_limits(x = c(20, 50)) +
  labs(
    x = "Mean Gini Index",
    y = "",
    title = "Mean Pre- and Post-Fiscal Gini by Decade and Electoral System"
  ) +
  plot_theme_base() +
  theme(
    legend.position = "right"
  )

# Save
save_plot(
  plot = gini_lollipop_plot,
  path = file.path(fig_dir, "gini_lollipop_plot.png")
)


#============================#
# Plot 9: Residualised Scatterplots Loop
#============================#

message("Creating residualised scatterplots...")

# Choose the lag to use (can loop over lags later if desired)
selected_lag <- "dis_gall_lag1"

# Run in serial (or use `mclapply` with `mc.cores`)
for (outcome_var in cpds_outcomes) {
  
  outcome_label <- labels$gini_lookup[[outcome_var]]
  lag_label     <- labels$dispro_lookup[[selected_lag]]
  
  # Skip if labels missing
  if (is.null(outcome_label) || is.null(lag_label)) {
    warning(glue::glue("Missing label for {outcome_var} or {selected_lag} — skipping."))
    next
  }
  
  # Filter to subset
  dt <- cpds_outcomes_df[
    region %in% c("Northern America", "Northern Europe", "Western Europe", "Southern Europe") &
      !is.na(get(outcome_var)) &
      !is.na(get(selected_lag)) &
      !is.na(region) &
      !is.na(year)
  ]
  
  # For proper colour scale 
  dt[, year_rev := max(year, na.rm = TRUE) - year]
  
  
  # Residualise both variables
  dt[, outcome_resid := resid(lm(get(outcome_var) ~ region + year, data = .SD))]
  dt[, lag_resid := resid(lm(get(selected_lag) ~ region + year, data = .SD))]
  
  # Plot
  plot <- ggplot(dt, aes(x = lag_resid, y = outcome_resid)) +
    geom_point(aes(colour = year_rev)) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
    scale_color_gradient(
      name = "Year",
      low = "red", high = "blue",
      breaks = seq(min(dt$year_rev, na.rm = TRUE), max(dt$year_rev, na.rm = TRUE), by = 10),
      labels = function(x) max(dt$year, na.rm = TRUE) - x
    ) +
    expand_limits(x = 0, y = 0) +
    labs(
      x = paste0("Residualised ", lag_label),
      y = paste0("Residualised ", outcome_label),
      title = paste0(outcome_label, " vs. ", lag_label)
    ) +
    facet_wrap(~region) +
    plot_theme_base() +
    theme(legend.position = "right", 
          strip.text = element_text(size = 35)) +
    guides(colour = guide_colorbar(barwidth = 1, barheight = 15))
  
  print(plot)
  
  # Save
  save_plot(
    plot = plot,
    path = file.path(fig_dir, paste0(outcome_var, "_vs_", selected_lag, ".png"))
  )
}


#============================#
# CPDS: Cabinet Ideology Prep
#============================#

message("Preparing CPDS data for Iversen/Soskice replication")

# 1. Create period variable
cpds[, is_period := fifelse(year <= 1998, "1960–1998", "1999–2022")]
cpds[, is_period := factor(is_period, levels = c("1960–1998", "1999–2022"))]

# 2. Label electoral systems
cpds[, prop_labelled := factor(prop, levels = 0:2, labels = c("SMSP", "Modified PR", "PR"))]

#============================#
# Plot 10: Mean Cabinet Ideology (gov_party)
#============================#

message("Creating plot: Mean Cabinet Ideology by Electoral System")

# Aggregate
is_cpds <- cpds[!is.na(prop_labelled), .(
  mean_gov_party = mean(gov_party, na.rm = TRUE)
), by = .(is_period, prop_labelled)]

# Plot
gov_party_plot <- ggplot(is_cpds, aes(x = mean_gov_party, y = prop_labelled)) +
  geom_line(aes(group = prop_labelled), colour = "#E7E7E7", linewidth = 2.5) +
  geom_point(aes(colour = is_period), size = 5) +
  scale_colour_manual(values = c("#436685", "#BF2F24")) +
  expand_limits(x = c(1, 5)) +
  labs(
    x = "Mean Cabinet Ideology (CPDS Index)",
    y = "",
    title = "Mean Cabinet Ideology by Electoral System", 
    colour = ""
  ) +
  plot_theme_base()

# Save
save_plot(
  plot = gov_party_plot,
  path = file.path(fig_dir, "cpds_mean_gov_party.png")
)

#============================#
# Plot 11: Cabinet Composition by Ideology
#============================#

message("Creating plot: Share of Cabinet by Ideology and Electoral System")

# Aggregate by type
is_cpds_all <- cpds[!is.na(prop_labelled), .(
  mean_left  = mean(gov_left1,  na.rm = TRUE),
  mean_center = mean(gov_cent1, na.rm = TRUE),
  mean_right = mean(gov_right1, na.rm = TRUE)
), by = .(is_period, prop_labelled)]

# Melt to long
is_cpds_long <- melt(is_cpds_all,
                     id.vars = c("is_period", "prop_labelled"),
                     variable.name = "ideology_type",
                     value.name = "mean_value"
)

# Rename types for facet labels
is_cpds_long[, ideology_type := factor(ideology_type,
                                       levels = c("mean_left", "mean_center", "mean_right"),
                                       labels = c("Left", "Center", "Right")
)]

# Plot
gov_ideology_plot <- ggplot(is_cpds_long, aes(x = mean_value, y = prop_labelled)) +
  geom_line(aes(group = prop_labelled), colour = "#E7E7E7", linewidth = 2.5) +
  geom_point(aes(colour = is_period), size = 4) +
  geom_vline(xintercept = 50, linetype = "dashed", linewidth = 1) +
  scale_colour_manual(values = c("#436685", "#BF2F24")) +
  scale_x_continuous(labels = scales::label_percent(scale = 1)) +
  expand_limits(x = c(0, 100)) +
  labs(
    x = "Share of Cabinet Positions",
    y = "",
    title = "Cabinet Composition by Ideology and Electoral System"
  ) +
  facet_wrap(~ideology_type) +
  plot_theme_base() +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        axis.title.x = element_text(size = 40),
        axis.text.x = element_text(size = 30),
        strip.text = element_text(size = 35)) 

# Save
save_plot(
  plot = gov_ideology_plot,
  path = file.path(fig_dir, "cpds_cabinet_composition.png")
)

