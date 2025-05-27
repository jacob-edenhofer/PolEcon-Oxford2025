#============================#
#       Initial Setup       #
#============================#

# Load packages and scripts
source(here::here("03 Simulations", "Week4", "Custom_scripts", "custom_packages.R"))
source(here::here("03 Simulations", "Week4", "Custom_scripts", "custom_functions.R"))

# Set theme and font
font_add_google("Chivo", "chivo")
showtext_auto()
theme_set(theme_minimal(base_family = "chivo"))

# Define color palette
my_colours <- qualitative_hcl(12, palette = "Dark 3")

# Set figure directory
fig_dir <- file.path(here::here("03 Simulations", "Week4", "Figures"), "/")

#============================#
#       Data Import         #
#============================#

esbg <- fread(here::here("03 Simulations", "Week4", "es_data-v5_0.csv"))

cpds <- openxlsx::read.xlsx(
  "https://cpds-data.org/wp-content/uploads/2024/11/cpds-1960-2022-update-2024-2.xlsx",
  colNames = TRUE, skipEmptyRows = TRUE, detectDates = TRUE
) |> as.data.table()

lijphart <- fread("https://andy.egge.rs/data/L.csv") |> as.data.table()

cabinets <- read_csv("https://www.parlgov.org/data/parlgov-development_csv-utf-8/view_cabinet.csv") |> as.data.table()

#============================#
#       Label Setup         #
#============================#

leg_type_labels <- c("Majoritarian", "Proportional", "Mixed")
elecrule_labels <- c("SMDP", "TRS", "AV", "BC", "BV", "PBV", "LV", "SNTV", "List PR", "STV", "Mixed Dependent", "Mixed Independent")
region1_labels <- c("Sub-Saharan Africa", "South Asia", "East Asia", "South East Asia", "Pacific Islands/Oceania",
                    "Middle East/North Africa", "Latin America", "Caribbean and non-Iberic America", 
                    "Eastern Europe/post-Soviet states", "Industrialized Countries (OECD)", "Oil Countries")
region2_labels <- c("Sub-Saharan Africa", "South Asia", "East Asia", "South East Asia", "Pacific Islands/Oceania",
                    "Middle East/North Africa", "Latin America", "Caribbean and non-Iberic America", 
                    "Eastern Europe/post-Soviet states", "Western Europe")

#============================#
#     Data Preparation      #
#============================#

esbg[, `:=`(
  region1_factor = factor(region1, 1:11, region1_labels),
  region2_factor = factor(region2, 1:10, region2_labels)
)]

# Expand to country-year panel
years_range <- esbg[, .(min_year = min(year), max_year = max(year)), by = country]
full_grid <- years_range[, .(year = min_year:max_year), by = country]
esbg_full <- merge(full_grid, esbg, by = c("country", "year"), all.x = TRUE)[order(country, year)]

# Forward fill
cols_to_fill <- setdiff(names(esbg_full)[sapply(esbg_full, is.numeric)], "year")
esbg_full[, (cols_to_fill) := lapply(.SD, \(x) nafill(x, type = "locf")), by = country, .SDcols = cols_to_fill]

# Recreate factors
esbg_full[, `:=`(
  legislative_type_factor = factor(legislative_type, 1:3, leg_type_labels),
  elecrule_factor = factor(elecrule, 1:12, elecrule_labels),
  region1_factor = factor(region1, 1:11, region1_labels),
  region2_factor = factor(region2, 1:10, region2_labels)
)]

#============================#
#     Dataset Subsets       #
#============================#

esbg_bmr_dem <- esbg[bmr_democracy == 1]
esbg_bmr_dem_full <- esbg_full[bmr_democracy == 1]
esbg_bmr_dem_leg <- esbg_bmr_dem[presidential == 0]
esbg_bmr_dem_leg_full <- esbg_bmr_dem_full[presidential == 0]

#============================#
#   Plotting Configuration  #
#============================#

plot_theme <- theme(
      plot.title      = element_text(hjust = 0.5, size = 48, face = "bold"),
      axis.title      = element_text(size = 45),
      axis.title.x    = element_blank(),
      axis.text       = element_text(size = 38),
      axis.text.x     = element_text(angle = 0, vjust = 0.5),
      legend.title    = element_text(size = 30),
      legend.text     = element_text(size = 25),
      legend.position = "bottom",
      legend.key.height = unit(1, "cm"),
      panel.grid.major.y = element_line(color = "grey95", linewidth = 0.6),
      panel.grid.minor   = element_blank()
    )


#============================#
#    Create plots         #
#============================#


# Create leg_share dataset
esbg_bmr_dem_leg_recent <- esbg_bmr_dem_leg[, .SD[which.max(year)], by = country]
leg_share <- esbg_bmr_dem_leg_recent[, .(count = .N), by = .(legislative_type, elecrule)]

# label all values of legislative_type and elecrule
leg_share[, legislative_type := factor(legislative_type, levels = 1:3, labels = leg_type_labels)]
leg_share[, elecrule := factor(elecrule, levels = 1:12, labels = elecrule_labels)]

## Use leg_share to create a barplot for the shares by legislative_type with fill by elecrule
elec_colours <- qualitative_hcl(length(unique(leg_share$elecrule)), palette = "Dark 3")

## Use leg_share to create a barplot for the shares by legislative_type with fill by elecrule
leg_share %>%
  ggplot(aes(x = factor(legislative_type), 
             y = count/sum(count), 
             fill = elecrule)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  scale_x_discrete(labels = leg_type_labels) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0, 0.5)) +
  scale_fill_manual(values = elec_colours) +
  labs(y = "Share", 
       fill = "Electoral rule",
       title = "Electoral Systems in Most Recent Legislative Elections") +
  plot_theme +
  theme(legend.position = "right", 
        legend.title = element_blank())

# Save the plot
save_plot(path = paste0(fig_dir, "leg_share.png"), plot = last_plot())


# Use esbg_bmr_dem_leg to create a boxplot of `enep` by legislative_type; use a continuous colour scale to colour points by year 
esbg_bmr_dem_leg %>%
  # Replace negative enep values with NA
  mutate(enep = ifelse(enep < 0, NA, enep)) %>%
  ggplot(aes(x = factor(legislative_type), y = enep, color = year)) +
  geom_boxplot(outlier.size = 0.5, outlier.colour = "grey") +
  geom_jitter(size = 1, alpha = 0.5, width = 0.42) +  # Add jitter width for better spread
  expand_limits(y = c(0, 15)) +
  scale_x_discrete(labels = leg_type_labels) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  scale_color_gradient(
    low = "blue", 
    high = "red", 
    limits = range(esbg_bmr_dem_leg$year, na.rm = TRUE),
  ) +
  labs(
    y = "ENEP", 
    color = "Year",
    title = "Effective Number of Electoral Parties by Electoral System"
  ) +
  guides(color = guide_colorbar(barheight = unit(12, "lines"))) +  
  plot_theme +
  theme(legend.position = "right")

# Save the plot
save_plot(path = paste0(fig_dir, "enep_leg.png"), plot = last_plot())


# Use the dame dataset to compute the difference between effective electoral and legislative number of parties 
dame <- esbg_bmr_dem_leg[, .(country, year, legislative_type, enep, enpp)]
dame[, enep := ifelse(enep < 0, NA, enep)]
dame[, enpp := ifelse(enpp < 0, NA, enpp)]
dame[, diff := enep / enpp]
dame[, diff := ifelse(diff < 1, NA, diff)]

# Use dame to create a boxplot of `diff` by legislative_type; use a continuous colour scale to colour points by year
dame %>%
  ggplot(aes(x = factor(legislative_type), y = diff, color = year)) +
  geom_boxplot(outlier.size = 0.5, outlier.colour = "grey") +
  geom_jitter(size = 1, alpha = 0.5, width = 0.42) +  # Add jitter width for better spread
  # expand_limits(y = c(-10, 10)) +
  scale_x_discrete(labels = leg_type_labels) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  scale_color_gradient(
    low = "blue", 
    high = "red", 
    limits = range(dame$year, na.rm = TRUE),
  ) +
  labs(
    y = "Ratio between ENEP and ENPP", 
    color = "Year",
    title = "Difference in Effective # of Electoral and Legislative Parties by Electoral System"
  ) +
  guides(color = guide_colorbar(barheight = unit(12, "lines"))) +  # Extend legend height
  plot_theme +
  theme(legend.position = "right")
  
# Save the plot
save_plot(path = paste0(fig_dir, "diff_enep_enpp_gb.png"), plot = last_plot())


# Use the cpds dataset 
cpds_elec <- cpds[, .(country, year, prop, rae_ele, rae_leg, effpar_ele, effpar_leg, dis_gall, dis_rel, dis_abso)]
cpds_elec[, prop := factor(prop, levels = c("0", "1", "2"), labels = c("SMSP", "Modified PR", "PR"))]

## For each country and year, calculate the ration between effpar_ele and effpar_leg
cpds_elec[, effpar_ratio := effpar_ele / effpar_leg]

# Use cpds_elec to create a boxplot of `effpar_ratio` by legislative_type; use a continuous colour scale to colour points by year
cpds_elec %>%
  filter(!is.na(prop)) %>%
  ggplot(aes(x = factor(prop), y = effpar_ratio, color = year)) +
  geom_boxplot(outlier.size = 0.5, outlier.colour = "grey") +
  geom_jitter(size = 1, alpha = 0.5, width = 0.42) +  # Add jitter width for better spread
  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  scale_color_gradient(
    low = "blue", 
    high = "red", 
    limits = range(cpds_elec$year, na.rm = TRUE),
  ) +
  labs(
    y = "Ration between ENEP and ENPP", 
    color = "Year",
    title = "Difference in Effective # of Electoral and Legislative Parties by Electoral System"
  ) +
  guides(color = guide_colorbar(barheight = unit(12, "lines"))) +  # Extend legend height
  plot_theme +
  theme(legend.position = "right")

# Save the plot
save_plot(path = paste0(fig_dir, "diff_enep_enpp_cpds.png"), plot = last_plot())


# Use CPDS dataset to create a timeseries of effpar_ele and effpar_leg by region (countryname)
cpds_elec[, region:= countrycode(country, "country.name", "region23")]

## Compute mean effpar_ele and effpar_leg by region and year
cpds_elec %>%
  group_by(region, year) %>%
  summarise(
    effpar_ele = mean(effpar_ele, na.rm = TRUE),
    effpar_leg = mean(effpar_leg, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  pivot_longer(cols = c("effpar_ele", "effpar_leg"), names_to = "type", values_to = "value") %>%
  ggplot(aes(x = year, y = value, color = type)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~region) +
  scale_color_manual(values = c("blue", "red"), labels = c("effpar_ele" = "Electoral", 
                                                           "effpar_leg" = "Legislative")) +
  scale_x_continuous(breaks = seq(1945, 2020, by = 5)) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  labs(
    y = "Effective Number of Parties",
    color = "Type",
    title = "Effective Number of Parties by Region"
  ) +
  expand_limits(y = c(0, 7)) +
  plot_theme +
  theme(axis.text.x = element_text(size = 9)) 

# Save the plot
save_plot(path = paste0(fig_dir, "effpar_by_region.png"), plot = last_plot())


# Let's now plot dis_gall over time, with color by region
cpds_elec %>%
  group_by(region, year) %>%
  summarise(
    dis_gall = mean(dis_gall, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = dis_gall, color = region)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(1945, 2020, by = 5)) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  labs(
    y = "Gallagher Index",
    title = "Gallagher Index by Region, 1960 - 2020"
  ) +
  expand_limits(y = c(0, 30)) +
  plot_theme 

# Save the plot
save_plot(path = paste0(fig_dir, "dis_gall_by_region.png"), plot = last_plot())


# Plot Lijphart data 
lijphart %>%
  ggplot(aes(x = eff_num_parl_parties_1945_2010, y = pct_minimal_winning_one_party_cabinet_1945_2010)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = country), size = 10, max.overlaps = 20) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1, scale = 1)) +
  geom_smooth(se = F, method = "lm") +
  expand_limits(x = 1) +
  labs(
    x = "Effective Number of Parties (Parliament)",
    y = "Percentage of Minimal Winning One Party Cabinets",
    title = "Minimal Winning Cabinets vs. Effective # of Parties"
  ) +
 plot_theme

# Save the plot
save_plot(path = paste0(fig_dir, "lijphart_enlep_mwc.png"), plot = last_plot())


# Plot effective number of parties against electoral disproportionality
lijphart %>%
  ggplot(aes(x = index_of_disproportionality_1945_2010, y = eff_num_parl_parties_1945_2010)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = country), size = 10, max.overlaps = 20) +
  geom_smooth(se = F, method = "lm") +
  expand_limits(x = 0, y = 0) +
  labs(
    x = "Electoral Disproportionality",
    y = "Effective Number of Parties (Parliament)",
    title = "Effective # of Parties vs. Electoral Disproportionality"
  ) +
  plot_theme 

# Save the plot
save_plot(path = paste0(fig_dir, "lijphart_disproportionality_enlp.png"), plot = last_plot())


# Use cpds data to plot some correlations between electoral disproprtionality and some outcome fiscal outcome variables 

## Create a variable that captures the redistributive nature of the tax-and-transfer system
cpds[, perc_change_gini := (postfisc_gini - prefisc_gini)/prefisc_gini]

## Extract outcome variables of interest
cpds_outcomes <- names(cpds)[grepl("_gini", names(cpds))]

## Lag disproportionality (dis_gall) by 1, 2, 3, and 4 years
cpds <- cpds[, `:=`(
  dis_gall_lag1 = shift(dis_gall, 1),
  dis_gall_lag2 = shift(dis_gall, 2),
  dis_gall_lag3 = shift(dis_gall, 3),
  dis_gall_lag4 = shift(dis_gall, 4)
), by = country]

## Vector of lagged variables 
lag_vars <- c("dis_gall_lag1", "dis_gall_lag2", "dis_gall_lag3", "dis_gall_lag4")

## Create vector of columns of interest 
cols_interest <- c("country", "year", "dis_gall", "prop", lag_vars, cpds_outcomes)

## Prune dataset to outcome variables of interest and dis_gall
cpds_outcomes_df <- cpds[, ..cols_interest]

## Create a region23 variable
cpds_outcomes_df[, region := countrycode(country, "country.name", "region23")]

## Lookup vector for gini variables
gini_lookup <- c(
 "postfisc_gini" = "Post-Fiscal Gini",
 "prefisc_gini" = "Pre-Fiscal Gini",
 "pretran_gini" = "Pre-Transfer Gini",
 "perc_change_gini" = "Percentage Change in Gini"
)

## Lookup vector for disproportionality variables
dispro_lookup <- c(
  "dis_gall" = "Gallagher Index",
  "dis_gall_lag1" = "Gallagher Index (Lagged 1 Year)",
  "dis_gall_lag2" = "Gallagher Index (Lagged 2 Years)",
  "dis_gall_lag3" = "Gallagher Index (Lagged 3 Years)",
  "dis_gall_lag4" = "Gallagher Index (Lagged 4 Years)"
)

## Label ``prop`` variable 
cpds_outcomes_df[, prop_labelled := factor(prop, levels = 0:2, labels = c("SMSP", "Modified PR", "PR"))]

## Create variable for decade from min to max of year variable
min_year <- floor(min(cpds_outcomes_df$year, na.rm = TRUE) / 10) * 10
max_year <- ceiling(max(cpds_outcomes_df$year, na.rm = TRUE) / 10) * 10
cpds_outcomes_df[, decade := cut(year, 
                                 breaks = seq(min_year, max_year + 10, by = 10), 
                                 labels = seq(min_year, max_year, by = 10), 
                                 right = FALSE)]
cpds_outcomes_df[, decade:= paste0(decade, "s")]

# Let us now create a lollipop plot of prefisc_gini and postfisc_gini by electoral system and decade
# Filter and aggregate
gini_summary <- cpds_outcomes_df[, .(
  prefisc_gini = mean(prefisc_gini, na.rm = TRUE),
  postfisc_gini = mean(postfisc_gini, na.rm = TRUE)
), by = .(decade, prop_labelled)]

# Convert to long format for easier plotting
gini_long <- melt(gini_summary, 
                  id.vars = c("decade", "prop_labelled"),
                  variable.name = "gini_type", 
                  value.name = "mean_gini")

# Optionally relabel variables
gini_long[, gini_type := factor(gini_type, 
                                levels = c("prefisc_gini", "postfisc_gini"),
                                labels = c("Pre-fiscal Gini", "Post-fiscal Gini"))]

# Create the lollipop plot
# Step 1: Wide format for proper line connections
gini_wide <- dcast(
  gini_long[!is.na(prop_labelled)], 
  decade + prop_labelled ~ gini_type, 
  value.var = "mean_gini"
)

# Step 2: Plot
jitter_width <- 0.2

gini_wide[, decade_num := as.numeric(as.factor(decade))]
gini_wide[, y_pos := decade_num + as.numeric(factor(prop_labelled)) * jitter_width - jitter_width]

ggplot(gini_wide) +
  # Connect pre- and post-fiscal Gini with a line (lollipop stem)
  geom_segment(aes(
    x = `Pre-fiscal Gini`,
    xend = `Post-fiscal Gini`,
    y = y_pos,
    yend = y_pos
  ), color = "#E7E7E7", linewidth = 2) +
  
  # Points for Pre-fiscal Gini
  geom_point(aes(
    x = `Pre-fiscal Gini`,
    y = y_pos,
    shape = prop_labelled,
    colour = "Pre-fiscal Gini"
  ), size = 3) +
  
  # Points for Post-fiscal Gini
  geom_point(aes(
    x = `Post-fiscal Gini`,
    y = y_pos,
    shape = prop_labelled,
    colour = "Post-fiscal Gini"
  ), size = 3) +
  
  # Y-axis ticks and labels (decade), reversed
  scale_y_continuous(
    breaks = gini_wide[, .(y_pos, decade)][, .SD[1], by = decade][["y_pos"]],
    labels = gini_wide[, .(y_pos, decade)][, .SD[1], by = decade][["decade"]],
    trans = "reverse"  # reverse the y-axis
  ) +
  
  scale_colour_manual(
    name = "Gini Type",
    values = c("Pre-fiscal Gini" = "#436685", "Post-fiscal Gini" = "#BF2F24")
  ) +
  expand_limits(x=c(20, 50)) + # Adjust x-axis limits
  scale_shape("Electoral System") +
  labs(
    x = "Mean Gini Index",
    y = "",
    title = "Mean Pre- and Post-Fiscal Gini by Decade and Electoral System"
  ) +
  theme_minimal() +
  theme(
    # change font type to something elegant (see above)
    text = element_text(family = "chivo"),
    axis.title      = element_text(size = 45),
    axis.title.x    = element_blank(),
    axis.text       = element_text(size = 38),
    axis.text.x     = element_text(angle = 0, vjust = 0.5),
    legend.title    = element_text(size = 35),
    legend.text     = element_text(size = 30),
    plot.title = element_text(size = 47, hjust  = 0.5, face = "bold"),
    panel.grid.major.y = element_line(color = "grey95", linewidth = 0.6),
    panel.grid.minor   = element_blank()
  )

# Save the plot
save_plot(plot = last_plot(), path = paste0(fig_dir, "/gini_lollipop_plot.png"))


## loop over outcome variables to create residualised scatterplots 
mclapply(cpds_outcomes, function(outcome_var) {
  # Create variables for the outcome variable and lagged variable names
  outcome_var_name <- paste0(outcome_var, "_resid")
  lag_var_name <- paste0(outcome_var, "_lag_resid")
  
  ## Prune to Northern America, Northern Europe, Western Europe, Southern Europe
  cpds_outcomes_df <- cpds_outcomes_df[region %in% c("Northern America", "Northern Europe", 
                                                     "Western Europe", "Southern Europe")]
  
  ## Drop rows with NA in any relevant variable
  dt <- cpds_outcomes_df[!is.na(get(outcome_var)) & 
                         !is.na(dis_gall) & 
                         !is.na(region) & 
                         !is.na(year) & 
                         !is.na(get(lag_vars[1]))]
  
  ## Compute residuals
  dt[, (outcome_var_name) := resid(lm(get(outcome_var) ~ region + year, data = .SD))]
  dt[, (lag_var_name) := resid(lm(get(lag_vars[1]) ~ region + year, data = .SD))]
  
  ## Plot
  plot <- ggplot(dt, aes_string(x = lag_var_name, y = outcome_var_name)) +
    geom_point(aes(colour = year)) +
    geom_smooth(se = F, method = "lm") +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
    scale_color_gradient(name = "",
                         low = "blue", 
                         high = "red", 
                         limits = range(dt$year, na.rm = TRUE), 
                         breaks = seq(min(dt$year, na.rm = TRUE), max(dt$year, na.rm = TRUE), by = 10), 
                         ) +
    expand_limits(x = 0, y = 0) +
    labs(
      x = paste0("Residualised ", dispro_lookup[lag_vars[1]]),
      y = paste0("Residualised ", gini_lookup[outcome_var]),
      title = paste0(gini_lookup[outcome_var], " vs. ", dispro_lookup[lag_vars[1]])
    ) +
      facet_wrap(~region) +
      theme_bw() +
      theme(
        # change font type to something elegant (see above)
        text = element_text(family = "chivo"),
        # change font size
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 35),
        plot.title = element_text(size = 47, hjust  = 0.5, face = "bold"), 
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 35),
        strip.text = element_text(size = 35),
        strip.background = element_rect(fill = "lightgrey", colour = "black"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
      ) +
      guides(
        colour = guide_colorbar(barwidth = 1, 
                                barheight = 15)
      )
  # save plot 
  save_plot(plot = plot, 
            path = paste0(fig_dir, outcome_var, "_vs_", lag_vars[1], ".png"))
  
}, mc.cores = 1)


#################################
# Replicate Iversen/Soskice 
# (2006) Figure 1
#################################

# Clean parlogv cabinet and esbg data 

## Create iso3c identifier and golder election id
cabinets <- cabinets %>%
  mutate(iso3c = countrycode(country_name, "country.name", "iso3c"),
         golder_elec_id = paste("L", iso3c, election_date, sep = "-")) 

## Create iso3c identifier and golder election id
esbg_is <- esbg %>%
  mutate(iso3c = countrycode(country, "country.name", "iso3c"),
         election_date = as.Date(date, format = "%B %d, %Y"),
         election_year = lubridate::year(election_date),
         golder_elec_id = ifelse(is.na(legislative_type),
                                 paste("P", iso3c, election_date, sep = "-"),
                                 paste("L", iso3c, election_date, sep = "-"))) %>%
  select(elec_id, golder_elec_id, country, election_year, everything())

## Merge cabinets and esbg data
cabinets_golder <- cabinets %>%
  left_join(esbg_is, by = "golder_elec_id") 

## Prune data 
cabinets_golder_pruned <- cabinets_golder %>%
  select(country_name, golder_elec_id, date, election_year, cabinet_name, 
         caretaker, cabinet_party, prime_minister, seats.x, 
         election_seats_total, party_name, left_right, presidential, 
         legislative_type, elecrule) %>%
  group_by(cabinet_name) %>%
  mutate(cabinet_ideology = mean(left_right, na.rm = TRUE)) %>%
  ungroup()


### create pruned dataset 
cabinets_golder_distinct <- cabinets_golder_pruned %>%
  filter(election_year >= 1949, 
         country_name %in% c("Australia", "Austria", "Belgium", "Canada", 
                             "Denmark", "Finland", "France", "Germany", 
                             "Ireland", "Israel", "Italy", "Japan", 
                             "Luxembourg", "Netherlands", "New Zealand", "Norway", 
                             "Sweden", "Switzerland", "United Kingdom")) %>%
  mutate(is_id = ifelse(election_year <= 1998, 1, 0)) %>%
  distinct(golder_elec_id, cabinet_ideology, .keep_all = TRUE) 

## Use cabinets_golder_distinct to create plot 
cabinets_golder_distinct %>%
  group_by(legislative_type, is_id) %>%
  summarise(mean_ideology = mean(cabinet_ideology, na.rm = TRUE)) %>% 
  mutate(legislative_type = factor(legislative_type, 
                                   levels = c("1", "2", "3"),
                                   labels = c("Majoritarian", "Proportional", "Mixed")),
         is_id = factor(is_id, 
                        levels = c("1", "0"),
                        labels = c("1949 - 1998", "1999 - 2020"))) %>%
  ggplot(aes(x = mean_ideology, y = legislative_type)) +
  geom_line(aes(group = legislative_type), colour="#E7E7E7", linewidth = 2.5) +
  geom_point(aes(colour = is_id), size = 5) +
  expand_limits(x = c(4, 6)) +
  scale_colour_manual("", values=c("#436685", "#BF2F24")) +
  labs(x = "Mean cabinet ideology (Parlgov)", y = "", title = "Mean cabinet ideology by electoral system") +
  plot_theme


#################################
# Replicate Iversen/Soskice using CPDS
# (2006) Figure 1
#################################

# We will use the ``prop`` and ``gov_party`` variables for this plot.
cpds[ , is_period := ifelse(year <= 1998, 1, 0)]
cpds[ , is_period := factor(is_period, 
                            levels = c("1", "0"),
                            labels = c("1960 - 1998", "1999 - 2022"))]

# gov_party Cabinet composition (Schmidt-Index):
# (1) Hegemony of right-wing (and center) parties (gov_left1=0),
# (2) dominance of right-wing (and center) parties (0<gov_left1<=33.33),
# (3) balance of power between left and right (33.33<gov_left1<66.67),
# (4) dominance of social-democratic and other left parties (66.67<=gov_left1<100),
# (5) hegemony of social-democratic and other left parties (gov_left=100).

# The gov_party variable already exists in the CPDS dataset, so we can use it directly.

is_cpds <- cpds[, .(mean_gov_party = mean(gov_party, na.rm = TRUE)), 
                  by = .(is_period, prop)]

is_cpds[, prop := factor(prop, 
                          levels = c("0", "1", "2"),
                          labels = c("SMSP", "Modified PR", "PR"))]

is_cpds %>%
  filter(!is.na(prop)) %>%
  ggplot(aes(x = mean_gov_party, y = prop)) +
  geom_line(aes(group = prop), colour="#E7E7E7", linewidth = 2.5) +
  geom_point(aes(colour = is_period), size = 5) +
  scale_colour_manual("", values=c("#436685", "#BF2F24")) +
  expand_limits(x=c(1, 5)) +
  labs(x = "Mean cabinet ideology (CPDS)", y = "", title = "Mean cabinet ideology by electoral system") +
  plot_theme


## Similar plot but now for gov_left1, gov_cent1, and gov_right1 (all faceted)
is_cpds_all <- cpds[, .(mean_gov_left1 = mean(gov_left1, na.rm = TRUE), 
                    mean_gov_cent1 = mean(gov_cent1, na.rm = TRUE), 
                    mean_gov_right1 = mean(gov_right1, na.rm = TRUE)),
                  by = .(is_period, prop)]

is_cpds_all[, prop := factor(prop, 
                              levels = c("0", "1", "2"),
                              labels = c("SMSP", "Modified PR", "PR"))]

is_cpds_all %>%
  filter(!is.na(prop)) %>%
  pivot_longer(cols = c(mean_gov_left1, mean_gov_cent1, mean_gov_right1), 
               names_to = "mean_gov", values_to = "mean_value") %>%
  mutate(mean_gov = factor(mean_gov, 
                           levels = c("mean_gov_left1", "mean_gov_cent1", "mean_gov_right1"),
                           labels = c("Left", "Center", "Right"))) %>%
  ggplot(aes(x = mean_value, y = prop)) +
  geom_line(aes(group = prop), colour="#E7E7E7", linewidth = 2.5) +
  geom_vline(xintercept = 50, linetype = "dashed", linewidth = 1) +
  geom_point(aes(colour = is_period), size = 3.5) +
  scale_colour_manual("", values=c("#436685", "#BF2F24")) +
  scale_x_continuous(labels = scales::label_percent(scale = 1)) +
  expand_limits(x=c(0, 100)) +
  labs(x = "Share of cabinet positions", 
       title = "Share of cabinet posts by ideology and electoral system") +
  facet_wrap(~ mean_gov) +
  theme_bw() +
  theme(
    # change font type to something elegant (see above)
    text = element_text(family = "chivo"),
    axis.title.x    = element_text(size = 45),
    axis.title.y = element_blank(),
    axis.text.y       = element_text(size = 38),
    axis.text.x     = element_text(size = 33, angle = 0, vjust = 0.5),
    legend.title    = element_text(size = 35),
    legend.text     = element_text(size = 30),
    legend.position = "bottom",
    strip.text = element_text(size = 35),
    plot.title = element_text(size = 49, hjust  = 0.5, face = "bold")
  )

# Save the plot
save_plot(plot = last_plot(), path = paste0(fig_dir, "/cpds_cabinet_ideology_elect.png"))
