#######################
# Preliminaries 
#######################

# import files from Custom_scripts 
source(here::here("03 Simulations", "Week4", "Custom_scripts", "custom_packages.R"))
source(here::here("03 Simulations", "Week4", "Custom_scripts", "custom_functions.R"))

# import data from Bormann and Golder (2020)
esbg <- as.data.table(read_csv_arrow(here::here("03 Simulations", "Week4", "es_data-v5_0.csv")))

# Import cpds data 
cpds_url <- "https://cpds-data.org/wp-content/uploads/2024/11/cpds-1960-2022-update-2024-2.xlsx"
cpds <- openxlsx::read.xlsx(cpds_url, colNames = TRUE, skipEmptyRows = TRUE, detectDates = TRUE)
cpds <- as.data.table(cpds)

# Import Lijphart data from Andy Eggers
lijphart_url <- "https://andy.egge.rs/data/L.csv"
lijphart <- read_csv_arrow(file = lijphart_url)
lijphart <- as.data.table(lijphart) 

# Set figure directory 
fig_dir <- paste0(here::here("03 Simulations", "Week4", "Figures"), "/")

########################
# Lookup label vectors 
########################

leg_type_labels <- c("Majoritarian", "Proportional", "Mixed")
elecrule_labels <- c("SMDP", "TRS", "AV", "BC", "BV", "PBV", "LV", "SNTV", "List PR", "STV", "Mixed Dependent", "Mixed Independent")
region1_labels <- c("Sub-Saharan Africa", "South Asia", "East Asia", "South East Asia", "Pacific Islands/Oceania",
                    "Middle East/North Africa", "Latin America", "Caribbean and non-Iberic America", 
                    "Eastern Europe/post-Soviet states", "Industrialized Countries (OECD)", "Oil Countries")
region2_labels <- c("Sub-Saharan Africa", "South Asia", "East Asia", "South East Asia", "Pacific Islands/Oceania",
                    "Middle East/North Africa", "Latin America", "Caribbean and non-Iberic America", 
                    "Eastern Europe/post-Soviet states", "Western Europe")

########################
# Data wrangling 
########################

# Assign region factor labels
esbg[, region1_factor := factor(region1, levels = 1:11, labels = region1_labels)]
esbg[, region2_factor := factor(region2, levels = 1:10, labels = region2_labels)]

##############################
# Expand to country-year panel
##############################

# (i) Add the missing years
years_range <- esbg[, .(min_year = min(year), max_year = max(year)), by = country]
full_grid <- years_range[, .(year = as.integer(min_year):as.integer(max_year)), by = country]

# Merge with original data
esbg_full <- merge(full_grid, esbg, by = c("country", "year"), all.x = TRUE)
setorder(esbg_full, country, year)

#########################################
# (ii) Fill values (forward only)
#########################################

# Identify numeric columns (excluding IDs and year)
numeric_cols <- names(esbg_full)[sapply(esbg_full, is.numeric)]
cols_to_fill <- setdiff(numeric_cols, c("year"))  # keep 'year' untouched

# Forward fill only
esbg_full[, (cols_to_fill) := lapply(.SD, \(x) nafill(x, type = "locf")), 
          by = country, .SDcols = cols_to_fill]

#########################################
# (iii) Recreate factor variables after fill
#########################################

esbg_full[, legislative_type_factor := factor(legislative_type, levels = 1:3, labels = leg_type_labels)]
esbg_full[, elecrule_factor := factor(elecrule, levels = 1:12, labels = elecrule_labels)]
esbg_full[, region1_factor := factor(region1, levels = 1:11, labels = region1_labels)]
esbg_full[, region2_factor := factor(region2, levels = 1:10, labels = region2_labels)]

###############################
# Create subsets for analysis
###############################

# Democracies only
esbg_bmr_dem <- esbg[bmr_democracy == 1]
esbg_bmr_dem_full <- esbg_full[bmr_democracy == 1]

# Legislative elections in democracies
esbg_bmr_dem_leg <- esbg_bmr_dem[presidential == 0]
esbg_bmr_dem_leg_full <- esbg_bmr_dem_full[presidential == 0]


####################################
# Exploratory data analysis
####################################

# Use esbg_bmr_dem_leg to plot the share of legislative_type when taking the most recent election for each country 
## Create a new data.table with the most recent election for each country
esbg_bmr_dem_leg_recent <- esbg_bmr_dem_leg[, .SD[which.max(year)], by = country]

# Now compute the share of each legislative type 
leg_share <- esbg_bmr_dem_leg_recent[, .(count = .N), by = .(legislative_type, elecrule)]
# label all values of legislative_type and elecrule
leg_share[, legislative_type := factor(legislative_type, levels = 1:3, labels = leg_type_labels)]
leg_share[, elecrule := factor(elecrule, levels = 1:12, labels = elecrule_labels)]

# Set aesthetics 
## Font type
font_add_google("Chivo", "chivo")
showtext_auto()

## Create colour palette
my_colours <- qualitative_hcl(length(unique(leg_share$elecrule)), palette = "Dark 3")

## Use leg_share to create a barplot for the shares by legislative_type with fill by elecrule
leg_share %>%
  ggplot(aes(x = factor(legislative_type), 
             y = count/sum(count), 
             fill = elecrule)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  scale_x_discrete(labels = leg_type_labels) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0, 0.5)) +
  scale_fill_manual(values = my_colours) +
  labs(y = "Share", 
       fill = "Electoral rule",
       title = "Electoral Systems in Most Recent Legislative Elections") +
  theme_minimal(base_family = "chivo") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 48, face = "bold"),
    axis.title.x = element_blank(),
    axis.title = element_text(size = 45),
    axis.text = element_text(size = 38),
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 25),
    panel.grid.major.y = element_line(color = "grey95", linewidth = 0.6),
    panel.grid.minor = element_blank()
  )

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
  guides(color = guide_colorbar(barheight = unit(12, "lines"))) +  # Extend legend height
  theme_minimal(base_family = "chivo") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 48, face = "bold"),
    axis.title.x = element_blank(),
    axis.title = element_text(size = 45),
    axis.text = element_text(size = 38),
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 25),
    legend.key.height = unit(1, "cm"),  # Additional control for spacing
    panel.grid.major.y = element_line(color = "grey95", linewidth = 0.6),
    panel.grid.minor = element_blank()
  )

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
  theme_minimal(base_family = "chivo") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 48, face = "bold"),
    axis.title.x = element_blank(),
    axis.title = element_text(size = 45),
    axis.text = element_text(size = 38),
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 25),
    legend.key.height = unit(1, "cm"),  # Additional control for spacing
    panel.grid.major.y = element_line(color = "grey95", linewidth = 0.6),
    panel.grid.minor = element_blank()
  )
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
  # expand_limits(y = c(-10, 10)) +
  # scale_x_discrete(labels = leg_type_labels) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  scale_color_gradient(
    low = "blue", 
    high = "red", 
    limits = range(cpds_elec$year, na.rm = TRUE),
  ) +
  labs(
    y = "Difference in ENEP and ENPP", 
    color = "Year",
    title = "Difference in Effective # of Electoral and Legislative Parties by Electoral System"
  ) +
  guides(color = guide_colorbar(barheight = unit(12, "lines"))) +  # Extend legend height
  theme_minimal(base_family = "chivo") +
  theme(plot.title = element_text(hjust = 0.5, size = 48, face = "bold"),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 45),
        axis.text = element_text(size = 38),
        axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 25),
        legend.key.height = unit(1, "cm"),  # Additional control for spacing
        panel.grid.major.y = element_line(color = "grey95", linewidth = 0.6),
        panel.grid.minor = element_blank()
  )

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
  theme_minimal(base_family = "chivo") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 48, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 45),
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    axis.text.y = element_text(size = 38),
    legend.title = element_text(size = 30),
    legend.position = "bottom",
    legend.text = element_text(size = 25),
    legend.key.height = unit(1, "cm"),  # Additional control for spacing
    panel.grid.major.y = element_line(color = "grey95", linewidth = 0.6),
    panel.grid.minor = element_blank()
  ) 

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
  theme_minimal(base_family = "chivo") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 48, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 45),
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 38),
    axis.text.y = element_text(size = 38),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 25),
    legend.key.height = unit(1, "cm"),  # Additional control for spacing
    panel.grid.major.y = element_line(color = "grey95", linewidth = 0.6),
    panel.grid.minor = element_blank()
  )

# Save the plot
save_plot(path = paste0(fig_dir, "dis_gall_by_region.png"), plot = last_plot())


# Plot Lijphart data 
lijphart %>%
  ggplot(aes(x = eff_num_parl_parties_1945_2010, y = pct_minimal_winning_one_party_cabinet_1945_2010)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = country), size = 7, max.overlaps = 20) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1, scale = 1)) +
  geom_smooth(se = F, method = "lm") +
  expand_limits(x = 1) +
  labs(
    x = "Effective Number of Parties (Parliament)",
    y = "Percentage of Minimal Winning One Party Cabinets",
    title = "Minimal Winning Cabinets vs. Effective # of Parties"
  ) +
  theme_minimal(base_family = "chivo") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 48, face = "bold"),
    axis.title.x = element_text(size = 45),
    axis.title.y = element_text(size = 45),
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 38),
    axis.text.y = element_text(size = 38),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 25),
    legend.key.height = unit(1, "cm"),  # Additional control for spacing
    panel.grid.major.y = element_line(color = "grey95", linewidth = 0.6),
    panel.grid.minor = element_blank()
  )

# Save the plot
save_plot(path = paste0(fig_dir, "lijphart.png"), plot = last_plot())


# Plot effective number of parties against electoral disproportionality
lijphart %>%
  ggplot(aes(x = index_of_disproportionality_1945_2010, y = eff_num_parl_parties_1945_2010)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = country), size = 7, max.overlaps = 20) +
  geom_smooth(se = F, method = "lm") +
  expand_limits(x = 0, y = 0) +
  labs(
    x = "Electoral Disproportionality",
    y = "Effective Number of Parties (Parliament)",
    title = "Effective # of Parties vs. Electoral Disproportionality"
  ) +
  theme_minimal(base_family = "chivo") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 48, face = "bold"),
    axis.title.x = element_text(size = 45),
    axis.title.y = element_text(size = 45),
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 38),
    axis.text.y = element_text(size = 38),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 25),
    legend.key.height = unit(1, "cm"),  # Additional control for spacing
    panel.grid.major.y = element_line(color = "grey95", linewidth = 0.6),
    panel.grid.minor = element_blank()
  )

# Save the plot
save_plot(path = paste0(fig_dir, "lijphart_disproportionality.png"), plot = last_plot())
