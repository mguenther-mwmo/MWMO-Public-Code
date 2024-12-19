# Script for stormwater quality plot: concentrations separated by sample type with median lines
# by Mirae Guenther, MWMO

# Before running code, have the data in a sample oriented format (each row contains the sample results of multiple parameters if sampled at the same time)
# and the data are cleaned so that there is at minimum columns for the following:
# -> "Station", the names of the sampling locations
# -> "sample_type", the flow conditions of the samples (baseflow, rain event, snow melt, etc.)
# -> a column for each of your water quality constituents of interest 
# Note: if the data are already in a long format, you will need to adjust if/how the data are reshaped for plotting

# Required packages: 
# tidyr, ggplot2, scales

# Set up main data frame
stations_df <- clean_df %>% 
  filter(Station == "SiteName" | Station == "SiteName2" | Station == "SiteName3") %>% # select the desired sites
  filter(sample_type != "Snow") %>% # remove rows with incorrect or undesired flow types
  mutate(logTP = log(TP)) %>% # create columns with log transformed concentration data
  mutate(logNO3 = log(NO3)) %>% 
  mutate(logCl = log(Cl)) %>% 
  dplyr::select(Station, sample_type, logTP, logNO3, logCl) %>% # build data frame with only the columns we need
  na.omit() # remove rows with missing values #only do this here if confident all variables are always sampled together

# Define the order we want to display the variables in the plot
var_order <- c("logTP", "logNO3", "logCl")

# Set our colors for sample type
sample_type_colors_3 <- c(
  "Base" = "blue",
  "Rain" = "skyblue",
  "Melt" = "#D55E00"
)

# Reshape the data to long format for faceted plotting
stations_long <- stations_df %>%
  pivot_longer(
    cols = c(logTP, logNO3, logCl), # columns to pivot
    names_to = "variable",          # new column for variable names
    values_to = "value"             # new column for variable values
  ) %>%
  mutate(variable = factor(variable, levels = var_order)) # set the variable plotting order per above

# Calculate median values for each variable and sample type
median_lines <- stations_long %>%
  group_by(variable, sample_type) %>% #separate data by sample type
  summarize(median_value = median(value, na.rm = TRUE), .groups = "drop") %>% # calculate medians
  mutate(color = sample_type) # set the color to correspond with the sample type

# Create the plot
BRM3_plot <- ggplot(stations_long, aes(x = variable, y = value, color = sample_type)) +
  geom_point(position = position_jitter(width = 0.2)) +  # jitter points to display of more data
  facet_wrap(~variable, scales = "free") + # create facet plot for each variable
  labs(x = "Sampled Variable", # x-axis label
    y = "log Concentration", # y-axis label
    color = "Sample Type") + # legend title
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                     labels = scales::math_format(10^.x)) + # display log scale on y-axis
  geom_blank(data = stations_long %>%
               mutate(min_value = case_when(
                 variable == "logTP" ~ -5,    # extend lower limit for logTP
                 variable == "logNO3" ~ -3.2,   # extend lower limit for logNO3
                 variable == "logCl" ~ 0.5,  # extend lower limit for logCl
                 TRUE ~ min(value, na.rm = TRUE)
               )),
             aes(y = min_value))+
  theme_minimal() +
  theme(text = element_text(size = 14),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5), # add border around each plot
        strip.text = element_blank()) + # remove facet label at top
  scale_color_manual(values = sample_type_colors_3) + # apply our chosen colors
  geom_hline(data = median_lines, aes(yintercept = median_value, 
                                      color = color), linetype = "dashed", linewidth = 0.75) # plot the median lines

# Print and save plot
print(BRM3_plot) # check out that snazzy plot
ggsave(paste0("C:/X/XX/XXX/file_name.pdf"), BRM3_plot, device = "pdf", width = 10, height = 8) # assign the plot save location and name
