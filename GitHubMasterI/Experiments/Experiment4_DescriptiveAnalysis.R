install.packages("ggplot2")
install.packages("ggalluvial")
library(ggplot2)
library(ggalluvial)
library(dplyr)
library(viridis)

setwd("/Users/marinacamacho/Desktop/Master_I/Modelling")

data <-  data.frame(read.csv("Alluvia_data4.csv"))

#filtered_data <- data
# Filter out rows where Time0 contains '-'
filtered_data <- data %>%
  filter(Maternal_smoking_around_birth == 0)
  #filter(Maternal_smoking_around_birth == 1)
  #filter(Breastfed_baby == 0)
  #filter(Breastfed_baby == 1)

# Filter out rows where Time0 contains '-'
filtered_data <- filtered_data %>%
  filter(Time0 != 'CVD-Diabetes')
# Filter out rows where Time0 contains '-'
filtered_data <- filtered_data %>%
  filter(Time0 != 'Depression-CVD')
# Filter out rows where Time0 contains '-'
filtered_data <- filtered_data %>%
  filter(Time0 != 'Depression-Diabetes')
# Filter out rows where Time0 contains '-'
filtered_data <- filtered_data %>%
  filter(Time0 != 'Depression-Diabetes-CVD')

# Filter out rows where Time0 contains '-'
filtered_data <- filtered_data %>%
  filter(Time1 != 'CVD-Diabetes')
# Filter out rows where Time0 contains '-'
filtered_data <- filtered_data %>%
  filter(Time1 != 'Depression-CVD')
# Filter out rows where Time0 contains '-'
filtered_data <- filtered_data %>%
  filter(Time1 != 'Depression-Diabetes')
# Filter out rows where Time0 contains '-'
filtered_data <- filtered_data %>%
  filter(Time1 != 'Depression-Diabetes-CVD')

# Filter out rows where Time0 contains '-'
filtered_data <- filtered_data %>%
  filter(Time2 != 'CVD-Diabetes')
# Filter out rows where Time0 contains '-'
filtered_data <- filtered_data %>%
  filter(Time2 != 'Depression-CVD')
# Filter out rows where Time0 contains '-'
filtered_data <- filtered_data %>%
  filter(Time2 != 'Depression-Diabetes')
# Filter out rows where Time0 contains '-'
filtered_data <- filtered_data %>%
  filter(Time2 != 'Depression-Diabetes-CVD')

grouped_data <- filtered_data %>%
  group_by(Time0, Time1, Time2) %>%
  summarise(Frequency = n()) %>%
  ungroup()

# Calculate the percentage for Frequency and round to 2 decimals
grouped_data <- grouped_data %>%
  mutate(Percentage = round((Frequency / sum(Frequency)) * 100, 2))

# Your ggplot2 code using Percentage as factor for fill
p <- ggplot(data = grouped_data,
            aes(axis1 = Time0,   # First variable on the X-axis
                axis2 = Time1, # Second variable on the X-axis
                axis3 = Time2, # Second variable on the X-axis
                y = Frequency)) +
  geom_alluvium(aes(fill = as.factor(Percentage))) +  # Use Percentage as factor
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Time1", "Time2"),
                   expand = c(0.15, 0.05)) +
  scale_fill_viridis(discrete = TRUE) + 
  labs(fill = "Percentage of Participants") +  # Change legend title
  theme_void() +
  theme(legend.title = element_text(size = 16), # Increase legend title size
        legend.text = element_text(size = 24),   # Increase legend text size
        legend.key.size = unit(1.5, "cm"))       # Increase legend key size

# Save the plot with high resolution
ggsave("high_res_plot_ms_0.png", p, width = 15, height = 13, dpi = 300)
