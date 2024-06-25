# Load the ggplot2 package
library(ggplot2)

# Read the CSV file
stars <- read.csv("Stars.csv")

# Combine the data into a single data frame with a 'type' column to differentiate plots
df_R <- data.frame(x = stars$A_M, y = stars$R, class = as.factor(stars$Spectral_Class), plot = "Radius vs A_M")
df_L <- data.frame(x = stars$A_M, y = stars$Temperature, class = as.factor(stars$Spectral_Class), plot = "Temperature vs A_M")

# Combine the two data frames
df_combined <- rbind(df_R, df_L)

# Define custom colors for each class
custom_colors <- c("M" = "red", "O" = "blue", "A" = "purple", "B" = "yellow", "F" = "orange", "G" = "brown", "K" = "green")

# Create the scatter plot with faceting and custom colors
ggplot(df_combined, aes(x = x, y = y, color = class)) +
  geom_point() +
  facet_wrap(~ plot, scales = "free_y") +  # Facet based on the 'plot' column
  labs(title = "Scatter Plots", x = "A_M", y = "", color = "Class") +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  theme_minimal()
