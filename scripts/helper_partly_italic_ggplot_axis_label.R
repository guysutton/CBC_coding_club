# Reprex for partly italicising axis labels in ggplot

# Load required packages
library(tidyverse)

# Load some data to use 
data <- palmerpenguins::penguins_raw
data <- data %>%
  janitor::clean_names()
head(data)

# Manually create exressions for x-axis labels 
label1 = expression(paste("Adelie Penguin (",italic("Pygoscelis adeliae"),")"))
label2 = expression(paste("Chinstrap Penguin (",italic("Pygoscelis antarctica"),")"))
label3 = expression(paste("Gentoo Penguin (",italic("Pygoscelis papua"),")"))
label3 = expression(paste("Gentoo penguin (Pygoscelis antarctica)"))

# Make plot 
data %>%
  ggplot(data = ., aes(x = species,
                          y = body_mass_g)) + 
  geom_boxplot() + 
  labs(x = "Species") + 
  # Manually add our labels 
  scale_x_discrete(labels = c(label1, label2, label3))

