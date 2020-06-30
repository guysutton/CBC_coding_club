###########################################################
###########################################################
# - CBC Coding Club
# - Tutorial #9: ggplot part #1
# - Rhodes Universtiy
# - Script written: 23/06/2020
# - By: Guy F. Sutton
###########################################################
###########################################################

###########################################################
# Aims:
# - 1. Make a scatterplot
# - 2. Make a lineplot
# - 3. Make a boxplot
# - 4. Make plots by groups 
# - 5. Edit plots to look nice(r)
# - 6. Save a high-quality figure to your PC
############################################################

# Load required packages ---------------------------------------------

library(tidyverse)

# Load and clean data ------------------------------------------------

# Load dataset 
insect_data <- readxl::read_excel("./data_raw/cochineal_fitness_data_double.xlsx") 

# Check to make sure data imported properly
head(insect_data)

# Clean up the data frame 
insect_data <- insect_data %>%
  mutate(plant_sp = as.factor(plant_sp),
         lineage = as.factor(lineage),
         crawlers_total = crawlers_start + crawlers_final)
head(insect_data)


# Set global ggplot theme --------------------------------------------

# All the plots will automatically take on my custom theme
theme_set(theme_classic() +
            theme(panel.border = element_rect(colour = "black", 
                                              fill = NA),
                  axis.text = element_text(colour = "black"),
                  axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), 
                                                            "mm")),
                  axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), 
                                                            "mm")),
                  legend.position = "none"))


# ggplot basics ------------------------------------------------------

# The basic functionality of a ggplot 
ggplot(<DATA>, aes(<MAPPING>)) +
  <GEOM>
  
# data = your data-frame
# mapping = your x and y variables, and any relevant groups (e.g. treatments)
# geom = shape of your graph (e.g. boxplot, lineplot)
  
# 1. Histogram (frequency distribution) ------------------------------

# Plot one continous variable 
# - e.g. Visualise distribution of number of crawlers made by females
ggplot(data = insect_data, aes(x = crawlers_final)) +
  geom_histogram()

# 2. Scatterplot -----------------------------------------------------

# Plot two continous variables 
# - e.g. Does mass at start of experiment correlate with mass at the end?
ggplot(data = insect_data, aes(x = mass_start,
                               y = mass_final)) +
  geom_point()

# 3. Barplot ---------------------------------------------------------

# Plot one categorical variable and one continous variable
# - e.g. Does the plant reared upon influence insect fecundity? 
ggplot(data = insect_data, aes(x = plant_sp,
                               y = crawlers_total)) +
  geom_col()

# First, let's calculate mean fecundity
mean_fec <- insect_data %>%
  group_by(plant_sp) %>%
  dplyr::summarise(mean_crawlers = mean(crawlers_final))

# Then, we can plot the graph we hoped for 
ggplot(data = mean_fec, aes(x = plant_sp, 
                            y = mean_crawlers)) +
  geom_col()

# 4. Boxplot ------------------------------------------------------------

# Plot one categorical variable and one continous variable
# - e.g. Does the plant reared upon influence insect fecundity? 
ggplot(data = insect_data, aes(x = plant_sp,
                               y = crawlers_total)) +
  geom_boxplot()

# Adding raw data onto boxplot
ggplot(data = insect_data, aes(x = plant_sp,
                               y = crawlers_total)) +
  geom_boxplot() +
  geom_jitter()

# 5. Grouped barplot -------------------------------------------------

# First, let's calculate mean and sd of fecundity
mean_fec <- insect_data %>%
  group_by(plant_sp, lineage) %>%
  dplyr::summarise(mean_crawlers = mean(crawlers_final),
                   sd_crawlers = sd(crawlers_final)) %>%
  dplyr::mutate(lower_sd = mean_crawlers - sd_crawlers,
                upper_sd = mean_crawlers + sd_crawlers) %>%
  dplyr::mutate(lower_sd = case_when(
    lower_sd < 0 ~ 0,
    lower_sd > 0 ~ lower_sd
  ))

# Then, we can plot the graph we hoped for 
# - x = ...argument for your main grouping
# - fill = ... argument for your sub-grouping 
ggplot(data = mean_fec, aes(x = plant_sp, 
                            y = mean_crawlers,
                            fill = lineage)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = lower_sd,
                    ymax = upper_sd),
                position = "dodge") +
  theme(legend.position = "right")

# 6. Grouped boxplot -------------------------------------------------

# Boxplot uses raw data, so no need to summarise beforehand. 

# Plot the graph we hoped for 
# - x = ...argument for your main grouping
# - fill = ... argument for your sub-grouping 
ggplot(data = insect_data, aes(x = plant_sp, 
                               y = crawlers_final,
                               fill = lineage)) +
  geom_boxplot() +
  theme(legend.position = "right")


# 7. Making your graph look pretty -----------------------------------

# Base graph
ggplot(data = insect_data, aes(x = plant_sp, 
                               y = crawlers_final, 
                               fill = lineage)) +
  geom_boxplot() +
  theme(legend.position = "right") +
  # Change x and y-axis labels and legend title
  labs(x = "Plant species",
       y = "No. of crawlers",
       fill = "Lineage",
       # Easy way to add panel labels
       subtitle = "(a)") +
  # Change colour of boxes/bars/points manually
  # Number of colours = no. of levels in sub-grouping (fill)
  # Here, we want 3 colours for foll - single, multiple, outcrossed
  scale_fill_manual(values = c("white", "grey80", "grey60")) +
  # Change x/y axis range and tick marks 
  # Breaks = seq =
  # First number = minimum y-value
  # Second number = maximum y-value
  # Distance between ticks on y-axis
  scale_y_continuous(breaks = seq(0, 125, 25),
                     limits = c(0, 125)) +
  # Change x-axis labels to italics (for species names)
  theme(axis.text.x = element_text(face = "italic"))

# 8. Grouped scatterplot/linegraph -----------------------------------

# Revisit basic graph where we plot two continous variables 
# - e.g. Does mass at start of experiment correlate with mass at the end?
ggplot(data = insect_data, aes(x = mass_start,
                               y = mass_final)) +
  geom_point()

# What if we wanted to look at whether relationship differed by plant species?
ggplot(data = insect_data, aes(x = mass_start,
                               y = mass_final,
                               #shape = lineage,
                               colour = lineage)) +
  geom_jitter() + # Change to jitter - overlapping points
  geom_smooth(method = "lm", se = F) + 
  # Change colour of boxes/bars/points manually
  # Number of colours = no. of levels in sub-grouping (fill)
  # Here, we want 3 colours for foll - single, multiple, outcrossed
  scale_colour_manual(values = c("gray80", "gray60", "gray40")) +
  # Change x and y-axis labels and legend title
  labs(x = "Initial female mass (g)",
       y = "Final female mass (g)",
       colour = "Lineage",
       # Easy way to add panel labs
       subtitle = "(b)") +
  # Plot different panels for different levels within plant_sp
  # i.e. plot a different panel for stricta and ficus-indica
  facet_wrap(~ plant_sp, ncol = 1) +
  theme(legend.position = "right") 


# 10. Save a high-quality graph --------------------------------------

ggsave("./figures/fig_1_boxplot_crawlers_by_lineage_plant_species.png",
       # Quality/resolution of fig - 600 = publication quality
       dpi = 600,
       # Control figure height and width
       # Usually have to play around with this 
       height = 4, 
       width = 5)

# 11. Putting multiple graphs into one plot --------------------------

# We have to store the graphs we want into variables/objects. 

# Store first graph
graph_1 <- ggplot(data = insect_data, aes(x = plant_sp, 
                               y = crawlers_final, 
                               fill = lineage)) +
  geom_boxplot() +
  theme(legend.position = "right") +
  # Change x and y-axis labels and legend title
  labs(x = "Plant species",
       y = "No. of crawlers",
       fill = "Lineage",
       # Easy way to add panel labs
       subtitle = "(a)") +
  # Change colour of boxes/bars/points manually
  # Number of colours = no. of levels in sub-grouping (fill)
  # Here, we want 3 colours for foll - single, multiple, outcrossed
  scale_fill_manual(values = c("gray80", "gray60", "gray40")) +
  # Change x/y axis range and tick marks 
  # Breaks = seq =
  # First number = minimum y-value
  # Second number = maximum y-value
  # Distance between ticks on y-axis
  scale_y_continuous(breaks = seq(0, 125, 25),
                     limits = c(0, 125)) +
  # Change x-axis labels to italics (for species names)
  theme(axis.text.x = element_text(face = "italic"))
graph_1

# Store second graph
graph_2 <- ggplot(data = insect_data, aes(x = mass_start,
                                          y = mass_final,
                                          #shape = lineage,
                                          colour = lineage)) +
  geom_jitter() + # Change to jitter - overlapping points
  geom_smooth(method = "lm", se = F) + 
  # Change colour of boxes/bars/points manually
  # Number of colours = no. of levels in sub-grouping (fill)
  # Here, we want 3 colours for foll - single, multiple, outcrossed
  scale_colour_manual(values = c("gray80", "gray60", "gray40")) +
  # Change x and y-axis labels and legend title
  labs(x = "Initial female mass (g)",
       y = "Final female mass (g)",
       colour = "Lineage",
       # Easy way to add panel labs
       subtitle = "(b)") +
  facet_wrap(~ plant_sp, ncol = 1) +
  theme(legend.position = "right") +
  geom_text(text = c("a"),
            x = 1, 
            y = 25)
graph_2

# Put the two graphs together 
library(cowplot)
plot_grid(graph_1, graph_2)
ggsave("./figures/fig_2_multiple_plots.png",
       # Quality/resolution of fig - 600 = publication quality
       dpi = 600,
       # Control figure height and width
       # Usually have to play around with this 
       height = 8, 
       width = 14)
