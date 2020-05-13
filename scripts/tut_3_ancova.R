###########################################################
###########################################################
# - Linear modelling in R 
# - CBC Coding Club
# - Tutorial #2: Linear models in R (ancova) 
# - Rhodes Universtiy
# - Script written: 03/05/2020
# - By: Guy F. Sutton
###########################################################
###########################################################

# Load required packages
library(tidyverse)


# I like my plots to look a certain way
theme_set(theme_classic() +
            theme(panel.border = element_rect(colour = "black", fill = NA),
                  axis.text = element_text(colour = "black"),
                  axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                  axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
                  legend.position = "none"))

###
# - Data import and cleaning
###

# Load dataset 
insect_data <- readxl::read_excel("./data_raw/cochineal_fitness_data.xlsx") 

# Check to make sure data imported properly
head(insect_data)
str(insect_data)

# Clean up the data frame 
insect_data <- insect_data %>%
  mutate(lineage = as.factor(lineage),
         crawlers_total = crawlers_start + crawlers_final)

###
# - What is the research question?
###

# Here, we set-up an experiment where we inoculated female cochineal insects 
# onto cactus plants of different lineages (single, multiple, outcrossed).
# - We measured initial female body mass, and measured fecundity.
# - Single - all insects used originate from one female (WAAINEK CULTURE)
# - Multiple- all insects used originate from 20 females (WAAINEK CULTURE)
# - Outcrossed - all insects used originate from field-collected females

# - I expect that genetic diversity is lowest for SINGLE and highest for OUTCROSSED.
# - Hypothesis: Outcrossed treatment will make the most crawlers, single treatment
#               will make the fewest crawlers. 

# RESEARCH QUESTION:
# - Do more genetically diverse cochineal insects produce more crawlers? 
   
###
# Step 1 - Explore the data (visualise)
###

# (1) Does no. of crawlers produced differ between lineage treatments? 
ggplot(data = insect_data, aes(x = lineage,
                               y = crawlers_total)) +
  geom_boxplot() + 
  labs(x = "Lineage (treatment)",
       y = "No. of crawlers produced")

# Hmmm. Maybe. Alot of variation in the data. 

# (2) Does initial female body mass have an affect on no. of crawlers produced?
ggplot(data = insect_data, aes(x = mass_start,
                               y = crawlers_total)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Initial female body mass (g)",
       y = "No. of crawlers produced")

# Yip. Bigger females produce more crawlers. 

# (3) Did initial female body mass differ amongst the treatments? 
ggplot(data = insect_data, aes(x = lineage,
                               y = mass_start)) +
  geom_boxplot() +
  labs(x = "Lineage (treatment)",
       y = "Initial female body mass (g)")

### With a small sample size (n = 5 per treament), it is difficult to
### make any strong inferences. However, we can see that initial female
### body mass may have been higher in the single treatment than in the other 
### two treatments, but it is unclear to me. 

# Plot the actual data points (this makes the pattern more clear)
ggplot(data = insect_data, aes(x = lineage,
                               y = mass_start)) +
  geom_boxplot() +
  geom_point() +
  labs(x = "Lineage (treatment)",
       y = "Initial female body mass (g)")

### That is a lot more clear. We can see that one of the females used 
### in the 'single' treatment was about 4x larger than most of the other 
### females used across all treatments, and that another two females 
### (the two that weighed ~ 0.005), were about double the size of the other 
### females used. 

# (4) Is there a potential interaction between initial female body mass
#     and lineage treatments? 
#     - i.e. Did initial female body mass affect total crawler production
#       differently for the three treatments?
ggplot(data = insect_data, aes(x = mass_start,
                               y = crawlers_total,
                               color = lineage)) + 
  geom_point() + 
  geom_smooth(aes(group=lineage), 
              method = "lm", 
              se = FALSE) + 
  theme(legend.position = "right")

### It appears that bigger females consistently produced more crawlers. 
### But, bigger females appeared to, on average, produce more 
### crawlers in the outcrossed treatment than in the other two treatments. 
### This will be important to test later on. 

###
# Step 2 - Run statistical tests 
###

# - 1. Were females different sizes when applying the treatments?
#      - Initial body mass certainly looked to be larger in some lineages. 
#      - Perform an analysis of variance to compare 
fem_diff_aov <- aov(mass_start ~ lineage, data = insect_data)

# Get type II sum of squares 
car::Anova(fem_diff_aov, type = 2)

### P > 0.05 indicates that, on average, female weight was not significantly
### different between the lineage treatments, at the start of the experiment. 
### That is very good news!!! (But, I wouldn't trust this with only a few reps 
### for each treatment - there is too much variance in the data currently 
### to rely on the ANOVA). 

# - 2. Did the number of crawlers produced differ amongst lineages (treatments)?
#      - This is the main research question 
craw_aov <- aov(crawlers_total ~ lineage, data = insect_data)

# Get type II sum of squares 
car::Anova(craw_aov, type = 2)

### Lineage did not have a significant effect on total crawlers produced. 
### But, we haven't factored in the potentially confounding influence of 
### the initial body mass of the females used in the 
### different lineage treatments. 

# - 3. Did the number of crawlers produced differ amongst lineage treatments, 
#      after accounting for different sized females being used in 
#      the different lineage treatments? 
#      - i.e. after accounting for different sized females, does lineage still
#        have an impact on number of crawlers produced? 
#      - Time to rip out the analysis of covariance (ancova)
#      - Here, we are going to test whether lineage had an effect on 
#        the number of crawlers produced, after controlling for 
#        female body size at the start of the experiment (mass_start)
mod_int <- aov(crawlers_total ~ lineage * mass_start, data = insect_data)
summary(mod_int)

# Get Type III errors (NOT TYPE II LIKE ANOVA)
# - Details of type I, II and III errors beyond scope here. 
# - Need to be very careful about choice of errors!!! 
car::Anova(mod_int, type="III")

# Look at the difference in output when type II errors used like ANOVA
car::Anova(mod_int, type="II")

# - When we use type III (correct choice) - lineage is not significant, 
#   after controlling for starting mass.
# - BUT, When we use type II - lineage is significant, after controlling
#   for starting mass. 

# Here, we see there was a significant interaction between lineage and 
# mass_start - meaning that the mass of females was different between
# the different lineages (look at the lineage:mass_start line P < 0.05). 
# - Look at the lineage line, we can see that after controlling for the differences
#   in female body mass at the start of the experiment (mass_start),
#   there is no significant difference in crawler output produced
#   on the different lineages. 

# - (4) How much variation was accounted for by your main predictor (lineage)?
COVmodelError <- aov(crawlers_total ~ lineage + Error(mass_start), 
                     data = insect_data)
summary(COVmodelError)
car::Anova(COVmodelError$Within, type = 2)
sjstats::anova_stats(car::Anova(COVmodelError$Within, 
                                type = 3)) %>% 
  dplyr::select(1:7)

### etasq = proportion of total variation accounted for by a given factor
### So, 21.2% (0.212) of the variation in the number of crawlers 
### being produced was accounted for by 'lineage'. 

###
# Step 3 - Make a graph of your predictions 
###

# Check data
head(insect_data)

# Refit as an 'lm'
mod.lm <- lm(crawlers_total ~ lineage * mass_start, 
             data = insect_data)

# Make new predictions for our model 
newdata <- expand.grid(lineage = levels(insect_data$lineage), 
                       mass_start = seq(min(insect_data$mass_start), 
                                        max(insect_data$mass_start),
                               l = 10))
fit <- predict(mod.lm, newdata = newdata, interval = "confidence")
fit <- data.frame(newdata, fit)
part.obs <- cbind(insect_data, 
                  part.obs = fitted(mod.lm) + resid(mod.lm))

# Reorder lineage 
fit <- fit %>%
  mutate(lineage = fct_relevel(lineage, 
                               "Single", 
                               "Multiple",
                               "Outcrossed"))
part.obs <- part.obs %>%
  mutate(lineage = fct_relevel(lineage, 
                               "Single", 
                               "Multiple",
                               "Outcrossed"))

# Plot the final graph
ggplot(data = fit, aes(x = mass_start, 
                       y = fit, 
                       group = lineage)) + 
  geom_point(data = part.obs, aes(y = part.obs, 
                                  group = lineage,
                                  colour = lineage)) + 
  geom_line(aes(colour = lineage)) + 
  geom_ribbon(aes(ymin = lwr, 
                  ymax = upr, 
                  fill = lineage), 
              alpha = 0.2) + 
  scale_y_continuous("No. of crawlers", 
                     breaks = seq(0, 250, 50),
                     limits = c(0, 250)) +
  scale_x_continuous("Initial female body mass (g)") + 
  theme(legend.position = "right") +
  labs(fill = "Lineage") +
  guides(colour = FALSE) 

###
# Step 6 - Write-up your results  
###

# Everyone has a different style of writing. 
# Below I give some examples. 
# This is my approach, it does not have to be yours. 

# NOTE: Ancova is often discussed with regards to homogeneity of slopes
#       - If interaction term is significant = heterogenous slopes
#       - If interaction term is n.s. = homogenous slopes. 

# A simple example:
# After controlling for differences in female body mass at the start of the 
# experiment, lineage did not have a significant influence on crawler output 
# (F = 2.69, P = 0.121). 

# TAKE HOME: YOU REALLY JUST NEED TO GO ADD MORE SAMPLES TO YOUR DATA 
#            BEFORE DOING ANYTHING HERE. 
#            - n = 5 is too small
#            - The plot shows that there may infact be a significant effect.

# *** BIIIIIG TAKE HOME MESSSAGE:
#     - Approximately 20% of the variation in crawler output 
#       was due to lineage, after controlling for differences in body mass.
#       - Use the biological significance of the result to guide you. 
#       - e.g. if the point of your study was to determine which cactus to 
#         mass rear a cochineal insect on, 2% difference could 
#         mean 1 million insects versus 0.8 million insects per year
#         - Statsitical significance aside, 
#           that is a BIOLOGICALLY / ECONOMICALLY SIGNIFICANT result! 
 








