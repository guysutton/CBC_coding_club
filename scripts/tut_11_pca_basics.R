###########################################################
###########################################################
# - CBC Coding Club
# - Tutorial #11: Basics of PCA - tidyverse style
# - Rhodes Universtiy
# - Script written: 07/07/2020
# - By: Guy F. Sutton
###########################################################
###########################################################

###########################################################
# Aims:
# - 1. Understand what a PCA is used for  
# - 2. Run a simple PCA analysis
# - 3. Interpret PCA output
# - 4. Understand how PCA can be used in further analyses
#      NB: You are not expected to understand the additional modelling
#          exercise per se: it is just there to illustrate
#          how you would use the data obtained from PCA 
############################################################

# Acknowledgements:
# - Most of the code and ideas in this script have been adapted and 
#   gleaned from three amazing blogposts by Julia Silge and 
#   Allison Horst
# https://juliasilge.com/blog/multinomial-volcano-eruptions/
# https://juliasilge.com/blog/cocktail-recipes-umap/
# https://allisonhorst.github.io/palmerpenguins/articles/articles/pca.html 

# Load required packages ---------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, 
               mapr, 
               tidyr, 
               stringr,
               here,
               corrr,
               GGally,
               recipes,
               tidytext,
               tidymodels,
               themis,
               ranger,
               yardstick,
               vip)

# The data we will use comes from Allison Horst's 'palmerpenguins' package
# which we must install remotely
remotes::install_github("allisonhorst/palmerpenguins")

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

######################################################################
# Raw data -----------------------------------------------------------
######################################################################

# Today, we are going to use some data that is already loaded into R. 
# Let's store the data in a variable called 'penguins'
penguins <- palmerpenguins::penguins_raw
head(penguins)

# Clean the data a bit 
penguins_clean <- penguins %>%
  # All purpose cleaning of column names
  janitor::clean_names() %>%
  # Select only columns of interest
  # We rename two columns below by new_name = old_name
  dplyr::select(species,
                island, 
                # Rename to bill instead of culmen for next two variables
                bill_length_mm = culmen_length_mm,
                bill_depth_mm = culmen_depth_mm,
                flipper_length_mm,
                body_mass_g,
                sex) %>%
  # Make all character columns into factors
  dplyr::mutate(across(where(is.character), as.factor)) %>%
  # Change species column levels to just a common name
  dplyr::mutate(species = case_when(
    species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
    species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
    species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo")) %>%
  # Drop any rows with missing data (e.g. NA)
  tidyr::drop_na(bill_length_mm:body_mass_g, sex)
head(penguins_clean)
glimpse(penguins_clean)

######################################################################
# Exploratory data analysis ------------------------------------------
######################################################################

# How many penguin individuals do we have for each 
# species and island? 
# Use .drop = FALSE to keep species x island combo with 0 penguins
penguins_clean %>%
  count(species, island, .drop = FALSE)

# So the data is a set of size and body morphology measurements on a range 
# of penguins belonging to different species, from different islands 
# and genders. 

# Let's check to see whether the relationships between 
# the variables are linear(-ish)
# e.g. is flipper_length_mm linearly correlated with body_mass_g? 
penguins_clean %>%
  # Select the variables to plot
  dplyr::select(species, 
                bill_length_mm:body_mass_g) %>% 
  # Plot the variables
  # Numeric variables go within the columns(...) argument to plot
  # correlations between them
  GGally::ggpairs(., aes(color = species),
                  columns = c("flipper_length_mm", 
                              "body_mass_g", 
                              "bill_length_mm", 
                              "bill_depth_mm"))

# Looks pretty good. Linear relationships all over the joint. 

######################################################################
# Principal component analysis ---------------------------------------
######################################################################

# What is a PCA?
# - Very simply, PCA is a statistical technique that allows
#   us to reduce the information contained in many variables
#   into a smaller number of summary variables (or indices)
# - This is usually done to make a large amount of data more easily 
#   interpretable and analysable. 

# - e.g. (1) Often researchers will collect a whole range of water chemistry
#   data (e.g. C, N, K), and you may have 20-odd variables in the end.
#   - In this case, you cannot fit each variable to a model (e.g. GLM),
#     to try predict Egeria dense distributions for example, 
#     unless you had 1000's of data points (remeber: the more variables you
#     have in a model, the more data you need).
#   - Instead, we can reduce the water chemistry data down to maybe 2 or 3
#     variables representing gradients in water chemistry. 
#     - e.g. Your first variable may be a C:N gradient, and 
#            your second variable may be a K/P gradient... 

# - e.g. (2) Other researchers may take many morphological measurements for 
#   hundreds of specimens when trying to morphologically describe new species.
#   - In my field of wasp taxonomy, researchers may measure 100+ morphological 
#     characters per specimen. 
#   - So, often they will run a PCA and reduce these measurements down to
#     maybe 3-10 axes which makes life much easier. 
#     - e.g. The first variable may now be gradient of antennal segments 
#            (e.g. separating species based on how many segments in their 
#            antennae), while the second variable could separate species
#            based on the distance between eyes, and the third variable 
#            representing a gradient of ovipositor lengths, and so on...

# Here, WeÅfll use the recipes package from tidymodels to perform a 
# principal component analysis (PCA).
# - Beforehand, we need to pre-process the data for PCA:
#   - (1) remove any NA values - PCA cannot handle any NA values
#   - (2) center all predictors,
#   - (3) scale all predictors.
#     - Steps 2 and 3 make the data have equal variance (variance = 1)
#     - This removes the issue with scales of different variables
#     - e.g. if body mass ranges from 10g to 30g,
#       and flipper_length_mm ranges from 100mm to 500mm, 
#       PCA will undoubtedly say flipper length is NB, 
#       purely because the units are larger. 


# Step 1: Recipe  ----------------------------------------------------

# Pre-process data for our PCA 
pca_recipe <- 
  # Provide the data frame name
  recipe(~., data = penguins_clean) %>% 
  # Any columns that aren't numeric (i.e. charatcer variables)
  # need to be defined here
  update_role(species, island, sex, new_role = "id") %>% 
  # Center and scale variables 
  step_normalize(all_predictors()) %>%
  # Starts the PCA process - doesn't actually do anything yet 
  step_pca(all_predictors()) 
pca_recipe


# Step 2: Run PCA ----------------------------------------------------

# Now actually run the PCA
pca_prep <- prep(pca_recipe)
pca_prep

# Extract the results from the PCA in tidy-format
tidied_pca <- tidy(pca_prep, 2)
tidied_pca


# Step 3: Visualise PCA results --------------------------------------

# Visualise which variables are correlated with which axes? 
# These values represent factor loadings. 
# The greater the value, the more strongly that variable is 
# correlated with that PC axis, and the sign gives the direction.
tidied_pca %>%
  filter(component %in% paste0("PC", 1:5)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL)

# Let's look ath the loadings on the different PCA axes
tidied_pca %>%
  filter(component %in% paste0("PC", 1:4)) %>%
  group_by(component) %>%
  top_n(8, abs(value)) %>%
  ungroup() %>%
  mutate(terms = reorder_within(terms, abs(value), component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  scale_fill_manual(values = c("#b6dfe2", "#0A537D")) +
  scale_y_reordered() +
  labs(x = "Absolute value of contribution",
       y = NULL, 
       fill = "Positive?") +
  theme(legend.position = "right")

# We must look at these loadings to decide on which variables differentiate 
# each PC axes.
# PC1 is differentiated by flipper_length vs bill_depth - i.e. species 
# with longer flippers, have smaller bill widths. 

# Step 4: How many PC axes do we need?  ------------------------------

# Extract variance explained by each PC axis
sdev <- pca_prep$steps[[2]]$res$sdev
percent_variation <- sdev^2 / sum(sdev^2)

# Convert values into a df
perc_df <- tibble(
  component = unique(tidied_pca$component),
  percent_var = percent_variation, 
  percent_cum = cumsum(percent_variation)) %>%
  dplyr::mutate(component = fct_inorder(component)) %>%
  # Make graph
  ggplot(aes(component, percent_var)) +
  # Plot columns for each PC axis
  geom_col(fill = "#b6dfe2") +
  # Plot a line of cumulative percentage
  geom_line(aes(y = percent_cum, group = 1),
            colour = "red") + 
  # Plot the points for cumulative percentage too
  geom_point(aes(y = percent_cum, group = 1), 
             colour = "red") + 
  # Add an 80% cumulative percent mark
  # Usually, most people include as many PC axes required to explain 
  # 80% cumulative variation
  geom_hline(yintercept = 0.8, linetype = "dashed") + 
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = NULL, 
       y = "Percent variance explained \nby each PCA component")
perc_df

# In this case, two PC axes is sufficient (PC1 and PC2). 

######################################################################
# Plot the actual PCA graph ------------------------------------------
######################################################################

# Define arrow style
arrow_style <- arrow(length = unit(.05, "inches"),
                     type = "closed")

# Plot PCA 
pca_plot <-
  # Get the PC axis scores
  juice(pca_prep) %>%
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color = species), 
             alpha = 0.8, 
             size = 2) +
  scale_colour_manual(values = c("seagreen3", "#0A537D", "deepskyblue1")) +
  theme(legend.position = "right") +
  labs(colour = "Penguin species")
pca_plot

# Now let's add the PCA biplot
# Arrows indicate direction and magnitude of variable contributions

# First, we need to get the factor loadings again, convert to wide format
head(tidied_pca)
pca_wider <- tidied_pca %>% 
  tidyr::pivot_wider(names_from = component, id_cols = terms)
head(pca_wider)

# Now we plot the biplot over the PCA plot
pca_plot +
  # Add arrows
  geom_segment(data = pca_wider,
               aes(xend = PC1, yend = PC2), 
               x = 0, 
               y = 0, 
               arrow = arrow_style) + 
  # Add variable names
  geom_text(data = pca_wider,
            aes(x = PC1, 
                y = PC2, 
                label = terms), 
            hjust = 0, 
            vjust = 1,
            size = 5)

# Interpretation:
# (1) The first axis represents an axis of penguin body mass. 
#     - This axis separates our species into larger (Gentoo) and smaller 
#       penguin species (Chinstap and Adelie), based on their 
#       respective body masses and flipper lengths. 
# (2) The second PC axis represents an axis of bill characteristis. 
#     - This axis separates our species (not very well) into species 
#       with larger (Adelie) and smaller (Chinstrap) bills. 

# While we don't need to look at any additional PC axes, 
# let's add PC3 into the mix, and see if we can differentiate Chinstrap 
# vs Adelie penguins. 

# Plot PCA 
pca_plot <-
  # Get the PC axis scores
  juice(pca_prep) %>%
  ggplot(aes(PC2, PC3)) +
  geom_point(aes(color = species), 
             alpha = 0.8, 
             size = 2) +
  scale_colour_manual(values = c("seagreen3", "#0A537D", "deepskyblue1")) +
  theme(legend.position = "right") +
  labs(colour = "Penguin species")
pca_plot

# Now we plot the biplot over the PCA plot
pca_plot +
  # Add arrows
  geom_segment(data = pca_wider,
               aes(xend = PC1, yend = PC2), 
               x = 0, 
               y = 0, 
               arrow = arrow_style) + 
  # Add variable names
  geom_text(data = pca_wider,
            aes(x = PC1, 
                y = PC2, 
                label = terms), 
            hjust = 0, 
            vjust = 1,
            size = 5)

# By adding PC3, we can clearly see that bill_depth can help us distinguish
# between Chinstrap and Adelie penguins. 

######################################################################
# What do we do now?  ------------------------------------------------
######################################################################

# Now that you have done your PCA, now what? 
# Well, as I said above, PCA is usually performed to reduce the number
# of variables in play down to a manageable amount of variables.
# e.g. You would usually take maybe 20 morphology measurements, you can't fit 
#      all of those as predictors in a model. 
#      So we can now extract the PC axis loadings, and use those as 
#      predictors in our model. 

# Let's extract the first three PC axes and then run a model to see 
# whether these axes (representative of morphological body traits)
# can accurately delineate penguin species.

# Extract axes 
penguin_df <-
  # Get the PC axis scores
  juice(pca_prep)
head(penguin_df)

# Before doing anything, we must remove rows with NA data 
penguin_df <- penguin_df %>%
  tidyr::drop_na()

# Now, we are going to run a multinomial regression using tidymodels
# Multinomial regression is an extension of binomial regression for when we
# have more than 2 outcomes (e.g. Adelie, Gentoo or Chinstrap penguins). 

# Step 1: Break data into bootstrap samples --------------------------
penguin_boot <- bootstraps(penguin_df)
head(penguin_boot)
head(penguin_df)

# Step 2: Set-up our multinomial regression --------------------------

# Because there are half Chinstraps as the other two species,
# we will use SMOTE upsampling to balance the dataset (details not NB now)
penguin_recipe <- recipe(species ~ ., data = penguin_df) %>%
  update_role(island, sex, new_role = "Id") %>%
  step_other(island) %>%
  step_other(sex) %>%
  step_dummy(island, sex) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_smote(species)
penguin_recipe


# Step 3: Prepare data for model ---------------------------------

penguin_prep <- prep(penguin_recipe)
juice(penguin_prep)


# Step 4: Define what model we will use ------------------------------

# Here, we tell R we want to use 'ranger' which is a random forest model
# implementation (machine learning model) that can handle multinomial 
# regression really well. 
rf_spec <- rand_forest(trees = 1000) %>%
  set_mode("classification") %>%
  set_engine("ranger")

penguin_wf <- workflow() %>%
  add_recipe(penguin_recipe) %>%
  add_model(rf_spec)
penguin_wf


# Step 5: Fit model to bootstrap samples -----------------------------

penguin_res <- fit_resamples(
  penguin_wf,
  resamples = penguin_boot,
  control = control_resamples(save_pred = TRUE))


# Step 6: Extract results --------------------------------------------

# Extract results
penguin_res %>%
  collect_metrics()

# Calculate confusion matrix to see how well our model predicted species
penguin_res %>%
  collect_predictions() %>%
  conf_mat(species, .pred_class)

# Gentoo was predicted correctly based on body morphology 100% of the time.
# Adelie was incorrectly classified (n = 11) as Chinstrap 1% of the time 
# Chinstrap was incorrectly classified (n = 13) as Adelie 2 % of the time 

# Which PC axes was significant? 
# THis is the multinomial version of variable importance 
rf_spec %>%
  set_engine("ranger", 
             importance = "permutation") %>%
  fit(
    species ~ .,
    data = juice(penguin_prep) %>%
      
      janitor::clean_names()
  ) %>%
  vip(geom = "point")

# Clearly PC1 was by far the most important variable to discriminate 
# amongst penguin species. 
# - Remember, PC1 was an axis of variation in penguin body size
#   (principally flipper length and body mass). 







