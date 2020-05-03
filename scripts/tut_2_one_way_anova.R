###########################################################
###########################################################
# - Linear modelling in R 
# - CBC Coding Club
# - Tutorial #2: Linear models in R (one-way anova) 
# - Rhodes Universtiy
# - Script written: 02/05/2020
# - By: Guy F. Sutton
###########################################################
###########################################################

# Load required libraries to run script 
# You may need to install the package first
# install.packages("tidyverse") - remove the # from this row and click run. 
library(tidyverse)

# I like my plots to look a certain way
theme_set(theme_classic() +
            theme(panel.border = element_rect(colour = "black", fill = NA),
                  axis.text = element_text(colour = "black"),
                  axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                  axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
                  legend.position = "none"))

# Load required libraries 
library(tidyverse)

# Import data
data <- readxl::read_excel("./data_raw/poisson_data.xlsx")

# Make temperature a factor variable
data <- data %>%
  mutate(temp = as.factor(temp))

# Check data imported properly
str(data)
head(data)

# What is the situation?
# Ran an experiment to determine
# - Does temperature influence insect reproduction?
# - Does insect body mass influence insect reproduction? 

# Temperature = temp - 4 levels:
# - 15 degrees, 20 degrees, 25 degrees, 30 degrees
# - factor variable 

# Insect body mass = adult_mass
# - numeric variable

# No of larvae produced by insects = larvae 
# - numeric variable

###
# Research question #2 - Does temperature effect insect fecundity? 
###

# Statistical analysis: 
# - Linear regression, with one categorical predictor
# - Otherwise known as one-way ANOVA

###
# Step 1 - Explore the data (visualise)
###

# Firstly, visualise relationship between predictor and response
ggplot(data = data, aes(x = temp,
                        y = larvae,
                        group = temp)) +
  geom_boxplot() +
  scale_x_discrete("temp",
                   labels = c("15", "20", "25", "30"))

###
# - Step 2 - Run a linear model
###

# Specify a linear model 
# - We use the 'aov' function to tell R to run an ANOVA
# - We then specificy our response var (larvae),
# - We then specificy our predictor var (temp),
#   (predictors go to the right of ~ sign). 
# - We also have to specify where R must look for this data
#   using the 'data = ...' argument 
mod1 <- aov(larvae ~ temp,
           data = data)

# Print ANOVA table
car::Anova(mod1, type="III")

# More intuitive to break it down into the groups
mod1 <- lm(larvae ~ temp,
           data = data)

# Print the summary of the model
summary(mod1)

###
# Step 3 - Model diagnostics 
###

# Before looking at the outcome of our model,
# we should first check whether the model we specified is 
# a good fit to the data or not. 
# We do this by performing model diagnostics 

# Basically, here we are going to check whether our 
# data and model meet the assumptions required to run a linear model.

# 1. Independence of data points
#    - This cannot be checked per se. 
#    - This is a property of your experimental design
#    - Make sure you design your experiment properly. 
#    - If you have non-independence, there are ways to deal 
#      it, will deal with this in a latter video (GLMM's)!!! Fun!!!

# 2. Are the residuals normally distributed?
#    - Often this assumption is applied to the data,
#      i.e. are your data / response normally distributed?
#    - This is not correct.
#    - The assumption is actually that your residuals are normally 
#      distributed. 
#      - Residuals are how far each data point deviates 
#        from the expected value / trend. 
#    - Residuals represent variation in the data that your model can't account for. 
#    - We use residual plots to discover patterns (QQ plot).
#    - We don't want to see any systematic patterns in residuals,
#      should be a straight 1:1 line. 
plot(mod1, which = 2)

# *** Alternative approach, probably more interpretable
resids <- resid(mod1) # extract model residuals
hist(resids) # plot a histogram

# Here, we can see that the points in the tails of the line do not deviate from the dashed line
# - This is an indication that the residuals are normally distributed.
# - As such, the model does capture the variation in our data well, notably
#   at the extremities of the dataset 
# - The histogram further supports that our residuals are normally distributed. 
# - TAKE HOME: ANOVA may be a good model to use for this analysis. 

# 3. Homogeneity of variance
#   - Testing the assumption of equality of variances. 
#   - Residuals vs fitted - we don't want to see any pattern.
#   - Here, we wan't to see that red line lie on the y = 0 line.
#   - You really don't want to see your white circles have any pattern (i.e. funnel, U-shape)
plot(mod1, which = 1)

# Here, the red line varies a bit, indicating that the variance varies slightly, 
# but there is nothing for concern. 
# You would be happy with this as your model satifies the homogeneity of variance assumption. 

# TAKE-HOME: ANOVA IS PROBABLY APPROPRIATE. 

###
# Step 4 - Interpreting model output
###

# - (1) Is my treatment/factor significant?
#       Look at the row for your predictor variable (here: temp)
#       Look at the P-value: P <0.05 means this variable is significant 
mod1 <- aov(larvae ~ temp,
           data = data)
car::Anova(mod1, type="II")

#       Here, we see that temp has a P-val of 0.008 (< 0.05)
#       Therefore, we can say that there is a significant association between
#       temperature and no. of larvae produced. 

# - (2) Which levels within your factor differ from each other? 
#       - e.g. are the number of larvae produced at t = 15 different
#         from t = 20, 25, 30, none, all? 
#       - We can use post-hoc tests (e.g. Tukey tests)
#       - Two options shown below

#   (2.1) Standard Tukey test 
tukey_res <- TukeyHSD(mod1)
tukey_res 

#   (2.2) emmeans Tukey test 
#         - My preference, provides confidence intervals 
emm1 <- emmeans(mod1, 
                specs = pairwise ~ temp,
                adjust = "tukey")
emm1

# Get 95% confidence intervals 
emm1$contrasts %>%
  confint()

# Combine both tables 
emm1$contrasts %>%
  summary(infer = TRUE) 

###
# Step 5 - Make a graph of your results 
###

# Much easier than plotting the 'lm' results 
ggplot(data = data, aes(x = temp,
                        y = larvae,
                        group = temp)) +
  geom_boxplot() +
  scale_x_discrete("Temperature (ÅãC)",
                   labels = c("15", "20", "25", "30")) +
  labs(y = "No. of larvae produced") +
  # Add signficance stars (use the emmeans output/tukey)
  # Use x = "..." to indicate which group
  # x = 1 means first group,
  # x = 2 means second group... 
  # Manually play around with y-values
  annotate("text", x = 1, y = 25, label = "a") +
  annotate("text", x = 2, y = 32, label = "b") +
  annotate("text", x = 3, y = 37, label = "b") +
  annotate("text", x = 4, y = 41, label = "b")

###
# Step 6 - Write-up your results  
###

# Everyone has a different style of writing. 
# Below I give some examples. 
# Any text within [] is me explaining, and not actually included in the text. 
# This is my approach, it does not have to be yours. 

# (1) The standard example is:
# Temperate had a significant influence on the number of larvae produced 
# (P < 0.05)

# - In my opinion, this is pretty poor. 
# - It really tells us nothing. 

# (2) Improvement on (1):
# Temperature had a significant influence on the number of larvae produced 
# (F = 4.201; P < 0.05). [where F = F-value, not beta like lm]. 

# (3) Actually explain your results.
#     - This is my prefered approach. 

# Temperature had a significant influence on the number of larvae 
# produced (F = 4.201; P < 0.05). 
# Fewer larvae were produced at 15 degrees than 20, 25 and 30 degrees (Fig. 1).
# Similar numbers of larvae were produced at 20, 25 and 30 degrees. 

# - We have told the reader exactly how the predictor effects response,
#   we have reported uncertainty, and referred the reader to our awesome graph!!! 

# Your results for your paper are now ready to go!!!