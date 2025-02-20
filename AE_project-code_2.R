allPackages <- c(
  "pROC", "stargazer", "haven", "MASS", "zoo", "aod", "htmltools",
  "dplyr", "psych", "summarytools", "corrplot", "oglmx",
  "glmtoolbox", "logistf", "mfx", "DescTools", "maxLik", "ggplot2",
  "ResourceSelection", "questionr", "sandwich", "lmtest", "pscl",
  "ucminf", "ordinal", "reshape", "generalhoslem", "brant",
  "LogisticDx", "gridExtra"
)

# Install missing packages
for(i in requiredPackages) {
  if(!require(i, character.only = TRUE)) install.packages(i)
}

# Load all required packages
for(i in requiredPackages) {
  library(i, character.only = TRUE)
}
################### read dataset##################

cellphone = read.csv2(file="CellPhone_train.csv", header=TRUE, sep=",")

###################checking Na ###############

colSums(is.na(cellphone)) %>% sort()
any(is.na(cellphone))
# dataset is clear and doesn't contain missing values


#---------------------------------------------------
str(cellphone)
#dependent variable
cellphone$price_range <- factor(cellphone$price_range, ordered = TRUE)
# Convert binary columns to factors
cellphone$clock_speed <- as.numeric(cellphone$clock_speed)
cellphone$m_dep <- as.numeric(cellphone$m_dep)
cellphone$blue <- as.factor(cellphone$blue)
cellphone$dual_sim <- as.factor(cellphone$dual_sim)
cellphone$four_g <- as.factor(cellphone$four_g)
cellphone$three_g <- as.factor(cellphone$three_g)
cellphone$touch_screen <- as.factor(cellphone$touch_screen)
cellphone$wifi <- as.factor(cellphone$wifi)
cellphone <- subset(cellphone, select = -ram)

# Check the structure again
str(cellphone)
################################################################################
#                       Explanatory Data Analysis
################################################################################

# Box plots

box1 <- ggplot(cellphone, aes(x = price_range, y = battery_power, fill = price_range)) + 
  geom_boxplot() + 
  labs(title = "Battery Power vs Price Range", x = "Price Range", y = "Battery Power") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red", "green", "orange"), guide = FALSE)

box2 <- ggplot(cellphone, aes(x = price_range, y = int_memory, fill = price_range)) + 
  geom_boxplot() + 
  labs(title = "Internal Memory vs Price Range", x = "Price Range", y = "Internal Memory") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red", "green", "orange"),, name = "Price Range",)

grid.arrange(box1, box2, ncol = 2)


# Scatter plots
scatter1 <- ggplot(cellphone, aes(x = battery_power, y = ram, color = price_range)) + 
  geom_point(alpha = 0.5) + 
  labs(title = "Battery Power vs RAM", x = "Battery Power", y = "RAM") +
  theme_minimal()

scatter2 <- ggplot(cellphone, aes(x = px_height, y = px_width, color = price_range)) + 
  geom_point(alpha = 0.5) + 
  labs(title = "Pixel Height vs Pixel Width", x = "Pixel Height", y = "Pixel Width") +
  theme_minimal()

grid.arrange(scatter1, scatter2, ncol = 2)

# Histograms
hist1 <- ggplot(cellphone, aes(x = battery_power)) + 
  geom_histogram(binwidth = 50, fill = "blue", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of Battery Power", x = "Battery Power", y = "Frequency") +
  theme_minimal()

hist2 <- ggplot(cellphone, aes(x = ram)) + 
  geom_histogram(binwidth = 200, fill = "green", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of RAM", x = "RAM", y = "Frequency") +
  theme_minimal()

grid.arrange(hist1, hist2, ncol = 2)

# Bar plots
bar1 <- ggplot(cellphone, aes(x = factor(blue))) + 
  geom_bar(fill = "blue", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of Bluetooth Capability", x = "Bluetooth (0 = No, 1 = Yes)", y = "Count") +
  theme_minimal()

bar2 <- ggplot(cellphone, aes(x = factor(four_g))) + 
  geom_bar(fill = "purple", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of 4G Capability", x = "4G (0 = No, 1 = Yes)", y = "Count") +
  theme_minimal()

grid.arrange(bar1, bar2, ncol = 2)


# Columns to create histograms for
columns <- c("battery_power", "px_width", "px_height", "sc_h", "sc_w", "ram")

# Create an empty list to store plots
histograms <- list()

# Loop through each column and create a histogram
for (column in columns) {
  p <- ggplot(cellphone, aes_string(x = column)) +
    geom_histogram(binwidth = 50, fill = "skyblue", color = "black") +
    labs(title = paste("Histogram of", column), x = column, y = "Frequency") +
    theme_minimal()
  
  histograms[[column]] <- p
}

# Arrange histograms into a grid
grid.arrange(grobs = histograms, ncol = 2)


# Correlation matrix
install.packages('RColorBrewer')
library(RColorBrewer)

corr_matrix <- cor(cellphone[, sapply(cellphone, is.numeric)], use = "complete.obs")
corrplot(corr_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

corrplot(corr_matrix, type="upper", order="hclust", col=brewer.pal(n=8, name="RdBu"))

################################################################################
#                              Modeling
################################################################################

############################## Ologit ##############################
# general model with all variables

gen_logit = ologit.reg(as.factor(price_range) ~ battery_power + blue + clock_speed + dual_sim + 
                         fc + four_g + int_memory + m_dep + mobile_wt + n_cores + pc + px_height +
                         px_width + sc_h + sc_w + talk_time + three_g + 
                         touch_screen+wifi, data = cellphone)


summary(gen_logit)


gen_logit_polr <- polr(as.factor(price_range) ~ battery_power + blue + clock_speed + dual_sim + 
                         fc + four_g + int_memory + m_dep + mobile_wt + n_cores + pc + px_height +
                         px_width + sc_h + sc_w + talk_time + three_g + 
                         touch_screen+wifi, data = cellphone)
coeftest(gen_logit_polr)
# test whether all insignificant variables all jointly insignificant
logit_reg = ologit.reg(as.factor(price_range) ~ battery_power + int_memory +  px_height+ 
                         px_width 
                       , data = cellphone) 

lrtest(gen_logit,logit_reg )
final_logit <- logit_reg
summary(final_logit)
#Likelihood ratio test

# Model 1: as.factor(price_range) ~ battery_power + blue + clock_speed + 
#   dual_sim + fc + four_g + int_memory + m_dep + mobile_wt + 
#   n_cores + pc + px_height + px_width + sc_h + sc_w + talk_time + 
#   three_g + touch_screen + wifi
# Model 2: as.factor(price_range) ~ battery_power + int_memory + px_height + 
#   px_width
# #Df  LogLik  Df Chisq Pr(>Chisq)
# 1  22 -2689.3                     
# 2   7 -2696.4 -15 14.14     0.5149

logit_reg_polr <- polr(as.factor(price_range) ~ battery_power + int_memory +  px_height+ 
                         px_width 
                       , data = cellphone)
coeftest(logit_reg_polr)
lrtest(gen_logit_polr, logit_reg_polr )
#Likelihood ratio test
#H0: both model are good as each other
#p-value = 0.5149, it means we fail to reject H0, so both model general and restricted models
#are good as each other, so we choose restricted model and all insignificant variables are 
#jointly insignificant and we are allowed to remove all insignificant in one step
final_logit_polr <- logit_reg_polr


# Brant'logit.reg1# Brant's test
# A significant test statistic provides evidence that the parallel
# regression assumption has been violated.
# the function works with polr model results
# specific - final logit model


brant(final_logit_polr)
# -------------------------------------------- 
#   Test for	X2	df	probability 
# -------------------------------------------- 
#   Omnibus		33.62	8	0
# battery_power	11.3	2	0
# int_memory	4.77	2	0.09
# px_height	7.51	2	0.02
# px_width	5.27	2	0.07
# -------------------------------------------- 
#   
#   H0: Parallel Regression Assumption holds

#The probability of Omnibus =0 <0.05 , so we reject H0, and the parallel Regression
#Assumption doesn't holds. The omnibus test results confirms violation of the assumption.
# so we can not use this model and one solution is going to oprobit model.

############################## Oprobit ##############################
# general probit model with all variables
#step 1
gen_probit <- oprobit.reg(as.factor(price_range) ~ battery_power + blue + clock_speed + dual_sim + 
                            fc + four_g + int_memory + m_dep + mobile_wt + n_cores + pc + px_height +
                            px_width +  sc_h + sc_w + talk_time + three_g + 
                            touch_screen + wifi, data = cellphone)


summary(gen_probit)

gen_probit_polr <- polr(as.factor(price_range) ~ battery_power + blue + clock_speed + dual_sim + 
                            fc + four_g + int_memory + m_dep + mobile_wt + n_cores + pc + px_height +
                            px_width +  sc_h + sc_w + talk_time + three_g + 
                            touch_screen + wifi, data = cellphone, method = "probit")
coeftest(gen_probit_polr)
probit_reg_polr <- polr(as.factor(price_range) ~ battery_power + int_memory +  px_height+ 
                          px_width 
                          , data = cellphone, method = "probit")
coeftest(probit_reg_polr)
lrtest(gen_probit_polr, probit_reg_polr)
# Model 1: as.factor(price_range) ~ battery_power + blue + clock_speed + 
#   dual_sim + fc + four_g + int_memory + m_dep + mobile_wt + 
#   n_cores + pc + px_height + px_width + sc_h + sc_w + talk_time + 
#   three_g + touch_screen + wifi
# Model 2: as.factor(price_range) ~ battery_power + int_memory + px_height + 
#   px_width
# #Df  LogLik  Df  Chisq Pr(>Chisq)
# 1  22 -2684.0                      
# 2   7 -2690.9 -15 13.906     0.5327

# test whether all insignificant variables all jointly insignificant
probit_reg = oprobit.reg(as.factor(price_range) ~ battery_power + int_memory +  px_height+ 
                           px_width 
                         , data = cellphone) 

lrtest(gen_probit,probit_reg )
#Likelihood ratio test
#H0: both model are good as each other
#p-value = 0.5327 , it means we fail to reject H0, so both model general and restricted models
#are good as each other, so we choose restricted model and all insignificant variables are 
#jointly insignificant and we are allowed to remove all insignificant in one step

#all variable are significant so we choose our model
final_probit = probit_reg
summary(final_probit)

final_probit_polr <- probit_reg_polr
coeftest(final_probit_polr)
#####################################################################
#                         all model with stargazer
#####################################################################

# Calculate AIC for each model
aic_values <- c(AIC(gen_logit_polr), AIC(final_logit_polr), AIC(gen_probit_polr), AIC(final_probit_polr))

# Generate summary table with stargazer
library(stargazer)
stargazer(gen_logit_polr, final_logit_polr, gen_probit_polr, final_probit_polr,  type = 'text', df = TRUE,
          add.lines = list(c("AIC", round(aic_values, 2))))

######################################################################
#                       Goodness-of-fit tests
######################################################################
final_probit_polr <- polr(price_range ~  battery_power + int_memory +  px_height+ 
                            px_width+battery_power*mobile_wt, 
                          , data = cellphone, method = "probit")
# lipsitz test
lipsitz.test(final_probit_polr)
#p-value = 0.9256

#pulkrob.chisq(final_probit_polr)
#this test used when we have categorical variables in our significant variables, we dont have

# Hosmer and Lemeshow test

logitgof(cellphone$price_range, fitted(final_probit_polr), g = 14, ord = TRUE)
# Hosmer and Lemeshow test (ordinal model)
# 
# data:  cellphone$price_range, fitted(final_probit_polr)
# X-squared = 52.937, df = 38, p-value = 0.1307


# both tests passed


######################################################################
# Pseudo-R2 statistics
pR2(final_probit_polr)
# llh	- The log-likelihood from the fitted model
# llhNull	- The log-likelihood from the intercept-only restricted model
# G2 - Minus two times the difference in the log-likelihoods
# McFadden - McFadden's pseudo r-squared
# r2ML	- Maximum likelihood pseudo r-squared
# r2CU	- Cragg and Uhler's pseudo r-squared


# fitting null model for pseudo-r2
# llh       llhNull            G2      McFadden          r2ML          r2CU 
# -2.687634e+03 -2.772589e+03  1.699097e+02  3.064099e-02  8.144625e-02  8.687600e-02 
# ############################## extending model ##############################

# # Define the ordered probit model with intercepts
 probit_reg1_interaction = oprobit.reg(price_range ~  battery_power + int_memory +  px_height+ 
                                         px_width+battery_power*mobile_wt ,  data = cellphone) 
# 
# Print the summary of the model
summary(probit_reg1_interaction)
AIC(probit_reg1_interaction) 
AIC(final_probit)
# > AIC(probit_reg1_interaction) 
# [1] 5393.268
# attr(,"df")
# [1] 9
# > AIC(final_probit)
# [1] 5395.899
# attr(,"df")
# [1] 7

final_model <- oprobit.reg(price_range ~  battery_power + int_memory +  px_height+ 
                             px_width+battery_power*mobile_wt ,  data = cellphone)

margins.oglmx(final_model)





