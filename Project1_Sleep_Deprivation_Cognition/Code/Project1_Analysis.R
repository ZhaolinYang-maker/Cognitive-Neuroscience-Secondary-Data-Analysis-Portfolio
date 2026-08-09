# Secondary Data Analysis Project1: Studying the Relationship between sleep deprivation and emotional regualtion and relationship between psychological states and cognitive performance

# Read the file from the local computer 
data <- read.csv("/Users/zhaolinyang/Desktop/Indpendent Project/Project 1st/Dataset.csv")

install.packages("qqplotr")
library(qqplotr)
install.packages("psych")
library(psych)
install.packages("lmtest")
library(lmtest)
install.packages("car")
library(car)
install.packages("boot")
library(boot)
install.packages("scatterplot3d")
library(scatterplot3d)


#=====================================================================================================================
# MODULE1: Dataset Overview and Descriptive Statistics
#=====================================================================================================================

# STEP1: Brief overall summary for the whole data
dim(data) # Quick check for the number of sample size (N = 60) and number of variables (p = 14)
names(data) # Labeling variable names. Especially useful for quick check for if there is any misspelling in code
summary(data) # Generate a brief descriptive summary of all variables

#Checking missing values in the whole data
colSums(is.na(data)) #Confirmation as described by the authors, there is no missing value in the dataset
sum(duplicated(data)) #Checking for any duplicated rows in the data 

# STEP2 Focusing on variables of interest with description and summary
# Seven variables of interest included in the two aims. Mainly focusing on the sample statistics of the 7 variables
variables <- c("Sleep_Hours", 
               "Sleep_Quality_Score", 
               "Emotion_Regulation_Score",
               "Stress_Level",
               "Stroop_Task_Reaction_Time",
               "N_Back_Accuracy",
               "PVT_Reaction_Time")

mean_values <- sapply(data[variables], mean) # Computation of each variable's mean
variance_values <- sapply(data[variables], var) # Computation of each variable's variance in sample
SD_values <- sapply(data[variables], sd) #Computation of each variable's standard deviation in sample
descriptive_statistics_table <-cbind(Mean = mean_values,
                                     variance = variance_values,
                                     SD = SD_values)
round(descriptive_statistics_table,2) #Combine the values to be a table for summary and comparison

#=====================================================================================================================
# MODULE2: Necessary Data Preparation prior to formal data analysis
#=====================================================================================================================

# STEP1: Generation of mean-centered variables (sleep-hours, sleep quality and emotional regulation score for AIM2)
data$c_sleep_hours <- data$Sleep_Hours - mean(data$Sleep_Hours) # Mean-centering of sleep duration in a newly created column
data$c_sleep_quality <- data$Sleep_Quality_Score - mean(data$Sleep_Quality_Score) # Mean-centering of sleep quality in a newly created column
data$c_emotion_regulation <- data$Emotion_Regulation_Score - mean(data$Emotion_Regulation_Score) # Mean-centering of emotion regulation in a newly created column

#STEP2 Converting continous stress level to categorical variable with three groups based on criteria of level of stress (Perceived Stress Scale PSS-10)
# PSS-10 evaluates stress levels on a 0 to 40 scale. It classifies low stress group (0-10), moderate stress (14-26) and high stress group (27-40)
data$Moderate_stress <- ifelse(14 <= data$Stress_Level
                             &data$Stress_Level <= 26, 
                             1, 0)
data$High_stress <- ifelse(27 <= data$Stress_Level&
                                data$Stress_Level <= 40, 
                              1, 0)
data$Stress_Group <- ifelse(data$Moderate_stress == 1, #Classification of different stress groups
                               "Moderate Stress",
                               ifelse(data$High_stress == 1,
                                      "High Stress",
                                      "Low Stress"
                                      )
                            ) 
data$Stress_Group <- factor(data$Stress_Group,
                            levels = c("Low Stress", "Moderate Stress", "High Stress")
                            ) #Low stress group (stress level: 0-13) is baseline group of reference for later ANCOVA

#=====================================================================================================================
# MODULE3: Reusable Assumption Diagnostics Functions
#=====================================================================================================================
# For valid conventional sampling-distribution-based inference, certain assumptions ought to be resonably satisfied for each fitted regression model.
# In other words, regression assumptions are examined for each fitted model rather than for the dataset as a whole.

#This module defines reusable diagnostic functions so that the same checks can be consistently and conveniently applied to each model in the linear analysis
#As discussed in project overview, particular attention will be given to (1) approximate normality of errors; (2) Linearity of relationship between predictors and outcome (3)homoscedasticity (homogeneity of variance of outcome variable)
# (4) influential observations (e.g., outliers in sample) and (5) multi-collinearity among predictors incorporated in multiple linear regression. 
# Notice, autocorrelation which may influence uncertainty of estimation is not explicitly checked here since the authors state that the observations are measured independently

#---------------------------------------------------------------
# CHECK 1: Diagnostics of Approximate Normality of Errors
# Because the OLS coefficient estimator is a linear function of the model errors, approximate normality of errors supports an approximately normal sampling distribution of coefficient estimates and therefore conventional t- and F-based inference.
# Therefore, normality of errors mainly supports the validity of conventional hypothesis testing based on t and F distributions,especially in small samples because in large sample, CLT can compensate
# QQ plot and the Shapiro-Wilk test (S-W test) will be used for checking the assumption
Residuals_Normality_Checking <- function(model){
  Residuals_Model <- residuals(model) # Inset any linear regression model, the functions automatically compute its residuals for later calculations if applicable
  par(mfrow=c(1,2))
       hist(Residuals_Model,
       breaks = 10,
       freq = FALSE,
       col = "black",
       border = "white",
       main = "Histogram of Regression Residuals",
       xlab = "Residuals") # Constructing histogram describing the residual distributions for visualization of distribution of redisuals in the existing sample
  
       qqnorm(Residuals_Model,
         main = "Q-Q Plot of Regression Residuals",
         ) # Drawing QQ plot for visually comparison of consistency between observed values of residuals and expected values of residuals given normality of errors
  
       qqline(Residuals_Model,
         col = "red"
         )
       
       print(shapiro.test(Residuals_Model)) #S-W test for statistically testing normality of residuals where H0 = no violation of normality of errors
} # As expected, once model is constructed and inserted into the fucntion, hitogram of residuals, Q-Q plot of residuals and result of S-W test should be delivered. 
#Based on the result, deciding whether the assumption of normality of errors is violated or not visually and statistically. Remedy (e.g., log-transformation is then required if there is violation)

#---------------------------------------------------------------
#CHECK2 Linearity of Relationship between Predictors and Outcome 
# Because the linear regression model presumes that conditional expectations of outcome given predictors can be expressed by a linear function, 
# Thereforem the inference and estimation are risky to be invalid and meaningless if the assumtpion is violated
#Scatterplot (fitted values VS. residuals) visual inspection can be used for checking the assumption. Notice, the scatterplot can also partially provide certain evidence that whether the assumption of Homoscedasticity is violated visually

Linearity_Relationship_Checking <- function(model){
  par(mfrow=c(1,1))
  plot(fitted(model),
       residuals(model),
       xlab = "Fitted Values",
       ylab = "Residuals",
       main = "Residuals VS Fitted Values")
} #If a random fluctuation of residuals without visually obvious curvature or pattern are observed, it provides no obvious evidence that the linear relationship between predictors and outcome variables is violated
#Additionally, if the dispersion of residuals shows no visually obvious cahnge across fitted values (e.g., cone shape of residuals), it may visually provides no obvious evidence that the assumption of homoscedasticity is violated

#---------------------------------------------------------------
# CHECK3 Homoscedasticity Homogeneity of Variance of Outcome Variables)
# Because the OLS coefficient estimator is a linear function of the model errors, satisfaction of homoscedasticity provides accurate reflection of sample statistics uncertainty in conventional sampling distribution
# Therefore, the estimation and inference will be possibly untrustworthy
# Scatterplot (fitted values VS residual values) provides a visual inspection way for diagnosis of the assumption. Breusch-Pagan test (B-P test) provides a statistical way for diagnosis of the asssumption

Homoscedasticity_Checking <- function(model){
  print(bptest(model)) #B-P test states H) as satisfaction of homoscedasticity; small P-value therefore suggests heteroscedasticity of errors
} # NOTICE: Considering the LLN, the statistical examination is suggested to be conducted as a supportive diagnosis in small sample

#---------------------------------------------------------------
# CHECK4 Potential influential Observations (outliers)
# Outliers showing substantial deviation influences variance of residuals and calculation of coefficients, potentially leading to more frequent typeII error
# Cook's distance measures the influence of each observation on the regression equation. 

Outlier_Checking <- function(model){
  par(mfrow=c(1,1))
  cd <- cooks.distance(model)
  plot(cd,
       type = "h",
       main = "Cook's Distance",
       xlab = "Observations",
       ylab = "Cook's Distance")
} # The plot shwos each observation's Cook's distances. When observing values with apparently substantial different, potential serious outliers may be implied

#---------------------------------------------------------------
# CHECK 5 Multicollinearity
# A high Multicollinearity among variables suggesting a high intrinsically high relationship among predictors makes it difficult to isolate each predictor's individual effect
# Therefore, the violation leads to more noise in coeffcientcalculation with increased estimation uncertainty despite possibly well predictive accuracy
# NOTICE: multicollinearity is only checked when there is two or more predictors incorporated into the model. 

Multicollinearity_Checking <- function(model){ # Use hte function when model includes at least two predictors
  VIF_Scores <- vif(model) #VIF. When VIF > 10, a problem of multicollinearity is possibly implied
  Tolerance <- 1/vif(model) #Tolerance; Reciprocal of VIF. When tolerance < 0.1, a problem of multicollinearity is possibly implied
  VIF_and_Tolerance <- cbind(VIF_Scores = VIF_Scores,
                             Tolerance = Tolerance)
  round(VIF_and_Tolerance, 5)
}

#All of the above defined functions can be used in any models directly before formal data analysis for diagnosis of assumption with change of specifci model as input

#=====================================================================================================================
# MODULE3: Data Analysis Aim 1: Sleepiness-Related Measurements and Emotion Regulation Ability
#=====================================================================================================================
# SUBSET1: Relationship Between Sleep Hours and Emotion Regulation Scores
Aim1_model1 <- lm(
  Emotion_Regulation_Score ~ c_sleep_hours,
  data=.GlobalEnv$data,
  model = TRUE
) # Construction of simple linear regression model
summary(Aim1_model1)

# Scatterplot Visualization
plot(data$c_sleep_hours,
     data$Emotion_Regulation_Score,
     xlab = "Sleep Hours",
     ylab = "Emotion Regulation Scores",
     main = "Relationship between Sleep Duration and Emotion Regulation")
curve(
  coef(Aim1_model1)[1] + coef(Aim1_model1)[2]*x,
  add = TRUE,
  col = "red"
)

Confidence_Interval_Band <- data.frame(
  data$c_sleep_hours,
  predict(
    Aim1_model1, 
    interval = "confidence", 
    level = 0.95,
  )
) # CI band representing uncertainty of conditional expectation corresponding to predictor values

# Assumption Checking
Residuals_Normality_Checking(Aim1_model1) # Checking for normality of errors
Linearity_Relationship_Checking(Aim1_model1) # Checking for whether relationship is linear
Homoscedasticity_Checking(Aim1_model1) # B-P test for checking homogeneity of variance
Outlier_Checking(Aim1_model1) # Checking for the existence of serious outlier

#Due to potential deviation from residual normality (S-W test shows a statistical significant result), bootstrap confidence intervals were additionally examined.
Bootstrapping_result <- Boot(Aim1_model1, R = 5000) # 5000 samples is recommended for stability
confint(Bootstrapping_result, level = 0.95) 

#SUBSET2: Relationship Between Sleep Quality and Sleep Hours and Emotional Regulation
Aim1_model2 <- lm(
  Emotion_Regulation_Score ~ c_sleep_hours + c_sleep_quality,
  data = data,
  model = TRUE
) # Construction of the multiple linear regression model
summary(Aim1_model2)

Residuals_Normality_Checking(Aim1_model2) 
Linearity_Relationship_Checking(Aim1_model2) 
Homoscedasticity_Checking(Aim1_model2) 
Outlier_Checking(Aim1_model2) #
Multicollinearity_Checking(Aim1_model2) # Checking for multicollinearity since there are two predictors now

confint(Aim1_model2, level = .95) # 95% confidence interval for parameter estimation

# 3D Space scatterplot Visualization
Three_D_visualization <- with(data,
                      scatterplot3d (Emotion_Regulation_Score ~ c_sleep_hours + c_sleep_quality,
                                     xlab = "Sleep Hours(mean-centering)", 
                                     ylab = "Sleep Quality(mean-centering)", 
                                     zlab = "Emotion Regulation Scores",
                                     main = "Multiple Regression Visualization of Sleep-Related Predictors of Emotion Regulation",
                                     pch = 20, 
                                     color = "blue")
)
Three_D_visualization$plane3d(Aim1_model2)  # OLS-based constructed hyperplane representing "best-fitting" solution in Multiple linear regression 

d_R_Square <- summary(Aim1_model2)$adj.r.squared - summary(Aim1_model1)$adj.r.squared # Difference on adjusted R^square in comparison
Comparison <- anova(Aim1_model1,Aim1_model2) # Comparison on improvement between model indicates whether the improvement is statistically significantly 
cat("Magnitude of R^2:", round(d_R_Square, 3),
    "\nP-Value:", round(Comparison$`Pr(>F)`[2], 3)) # Output results; Magnitude and statistical significance of improvement

Moderation_model3 <- lm(Emotion_Regulation_Score ~ c_sleep_hours*c_sleep_quality, data = data) # Incorporating interaction term
summary(Moderation_model3) # Primarily focusing on statistical significance of interaction term

#END for the AIM1

#=====================================================================================================================
# MODULE4: Data Analysis Aim 2: Cogntive Performance and Psychological States
#=====================================================================================================================



