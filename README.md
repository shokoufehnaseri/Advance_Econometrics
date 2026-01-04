# Mobile Phone Price Range Prediction (Ordinal Models)

## Overview
This project analyzes the determinants of **mobile phone price ranges** using an **ordinal response framework**.  
The goal is to identify which technical features of mobile phones most strongly influence their price category and to compare different ordinal modeling approaches.

The analysis applies **ordered logistic and ordered probit models**, evaluates model assumptions, and selects the best-performing specification using statistical tests and information criteria.

---

## Dataset
- **Source:** Mobile phone specifications dataset  
- **Target variable:** `price_range` (ordinal outcome)  
- **Features:** Battery power, internal memory, screen resolution, connectivity features (4G, WiFi, Bluetooth), hardware characteristics, and usage indicators  

The dataset contains **no missing values**, allowing direct modeling after appropriate variable transformations.

---

## Methodology

### Exploratory Data Analysis
- Boxplots, histograms, scatter plots, and bar charts  
- Correlation analysis to understand relationships among numeric predictors  
- Visualization of feature distributions across price categories  

### Modeling Approach
1. **Ordered Logit Model**
   - Full model with all explanatory variables
   - Likelihood ratio tests to eliminate jointly insignificant predictors
   - Brant test to assess the proportional odds assumption

2. **Ordered Probit Model**
   - Estimated after detecting violations of the proportional odds assumption
   - Model selection using likelihood ratio tests and AIC
   - Interaction effects explored to improve model fit

### Model Evaluation
- Likelihood ratio tests
- AIC comparison
- Goodness-of-fit tests (Lipsitz, Hosmer–Lemeshow for ordinal models)
- Pseudo R² measures (McFadden, Cragg–Uhler)
- Marginal effects analysis for interpretability

---

## Key Findings
- **Battery power**, **internal memory**, and **screen resolution** are strong predictors of higher price categories  
- Ordered probit models provide a better fit when the proportional odds assumption is violated  
- Interaction effects further improve model performance  
- Final models pass goodness-of-fit tests and show reasonable explanatory power  

---

## Tools & Technologies
- **Language:** R  
- **Statistical Techniques:**  
  - Ordered Logit & Ordered Probit Models  
  - Likelihood Ratio Tests  
  - Goodness-of-Fit Tests  
  - Marginal Effects Analysis  
- **Key Libraries:**  
  `oglmx`, `MASS`, `ordinal`, `pscl`, `lmtest`, `ResourceSelection`, `stargazer`, `ggplot2`, `corrplot`

---

## Project Type
**Academic / Applied Econometrics & Statistical Modeling Project**
