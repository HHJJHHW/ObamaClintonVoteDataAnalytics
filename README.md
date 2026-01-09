# 2008 Democratic Primary Analytics — Obama vs. Clinton

This project analyzes **county-level voting outcomes from the 2008 Democratic primaries** to understand how demographics relate to candidate support and to **predict Obama’s vote margin** using modern statistical and machine-learning methods.

## Objective

* Explain how demographic and socioeconomic factors relate to vote share
* Predict **Obama’s winning margin (%)** at the county level
* Translate insights into **campaign targeting strategies**

## Methods

* **Data preparation:** imputed missing demographic values with column means; split data into train/test by election date
* **Feature engineering:** constructed Obama margin and margin percentage as the prediction target
* **Visualization:** scatter + LOESS plots of **vote share vs. ethnicity %** (by county)
* **Supervised models:** Null model, Linear Regression, Lasso (with inner CV for λ), and **Random Forest**
* **Model evaluation:** **16-fold cross-validation** using RMSE
* **Unsupervised learning:** **PCA + K-means (k=4)** to identify county archetypes by socioeconomic status
* **Causal diagnostics:** compared simple vs. fully controlled regressions to reveal **omitted variable bias**

## Key Findings

* **Random Forest achieved the lowest RMSE**, outperforming linear and lasso models in predicting vote margin
* Counties with higher **Black population shares** showed higher support for Obama, while higher **White and Hispanic shares** favored Clinton (visual + regression evidence)
* PCA/K-means revealed **distinct county clusters** driven by income and education levels, implying different campaign responsiveness
* Simple demographic regressions were misleading; **adding full controls reversed effects**, demonstrating the importance of accounting for confounders

## Impact

* Shows how **predictive modeling + causal reasoning** can guide campaign resource allocation
* Provides a data-driven framework for **targeted messaging** based on demographic composition and county archetypes
