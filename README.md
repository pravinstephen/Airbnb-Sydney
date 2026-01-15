# Airbnb Sydney: Quantitative Pricing & Distribution Analysis

## Project Overview
This project performs an exploratory data analysis (EDA) of ~11,000 Airbnb listings in Sydney, Australia. The analysis focuses on understanding pricing dynamics, geographic premiums, and the suitability of statistical distributions for modeling "messy" real-world pricing data.

## Key Technical Features
* **Language:** R
* **Libraries:** `tidyverse`, `dplyr`, `readr` 
* **Methodologies:**
    * **Data Engineering:** Developed a `price.per.guest` metric and utilized `dplyr` to segment properties into quantile-based "Review Brackets".
    * **Distribution Modeling:** Fitted **Gamma(α, β)** distributions to price-per-guest observations using Maximum Likelihood Estimation (MLE).
    * **Geospatial Analysis:** Conducted regression and visualization of the relationship between longitude and pricing premiums across the Sydney CBD.
    * **Comparative Analysis:** Evaluated mean and median price variances across room types (Entire Home vs. Private Room) and review frequency brackets.

## Key Commercial Insights
* **Geographic Trends:** Identified a positive correlation between higher longitudes and pricing premiums, reflecting market-value increases from Western to Eastern Sydney.
* **Model Suitability:** Confirmed that while the Gamma distribution effectively represents right-skewed pricing histograms, specific mid-tier price clusters show higher empirical density than the theoretical model.

## How to Use
1. Ensure `AirbnbSydney.csv` is in your working directory.
2. Run `Airbnb Sydney.R` to reproduce the statistical summaries, distribution plots, and geospatial analysis.
