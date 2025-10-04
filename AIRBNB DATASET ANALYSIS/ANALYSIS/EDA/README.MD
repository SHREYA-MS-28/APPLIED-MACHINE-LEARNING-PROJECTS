# Exploratory Data Analysis (EDA)

This section provides an exploratory view of Airbnb NZ data (2024–2025), focusing on descriptive statistics, seasonal patterns, and host/review characteristics. The goal is to uncover early insights and prepare the ground for advanced modeling.

---
##  Plots

Key visualizations stored in the `Plots/` folder:

- **avg_reviews_per_month.png**  
  Average reviews per month, showing seasonal fluctuations.  

- **avg_price_per_month.png**  
  Trends in average monthly prices, highlighting dips around Jan 2025.  

- **availability_vs_price.png**  
  Scatterplot of listing availability vs price, revealing outliers and clustering of low-price listings.  

- **avg_num_reviews_per_month.png**  
  Average number of reviews per month, with strong growth from early 2025.  

- **price_distribution.png**  
  Price distribution comparison: Auckland vs Queenstown-Lakes District.  

- **avg_review_subscores_by_month.png**  
  Trends across sub-scores (accuracy, check-in, cleanliness, etc.), showing consistently high performance.  

- **correlation_review_scores.png**  
  Correlation matrix of review sub-scores, showing strong positive relationships across all components.  

---
##  Tables

Summary statistics stored in the `Tables/` folder:

- **superhost_summary_table.png**  
  Comparison of Superhosts vs non-Superhosts (average price, rating, and counts).  

- **outlier_summary_table.png**  
  Top variables with highest number of outliers (reviews, revenue, property size attributes).  

- **monthly_price_stats_table.png**  
  Monthly descriptive stats (mean, median, SD, min, max, count) for prices.  

---
## Key Insights

- **Pricing Trends:** Prices peak in late 2024 but fall sharply in Jan 2025 before recovering.  
- **Superhosts:** Charge lower average prices but achieve higher ratings, showing stronger value delivery.  
- **Reviews:** Average review activity and volume increased notably in 2025.  
- **Regional Differences:** Both Auckland and Queenstown show heavy-tailed price distributions with extreme outliers.  
- **Quality Scores:** Review subscores are consistently high (>4.7), with cleanliness and communication as key drivers.  

---

 Next steps: These findings guide segmentation, regression modeling, and sentiment analysis in the following modules.
