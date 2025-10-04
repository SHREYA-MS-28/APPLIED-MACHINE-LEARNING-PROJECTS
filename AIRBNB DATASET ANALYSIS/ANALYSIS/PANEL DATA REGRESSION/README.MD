# Panel Data Regression

This section applies panel data econometric models to Airbnb NZ (2024–2025) to investigate the drivers of listing prices while accounting for both cross-sectional and temporal variation. Panel regression allows us to control for unobserved heterogeneity (e.g., listing-specific characteristics) and to assess causal relationships more robustly than pooled models.

---

##  Model Summary

- **panel_regression_airbnb_prices.png**  
  *Fixed Effects (two-way) and Random Effects panel regression models of Airbnb prices. Results show that the Hausman test strongly favors the Fixed Effects model (χ² = 17,643, p < 0.001), indicating that unobserved heterogeneity is correlated with predictors. Key significant drivers include accommodates, review scores (accuracy, communication), bedrooms, and availability metrics.*

---

## Key Insights

- **Model Choice:** The Hausman test decisively supports the Fixed Effects model, meaning the Random Effects estimator is inconsistent.  
- **Capacity Effects:** *Accommodates* and *bedrooms* positively affect price, but their interaction with reviews shows diminishing returns.  
- **Quality Signals:** Review scores (accuracy, communication) are strongly associated with higher prices, while low *value* ratings reduce price.  
- **Availability & Nights:** Availability windows (30, 90 days) and minimum nights show negative associations with price, possibly reflecting discounting strategies for more flexible listings.  
- **Scale of Reviews:** Higher *number of reviews* reduces price slightly, potentially reflecting older or more budget-oriented properties.  

---

 This analysis highlights the importance of controlling for unobserved listing-specific effects. The results provide a foundation for more targeted strategies in pricing, capacity management, and host quality improvement.
