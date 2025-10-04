# Airbnb-NZ-Performance-Analysis-2024-2025

Comprehensive analysis of **575,000+ Airbnb listings** and **33M guest reviews** across New Zealand (July 2024 – June 2025).  
This project explores **price drivers, host behavior, guest sentiment, and market segmentation** using advanced data analytics techniques.

---

## Project Overview
The New Zealand Airbnb market is a dynamic ecosystem shaped by pricing strategies, host responsiveness, and guest satisfaction.  
Our objective was to **uncover insights that guide hosts, platforms, and policymakers** by combining multiple analytical approaches:
- **Exploratory Data Analysis (EDA):** Market-level distributions, seasonal demand, and pricing trends.  
- **Segmentation (Clustering):** Identifying host and property types using k-prototypes and silhouette validation.  
- **Panel Data Regression:** Econometric modeling of price drivers across regions and time.  
- **Sentiment Analysis:** Mining 33M guest reviews with lexicons (AFINN, Bing, NRC) to quantify customer experience.

---

## Data Overview
- **Listings Dataset (575,000+ rows):**  
  - Variables include: price, property type, amenities count, host response metrics, reviews per month, region.  
  - Coverage: **Auckland, Queenstown, Wellington, Christchurch, Dunedin** (July 2024 – June 2025).  

- **Reviews Dataset (33M+ reviews):**  
  - Text data with guest comments and ratings.  
  - Used for sentiment scoring and keyword extraction.  

- **Panel Structure:**  
  - Monthly snapshots of listings → **panel data** (time-series cross-sectional).  
  - Enables modeling of **longitudinal effects** on price and demand.  

---

## Objectives of the Analysis
The analysis is structured into **four key components**, each serving a distinct purpose:

1. **Exploratory Data Analysis (EDA)**  
   - Purpose: Understand price distributions, seasonal patterns, and regional dynamics.  
   - Example Insight: Auckland median nightly price ≈ NZD 160 vs. Queenstown ≈ NZD 210.  

2. **Segmentation (Clustering)**  
   - Purpose: Group hosts and properties into meaningful clusters based on amenities, response rates, and property type.  
   - Outcome: Identified 5 distinct clusters; silhouette score peaked at **0.46** with k=5.  

3. **Panel Data Regression**  
   - Purpose: Identify **causal drivers of price** while controlling for time and regional effects.  
   - Key Variables: amenities count, property type, reviews per month, host tenure, location.  
   - Method: Fixed-effects and random-effects regression, validated with Hausman test.  

4. **Sentiment Analysis**  
   - Purpose: Extract guest experience signals from 33M reviews.  
   - Method: Lexicon-based scoring (AFINN, Bing, NRC).  
   - Findings: Positive sentiment dominates (+0.62 avg. score), with common praise for *cleanliness* and *location*, and negatives for *check-in* and *fees*.  

---

## Repository Structure
```plaintext
├── analysis/                 # Core analytics
│   ├── EDA                   # Data exploration & visualization
│   ├── airbnb-segmentation   # Clustering & segmentation
│   ├── panel-data-regression # Econometric modeling
│   └── sentiment-analysis    # Text mining & NLP on reviews
│
├── data/                     # Raw & cleaned datasets
├── notebooks/                # Reproducible Jupyter/R notebooks
├── report/                   # Final written report (PDF)
├── Presentation/             # Executive presentation (PDF)
├── LICENSE
└── README.md                 # You are here
---

```
Key Insights
- **Pricing Dynamics:**  
  - Median nightly price (Auckland): **NZD 160** | Queenstown: **NZD 210**  
  - Price strongly influenced by property type, amenities, and seasonality.  

- **Segmentation Findings:**  
  - 5 distinct clusters identified (Entire homes vs. Private rooms, host tenure, response rates).  
  - Best cluster silhouette score: **0.46** with k=5.  

- **Host Behavior:**  
  - Superhosts maintain response rates **>97%** with lower churn risk.  
  - Long-tenure hosts charge ~**15% higher** on average.  

- **Guest Sentiment:**  
  - Positive sentiment dominates (avg. sentiment score **+0.62**).  
  - Common positive keywords: *clean, cozy, location*.  
  - Negative drivers: *check-in issues, noise, hidden fees*.  

---

## Deliverables
- **[Full Report](report/Airbnb-analysis-combined-report.pdf)** – detailed write-up with methods, visuals, and findings.  
- **[Presentation](Presentation/Airbnb%20Analysis%20presentation.pdf)** – business-facing summary of key insights.  
- **Analysis Subfolders** – reproducible code, plots, and tables.  

---

## Tech Stack
- **Languages:** R, Python  
- **Libraries:** `tidyverse`, `plm`, `cluster`, `factoextra`, `ggplot2`, `nltk`, `textdata`  
- **Tools:** Power BI, Jupyter, RStudio, GitHub  

---
