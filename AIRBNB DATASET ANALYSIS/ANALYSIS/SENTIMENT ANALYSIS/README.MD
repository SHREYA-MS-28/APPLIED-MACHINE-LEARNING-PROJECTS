# Airbnb Sentiment Analysis

This folder contains the sentiment analysis of Airbnb reviews, conducted using multiple lexicons (AFINN, Bing, NRC).  
The analysis evaluates customer sentiment trends, emotional tone, and review dynamics over time.

---

## Structure

### 1. Plot
This directory holds visualizations generated during the sentiment analysis.

- **afinn_sentiment_distribution.png** – Distribution of sentiment scores using AFINN lexicon  
- **afinn_sentiment_trend_top5.png** – Top 5 sentiment trend categories (AFINN)  
- **bing_sentiment_distribution.png** – Distribution of positive and negative words using Bing lexicon  
- **bing_sentiment_monthly_pos_neg.png** – Monthly trend of positive vs negative sentiments (Bing)  
- **bing_sentiment_trend_top5.png** – Top 5 monthly sentiment categories (Bing)  
- **negative_wordcloud.png** – Word cloud of most frequent negative words  
- **nrc_pos_neg_ratio.png** – Ratio of positive to negative words (NRC)  
- **nrc_sentiment_distribution.png** – Sentiment distribution across NRC emotional categories  
- **positive_wordcloud.png** – Word cloud of most frequent positive words  
- **reviews_per_month.png** – Review volume trends across months  
- **sentiment_heatmap.png** – Heatmap of sentiment categories across reviews  

---
### 2. Table
This directory contains summary tables supporting the sentiment analysis (e.g., word frequency counts, sentiment ratios, and category breakdowns).

---

## Key Insights
- Sentiment analysis shows a strong predominance of **positive words**, though negative feedback highlights recurring pain points.  
- Monthly sentiment trends reflect seasonal variations in guest experiences.  
- Word clouds capture **drivers of satisfaction (positive)** and **sources of dissatisfaction (negative)**.  
- NRC lexicon reveals underlying emotions like *trust, anticipation,* and *disgust*, which provide a deeper understanding of customer feedback.  

---

## Next Steps
- Link sentiment trends to review ratings and occupancy data.  
- Explore geographic differences in sentiment (Auckland vs Queenstown).  
- Apply topic modeling to uncover themes within negative reviews.  
