#  Customer Segmentation Using K-Means Clustering  

##  Project Overview  
This project applies **K-Means clustering** to a marketing campaign dataset (~2,000 customers) to segment customers based on **demographic and purchasing behaviour**.  
The goal was to identify distinct customer groups, evaluate optimal cluster solutions, and design actionable **buyer personas** to support targeted marketing strategies.  

---

##  Methodology  
- **Data Preparation**  
  - Cleaned dataset by removing extreme outliers and imputing missing values.  
  - Engineered features: Income, Recency, Spending categories (Wine, Meat), Web & Catalog Purchases, Tenure.  
  - Standardized variables to ensure comparability across scales.  

- **Clustering Approach**  
  - Applied **K-Means clustering** with k = 2–5.  
  - Evaluated solutions using **Elbow Method** and **Silhouette Score (0.30 for k=2)**.  
  - Visualized results with PCA-based cluster plots and fviz_cluster.  

- **Buyer Persona Development**  
  - Designed representative personas for each cluster using demographic and behavioural traits.  
  - Linked clusters to strategic marketing recommendations.  

---

##  Key Findings  
- **2-Cluster Solution (Best Fit)**  
  - **Cluster 1: Value-Hunting Olivia** – Low-income, long recency, low spenders, price-sensitive and disengaged.  
  - **Cluster 2: Engaged Ethan** – High-income, high spenders, digitally engaged, recent activity, strong loyalty.  

- **Alternative Cluster Solutions**  
  - 3 clusters → Balanced segmentation, interpretable differences.  
  - 4–5 clusters → More granular but overlapping, less actionable.  

- **Optimal Clusters:** k=2 strikes the best balance between **interpretability and business value**.  

---

##  Strategic Insights  
- **Cluster 1 (Value-Hunting Olivia):**  
  - Run **re-engagement campaigns** with discounts and loyalty rewards.  
  - Encourage digital adoption via onboarding promotions.  

- **Cluster 2 (Engaged Ethan):**  
  - Focus on **retention and upselling** with premium offers, exclusives, and personalized campaigns.  
  - Prioritize digital-first loyalty programs to strengthen engagement.  

---

##  Files in this Project  
- `K MEANS CLUSTERING.R` → R script for clustering analysis  
- `marketing_campaign-2.csv` → Input dataset  
- `K MEANS CLUSTERING REPORT.pdf` → Detailed clustering analysis and personas  
- `CLUSTERING ANALYSIS CASE STUDY.pdf` → Case study instructions and context  

---

##  Tools & Techniques  
- **Language:** R  
- **Methods:** K-Means Clustering, PCA, Elbow Method, Silhouette Score  
- **Visualization:** fviz_cluster, PCA-based cluster plots  
- **Frameworks:** Buyer Persona Design, CRISP-DM  

---
