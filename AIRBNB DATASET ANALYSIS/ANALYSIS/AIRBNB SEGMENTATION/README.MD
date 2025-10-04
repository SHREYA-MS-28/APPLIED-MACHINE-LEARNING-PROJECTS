# Airbnb Segmentation Analysis

This folder contains the segmentation analysis conducted on Airbnb listings in Auckland and Queenstown.  
The analysis explores clustering patterns across multiple listing attributes (price, amenities, property type, host characteristics, etc.), with both **plots** and **tables** for interpretation.

---

## Structure

### 1. Plots
This directory contains visual outputs of the segmentation analysis, including histograms, cluster visualizations, and silhouette plots.

- **hist_price_akl.png** – Histogram of log-transformed prices in Auckland  
- **hist_price_qt.png** – Histogram of log-transformed prices in Queenstown  
- **hist_amenities_akl.png** – Histogram of amenities count in Auckland  
- **hist_amenities_qt.png** – Histogram of amenities count in Queenstown  
- **cluster_silhouette_qt.png** – Silhouette plot for Queenstown clusters  
- **cluster_3dplot_5clusters.png** – 3D visualization of 5-cluster solution  
- **cluster_3dplot_3clusters.png** – 3D visualization of 3-cluster solution  

---
### 2. Tables
This directory contains the tabular summaries and clustering outputs.

- **cluster_property_type_distribution.png** – Cluster-wise property type breakdown  
- **cluster_reviews_hostsince_summary.png** – Cluster summary for number of reviews and host tenure  
- **cluster_silhouette_summary.png** – Summary of silhouette scores across clusters  
- **cluster_summary_hostresp_desc.png** – Cluster summary for host response rate and description length  

---

## Key Insights
- Price and amenities distributions differ notably between Auckland and Queenstown.  
- Silhouette scores indicate moderate to good cluster separation in the 3-cluster solution.  
- Property type clusters show strong dominance of *entire places*, with a smaller share of private/shared rooms.  
- Host-related factors (reviews, tenure, response rates) provide additional segmentation power.  

---

## Next Steps
- Compare segmentation performance across different distance metrics.  
- Link clusters to listing performance (occupancy rate, review scores).  
- Explore potential for customer persona definitions based on host & property attributes.  
