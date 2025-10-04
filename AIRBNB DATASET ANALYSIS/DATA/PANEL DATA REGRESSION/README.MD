# Panel Data Regression Datasets

This folder contains **sample Airbnb listings datasets** covering **12 months of data (2024–2025)** across New Zealand, specifically prepared for **panel data regression analysis**.  

Panel data (longitudinal data) tracks the same listings over multiple months, making it possible to:  
- Analyze changes in pricing and availability over time  
- Control for unobserved listing-level characteristics  
- Study market trends in different regions  

## Metadata (Sample Files)
- **Rows:** Each row = one listing in a given month  
- **Columns:** ~80–85 variables (may vary slightly by month), including:  
  - `listing_id` – Unique identifier for each property  
  - `date` – Reference month/year  
  - `price` – Nightly price in NZD  
  - `minimum_nights`, `availability_30`, `availability_365`  
  - `number_of_reviews`, `review_scores_rating`  
  - `neighbourhood_cleansed`, `room_type`, `property_type`  
  - Host-related info (`host_id`, `host_response_time`, `host_is_superhost`)  

## Full Datasets  
Due to GitHub’s file size limitations, only **sample CSVs** are stored here.  
The **complete panel datasets** (12 months, full scale) can be accessed via Google Drive:  

[Full Panel Data Regression Datasets](https://drive.google.com/drive/folders/1j2mE-3xbYv5dT86nBquuNzlFyG3Vg6r-?usp=share_link)  

## Usage
These datasets are intended for **econometric modeling** using fixed-effects and random-effects regression, with applications in:  
- Pricing dynamics  
- Demand elasticity  
- Regional performance comparisons  
