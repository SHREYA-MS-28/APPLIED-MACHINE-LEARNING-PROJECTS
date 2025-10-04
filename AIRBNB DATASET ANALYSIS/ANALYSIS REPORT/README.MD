# Report

This folder contains the **final integrated report** of the Airbnb NZ Performance Analysis (2024–2025).  
The report consolidates findings from exploratory analysis, panel data regression, market segmentation, and sentiment analysis into a single comprehensive document.

---

## File

- **Airbnb-analysis-combined-report.pdf**  
  A full project report (24 pages) presenting background, methodology, statistical outputs, and managerial insights.

---

## Highlights from the Report

### Data Overview
- **Source**: InsideAirbnb dataset for New Zealand (Jul 2024 – Jun 2025).  
- **Size**: 576,621 rows × 86 columns (~49.6 million values).  
- **Missing values**: ~1.16%.  
- **Format**: Unbalanced and irregular panel data across 12 months.  
- **Focus regions**: Auckland (13,944 unique listings, 113,268 monthly rows) and Queenstown (7,258 unique listings, 65,693 monthly rows):contentReference[oaicite:0]{index=0}.

---

### Research Questions
1. **Panel Data Regression – Price Drivers**  
   What factors most influence Airbnb pricing in New Zealand?

2. **Market Segmentation – Auckland & Queenstown**  
   How can listings be grouped into meaningful market clusters?

3. **Sentiment & Emotion Analysis – Guest Reviews**  
   What do guest reviews reveal about strengths, weaknesses, and service quality?

---

### Key Results

#### 1. Price Drivers (Panel Regression)
- Median nightly price: **NZD 203**; mean: **NZD 394** (skewed by extreme luxury listings >NZD 100,000).  
- **Hausman test** favoured Fixed Effects over Random Effects (χ² = 17,643, df = 18, p < 0.001).  
- Price increases strongly with bedrooms & guest capacity but shows **diminishing returns**.  
- **Review scores**: a +0.1 gain in *accuracy* or *communication* raises price by **5–7%**.  
- **Review activity** is a strong trust signal: median 26 lifetime reviews, mean 68.  
- Model explanatory power: **Adjusted R² (within) = 0.42**:contentReference[oaicite:1]{index=1}.

#### 2. Market Segmentation
- **Auckland (3 clusters, silhouette = 0.57)**  
  - Family-Friendly Homes (~NZD 327/night, 4 guests, rich amenities).  
  - Budget Private Rooms (~NZD 111/night, 2 guests, cost-conscious).  
  - Ultra-Luxury Retreats (~NZD 35,000/night, rare & exclusive).

- **Queenstown (5 clusters, silhouette = 0.46)**  
  - Reliable Comfort (Superhost, ~NZD 536/night, high amenities).  
  - Standard Hosts (~NZD 472/night, fewer reviews/amenities).  
  - High-Value Superhosts (~NZD 665/night, instant bookable, strong guest trust).  
  - Budget Private Rooms (~NZD 182/night, Superhost service).  
  - Signature Stays (~NZD 36,000/night, ultra-premium, niche).:contentReference[oaicite:2]{index=2}

#### 3. Sentiment Analysis
- Dataset: **33.9M reviews (2011–2025)**, filtered to **Jul 2024 – Jun 2025** (≈510k reviews peak in Nov).  
- Net sentiment score: **+15.43M** (94.4% positive vs 5.6% negative).  
- **Emotions (NRC lexicon):**  
  - Positive (29.99%), Trust (19.17%), Joy (18.84%), Anticipation (13.19%).  
  - Negative emotions: Surprise (7.2%), Sadness (4.6%), Fear (1.5%), Anger (1.1%), Disgust (0.8%).  
- **Positive word cloud:** *great, lovely, clean, comfortable, beautiful*.  
- **Negative word cloud:** *cold, issues, noise, problem*.:contentReference[oaicite:3]{index=3}

---

### Limitations
- Unbalanced & irregular panel structure → filtered listings with ≥9 months of data.  
- ID formatting issues resolved by treating IDs as strings.  
- K-prototypes clustering required silhouette validation (elbow not suitable).  
- Lexicon-based sentiment analysis limits context sensitivity (e.g., phrases like *“to die for”* misclassified).:contentReference[oaicite:4]{index=4}

---

### Managerial Insights
- **Revenue optimisation**: Invest in early-stage property upgrades (extra bedroom, improved accuracy/communication scores).  
- **Market targeting**:  
  - Auckland = simpler 3-tier market (family, budget, luxury).  
  - Queenstown = layered 5-tier market with strong premium niches.  
- **Guest experience**: Prioritise cleanliness, comfort, and communication while addressing recurring issues (noise, maintenance, temperature).  
- **Seasonality**: Align availability and pricing strategies with peak tourism (Oct–Dec for Auckland, Dec peak for Queenstown).  

---

## Usage

This folder is intended for:
- Stakeholders and Airbnb hosts seeking **strategic insights**.  
- Academic reviewers examining **data-driven methodologies**.  
- Practitioners in tourism, pricing strategy, and service quality improvement.

---

## References
- InsideAirbnb (2025). [Get the Data](https://insideairbnb.com/get-the-data/)  
- Wang & Nicolau (2017). *Price determinants of Airbnb rentals.* Int. J. Hospitality Mgmt.  
- Gunter & Önder (2018). *Determinants of Airbnb demand in Vienna.* Tourism Economics.  
- Xie et al. (2014). *The business value of online consumer reviews.* Int. J. Hospitality Mgmt.:contentReference[oaicite:5]{index=5}

---
