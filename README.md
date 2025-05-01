# Toronto Restaurant Analysis
As a city with a vibrant and culturally diverse population, Toronto’s food scene features a wide variety of cuisines from around the world, ranging from fast food to family-owned cafes and fine dining restaurants. With thousands of restaurants across the city, online ratings on platforms like Yelp can have a strong influence on the decisions of individuals, as well as the marketing and operational decision-making of the business owners. Using restaurant data from Yelp, I aim to investigate the research question: *How do key factors, such as location, price, categories, authenticity and review count, contribute to the rating of a restaurant?*

The dataset used for this analysis was from the [Yelp Fusion API](https://business.yelp.com/data/products/fusion/) and consisted of 6167 restaurants. Additional neighbourhood boundary data from [Toronto Open Data](https://open.toronto.ca/dataset/neighbourhoods/) was used. First, I cleaned the data, then extracted interesting predictor variables and conducted exploratory data analysis. Then, I used various regression models to gain insight into the patterns related to the research question: a linear regression model, generalized linear mixed model, regression tree, random forest, gradient boosting models and XGBoost. These models suggested relationships between models and variable importances. Moreover, XGBoost had the lowest Test RMSE, suggesting that it had the best fit on the data.

## Organization

- [docs/](docs) - the site libraries and files
- [data/](data) - the raw and cleaned data files
- [reports/](reports) - the midterm and final report
- [data-wrangling.md](data-wrangling.md) - the data acquisition and cleaning process
- other miscellaneous files needed for site setup


## Resources
The website can be accessed at: https://lucieyang1.github.io/toronto-restaurants-analysis/

The full report is available at: [reports/final_report.pdf](reports/final_report.pdf)
