JSC370 Midterm Project - Part 1: Data Wrangling
================
Lucie Yang
March 17, 2025

This file includes all the work for data acquisition, preprocessing, and
wrangling.

# Data Acquisition

Function to call the Yelp API. An account on Yelp for Developers needs
to be made first, then get the API key. Set it with
`Sys.setenv(API_KEY = "YOUR-API-KEY")`.

``` r
auth_token <- Sys.getenv("API_KEY")

call_yelp_api <- function(offset = 0, category) {
  response <- GET(
    url   = "https://api.yelp.com/v3/businesses/search",
    query = list(
      location = 'Toronto, ON',
      term = 'restaurants',
      limit = 50,
      offset = offset,
      categories = category
      ),
    add_headers(
      'accept' = 'application/json',
      'authorization' = paste('Bearer', auth_token)
    )
  )
    
  if (status_code(response) == 200) {
    return(httr::content(response))
  } else {
    warning(paste("Error with status code:", status_code(response), 
                  httr::content(response)))
    return(NULL)  
  }
}
```

Due to limitations of Yelp API (only 240 results per query), I needed to
iterate through different search queries. I decided to iterate through
134 different categories, as defined here.

``` r
categories = c('afghani', 'african', 'arabian', 'argentine', 'armenian',
               'asianfusion', 'asturian', 'australian', 'austrian', 'baguettes',
               'bangladeshi', 'basque', 'bavarian', 'bbq', 'beergarden',
               'beerhall', 'belgian', 'bistros', 'blacksea', 'brasseries',
               'brazilian', 'breakfast_brunch', 'british', 'buffets', 'bulgarian', 
               'burgers', 'burmese', 'cafes', 'cafeteria', 'cajun', 
               'cambodian', 'caribbean', 'catalan', 'cheesesteaks', 'chicken_wings',
               'chickenshop', 'chilean', 'chinese', 'comfortfood', 'creperies',
               'cuban', 'czech', 'delis', 'dimsum', 'diners', 'dinnertheater',
               'dumplings', 'eastern_european', 'ethiopian', 'filipino', 'fishnchips',
               'food_court', 'foodstands', 'french', 'gamemeat', 'gastropubs',
               'german', 'greek', 'guamanian', 'halal', 'hawaiian', 
               'honduran', 'hotdogs', 'hotpot', 'hungarian', 'indian', 'indonesian', 
               'international', 'irish', 'israeli', 'italian', 'japanese', 
               'korean', 'kosher', 'laotian', 'latin', 'malaysian', 'mediterranean', 
               'mexican', 'mideastern', 'mongolian', 'moroccan', 'newamerican', 
               'newcanadian', 'nicaraguan', 'nightfood', 'noodles', 'oriental',
               'pakistani', 'panasian', 'persian', 'peruvian', 'pita',
               'pizza', 'polish', 'polynesian', 'popuprestaurants', 'portuguese',
               'potatoes', 'poutineries', 'pubfood', 'romanian', 'russian',
               'salad', 'sandwiches', 'scandinavian', 'scottish', 'seafood', 
               'singaporean', 'slovakian', 'somali', 'soulfood', 'soup',
               'spanish', 'srilankan', 'steak', 'sushi', 'swedish',
               'syrian', 'szechuan', 'taiwanese', 'tapas', 'tex-mex', 
               'thai', 'tradamerican', 'turkish', 'ukrainian', 'uzbek',
               'vegan', 'vegetarian', 'vietnamese', 'wok', 'wraps', 'yugoslav')

length(categories)
```

Due to limitations of the Yelp API (only 50 results for each query, and
only 240 results for the same query), I ran each category over at most 5
iterations with offsets to get the maximum results properly without
overlap.

``` r
businesses_list <- list()
for (category in categories) {
  print(paste("Category:", category))
  
  offset <- 0
  
  while (offset <= 190) {
    print(paste("Iteration:", (offset / 50) + 1, ", Offset:", offset))
    print(paste("Num Businesses:", length(businesses_list)))
    
    response_list <- call_yelp_api(offset, category)
    
    if (is.null(response_list) || length(response_list$businesses) == 0) {
      break
    }
    
    for (i in seq_along(response_list$businesses)) {
      business <- response_list$businesses[[i]]
    
      business_data <- data.frame(
        id = business$id,
        name = business$name,
        alias = business$alias,
        
        categories = ifelse(length(business$categories) > 0, 
                           paste(sapply(business$categories, function(x) x$title), 
                                 collapse = ", "), 
                           NA),
        latitude = ifelse(!is.null(business$coordinates$latitude), 
                          business$coordinates$latitude, NA),
        longitude = ifelse(!is.null(business$coordinates$longitude), 
                           business$coordinates$longitude, NA),
        phone = ifelse(!is.null(business$display_phone), business$display_phone, NA),
        price = ifelse(!is.null(business$price), business$price, NA),
        rating = ifelse(!is.null(business$rating), business$rating, NA),
        review_count = ifelse(!is.null(business$review_count), business$review_count, NA),
        is_closed = ifelse(!is.null(business$is_closed), business$is_closed, NA),
        address = ifelse(!is.null(business$location$address1), business$location$address1, NA),
        distance = ifelse(!is.null(business$distance), business$distance, NA),
        url = ifelse(!is.null(business$url), business$url, NA)
      )
      
      businesses_list <- append(businesses_list, list(business_data))
    }
    
    if (offset == 150) {
      offset <- offset + 40
    }
    
    offset <- offset + 50
  }
}

restaurants <- do.call(rbind, businesses_list)

print(restaurants)
```

I exported the data as csv for future use/merging, so I do not have to
re-call the API every time I do analysis.

``` r
write.csv(restaurants, "data/restaurants_toronto.csv", row.names = FALSE)
```

------------------------------------------------------------------------

# Data Cleaning / Wrangling

Load the data (or skip, if restaurants already exists from the previous
steps)

``` r
restaurants <- read.csv("data/restaurants_toronto.csv")
```

Remove duplicate restaurants. Some restaurants have multiple categories,
so were included in multiple category results.

``` r
restaurants <- restaurants %>%
  distinct(id, .keep_all = TRUE)
```

Checking data size, summaries, quality and errors.

``` r
dim(restaurants)
```

``` r
str(restaurants)
```

``` r
head(restaurants)
```

``` r
tail(restaurants)
```

``` r
summary(restaurants)
```

Restaurants with no reviews (and hence 0 rating) are uninformative and
will skew the analysis.

``` r
dim(restaurants %>% filter(review_count == 0))
dim(restaurants %>% filter(rating == 0))
```

There are 703 of these, which is around 10% of the data. They are
removed.

``` r
restaurants <- restaurants %>% 
  filter(review_count != 0)
```

Looking at the `is_closed` attribute in the summary, it is FALSE for all
restaurants. It is not informative, so it will be removed.

``` r
restaurants <- restaurants %>% 
  select(-is_closed)
```

Check null values in each column.

``` r
colSums(is.na(restaurants))
```

Look at which restaurants have missing latitude or longitude

``` r
restaurants %>% 
  filter(is.na(latitude) | is.na(longitude))
```

Five of the six are food stands or food trucks, which possibly do not
have a fixed address. Remove them, since there are only 6, and location
is important to the analysis

``` r
restaurants <- restaurants %>% 
  filter(!is.na(latitude) &  !is.na(longitude))
```

Group into neighbourhoods, using shapefile from [Toronto Open
Data](https://open.toronto.ca/dataset/neighbourhoods/).

``` r
neighbourhoods <- st_read("data/neighbourhoods/neighbourhoods.shp")

restaurants_sf <- st_as_sf(restaurants, coords = c("longitude", "latitude"), crs = 4326)
neighbourhoods <- st_transform(neighbourhoods, crs = st_crs(restaurants_sf)) # use same CRS

restaurants_sf <- st_join(restaurants_sf, neighbourhoods, left = TRUE) # spatial join
rest_neighbourhoods <- restaurants_sf %>% 
  st_drop_geometry() %>% 
  select(id, AREA_SH5, AREA_NA7) 
```

Join neighbourhoods with the original data (to preserve original
latitude and longitude attributes), and select only relevant columns.

``` r
restaurants <- restaurants %>% 
  left_join(rest_neighbourhoods, by = join_by(id)) %>% 
  rename(neigh_id = AREA_SH5, neighbourhood = AREA_NA7)
```

From the latitude and longitude summary earlier, it appeared there may
be some restaurants not in Toronto. This is now easy to check against
the neighbourhood data!

``` r
restaurants %>% 
  filter(is.na(neighbourhood)) %>% 
  arrange(distance)
```

From manually searching some on Google Maps, it indeed appears that they
range from being in Mississauga to being in New York! Remove them (585)
from the dataset.

``` r
restaurants <- restaurants %>% 
  filter(!is.na(neighbourhood))
```

``` r
table(restaurants$price)
```

Convert price into a factor and more understandable price levels

``` r
restaurants <- restaurants %>% 
  mutate(
    price_level = case_when(
      price == "$" ~ "Low",
      price == "$$" ~ "Medium",
      price == "$$$" ~ "High",
      price == "$$$$" ~ "Very High"
    ),
    price_level = factor(price_level, 
                         levels = c("Low", "Medium", "High", "Very High"))
  )
```

Since there are too many categories, and since restaurants can be in
multiple categories, I make larger, broader categories based on business
type and cuisine by continent

``` r
restaurants <- restaurants %>% 
  mutate (
    type = case_when(
      str_detect(categories, "Dinner Theater|Comedy|Karaoke") ~ "Entertainment",
      str_detect(categories, "Cafe|Coffee|Tea|Bakery|Bakeries|Dessert|Patisserie") ~ "Cafe/Bakery",
      str_detect(categories, "Bar|Pub|Gastropub|Club") ~ "Bar",
      str_detect(categories, "Fast Food") ~ "Fast Food",
      str_detect(categories, "Pizza|Pop-Up|Foodstands|Food Court|Hot Dogs|Poutineries|Burgers") ~ "Street Food",
      str_detect(categories, "Steakhouses") ~ "Steakhouses",
      str_detect(categories, "Bistro") ~ "Bistro",
      str_detect(categories, "Brunch|Breakfast") ~ "Brunch/Breakfast",
      str_detect(categories, "Diner") ~ "Diner",
      str_detect(categories, "Buffet") ~ "Buffet",
      str_detect(categories, "Sandwiches|Delis|Subs") ~ "Deli/Sandwiches",
      str_detect(categories, "Lounges") ~ "Lounges",
      TRUE ~ "Other"
    ),
    
    cuisine = case_when(
      str_detect(categories, "Mediterranean") ~ "Mediterranean",
      str_detect(categories, "French|German|Greek|Italian|Portuguese|Spanish|Austrian|Basque|British|Czech|Polish|Romanian|Russian|Scandinavian|Swedish|Ukrainian|Yugoslav|Modern European|Irish|Belgian") ~ "European",
      str_detect(categories, "Indian|Bangladeshi|Sri Lankan|Afghan|Pakistani|Nepalese") ~ "South Asian",
      str_detect(categories, "Chinese|Japanese|Korean|Thai|Vietnamese|Taiwanese|Hakka|Singaporean|Malaysian|Indonesian|Filipino|Mongolian|Szechuan|Laotian|Hong Kong|Cantonese|Dim Sum|Asian|Ramen|Sushi|Burmese|Oriental") ~ "East Asian",
      str_detect(categories, "Middle Eastern|Turkish|Syrian|Arabic|Lebanese|Egyptian|Persian") ~ "Middle Eastern",
      str_detect(categories, "Mexican|Cuban|Brazilian|Argentine|Latin|Caribbean|Honduran|Nicaraguan|Peruvian|Cajun|Salvadoran|Colombian|Venezuelan|Haitian") ~ "Latin American",
      str_detect(categories, "Canadian|American|Tex-Mex|Hawaiian") ~ "North American",
      str_detect(categories, "Moroccan|African|Ethiopian") ~ "African",
      str_detect(categories, "International") ~ "International",
      TRUE ~ "Other"
)
    
  )
```

I also extract the first category for each restaurant

``` r
restaurants <- restaurants %>% 
  mutate(
    first_category = str_split(categories, ",") %>%
            sapply(function(x) x[1])
  )
```

There are way too many categories to work with, and the first category
may not be the most relevant category. So, I probably won’t use this.

``` r
length(unique(restaurants$first_category))
```

View the distribution of the broader categories.

``` r
table(restaurants$type)
table(restaurants$cuisine)
```

Since there were a lot of “other” values in both broader categories, I
checked its content in to see if there were any categories that I missed
that can be put in the broader categories. If there were, I modified the
mutate code and reran to recheck.

``` r
restaurants %>%
  filter(type == "Other")

restaurants %>%
  filter(cuisine == "Other")
```

As a proxy measure for authenticity of the restaurant, I include a count
of the number of restaurants with the same name.

``` r
restaurants <- restaurants %>%
  group_by(name) %>%
  mutate(name_count = n()) %>%
  ungroup()
```

Check for outliers in numeric variables

``` r
par(mfrow=c(2, 2))
boxplot(restaurants$rating, main = "Rating")
boxplot(restaurants$review_count, main = "Review Count")
boxplot(restaurants$distance, main = "Distance")
boxplot(restaurants$name_count, main = "Same Name Count")
```

Closer look at outliers based on boxplots: Rating has 112 outliers, but
looks like reasonable observations (poor quality restaurants or low
number of reviews), so will be left in

``` r
Q1 <- quantile(restaurants$rating, 0.25)
Q3 <- quantile(restaurants$rating, 0.75)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
outliers <- restaurants %>% 
  filter(rating < lower_bound)

outliers
```

Review Count has 554 outliers, but looks like reasonable observations
(just popular restaurants?), so will be left in

``` r
Q1 <- quantile(restaurants$review_count, 0.25)
Q3 <- quantile(restaurants$review_count, 0.75)
IQR <- Q3 - Q1

upper_bound <- Q3 + 1.5 * IQR
outliers <- restaurants %>% 
  filter(review_count > upper_bound)

outliers
```

Distance has 123 outliers, but we already checked these are in Toronto,
so will be left in

``` r
Q1 <- quantile(restaurants$distance, 0.25)
Q3 <- quantile(restaurants$distance, 0.75)
IQR <- Q3 - Q1

upper_bound <- Q3 + 1.5 * IQR
outliers <- restaurants %>% 
  filter(distance > upper_bound)

outliers
```

Same name count has 1067 outliers, but it looks like every restaurant
with more than one restaurant with the same name is considered an
outlier.

``` r
Q1 <- quantile(restaurants$name_count, 0.25)
Q3 <- quantile(restaurants$name_count, 0.75)
IQR <- Q3 - Q1

upper_bound <- Q3 + 1.5 * IQR
outliers <- restaurants %>% 
  filter(name_count > upper_bound)

outliers
```

Taking a closer look at the very skewed variables

``` r
par(mfrow = c(1, 2))
hist(restaurants$review_count)
hist(restaurants$name_count)
```

Filter restaurants that have at least 3 reviews, so that the rating is
more reliable

``` r
restaurants <- restaurants %>% 
  filter(review_count >= 3)
```

Change the name_count variable to categories since most of them are 1.

``` r
restaurants <- restaurants %>% 
  mutate(
    size_level = case_when (
      name_count == 1 ~ "Single Location",
      name_count <= 3 ~ "Small Chain",
      name_count <= 10 ~ "Medium Chain",
      TRUE ~ "Large Chain"
    ),
    size_level = factor(size_level, 
                         levels = c("Single Location", "Small Chain", 
                                    "Medium Chain", "Large Chain"))
  )
```

``` r
table(restaurants$size_level)
```

Select useful/relevant variables.

``` r
restaurants <- restaurants %>% 
  select(id, name, latitude, longitude, price_level,
         rating, review_count, distance, neigh_id, neighbourhood,
         type, cuisine, size_level)
```

``` r
dim(restaurants)
```

Export as csv for next steps (for convenience, so I no longer need to
run the whole Rmd to get the cleaned data).

``` r
write.csv(restaurants, "data/cleaned_restaurants.csv", row.names = FALSE)
```
