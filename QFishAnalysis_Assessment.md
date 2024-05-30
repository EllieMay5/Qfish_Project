---
title: "QFishAnalysis_Assessment.Rmd"
author: "Ellana Pierce"
date: "2024-05-17"
output: html_document
---

Part 1: Obtaining data 

``` r
# setwd("/Users/ellanapierce/Desktop/Data_Science/QFish_Project/CommercialLineFish")
getwd()
```

```
## [1] "/Users/ellanapierce/Desktop/Data_Science/QFish_Project"
```

``` r
Fish <- read.csv("CommercialLineFish.csv")
```


``` r
file.exists("/Users/ellanapierce/Desktop/Data_Science/QFish_Project/CommercialLineFish.csv") 
```

```
## [1] TRUE
```

``` r
#False return, then TRUE return, now that i have fixed the wording/path errors.
```

I have chosen the data I want to use from the Qfish website and data set, exported it to csv from download in excel, and saved to my new WD that is connected to my GitHub repository.
I have also called the data to an object called Fish, so it's easier for analysis than typing the full name each time. 

Let's have a look at the data, to makesure it's come into R and looks right.

``` r
view(Fish)
```

```
## Error in view(Fish): could not find function "view"
```

``` r
# Plot 1: Bar Plot of Total Licences per Year
ggplot(data = Fish_clean, aes(x = Year, y = Licences)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Total Licences Per Year", x = "Year", y = "Total Licences") +
  theme_minimal()
```

```
## Warning in min(x): no non-missing arguments to min; returning Inf
```

```
## Warning in max(x): no non-missing arguments to max; returning -Inf
```

```
## Warning in min(d[d > tolerance]): no non-missing arguments to min; returning Inf
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range
## (`geom_bar()`).
```

![plot of chunk Check the data](figure/Check the data-1.png)
Looks good!

Part 2: Tidying the Data

``` r
#install.packages("tidyverse")
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ───────────────────────── tidyverse 2.0.0 ──
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
## ✔ purrr     1.0.2     ✔ tidyr     1.3.1
## ✔ readr     2.1.5     
## ── Conflicts ─────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```



``` r
# Load necessary libraries
# Load necessary libraries
library(ggplot2)
#install.packages("dplyr")
library(dplyr)

# Convert Year, Licences, Days, and Tonnes columns to numeric
Fish$Year <- as.character(Fish$Year)
Fish$Licences <- as.numeric(Fish$Licences)
Fish$Days <- as.numeric(Fish$Days)
Fish$Tonnes <- as.numeric(gsub(",", "", Fish$Tonnes))  # Remove commas before converting to numeric

# Verify the changes
str(Fish)
```

```
## 'data.frame':	36 obs. of  4 variables:
##  $ Year    : chr  "1990" "1991" "1992" "1993" ...
##  $ Licences: num  660 606 566 614 585 610 719 844 829 814 ...
##  $ Days    : num  24863 23212 25423 29976 30298 ...
##  $ Tonnes  : num  3613 3688 3801 3854 3749 ...
```

``` r
# Print summary to check data types
str(Fish)
```

```
## 'data.frame':	36 obs. of  4 variables:
##  $ Year    : chr  "1990" "1991" "1992" "1993" ...
##  $ Licences: num  660 606 566 614 585 610 719 844 829 814 ...
##  $ Days    : num  24863 23212 25423 29976 30298 ...
##  $ Tonnes  : num  3613 3688 3801 3854 3749 ...
```

``` r
summary(Fish_clean)
```

```
##       Year        Licences        Days         Tonnes     
##  Min.   : NA   Min.   :108   Min.   :456   Min.   :50.28  
##  1st Qu.: NA   1st Qu.:108   1st Qu.:456   1st Qu.:50.28  
##  Median : NA   Median :108   Median :456   Median :50.28  
##  Mean   :NaN   Mean   :108   Mean   :456   Mean   :50.28  
##  3rd Qu.: NA   3rd Qu.:108   3rd Qu.:456   3rd Qu.:50.28  
##  Max.   : NA   Max.   :108   Max.   :456   Max.   :50.28  
##  NA's   :1
```
I've downloaded the package I need for making and editing my plots. 

I have tidied my data in R as needed prior, just out of habit and not wanting to double handle.

Checking for missing values... allgood. I will run the function na.omit ust in case I missed a missing val!

``` r
summary(Fish)
```

```
##      Year              Licences           Days            Tonnes         
##  Length:36          Min.   : 108.0   Min.   :   456   Min.   :    50.28  
##  Class :character   1st Qu.: 389.5   1st Qu.: 17815   1st Qu.:  2261.14  
##  Mode  :character   Median : 469.0   Median : 22964   Median :  3053.18  
##                     Mean   : 546.1   Mean   : 49142   Mean   :  6385.72  
##                     3rd Qu.: 671.5   3rd Qu.: 33016   3rd Qu.:  4165.79  
##                     Max.   :1693.0   Max.   :884555   Max.   :114943.04
```


``` r
library(ggplot2)

# Convert Year and Tonnes columns to numeric if not already done
Fish$Year <- as.numeric(Fish$Year)
```

```
## Warning: NAs introduced by coercion
```

``` r
Fish$Tonnes <- as.numeric(gsub(",", "", Fish$Tonnes))  # Remove commas before converting to numeric

# Create the plot with customised colors, background, legend, and grey grid lines
ggplot(Fish, aes(x = Year, y = Tonnes, fill = "Tonnes")) +
  geom_bar(stat = "identity", color = "navy", fill = "grey", size = 0.5) +  # Thinner navy border
  labs(x = "Year", y = "Tonnes") +  # Removed the title here
  scale_x_continuous(breaks = Fish$Year) +
  theme(axis.title = element_text(size = 14, face = "bold"),  # Increase font size for axis titles
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10),  # Increase font size for x-axis labels
        axis.text.y = element_text(size = 10),  # Increase font size for y-axis labels
        panel.background = element_rect(fill = "white", color = "black"),  # Set white background with black border
        panel.grid.major = element_line(color = "grey"),  # Add grey major grid lines
        legend.position = "top",  # Position the legend at the top
        legend.title = element_blank(),  # Remove legend title
        legend.key = element_rect(fill = "lightblue", color = "black")) +  # Customize legend key
  guides(fill = guide_legend(title = "Legend"))  # Add legend title
```

```
## Warning: Removed 2 rows containing missing values or values outside the scale range
## (`geom_bar()`).
```

![plot of chunk Converting my data as needed](figure/Converting my data as needed-1.png)

Plot interpretation: Having a look at my plot, I can see the time series shows a few things.
Firstly, there is an overall increase in catches from 1990 to the early 2000's.
This increase, after peaking prior to 2003, begins a downwards trend
with the catch per year decreasing uniformly for the next approx 20 years until present time.
I believe the climb then decline in the catch tonnes per year, could be for a few different reasons.

- Firstly, the intense fishing pressures of the initial years prior to 2000, 
could have led to a depletion in the populations of the fisheries targeted species. 
This hypothesis, also relates to the possibly disruption of the dynamic marine ecosystems in general,
when these larger predatory fish are removed - then leading to changes in the overall fish population, 
their role in the ecosystem and food web. 

- Secondly, it could be due to the changes in regulation and management of fishing areas and practices.
The conservation and protection of marine areas in particular, has grown immensely over the past 25 years.
Efforts like Marine Protected Areas (MPA) of different levels which can introduce the restrictions around certain gear,
fishing practices, amounts or species taken and more.
This has all become an absolute necessity in especially the past 15 years,
with many populations in decline and fisheries history showing the impacts of over fishing
and the vital need for certain implementations to allow targeted fish stocks to recover.
These can be introduced as quotas, seasonal restrictions (breeding/size LHS related to sp.),
and like mentioned earlier - MPA's/

- Thirdly, in conjunction with the past two hypothesis, the gear advancements that have occurred over this time frame,
has meant that more selective fishing have taken over from destructive methods like longline fishing,
in turn reducing bycatch and paving the way (slowly) for more sustainable fishing practices!
Furthermore, technilogical advancements have also allowed for better practice,
mitigation of bycatch, and more selective practices for commercial fishing along with the massive
growth of aquaculture and in particular, onland aquaculture too!

- Lastly, to add to the previous points - Globally and especially in more westernised cultures like Aus and USA,
many people are becoming increasingly concerned about the practices, degredation,
proccessing and many other factors of how things end up on their plate.
This change in consumerism can also heavily impact the market value of targeted species
and encourages large corporations and commercial fishing companies to move towards more
sustainable and long-term practices that align with their customers needs. 

Figure 1. Annual catch (in tonnes) from 1990 to 2023. The data shows an increasing
trend in catch until the early 2000s, followed by a significant decline.
This trend reflects the depletion of fish populations due to overfishing and the
subsequent implementation of stricter fishery management practices and a shift
towards more selective fishing methods.



Optional extra:
Let's have a looksie at how we can plot this data in different ways.

``` r
ggplot(data = Fish, aes(x = Year, y = Tonnes)) +
  geom_line() +
  geom_point() +
  labs(title = "Total Tonnes Over the Years", x = "Year", y = "Tonnes")
```

```
## Warning: Removed 2 rows containing missing values or values outside the scale range
## (`geom_line()`).
```

```
## Warning: Removed 2 rows containing missing values or values outside the scale range
## (`geom_point()`).
```

![plot of chunk Line Plot](figure/Line Plot-1.png)

Part 3: Add your plot and markdown report to e-portfolio

knit("mb5370_module_1_report.Rmd") # produces a markdown (.md) file in your folder.
markdownToHTML("mb5370_module_1_report.md", "mb5370_module_1_report.html", fragment.only=TRUE) # produces clean .html file in your folder to paste into Google Sites.

``` r
#install.packages("knitr")
#install.packages("markdown")
library(knitr)
library(markdown)
knit("QFishAnalysis_Assessment.Rmd") # produces a markdown (.md) file in your folder.
```

```
## 
## 
## processing file: QFishAnalysis_Assessment.Rmd
```

```
## Error in parse_block(g[-1], g[1], params.src, markdown_mode): Duplicate chunk label 'QFish', which has been used for the chunk:
## # setwd("/Users/ellanapierce/Desktop/Data_Science/QFish_Project/CommercialLineFish")
## getwd()
## Fish <- read.csv("CommercialLineFish.csv")
```

``` r
markdownToHTML("QFishAnalysis_Assessment.Rmd", "QFishAnalysis_Assessment.Rmd.html", fragment.only=TRUE) # produces clean .html file in your folder to paste into Google Sites.
```





