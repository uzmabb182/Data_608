#  FED Mandate Analysis Dashboard (Shiny App)

This interactive R Shiny app analyzes whether the **Federal Reserve (FED)** has successfully met its dual mandate:
- Maintain **low inflation**
- Achieve **maximum employment**

Users can explore historical trends, customize thresholds, and view how inflation and unemployment affect interest rate decisions.

---

## 🔧 Features

-  **Time Series Visualizations** for:
  - Fed Funds Rate
  - Inflation Rate (with user-defined threshold)
  - Unemployment Rate (with user-defined threshold)

-  **Mandate Status Bar Charts** showing:
  - Monthly mandate achievement for each year

-  **Data Table** of merged and filtered records

-  **FED Effectiveness Summary**:
  - % of months where mandate was met, partially met, or missed

-  **Impact Analysis Tab**:
  - View regression and scatter plot showing how inflation and unemployment affect the Fed rate

---

##  Live App (optional)

>  [Click here to launch the app on Shinyapps.io](https://yourname.shinyapps.io/fed-mandate-app)  
> *(You can remove this section if you're not hosting the app online yet)*

---

##  How to Run Locally

###  Prerequisites

- [R](https://cran.r-project.org/)
- [RStudio](https://posit.co/download/rstudio-desktop/)
- R packages: `shiny`, `tidyverse`, `lubridate`, `plotly`, `shinydashboard`, `DT`, `reshape`

# FED Mandate Analysis Dashboard

...

##  Files

- `app.R` — Full Shiny application code
- `README.md` — This file

## 📊 Data Sources

All datasets are pulled live from public GitHub links:
- [Unemployment Data](https://raw.githubusercontent.com/uzmabb182/Data_608/refs/heads/main/Week_2/unemployment_rate.csv)
- [Fed Funds Rate](https://raw.githubusercontent.com/uzmabb182/Data_608/refs/heads/main/Week_2/fed_fund_rate.csv)
- [Consumer Price Index](https://raw.githubusercontent.com/uzmabb182/Data_608/refs/heads/main/Week_2/consumer_price_index.csv)

##  About the Project

Created as part of the **Data 608 - Data Visualization** course at **CUNY SPS**.  
Analyzes how the Federal Reserve responds to changing economic conditions over time.

##  Author

**Mubashira Qari**  
 [Add your contact info or LinkedIn if desired]




