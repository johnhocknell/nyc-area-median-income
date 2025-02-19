# **Measuring Area Median Income in NYC**

**Author**: John Hocknell  
**Date**: Fall 2024

Measuring AMI is an R project that creates an interactive web map application to
help stakeholders understand how area median income (AMI) is measured in New 
York City and how it can be improved. The app can be accessed at
[https://johnhocknell.shinyapps.io/NYCAMI/](https://johnhocknell.shinyapps.io/NYCAMI/).
The paper explaining the application and AMI in depth can be accessed at
[https://docs.google.com/document/d/1QhQUplf6-l1bgd-Ci-qiQVEyNrg28LZpqIOOAbAInFs/edit?usp=sharing](https://docs.google.com/document/d/1QhQUplf6-l1bgd-Ci-qiQVEyNrg28LZpqIOOAbAInFs/edit?usp=sharing).

---

## File Structure
* Data
  * Tabular
  * Vector
* Output
* Scripts
  * **get_data.R**: Acquires, cleans, formats, and saves median income data 
  with geometries for efficient access by the interactive Shiny app. Median 
  family income (MFI) and median household income (MHI) are sourced from the
  Census 2022 American Community Survey.
  * **app.R**: Creates an interactive Shiny map app analyzing area median 
  incomes in NYC by allowing users to use a widget to select from the following 
  geographic scales: county, community district tabulation area (CDTA), zip code
  tabulation area (ZCTA), and Census tract.
  
## Packages used
* tidyr
* dplyr
* sf
* leaflet
* googlesheets4
* geojsonio
* openxlsx
* tidycensus
* zctaCrosswalk
* tigris
* htmlwidgets
* shiny
* classInt
* googlesheets4

# Usage
This R project can be used to run this Shiny app on your local machine. Follow
the link above to access the publicly shared application deployed and hosted 
through shintapps.io, a self-service platform that runs the self-contained 
application in the cloud. This service was chosen for its ease of use with the 
Shiny package and its free tier. When being visited, the application is active 
and accrues active hours, regardless of the number of concurrent visitors. Free 
accounts have a maximum of twenty-five active hours per month, and going over 
this limit will make the application unavailable until the next usage period. 
After ten minutes of no use, the application goes into an idle state and does 
not accrue active hours. Visiting the application when it is idle will 
reactivate it, and the first user to do this will experience longer loading 
times.
