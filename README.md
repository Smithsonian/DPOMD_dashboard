# DPOMD Dashboard

Dashboard to display general and detailed statistics of digitization projects by DPO Mass Digitization, OCIO, Smithsonian (https://dpo.si.edu/mass-digitization-program).

Written in R/Shiny. 

Packages required:

 * shiny
 * dplyr
 * shinyWidgets
 * DT
 * plotly
 * shinycssloaders

The app reads an .RData file that contains the dataframes used in each table and plot. This RData file is created by another script that reads the data from a Postgres database to generate summary statistics and time series. The script it run every Monday morning using cron.

## The Dashboard

The dashboard displays four tabs:

### Summary - List of projects and main summary statistics

![summary](https://user-images.githubusercontent.com/2302171/81701791-788ff080-9438-11ea-97b8-d3eb4ebcebdf.png)

### Progress in Select Projects - Figures of the number of images captured by day or month for some projects

![progress](https://user-images.githubusercontent.com/2302171/81701798-7af24a80-9438-11ea-934c-d1e203032730.png)

### Daily/Monthly Statistics - Tables of images and objects digitized by day and month, plus buttons to download the data in CSV/Excel.

![stats](https://user-images.githubusercontent.com/2302171/81701802-7cbc0e00-9438-11ea-949d-276e82a60c6a.png)

