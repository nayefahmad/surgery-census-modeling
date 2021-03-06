# Surgery census modeling


## Background 
The number of patients in the surgery ward(s) of a hospital at a point in time is referred to as the **surgery census**. Hospital operations managers may need to ensure that the daily surgery census follows specific patterns. As an example, it may be desirable reduce variability in census across days of the week, as this variability can cause very high workloads on certain days.

Two main factors affect the surgery census: 
* **Inflow**: the pattern of "arrivals" coming out of the operating room (OR). Specifically, there are two groups of patients of interest: those who are discharged from the hospital directly after surgery in the OR, and those who will stay in the surgery ward for longer. 
* **Length of stay**: If the arrival rate remains the same, but patients stay longer on average, this will increase the average census, and vice versa. 

This project allows users to analyze changes in the weekly pattern of the surgery census, based on changes in the surgical schedule and patients' length of stay distributions in various surgical specialties. 

Here's overview of the simulation model: 

![model diagram](https://raw.githubusercontent.com/nayefahmad/surgery-census-modeling/master/data/model-overview-diagram.jpg)


## Repo structure
* **data**: Three CSV files. Two of them are alternative surgery schedules, specifying, the number of same-day discharge ("SDC") cases and multiple-day LOS ("SDA") cases for each day of the week, and for each surgical specialty. 
* **src**: To run the simulation, you only need to use the master file, *00_master_census-simulations.R*. The other two files process the data from the input CSVs, and define functions that are used in the master file. 
* **results/dst**: This is where outputs from the master R file will be saved. 

## Quickstart guide
1. Open the input schedule CSV file in the **data** folder. The filename is *surgery-weekly-schedule.csv*. 
2. Vary the number of SDCs and SDAs as necessary to specify the scenario you're interested in. For example, you may want to investigate the effect of reserving one day of the week for SDCs only. 
3. Launch an RStudio session using the .RProj file in the project root folder. 
4. In RStudio, run the master file from the **src** folder. Filename: *00_master_census-simulations.R*
5. View results in the **results/dst** folder. 


