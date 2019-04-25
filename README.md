# surgery-census-modeling


The number of patients in the surgery ward(s) of a hospital at a point in time is referred to as the **surgery census**. Hospital operations managers may need to ensure that the daily surgery census follows specific patterns. As an example, it may be desirable reduce variability in census across days of the week, as this variability can cause very high workloads on certain days.

Two main factors affect the surgery census: 
* **Inflow**: the pattern of "arrivals" coming out of the operating room (OR). Specifically, there are two groups of patients of interest: those who are discharged from the hospital directly after surgery in the OR, and those who will stay in the surgery ward for longer. 
* **Length of stay**: If the arrival rate remains the same, but patients stay longer on average, this will increase the average census, and vice versa. 

This project allows users to analyze changes in the weekly pattern of the surgery census, based on changes in the surgical schedule and patients' length of stay distributions in various surgical specialties. 

\  
\  

Here's overview of the simulation model: 

![model diagram](https://raw.githubusercontent.com/nayefahmad/surgery-census-modeling/master/data/model-overview-diagram.jpg)

