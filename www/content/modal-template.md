The uploaded CSV must contain weekly hospital admissions data and include exactly the following three columns:

1.  `date` - the date of the last day of the MMWR Week (Saturday). Accepted formats: MM/DD/YYYY, MM-DD-YYYY, or YYYY-MM-DD.

2.  `target_group` - the target group for each row of the epidemiological indicator being forecasted (e.g., “Pediatric”, “Adult”, “Overall”). An “Overall” group is required if multiple subgroups are present.

3.  `value` - the value of that epidemiological indicator for the corresponding date.


#### Optional columns

The following columns can optionally be provided, which will be used by one
or more models if included:

1. `population` - the population per target group, such as the total population, or the estimated population covered by the surveillance network. **INFLAenza** will use this column as a regression offset term. It is allowed to vary by date as well, for example, if the population changes 
each year. Providing this column may improve forecasts under certain technical conditions.
