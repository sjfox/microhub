The uploaded CSV must contain weekly hospital admissions data and include exactly the following three columns:

1.  `date` - the date of the last day of the MMWR Week (Saturday). Accepted formats: MM/DD/YYYY, MM-DD-YYYY, or YYYY-MM-DD.

2.  `target_group` - the target group for each row of the epidemiological indicator being forecasted (e.g., “Pediatric”, “Adult”, “Overall”). An “Overall” group is required if multiple subgroups are present.

3.  `value` - the value of that epidemiological indicator for the corresponding date. The template's example uses raw weekly hospital admissions (counts), but `value` may also be uploaded as a proportion -- a fraction between 0 and 1, such as test positivity or bed occupancy -- when Data Type on this tab is set to "Proportion (0-1)".


#### Optional columns

1. `retrospective_group` - runs every model and reference week independently for each distinct value of this column, using only that value's own rows (e.g., one full retrospective run per location). The template's example uses four placeholder locations — CountryA, CountryB, CountryC, and CountryD — to be replaced with your own location names. Each location also includes the "Pediatric", "Adult", and "Overall" target groups, with "Overall" always equal to "Pediatric" + "Adult" for that location and week.

    Every value of `retrospective_group` must cover the exact same set of weeks — if one group is missing a week another has, the upload will be rejected. Because different groups can be in different hemispheres or climates, the Retrospective tab lets you set a separate Local Seasonality zone for each group after upload, rather than sharing one zone across all of them.

2. `population` - the population per target group, such as the total population, or the estimated population covered by the surveillance network. **INFLAenza** will use this column as a regression offset term. It is allowed to vary by date as well, for example, if the population changes each year. Providing this column may improve forecasts under certain technical conditions.
