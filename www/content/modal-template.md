The uploaded CSV must contain weekly hospital admissions data and include exactly the following three columns:

1.  `date` - the date of the last day of the MMWR Week (Saturday). Accepted formats: MM/DD/YYYY, MM-DD-YYYY, or YYYY-MM-DD.

2.  `target_group` - the target group for each row of admissions counts (e.g., “Pediatric”, “Adult”, “Overall”). An “Overall” group is required if multiple subgroups are present.

3.  `value` - the total hospital admissions for that MMWR Week (Sunday through Saturday).

To ensure robust forecasts, we recommend using post-COVID data starting from 2022 up to the week the forecast is to be made. Using less historical data may result in unanticipated behavior from some models.
