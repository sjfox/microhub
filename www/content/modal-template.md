The data need to be formatted in the exact way they were specified as part of the Paraguay Forecast Hub, otherwise the models will not work properly and the application may crash. The uploaded csv must contain weekly SARI hospital admission for the sentinel surveillance system and have the following columns with the correct definitions:

1.  `year` - this must be the calendar year for the specific week

2.  `week` - this must be the epiweek for the specific week, which is defined from Sunday to Saturday and is encoded according to the MMWR Week definition

3.  `date` - this must be the date of the last day of the MMWR Week (Saturday)

4.  `target_group` - this should correspond to the target groups of the hospital admissions counts. The example in the template represents age with the main target groups of “Pediatric”, “Adult”, and “Overall”.

5.  `value` - this should be sum total of hospital admissions from Sunday to Saturday of the specified MMWR Week. Alternatively it can be thought of as the total hospital admissions including the values from the specified date and the six days prior.

In their current forms, many of the forecasting models assume that the data will contain specific amounts of historical data, so to ensure robust forecasts we suggest using data similar to what was used in the Paraguay Forecast Hub, which includes all weekly SARI hospital admissions dating back to the first week of 2015 up to the week the forecast is to be made. Using less data may result in unanticipated forecasts from some or all of the models.
