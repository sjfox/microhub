Should the population offset be used during model fitting, if provided?

If population data corresponding to the target groups are uploaded, INFLAenza will run with a population offset. The data template's optional `population` column provides example population data across two age groups ("Pediatric" and "Adult") with an "Overall" category that is the sum of the "Pediatric" and "Adult" populations.

The uploaded CSV must contain population data that correspond to the specified target groups and have the following columns:

1.  `year` - optional; use this column if you have historical population figures that change from year to year.

2.  `target_group` - required; should correspond to the target groups of the hospital admissions counts. The example in the template represents age with the main target groups of "Pediatric", "Adult", and "Overall".

3.  `population` - required; the total number of people in each target group. If an "Overall" category is applicable, the sum of the individual targets should equal the "Overall" category.
