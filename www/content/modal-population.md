If population data corresponding to the target groups are uploaded, the INFLAenza model will run with a population offset. This template provides Paraguay population data across two age groups ("Pediatric" and "Adult") with an "Overall" category that is the sum of the "Pediatric" and "Adult" populations.

The uploaded csv must contain population data that correspond to the specified target groups and have the following columns with the correct definitions:

1.  `year` - use this optional column is if there is substantial historic population data.

2.  `target_group` - this mandatory column should correspond to the target groups of the hospital admissions counts. The example in the template represents age with the main target groups of “Pediatric”, “Adult”, and “Overall”.

3.  `population` - this mandatory column should be sum total of people in each target group. If an "Overall" category is applicable, the sum of the individual targets should equal the "Overall" category.