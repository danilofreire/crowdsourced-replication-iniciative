# Variables

## L2data (country-level)

None of these variables have labels in the original data. I'm not 100% sure they mean what I think they do, so take the list below with a grain of salt.

* Immigrant Stock - `foreignpct`
* Change in Immigrant Stock - `netmigpct`
* Social Welfare Expenditures - `socx`
* Employment Rate - `emprate`

Then `country` and `year`.

## ZA2900 (International Social Survey Program, 1996)

* Old Age Care - `v39`
* Unemployed - `v41`
* Reduce Income Differences - `v42`
* Jobs - `v36`
* Female - `v200`
* Age - `v201`
* Education categories (Primary or less, Secondary and University or more; with secondary as reference) - `v205` (_note: we might need to recode that_)
* Employment categories (Part-time, Not active, Active unemployed, and Full-time; with full-time as the reference category) - `v207` (maybe `v206`? :confused:) (_note: we also have to check if that's the right reference category_)
* Country - `v3`

The project coordinator also writes: "_Respondents chose among ordinal categories of definitely should be, probably should be, probably should not be, and definitely should not be for each. These are collapsed into a dichotomous variable where affirmative answers =1._"

## ZA4700 (ISSP, 2006)

Note: The variable numbers are _not_ the same in the two datasets.

* Old Age Care - `V28`
* Unemployed - `V30`
* Reduce Income Differences - `V31`
* Jobs - `V25`
* Female - `sex`
* Age - `age`
* Education categories (Primary or less, Secondary and University or more; with secondary as reference) - `degree` (_note: we might need to recode that_)
* Employment categories (Part-time, Not active, Active unemployed, and Full-time; with full-time as the reference category) - `spwrkst` (?) (note: _we also have to check if that's the right reference category_)
* Country - `V3`

_I think_ those are ther correct labels. I've checked the labels several times and I hope they are right. 
