#Potential dependent Variable to examine
total_ag_income <- read_dta('raw_data/glss4_new2/aggregates/agg2.dta')



#I think by looking at the relationship between some of these aggregate variables
#will help us to test the variables that are added together to make them!

rent_income <- read_dta('raw_data/glss4_new2/aggregates/agg4.dta')

remittance_income <- read_dta('raw_data/glss4_new2/aggregates/agg5.dta')


food_expense <- read_dta('raw_data/glss4_new2/aggregates/agg7.dta')

house_expense <- read_dta('raw_data/glss4_new2/aggregates/agg8.dta')


other_expense <- read_dta('raw_data/glss4_new2/aggregates/agg9.dta')


estimated_other_expense <- ('raw_data/glss4_new2/aggregates/agg11.dta')


remittance_expense <- read_dta('raw_data/glss4_new2/aggregates/agg12.dta')

#Potential dependent Variable to examine: Total income from agriculture per household
total_ag_income <- read_dta('raw_data/glss4_new/aggregates/agg2.dta')

#Included this expense because I assume homes that are used as farmland and or crop processing would pay higher bills
house_expense <- read_dta('raw_data/glss4_new/aggregates/agg8.dta')

#Maybe we can look at the rental income of households who rent out land to be farmed on.
rent_income <- read_dta('raw_data/glss4_new/aggregates/agg4.dta')

#Remittance is when foreign workers transfer money to their home country. Not educational or agricultural, but it might have some effect on a household or education
remittance_income <- read_dta('raw_data/glss4_new/aggregates/agg5.dta')
remittance_expense <- read_dta('raw_data/glss4_new/aggregates/agg12.dta')

#Households that are employed in aggriculture might spend less money on food than the typical worker