# Advertising_multiple-linear-regression-model
To  investigate the relationship between sales and the explanatory variables facebook, youtube and newspaper.
The data in the Advertising.txt file contains the impact of three advertising media (facebook, youtube and newspaper) on sales for a sample of 200 companies.

You can load the data into R using acommand similar to read.table("C:/R/Advertising.txt",header=T). Note that you will need to specify your own path to the file.
The recorded variables are as follows: 
• facebook: Facebook advertising budget in thousands of dollars
• youtube: YouTube advertising budget in thousands of dollars
• newspaper: Newspaper advertising budget in thousands of dollars
• sales: Company sales in hundred-thousands of dollars

### Task: 
1. Produce a plot showing the pairwise relationships between the four variables. Comment on your results.
2. It is proposed to fit a linear regression model with sales as the response variable, and facebook, youtube, newspaper as explanatory variables. Fit the proposed multiple linear regression model, including all three explanatory variables but no interaction terms. Carry out appropriate analyses to decide which terms to retain in the model. 
3. It an effort to improve the analysis in part (b), it is now proposed to fit a linear regression model with sales as the response variable, and √youtube , facebook, as explanatory variables. Fit the new proposed multiple linear regression model, including the two explanatory variables together with an interaction term. 
