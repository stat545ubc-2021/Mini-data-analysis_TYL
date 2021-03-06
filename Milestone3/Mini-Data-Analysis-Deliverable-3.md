Mini Data-Analysis Deliverable 3
================

# Welcome to your last milestone in your mini data analysis project\!

In Milestone 1, you explored your data and came up with research
questions. In Milestone 2, you obtained some results by making summary
tables and graphs.

In this (3rd) milestone, you’ll be sharpening some of the results you
obtained from your previous milestone by:

  - Manipulating special data types in R: factors and/or dates and
    times.
  - Fitting a model object to your data, and extract a result.
  - Reading and writing data as separate files.

**NOTE**: The main purpose of the mini data analysis is to integrate
what you learn in class in an analysis. Although each milestone provides
a framework for you to conduct your analysis, it’s possible that you
might find the instructions too rigid for your data set. If this is the
case, you may deviate from the instructions – just make sure you’re
demonstrating a wide range of tools and techniques taught in this class.

## Instructions

**To complete this milestone**, edit [this very `.Rmd`
file](https://raw.githubusercontent.com/UBC-STAT/stat545.stat.ubc.ca/master/content/mini-project/mini-project-3.Rmd)
directly. Fill in the sections that are tagged with `<!--- start your
work here--->`.

**To submit this milestone**, make sure to knit this `.Rmd` file to an
`.md` file by changing the YAML output settings from `output:
html_document` to `output: github_document`. Commit and push all of your
work to your mini-analysis GitHub repository, and tag a release on
GitHub. Then, submit a link to your tagged release on canvas.

**Points**: This milestone is worth 40 points (compared to the usual 30
points): 30 for your analysis, and 10 for your entire mini-analysis
GitHub repository. Details follow.

**Research Questions**: In Milestone 2, you chose two research questions
to focus on. Wherever realistic, your work in this milestone should
relate to these research questions whenever we ask for justification
behind your work. In the case that some tasks in this milestone don’t
align well with one of your research questions, feel free to discuss
your results in the context of a different research question.

# Setup

Begin by loading your data and the tidyverse package below:

``` r
library(datateachr) # <- might contain the data you picked!
library(tidyverse)
library(forcats)
library(lubridate)
library(ggplot2)
```

From Milestone 2, you chose two research questions. What were they? Put
them here.

<!-------------------------- Start your work below ---------------------------->

1.  *Does different species have different relationships between the
    tree age and diameter?*
2.  *Is there any spatial trend in the tree height or diameter in
    different areas?*
    <!----------------------------------------------------------------------------->

# Exercise 1: Special Data Types (10)

For this exercise, you’ll be choosing two of the three tasks below –
both tasks that you choose are worth 5 points each.

But first, tasks 1 and 2 below ask you to modify a plot you made in a
previous milestone. The plot you choose should involve plotting across
at least three groups (whether by facetting, or using an aesthetic like
colour). Place this plot below (you’re allowed to modify the plot if
you’d like). If you don’t have such a plot, you’ll need to make one.
Place the code for your plot below.

<!-------------------------- Start your work below ---------------------------->

``` r
# (0) Prepare the needed dataset (VanTreeUBC)

# Filter observations in specific five areas
chosen_area <- c("ARBUTUS-RIDGE","DUNBAR-SOUTHLANDS","KITSILANO","SHAUGHNESSY","WEST POINT GREY")
VanTreeUBC <- filter(vancouver_trees, neighbourhood_name %in% chosen_area) %>%
  mutate(tree_age = as.numeric(difftime(as.Date("2021-10-09"),date_planted))/365)

# Filter observations by amounts: Keep the first 5% (totally 11 species) based on observation amounts.
VanTreeUBC_summary <-VanTreeUBC %>%
  group_by(species_name) %>%
  summarise(across(diameter, .f=list("mean"=mean,"min"=min,"max"=max,"median"=min,"sd"=sd),na.rm=TRUE),n=n()) %>%
  mutate(range = diameter_max-diameter_min) 
VanTreeUBC_summary <- filter(VanTreeUBC_summary, n>=quantile(VanTreeUBC_summary$n,probs=0.95))

# "VanTreeUBC_chosen" will be used in the following analysis.
VanTreeUBC_chosen<- VanTreeUBC %>%
  filter(species_name %in% VanTreeUBC_summary$species_name)
```

<!----------------------------------------------------------------------------->

Now, choose two of the following tasks.

1.  Produce a new plot that reorders a factor in your original plot,
    using the `forcats` package (3 points). Then, in a sentence or two,
    briefly explain why you chose this ordering (1 point here for
    demonstrating understanding of the reordering, and 1 point for
    demonstrating some justification for the reordering, which could be
    subtle or speculative.)

2.  Produce a new plot that groups some factor levels together into an
    “other” category (or something similar), using the `forcats`
    package (3 points). Then, in a sentence or two, briefly explain why
    you chose this grouping (1 point here for demonstrating
    understanding of the grouping, and 1 point for demonstrating some
    justification for the grouping, which could be subtle or
    speculative.)

3.  If your data has some sort of time-based column like a date (but
    something more granular than just a year):
    
    1.  Make a new column that uses a function from the `lubridate` or
        `tsibble` package to modify your original time-based column. (3
        points)
          - Note that you might first have to *make* a time-based column
            using a function like `ymd()`, but this doesn’t count.
          - Examples of something you might do here: extract the day of
            the year from a date, or extract the weekday, or let 24
            hours elapse on your dates.
    2.  Then, in a sentence or two, explain how your new column might be
        useful in exploring a research question. (1 point for
        demonstrating understanding of the function you used, and 1
        point for your justification, which could be subtle or
        speculative).
          - For example, you could say something like “Investigating the
            day of the week might be insightful because penguins don’t
            work on weekends, and so may respond differently”.

<!-------------------------- Start your work below ---------------------------->

**Task Number**: 1

Produce a new plot that reorders a factor in your original plot, using
the **forcats** package.

``` r
# Convert "species_name" from  "chr"  to  "fct"
VanTreeUBC_chosen$species_name <- as.factor(VanTreeUBC_chosen$species_name)
head(VanTreeUBC_chosen$species_name)
```

    ## [1] CERASIFERA  AMERICANA   KOBUS       RUBRUM      AMERICANA   PLATANOIDES
    ## 11 Levels: ACERIFOLIA   X AMERICANA CERASIFERA EUCHLORA   X ... SYLVATICA

``` r
# Boxplot (in Ans1.2.2) from my milestone 2
bp1 <- ggplot(VanTreeUBC_chosen,aes(y=species_name,x=diameter))+
  geom_boxplot(alpha=0.8)
bp1+ggtitle("Boxplot-1 (from Milestone2): Species against Diameter")+xlab("Diameter (inch)")+ylab("Species")
```

![](Mini-Data-Analysis-Deliverable-3_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
# Create a new data frame with the levels of "species_name" in increasing order of their average diameter.
VanTreeUBC_spe_dia <- VanTreeUBC_chosen %>%
  mutate(species_name = forcats::fct_reorder(species_name,diameter, mean))

# New boxplot 
bp2 <- ggplot(VanTreeUBC_spe_dia, aes(y=species_name,x=diameter)) +
  geom_boxplot(alpha=0.8)
bp2+ggtitle("Boxplot-2 (reordered): Species against Diameter")+xlab("Diameter (inch)")+ylab("Species")
```

![](Mini-Data-Analysis-Deliverable-3_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
attributes(VanTreeUBC_chosen$species_name)
```

    ## $levels
    ##  [1] "ACERIFOLIA   X" "AMERICANA"      "CERASIFERA"     "EUCHLORA   X"  
    ##  [5] "HIPPOCASTANUM"  "KOBUS"          "PLATANOIDES"    "PSEUDOPLATANUS"
    ##  [9] "RUBRUM"         "SERRULATA"      "SYLVATICA"     
    ## 
    ## $class
    ## [1] "factor"

``` r
attributes(VanTreeUBC_spe_dia$species_name)
```

    ## $levels
    ##  [1] "SYLVATICA"      "RUBRUM"         "KOBUS"          "CERASIFERA"    
    ##  [5] "EUCHLORA   X"   "SERRULATA"      "PSEUDOPLATANUS" "PLATANOIDES"   
    ##  [9] "AMERICANA"      "HIPPOCASTANUM"  "ACERIFOLIA   X"
    ## 
    ## $class
    ## [1] "factor"

In the original plot (Boxplot-1), it’s hard to find a trend of tree
diameters. After ordering the species names with the mean diameters, I
can distinguish the overall variations across species. It helps to
understand that different species might have various conditions
regarding the diameters. I notice that the “ACEROFLOLIA” has the largest
average around 30 inches in diameters.

<!----------------------------------------------------------------------------->

<!-------------------------- Start your work below ---------------------------->

**Task Number**: 3

Make a new column that uses a function from the lubridate or tsibble
package to modify your original time-based column.

``` r
# Extract "date_planted" from the original dataset, and then convert it into "ymd" format
planted <- VanTreeUBC_chosen %>%
  select(date_planted) %>% 
  mutate(date_planted=ymd(date_planted)) 

# Assign end date (same as what I defined in Milestone 1)
planted$end_ref <-ymd("2021-10-09")

# Create a new interval variable
int <- interval(planted$date_planted,planted$end_ref)

# Compute tree age information by "lubridate::time_length" function
VanTreeUBC_chosen$tree_age_2 <- lubridate::time_length(int, "year")

# Compare the tree age information computed by two different ways
diff_max <- max(VanTreeUBC_chosen$tree_age_2-VanTreeUBC_chosen$tree_age,na.rm=TRUE)
diff_min <- min(VanTreeUBC_chosen$tree_age_2-VanTreeUBC_chosen$tree_age,na.rm=TRUE)
cat("Differences between two tree age columns: max = ",diff_max," , min = ",diff_min,"\n")
```

    ## Differences between two tree age columns: max =  -0.002739726  , min =  -0.02191781

In order to analyze the relationships between tree age and tree growth,
I have tried to use “**difftime**” to calculate the time length of trees
for the “tree\_age” variable. However, there is no “year” option in the
unit as far as I know. So, I just calculated the time lengths in day
unit, and then divided them by 365 as the year lengths. Considering the
normal/leap year, the tree age values might be influenced by the
computation in this way. In this section, I used the
“**lubridate::time\_length**” function to obtain the tree age
information and compare the two columns. According to the results, the
two series are quite similar. Hence, my analysis will not be influenced
by the way I used to produce the “tree\_age” information.

<!----------------------------------------------------------------------------->

# Exercise 2: Modelling

## 2.0 (no points)

Pick a research question, and pick a variable of interest (we’ll call it
“Y”) that’s relevant to the research question. Indicate these.

<!-------------------------- Start your work below ---------------------------->

**Research Question**:

*Does different species have different relationships between the tree
age and diameter?*

**Variable of interest**:

**tree age** and **diameter**

<!----------------------------------------------------------------------------->

## 2.1 (5 points)

Fit a model or run a hypothesis test that provides insight on this
variable with respect to the research question. Store the model object
as a variable, and print its output to screen. We’ll omit having to
justify your choice, because we don’t expect you to know about model
specifics in STAT 545.

  - **Note**: It’s OK if you don’t know how these models/tests work.
    Here are some examples of things you can do here, but the sky’s the
    limit.
      - You could fit a model that makes predictions on Y using another
        variable, by using the `lm()` function.
      - You could test whether the mean of Y equals 0 using `t.test()`,
        or maybe the mean across two groups are different using
        `t.test()`, or maybe the mean across multiple groups are
        different using `anova()` (you may have to pivot your data for
        the latter two).
      - You could use `lm()` to test for significance of regression.

<!-------------------------- Start your work below ---------------------------->

**Statistical Method**: linear regression **Purpose**: To understand the
relationships between tree age and diameter across different species.

``` r
# Plot all groups and observe the trends
plt_2.1.1 <- ggplot(VanTreeUBC_spe_dia,aes(tree_age,diameter))+
  geom_point(alpha=0.4,size=0.5,na.rm=TRUE)+
  xlab("Tree age (year)")+ylab("Diameter (inch)")+
  facet_wrap(~species_name)
plt_2.1.1+ggtitle("Plot_2.1.1: Relationships between tree age and diameter across species")
```

![](Mini-Data-Analysis-Deliverable-3_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
# Check the number of all levels in the reordered variable(VanTreeUBC_spe_dia)
attributes(VanTreeUBC_spe_dia$species_name)
```

    ## $levels
    ##  [1] "SYLVATICA"      "RUBRUM"         "KOBUS"          "CERASIFERA"    
    ##  [5] "EUCHLORA   X"   "SERRULATA"      "PSEUDOPLATANUS" "PLATANOIDES"   
    ##  [9] "AMERICANA"      "HIPPOCASTANUM"  "ACERIFOLIA   X"
    ## 
    ## $class
    ## [1] "factor"

``` r
# Define the number and the name string for the following analysis
NL<-nlevels(VanTreeUBC_spe_dia$species_name)
NML<-levels(VanTreeUBC_spe_dia$species_name)

# Compute linear models for all species and record the fitting results
rs<-data.frame()
for (i in seq(1,NL))
{
  sp<-VanTreeUBC_spe_dia %>%
  filter(species_name==NML[i])%>%
  drop_na()
  if(nrow(sp)>10) {
  md<-lm(formula= diameter ~ tree_age, sp)
  px<-seq(0,max(sp$tree_age))
  py<-px*md$coefficients[2]+md$coefficients[1]
  pred<-data.frame(px,py)
  rs <- rbind(rs,broom::glance(md))
  }  else{
    rs[i,]<-NA
    }
}

# Arrange the statistics according to the magnitudes of the R-squared values
rs$id<-seq(1,NL)
rs<-arrange(rs,desc(r.squared))


# Find species result(s) with R-squared larger than 0.5
#     (i.e. the relationship(s) between tree age and diameter is/are strong enough)
rs_keep<-rs %>% filter(r.squared>=0.5) 

# Re-calculate the linear model of the kept species
# ---Extract kept species data
sp1<-VanTreeUBC_spe_dia %>%
  filter(species_name==NML[rs_keep$id])%>%
  drop_na()
# ---Compute linear model
md1<-lm(formula= diameter ~ tree_age, sp1)
# ---Create a sequence for plotting fitting result
px1<-seq(0,max(sp1$tree_age))
py1<-px1*md1$coefficients[2]+md1$coefficients[1]
pred<-data.frame(px1,py1) # Prediction according to the linear model (md1)
# ---Plot the results
ts <-sprintf("Plot_2.1.2: Species: %s\nLinearRegression: y = %.3f*x%.3f\nR-squared=%.4f",
             NML[rs_keep$id],md1$coefficients[2],md1$coefficients[1],rs_keep$r.squared)
plt_2.1.2<-ggplot(sp1)+geom_point(aes(tree_age,diameter),alpha=0.6,na.rm=TRUE)+
  geom_line(data=pred,aes(x=px1,y=py1))+xlab("Tree age (x)")+ylab("Diameter (y)")
plt_2.1.2+ggtitle(ts)
```

![](Mini-Data-Analysis-Deliverable-3_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->
<!----------------------------------------------------------------------------->

## 2.2 (5 points)

Produce something relevant from your fitted model: either predictions on
Y, or a single value like a regression coefficient or a p-value.

  - Be sure to indicate in writing what you chose to produce.
  - Your code should either output a tibble (in which case you should
    indicate the column that contains the thing you’re looking for), or
    the thing you’re looking for itself.
  - Obtain your results using the `broom` package if possible. If your
    model is not compatible with the broom function you’re needing, then
    you can obtain your results by some other means, but first indicate
    which broom function is not compatible.

<!-------------------------- Start your work below ---------------------------->

``` r
Ans2.2<-broom::glance(md1)
print(Ans2.2)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic  p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1     0.615         0.614  3.44      731. 7.51e-97     1 -1218. 2442. 2454.
    ## # … with 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

``` r
outputs<-sprintf("Species=%s, Slope= %.4f, Intercept=%.4f, R-squared=%.4f\n", NML[rs_keep$id],md1$coefficients[2],md1$coefficients[1],rs_keep$r.squared)
cat("Linear regression result\n",outputs)
```

    ## Linear regression result
    ##  Species=SERRULATA, Slope= 0.4334, Intercept=-0.2727, R-squared=0.6153

Based on Plt\_2.1.1, it’s hard to find obvious trends in the subplots.
Therefore, I tried to compute the linear regression models to understand
those relationships. According to the table “rs” computed in **Answer
2.1**, I found that the R-squared results are pretty small. Only one
species has a strong relationship between tree\_age and diameter, which
has an R-squared larger than 0.5, so I only plotted this species.

The species name is “**SERRULATA**”.

First, I printed out the results obtained from “**broom::glance**”
function. In this part, I got the statistics of the fitting results of
the linear model. It helps me understand that there is an obvious
relationship between tree age and the diameter by a large R-squared
value of 0.615.

The slope, intercept, and R-squared values are shown above. Basically,
the diameters of **SERRULATA** became larger when the trees got older.
However, we can see that there is a vertical cluster at the right end of
the plot. Hence, the diameters might be influenced by not only tree ages
but also other variables. If we want to know the reason for the vertical
cluster on the right end of the plot, we need to further study it.

<!----------------------------------------------------------------------------->

# Exercise 3: Reading and writing data

Get set up for this exercise by making a folder called `output` in the
top level of your project folder / repository. You’ll be saving things
there.

## 3.1 (5 points)

Take a summary table that you made from Milestone 2 (Exercise 1.2), and
write it as a csv file in your `output` folder. Use the `here::here()`
function.

  - **Robustness criteria**: You should be able to move your Mini
    Project repository / project folder to some other location on your
    computer, or move this very Rmd file to another location within your
    project repository / folder, and your code should still work.
  - **Reproducibility criteria**: You should be able to delete the csv
    file, and remake it simply by knitting this Rmd file.

<!-------------------------- Start your work below ---------------------------->

``` r
# Summary table I made from Milestone 2
Ans1.2.1_d <-VanTreeUBC %>%
  group_by(species_name) %>%
  summarise(across(diameter, .f=list("mean"=mean,"min"=min,"max"=max,"median"=min,"sd"=sd),na.rm=TRUE),n=n()) %>%
  mutate(range = diameter_max-diameter_min) 

# Output the csv file
write_csv(Ans1.2.1_d, here::here("output","SummaryTableFromMilestone-2.csv"))
```

<!----------------------------------------------------------------------------->

## 3.2 (5 points)

Write your model object from Exercise 2 to an R binary file (an RDS),
and load it again. Be sure to save the binary file in your `output`
folder. Use the functions `saveRDS()` and `readRDS()`.

  - The same robustness and reproducibility criteria as in 3.1 apply
    here.

<!-------------------------- Start your work below ---------------------------->

``` r
saveRDS(md1, file=here::here("output","Milestone-3_LM.rds"));
LM<-readRDS(here::here("output","Milestone-3_LM.rds"))
```

<!----------------------------------------------------------------------------->

# Tidy Repository

Now that this is your last milestone, your entire project repository
should be organized. Here are the criteria we’re looking for.

## Main README (3 points)

There should be a file named `README.md` at the top level of your
repository. Its contents should automatically appear when you visit the
repository on GitHub.

Minimum contents of the README file:

  - In a sentence or two, explains what this repository is, so that
    future-you or someone else stumbling on your repository can be
    oriented to the repository.
  - In a sentence or two (or more??), briefly explains how to engage
    with the repository. You can assume the person reading knows the
    material from STAT 545A. Basically, if a visitor to your repository
    wants to explore your project, what should they know?

Once you get in the habit of making README files, and seeing more README
files in other projects, you’ll wonder how you ever got by without
them\! They are tremendously helpful.

## File and Folder structure (3 points)

You should have at least four folders in the top level of your
repository: one for each milestone, and one output folder. If there are
any other folders, these are explained in the main README.

``` r
# Create the folders (for first time only)
#dir.create(here::here("Milestone1"))
#dir.create(here::here("Milestone2"))
#dir.create(here::here("Milestone3"))
```

Each milestone document is contained in its respective folder, and
nowhere else.

Every level-1 folder (that is, the ones stored in the top level, like
“Milestone1” and “output”) has a `README` file, explaining in a
sentence or two what is in the folder, in plain language (it’s enough to
say something like “This folder contains the source for Milestone 1”).

## Output (2 points)

All output is recent and relevant:

  - All Rmd files have been `knit`ted to their output, and all data
    files saved from Exercise 3 above appear in the `output` folder.
  - All of these output files are up-to-date – that is, they haven’t
    fallen behind after the source (Rmd) files have been updated.
  - There should be no relic output files. For example, if you were
    knitting an Rmd to html, but then changed the output to be only a
    markdown file, then the html file is a relic and should be deleted.

Our recommendation: delete all output files, and re-knit each
milestone’s Rmd file, so that everything is up to date and relevant.

PS: there’s a way where you can run all project code using a single
command, instead of clicking “knit” three times. More on this in STAT
545B\!

## Error-free code (1 point)

This Milestone 3 document knits error-free. (We’ve already graded this
aspect for Milestone 1 and 2)

## Tagged release (1 point)

You’ve tagged a release for Milestone 3. (We’ve already graded this
aspect for Milestone 1 and 2)
