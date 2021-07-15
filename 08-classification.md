Classification
================
Will Doyle

Classification is the process of predicting group membership.
Understanding which individuals are likely to be members of which groups
is a key task for data scientists. For instance, most recommendation
engines that are at the hear of consumer web sites are based on
classification algorithms, predicting which consumers are likely to
purchase which products.

## Pizza

Today we’ll be working with the pizza dataset, which comes from the
subreddit random acts of pizza. Each line represents a post to this
subreddit. We have various characteristics of these posts, along with
the request text from the post itself. We’ll use these characteristics
of the posts to predict whether or not the poster received pizza. This
lesson is inspired by [this
article](http://www.aaai.org/ocs/index.php/ICWSM/ICWSM14/paper/download/8106/8101).

``` r
library(knitr)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.0     ✓ dplyr   1.0.5
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(modelr)
library(yardstick)
```

    ## For binary classification, the first factor level is assumed to be the event.
    ## Use the argument `event_level = "second"` to alter this as needed.

    ## 
    ## Attaching package: 'yardstick'

    ## The following objects are masked from 'package:modelr':
    ## 
    ##     mae, mape, rmse

    ## The following object is masked from 'package:readr':
    ## 
    ##     spec

``` r
library(tidymodels)
```

    ## ── Attaching packages ────────────────────────────────────── tidymodels 0.1.2 ──

    ## ✓ broom     0.7.6      ✓ recipes   0.1.15
    ## ✓ dials     0.0.9      ✓ rsample   0.0.8 
    ## ✓ infer     0.5.3      ✓ tune      0.1.2 
    ## ✓ modeldata 0.1.0      ✓ workflows 0.2.1 
    ## ✓ parsnip   0.1.4

    ## ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
    ## x broom::bootstrap() masks modelr::bootstrap()
    ## x scales::discard()  masks purrr::discard()
    ## x dplyr::filter()    masks stats::filter()
    ## x recipes::fixed()   masks stringr::fixed()
    ## x dplyr::lag()       masks stats::lag()
    ## x yardstick::mae()   masks modelr::mae()
    ## x yardstick::mape()  masks modelr::mape()
    ## x yardstick::rmse()  masks modelr::rmse()
    ## x yardstick::spec()  masks readr::spec()
    ## x recipes::step()    masks stats::step()

``` r
library(probably)
```

    ## 
    ## Attaching package: 'probably'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.factor, as.ordered

``` r
load("za.Rdata")
```

| name             | Description                                                                  |
|------------------|------------------------------------------------------------------------------|
| got\_pizza       | Did the person who posted get pizza? 1=Yes, 0=No                             |
| got\_pizza\_f    | Did the person who posted get pizza “Yes” or “No” (factor)                   |
| karma            | The redditor’s total upvotes on Reddit.                                      |
| age              | How long has the user been on Reddit (in days).                              |
| raop\_age        | How long has the user been posting on Random Acts of Pizza (raop) (in days). |
| pop\_request     | How popular was this request?                                                |
| activity         | How many comments for this request?                                          |
| total\_posts     | How many times has the user posted on Reddit?                                |
| raop\_posts      | How many times has the user posted on Random Acts of Pizza?                  |
| prev\_raop\_post | Has the person posted previously on the subreddit?                           |
| words            | Number of words in the request                                               |
| poor             | Word “poor” appears in the post                                              |
| student          | Word “student” appears in the post                                           |
| grateful         | Word “grateful” appears in the post                                          |
| score            | Sentiment score, more positive words+, more negative words -                 |

## Conditional Means as a Classifier

We’ll start by generating some cross tabs and some quick plots, showing
the probability of receiving pizza according to several characteristics
of the post. We start with a basic crosstab of the dependent variable.
We use `prop.table` to change this from raw counts to proportions. I
also provide a brief example of how to do a table using the `kable`
function.

``` r
#Cross Tabs

za%>%
  count(got_pizza)%>% # Count numbers getting pizza
  mutate(p=prop.table(n))%>% #mutate for proportions using prop.table
  kable(format="markdown") # output to table
```

| got\_pizza |    n |         p |
|-----------:|-----:|----------:|
|          0 | 4273 | 0.7532170 |
|          1 | 1397 | 0.2462542 |
|         NA |    3 | 0.0005288 |

So, about 75% of the sample didn’t get pizza, about 25% did.

Next, we cross-tabulate receiving pizza with certain terms. First, if
the request mentioned the word “student.”

``` r
za%>%
  group_by(student,got_pizza)%>%
  summarize(n=n())%>%
  mutate(prop=n/sum(n))%>%
  subset(select=c("student","got_pizza","prop"))%>%
  spread(got_pizza,prop)%>%
  kable()
```

    ## `summarise()` has grouped output by 'student'. You can override using the `.groups` argument.

| student    |         0 |         1 |      <NA> |
|:-----------|----------:|----------:|----------:|
| No student | 0.7576047 | 0.2418213 | 0.0005739 |
| Student    | 0.7017937 | 0.2982063 |        NA |

Next, if the request mentioned the word “grateful.”

``` r
za%>%
  group_by(grateful,got_pizza)%>%
  summarize(n=n())%>%
  mutate(prop=n/sum(n))%>%
  subset(select=c("grateful","got_pizza","prop"))%>%
  spread(got_pizza,prop)%>%
  kable()
```

    ## `summarise()` has grouped output by 'grateful'. You can override using the `.groups` argument.

| grateful             |         0 |         1 |      <NA> |
|:---------------------|----------:|----------:|----------:|
| Grateful not in post | 0.7551504 | 0.2443026 | 0.0005469 |
| Grateful in post     | 0.6968085 | 0.3031915 |        NA |

Crosstabs using binary data are equivalent to generating conditional
means, as shown below.

``` r
#Predictions using conditional means

za%>%
  group_by(grateful)%>%
  summarize(mean(got_pizza,na.rm=-TRUE))
```

    ## # A tibble: 2 x 2
    ##   grateful             `mean(got_pizza, na.rm = -TRUE)`
    ##   <fct>                                           <dbl>
    ## 1 Grateful not in post                            0.244
    ## 2 Grateful in post                                0.303

We can also use conditional means to get proportions for very particular
sets of characteristics. In this case, what about individuals who
included some combination of the terms “grateful”,“student” and “poor”
in their posts?

``` r
za%>%group_by(grateful,student)%>%summarize(mean(got_pizza))
```

    ## `summarise()` has grouped output by 'grateful'. You can override using the `.groups` argument.

    ## # A tibble: 4 x 3
    ## # Groups:   grateful [2]
    ##   grateful             student    `mean(got_pizza)`
    ##   <fct>                <fct>                  <dbl>
    ## 1 Grateful not in post No student            NA    
    ## 2 Grateful not in post Student                0.290
    ## 3 Grateful in post     No student             0.284
    ## 4 Grateful in post     Student                0.474

``` r
za_sum<-za%>%
  group_by(grateful,student,poor)%>%
  summarize(mean_pizza=mean(got_pizza))
```

    ## `summarise()` has grouped output by 'grateful', 'student'. You can override using the `.groups` argument.

``` r
kable(za_sum)
```

| grateful             | student    | poor             | mean\_pizza |
|:---------------------|:-----------|:-----------------|------------:|
| Grateful not in post | No student | Poor not in post |          NA |
| Grateful not in post | No student | Poor in post     |   0.2549020 |
| Grateful not in post | Student    | Poor not in post |   0.2933673 |
| Grateful not in post | Student    | Poor in post     |   0.2571429 |
| Grateful in post     | No student | Poor not in post |   0.2839506 |
| Grateful in post     | No student | Poor in post     |   0.2857143 |
| Grateful in post     | Student    | Poor not in post |   0.5000000 |
| Grateful in post     | Student    | Poor in post     |   0.0000000 |

## Probability of Receiving Pizza, Using Various Terms in Post

``` r
gg<-ggplot(za_sum,aes(x=grateful,y=mean_pizza,fill=grateful))
gg<-gg+geom_bar(stat="identity")
gg<-gg+facet_wrap(~student+poor)
gg
```

    ## Warning: Removed 1 rows containing missing values (position_stack).

![](08-classification_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Logistic regression as a classifier

Logistic regression is set up to handle binary outcomes as the dependent
variable. The downside to logistic regression is that it is modeling the
log odds of the outcome, which means all of the coefficients are
expressed as log odds, which no one understands intuitively.

We’re going to use the “tidymodels” approach to running this model,
which works much better for a standard data science workflow. It begins
with splitting the data into testing and training datasets using the
`initial_split` function.

``` r
# Training and testing datasets

za_split<-initial_split(za,prop=.5)

za_train<-training(za_split)

za_test<-testing(za_split)
```

Next we set up the terms of the model.

``` r
#  Model terms

za_formula<-as.formula("got_pizza_f~
             age+
             karma+
             total_posts+
             raop_posts+
             student+
             grateful+
             pop_request+
             score")
```

Next we’ll set up the recipe. Notice the use of a log function for total
posts, which follow a classic exponential distribution. We use
`offset=1` to avoid attempting to take the log of 0.

``` r
logit_rec<-recipe(za_formula, data=za)%>%
  step_log(total_posts,offset = 1)
```

Now we’re going to run the model. This approach is a little different
than what we’ve been using up until now. We’re going to create a
`logit_mod` object by fitting a logistic regression to our outcome. The
`set_engine` function says what particular kind of logistic regression
we want to fit– in this case we want to do classification

``` r
logit_mod <- 
  logistic_reg() %>% 
  set_engine("glm")%>%
  set_mode("classification")
```

## Put the workflow together

``` r
logit_wf<-workflow()%>%
  add_recipe(logit_rec)%>%
  add_model(logit_mod)
```

``` r
logit_results<-fit(logit_wf,data=za_train)
```

``` r
logit_results%>%
  tidy()
```

    ## # A tibble: 9 x 5
    ##   term                        estimate std.error statistic  p.value
    ##   <chr>                          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)              -1.58       0.0804      -19.7   4.78e-86
    ## 2 age                       0.0000851  0.000178      0.479 6.32e- 1
    ## 3 karma                    -0.00000288 0.0000122    -0.236 8.13e- 1
    ## 4 total_posts               0.0995     0.0361        2.75  5.89e- 3
    ## 5 raop_posts                0.925      0.150         6.15  7.97e-10
    ## 6 studentStudent            0.228      0.159         1.44  1.50e- 1
    ## 7 gratefulGrateful in post  0.590      0.217         2.72  6.50e- 3
    ## 8 pop_request               0.0275     0.00514       5.36  8.29e- 8
    ## 9 score                    -0.0137     0.00899      -1.53  1.27e- 1

With these results in hand we can generate predicted classifications.

We can convert the predictions to a binary variable by setting a
“threshold” of .5. Any prediction above .5 is considered to be a 1,
anything below, a 0. We’ll compare the actual “truth” of whether or not
someone got a pizza with our prediction from the model using what’s
called a “confusion matrix” (really).

``` r
logit_results%>%
  predict(za_test)%>%
  bind_cols(za_test)%>%
  conf_mat(truth=got_pizza_f,estimate=.pred_class)
```

    ##           Truth
    ## Prediction   No  Yes
    ##        No  2117  655
    ##        Yes   27   31

The confusion matrix generated here is explained
[here](https://topepo.github.io/caret/measuring-performance.html#class).

We’re usually interested in three things: the overall accuracy of a
classification is the proportion of cases accurately classified. The
sensitivity is the proportion of “ones” that are accurately classified
as ones– it’s the probability that a case classified as positive will
indeed be positive. Specificity is the probability that a case
classified as 0 will indeed be 0.

## Accuracy: proportion correctly identifed

``` r
logit_results%>%
  predict(za_test)%>%
  bind_cols(za_test)%>%
  metrics(truth=got_pizza_f,estimate=.pred_class)
```

    ## # A tibble: 2 x 3
    ##   .metric  .estimator .estimate
    ##   <chr>    <chr>          <dbl>
    ## 1 accuracy binary        0.759 
    ## 2 kap      binary        0.0473

## Sensitivity, probability of saying it’s a Yes when it’s really a yes

``` r
logit_results%>%
  predict(za_test)%>%
  bind_cols(za_test)%>%
 sens(truth=got_pizza_f,estimate=.pred_class,event_level="second")
```

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 sens    binary        0.0452

## Specificity, probability of saying it’s a No when it’s really a No

``` r
logit_results%>%
  predict(za_test)%>%
  bind_cols(za_test)%>%
  spec(truth=got_pizza_f,estimate=.pred_class,event_level="second")
```

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 spec    binary         0.987

*Question: how do you get perfect specificity? How do you get perfect
sensitivity?*

\#Thresholds

As we vary the threshold from 0 to 1, the sensitivity will decrease,
while the specificity will increase. The best models will be able to
have both high sensitivity and specificty at an threshold. The code
below shows what happens to sensitivity and specificity as thresholds go
from 0 to 1.

``` r
th<-logit_results%>%
  predict(za_test,type="prob")%>%
  bind_cols(za_test)%>%
   threshold_perf(truth=got_pizza_f,
                 estimate=.pred_Yes,
                 thresholds=seq(0,1,by=.1),metrics=c("sens","spec"))

ggplot(filter(th,.metric%in%c("sens","spec")),
       aes(x=.threshold,y=.estimate,color=.metric))+
  geom_line()
```

![](08-classification_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

## Area Under the Curve (AUC)

The area under the curve considers both the sensitivity (does the model
accurately predict every positive outcome) with the specificity (does
the model accurately predict every negative outcome) for a given model,
and does so across every possible threshold value.

``` r
logit_results%>%
  predict(za_test,type="prob")%>%
  bind_cols(za_test)%>%
  roc_auc(truth=got_pizza_f,.estimate=.pred_Yes,event_level="second")
```

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.575

``` r
logit_results%>%
  predict(za_test,type="prob")%>%
  bind_cols(za_test)%>%
  roc_curve(truth=got_pizza_f,.estimate=.pred_Yes,event_level="second")%>%
  autoplot()
```

![](08-classification_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

## Use “last fit” to get same results.

``` r
logit_final<-last_fit(logit_wf,za_split)
```

    ## 
    ## Attaching package: 'rlang'

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     %@%, as_function, flatten, flatten_chr, flatten_dbl, flatten_int,
    ##     flatten_lgl, flatten_raw, invoke, list_along, modify, prepend,
    ##     splice

    ## 
    ## Attaching package: 'vctrs'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     data_frame

    ## The following object is masked from 'package:tibble':
    ## 
    ##     data_frame

``` r
logit_final$.metrics
```

    ## [[1]]
    ## # A tibble: 2 x 4
    ##   .metric  .estimator .estimate .config             
    ##   <chr>    <chr>          <dbl> <chr>               
    ## 1 accuracy binary         0.759 Preprocessor1_Model1
    ## 2 roc_auc  binary         0.575 Preprocessor1_Model1

# Plotting results from logisitc regression

Because individual coefficients are so hard to understand, most of the
time we convert the results to predicited probabilities, using a range
of hypothetical values, as in the code below.

``` r
hypo_data<-za_train%>%data_grid(
  age=mean(age,na.rm=TRUE),
  karma=mean(karma,na.rm=TRUE),
  total_posts=mean(total_posts,na.rm=TRUE),
  raop_posts=seq_range(raop_posts,n=100),
  student=as_factor(levels(student)),
  grateful=as_factor(levels(grateful)[1]),
  pop_request=mean(pop_request,na.rm=TRUE),
  score=mean(score,na.rm=TRUE)
)

plot_data<-logit_results%>%
  predict(hypo_data,type="prob")%>%
  bind_cols(hypo_data)%>%
  rename(`Post Includes "Student"`=student)

plot_data%>%
ggplot(aes(x=raop_posts,y=.pred_Yes,color=`Post Includes "Student"`))+
  geom_line()+
  xlab("Number of Posts on RAOP")+
  ylab("Prob(Pizza)")
```

![](08-classification_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->
