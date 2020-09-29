Lab 3
================

<img src="https://mk0labs2lovei6j157sd.kinstacdn.com/wp-content/uploads/2014/03/ChocolateYellowBrownLabs_2880x1000px-e1508873405125.jpg" width="100px" />

<p style="color:rgb(182,18,27);font-family:corbel">

Econ B2000, MA Econometrics

</p>

<p style="color:rgb(182,18,27);font-family:corbel">

Kevin R Foster, the Colin Powell School at the City College of New York,
CUNY

</p>

<p style="color:rgb(182,18,27);font-family:corbel">

Fall 2020

</p>

For this lab, we will use simple k-nn techniques of machine learning to
try to guess people’s neighborhoods. Knn is a fancy name for a really
simple procedure:

  - take an unclassified observation
  - look for classified observations near it
  - guess that it is like its neighbors

We’ll zoom into groups. You get the rest of the class to prepare and
will write up results in homework. I had posted the 20-min lecture
already but don’t worry if you haven’t watched that yet or read the
notes, you can do that over the week ahead.

The idea here is to try to classify people into neighborhood. You’ve
probably done this in your ordinary life: meet someone and guess what
neighborhood they live in. Here we try to train the computer, using the
PUMS data again.

Start with looking at the differences in means of some of the variables
and put that together with your own knowledge of the city. You might
want to subset the data – are you trying to predict everbody? Are young
people easier? Retirees? College grads?

Then use a k-nn classification. Start by just trying to predict the
borough not the neighborhood. Create this factor:

``` r
dat_NYC <- subset(acs2017_ny, (acs2017_ny$in_NYC == 1)&(acs2017_ny$AGE > 20) & (acs2017_ny$AGE < 66))
attach(dat_NYC)
borough_f <- factor((in_Bronx + 2*in_Manhattan + 3*in_StatenI + 4*in_Brooklyn + 5*in_Queens), levels=c(1,2,3,4,5),labels = c("Bronx","Manhattan","Staten Island","Brooklyn","Queens"))
```

What variables do we think are relevant in classifying by borough?
**NOT** PUMA since neighborhood likely perfectly classifies… I’ll give
an example, where I try income\_total, owner\_cost combined with
rent\_cost. You should find other data to do better.

Here is some code to get you started. Best to normalize – here is a bit
of code to get the data to all be in the (0,1) interval.

``` r
norm_varb <- function(X_in) {
  (max(X_in, na.rm = TRUE) - X_in)/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE) )
}
```

Next, fix up the data,

``` r
is.na(OWNCOST) <- which(OWNCOST == 9999999)
housing_cost <- OWNCOST + RENT
norm_inc_tot <- norm_varb(INCTOT)
norm_housing_cost <- norm_varb(housing_cost)
```

Here we create the dataframe to use,

``` r
data_use_prelim <- data.frame(norm_inc_tot,norm_housing_cost)
good_obs_data_use <- complete.cases(data_use_prelim,borough_f)
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(borough_f,good_obs_data_use)
```

Next split the data into 2 parts: one part to train the algo, then the
other part to test how well it works for new data. Here we use an 80/20
split.

``` r
set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]
```

Finally run the k-nn algo and compare against the simple means,

``` r
summary(cl_data)
prop.table(summary(cl_data))
summary(train_data)
require(class)
for (indx in seq(1, 9, by= 2)) {
 pred_borough <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
num_correct_labels <- sum(pred_borough == true_data)
correct_rate <- num_correct_labels/length(true_data)
print(c(indx,correct_rate))
}
```

Now find some other data that will do a better job of classifying. How
good can you get it to be? At what point do you think there might be a
tradeoff between better classifying the training data and doing worse at
classifying the test data?

Can you classify neighborhoods better? Perhaps there are some variables
that easily classify certain neighborhoods? Try it.
