Penguins Plots
================
Katie
2025-09-17

# Penguin Plot Practice

### Setting Up Our Environment

**Notes**: To set up our environment we will start by loading the
‘tidyverse’, ‘palmer penguins’, and ‘ggplot2’ packages.

\*Be sure that the packages have been installed first using
`install.package("tidyverse")` and the like.

``` r
library(tidyverse)
library(palmerpenguins)
library(ggplot2)
```

### Preparing Our Data

The aim is to visualise the relationship between size of penguins
differentiated by species. A great first step is to figure out what
we’re working with data-wise.

- `colnames()` will list out the names of the columns.

- `head()` will print the first 6 lines of data.

``` r
colnames(penguins)
```

    ## [1] "species"           "island"            "bill_length_mm"   
    ## [4] "bill_depth_mm"     "flipper_length_mm" "body_mass_g"      
    ## [7] "sex"               "year"

``` r
head(penguins)
```

    ## # A tibble: 6 × 8
    ##   species island    bill_length_mm bill_depth_mm flipper_length_mm body_mass_g
    ##   <fct>   <fct>              <dbl>         <dbl>             <int>       <int>
    ## 1 Adelie  Torgersen           39.1          18.7               181        3750
    ## 2 Adelie  Torgersen           39.5          17.4               186        3800
    ## 3 Adelie  Torgersen           40.3          18                 195        3250
    ## 4 Adelie  Torgersen           NA            NA                  NA          NA
    ## 5 Adelie  Torgersen           36.7          19.3               193        3450
    ## 6 Adelie  Torgersen           39.3          20.6               190        3650
    ## # ℹ 2 more variables: sex <fct>, year <int>

As we can see, there are a few lines that are missing data. We are going
to take these out of our results by creating a dataframe with a new
name.

We can also create new columns where we are measuring body mass and
flipper length in kilograms and meters instead of grams and millimeters
for ease of understanding.

``` r
penguins2 <- penguins %>%
              drop_na() %>%
              mutate(body_mass_kg=body_mass_g/1000, flipper_length_m=flipper_length_mm/1000)

head(penguins2)
```

    ## # A tibble: 6 × 10
    ##   species island    bill_length_mm bill_depth_mm flipper_length_mm body_mass_g
    ##   <fct>   <fct>              <dbl>         <dbl>             <int>       <int>
    ## 1 Adelie  Torgersen           39.1          18.7               181        3750
    ## 2 Adelie  Torgersen           39.5          17.4               186        3800
    ## 3 Adelie  Torgersen           40.3          18                 195        3250
    ## 4 Adelie  Torgersen           36.7          19.3               193        3450
    ## 5 Adelie  Torgersen           39.3          20.6               190        3650
    ## 6 Adelie  Torgersen           38.9          17.8               181        3625
    ## # ℹ 4 more variables: sex <fct>, year <int>, body_mass_kg <dbl>,
    ## #   flipper_length_m <dbl>

Now we can get some stats about these species. We’ll do that by creating
a tibble.

``` r
penguins2 %>%
  group_by(species) %>%
  summarise(mean_bill_length_mm = mean(bill_length_mm),
            mean_flipper_length_m = mean(flipper_length_m),
            mean_body_mass_kg = mean(body_mass_kg))
```

    ## # A tibble: 3 × 4
    ##   species   mean_bill_length_mm mean_flipper_length_m mean_body_mass_kg
    ##   <fct>                   <dbl>                 <dbl>             <dbl>
    ## 1 Adelie                   38.8                 0.190              3.71
    ## 2 Chinstrap                48.8                 0.196              3.73
    ## 3 Gentoo                   47.6                 0.217              5.09

From this data, we can see that flipper length and body mass correspond
more closely with one another. That would make sense as we would expect
to see larger penguins have larger flipper to support and propel their
increased mass.

### Plot It Out

Lets see what this all stacks up to.

``` r
ggplot(data = penguins2) +
  geom_point(mapping = aes(x=flipper_length_m, y=body_mass_kg,colour = species))
```

![](Penguins-Plots_files/figure-gfm/Skeleton%20Plot-1.png)<!-- -->

That’s looking okay, but we can add a bit of pizzazz:

- We can use features like `geom_jitter()` to separate the points of our
  scatter plot to be able to see them more easily.

- `geom_smooth()` adds a trend line.

- We can also use `facet_wrap()` to separate out a variable and create
  individual graphs to compare.

``` r
mindate <- min(penguins2$year) 
maxdate <- max(penguins$year) ##gathering metadata for the caption, so that if new penguin measurements are added to our original database we will have an immediately updated caption

ggplot(data = penguins2) +
  geom_jitter(mapping = aes(x=flipper_length_m, y=body_mass_kg,colour = species)) +
  geom_smooth(mapping = aes(x=flipper_length_m, y=body_mass_kg)) +
  labs(title = "Comparative Sizes of Penguins",
       caption = paste0("Palmer Penguin Study ", mindate, " - ", maxdate),##here's where they come in!
       x= "Flipper Length (m)",
       y= "Body Mass (kg)") +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap(~species)
```

![](Penguins-Plots_files/figure-gfm/Final%20Plot-1.png)<!-- -->

Here we can finally see the clear jump in overall size when it comes to
the Gentoo penguins. With trendlines, facets, and labels we have a
visualisation that is immediately and intuitively understood by viewers.

<figure>
<img
src="https://cdn.download.ams.birds.cornell.edu/api/v1/asset/612764627/2400"
alt="A majestic Gentoo" />
<figcaption aria-hidden="true">A majestic Gentoo</figcaption>
</figure>
