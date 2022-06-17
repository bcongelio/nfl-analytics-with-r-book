# An Introduction to NFL Analytics and the R Programming Language

Need to discuss `nflfastR` versus `nflreadr` prior to all of this. Basically comes down to loading both packages but, ultimately, mostly using `nflreadr` since it has become a "catch all" package for the `nflverse`.

As well, if you do not yet have R, RStudio, and the required packages loaded, please see Chapter \#\# for instructions on getting all of the necessary components installed on your computer.

## `nflreadr`: Weekly vs. Play-by-Play Stats

Within `nflreadr`, you have two main options for retrieving NFL statistics. The first is running the function `load_player_stats()` while the other is `load_pbp`.

There is a **very important distinction between the two**.

Take this example: let's say you want to get Ben Roethlisberger's total passing yards from the 2021 season and start to do so using the `load_pbp` function:




```r
data <- nflreadr::load_pbp(2021)

ben.passing <- data %>%
  group_by(passer) %>%
  filter(passer == "B.Roethlisberger" & pass == 1 & season_type == "REG") %>%
  summarize(total.yards = sum(yards_gained, na.rm = T))

tibble(ben.passing)
```

```
## # A tibble: 1 x 2
##   passer           total.yards
##   <chr>                  <dbl>
## 1 B.Roethlisberger        3523
```

Based on the above code, using `load_pbp()`, we get a return that Ben Roethlisberger had 3,523 passing yards during the 2021 NFL regular season. However, if you check his [Pro Football Reference page](https://www.pro-football-reference.com/players/R/RoetBe00.htm), you will see that his actual number of passing yards during this period was 3,740.

So, what gives? Shouldn't calcuating the total number of passing yards using the official NFL play-by-play data provide accurate results?

The short answer: it can, but it takes a bit more leg work to make it happen. To try to get it as close as possible using `load_pbp()`, we will have to add some other varibles into the `filter()` function:


```r
ben.updated <- data %>%
  group_by(passer) %>%
  filter(passer == "B.Roethlisberger" & complete_pass == 1 & sack == 0 & !is.na(down) &
           penalty == 0) %>%
  summarize(total.yards = sum(passing_yards, na.rm = T))

tibble(ben.updated)
```

```
## # A tibble: 1 x 2
##   passer           total.yards
##   <chr>                  <dbl>
## 1 B.Roethlisberger        3889
```

As you can see, numerous variables were added to the `filter()` function. To eliminate yardage loss due to sacks, we are now filtering to plays where `sack == 0`, meaning there is NOT a sack on the play. As well, we are filtering any plays where a `down` is not associated which drops any two-point conversion attempt. Finally, we are getting the yardage for just `complete_pass` and dropping any play where a penalty happened.

Unfortunately, despite this, we are now overshooting Ben's total passing yards during the 2021 regular season as the code above results in an output of 3,889 yards.

This is a perfect example of the difference between `load_pbp()` and `load_player_stats()`. In the case of the first, it can be exceedingly difficult to get the "official statistics" to output because of the number of variables that need to be accounted for.

Fortunately, the `nflverse` team developed the `load_player_stats()` function to avoid this type of problem. As an example, let's do the same scenario as above:


```r
weekly.data <- nflreadr::load_player_stats(2021)

ben.weekly <- weekly.data %>%
  group_by(player_name) %>%
  filter(player_name == "B.Roethlisberger" & season_type == "REG") %>%
  summarize(total.yards = sum(passing_yards))

tibble(ben.weekly)
```

```
## # A tibble: 1 x 2
##   player_name      total.yards
##   <chr>                  <dbl>
## 1 B.Roethlisberger        3740
```

## Getting Started: Grabbing 2021 Season Data

All chapter sections start with a second-level (`##`) or higher heading followed by your section title, like the sections above and below here. You can have as many as you want within a chapter.

### Exercise: QB Aggresiveness on 3rd Down

## Getting Started: Retrieving 2021 Weekly Stats

### Exercise: 2021 QB Air Yards per Attempt Leaders

## Retrieving Multiple Data for Multiple Seasons

## Other Sources of Data for NFL Analytics
