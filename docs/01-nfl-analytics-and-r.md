# An Introduction to NFL Analytics and the R Programming Language (\*rough draft)



As mentioned in the Preface of this book, the `nflverse` has drastically expanded since the inception of `nflfastR` in April of 2020. In total, the current version of the `nflverse` is comprised of five separate R packages:

1.  `nflfastR`
2.  `nflseedR`
3.  `nfl4th`
4.  `nflreadr`
5.  `nflplotR`

Installing the `nflverse` as a package in R will automatically install all five packages. However, the core focus of this book will be on `nflreadr`. It is understadable if you are confused by that, since the Preface of this book introduce the `nflfastR` package.

Because of that, it is important to note that the `nflreadr` package, as explained by its author (Tan Ho), is a "minimal package for downloading data from `nflverse` repositories. It inclues caching, optional progress updates, and data dictionaries." In other words, using `nflreadr` allows for quick and easy access to all the data needed for you to work through this book and learn how to do NFL analytics in R.

Using `nflreadr::` while coding will provide you nearly idential options as using `nflfastR::`. Moreover, the `nflreadr` packages inclues a number of data options not included in `nflfastR` such as combine, draft picks, contract, trades, injury information, and access to statistics on Pro Football Reference.

While `nflfastR` did initially serve as the foundation of the "amateur NFL analytics" movement, the `nflreadr` package has superceded by serving as the "catchall" package for all the various bits and pieces of the `nflverse`. Because of this, and to maintain consistency throughout, this book - nearly exclusively - will use `nflreadr::` when calling functions housed within the `nflverse` rather than `nflfastR::`.

## `nflreadr`: An Introduction to the Data

The most important part of the `nflverse` is, of course, the data. To begin, we will examine the core data that underpins the `nflverse`: player weekly stats and the more advanced and robust play-by--play data. Using `nflreadr`, the end user is able to collect weekly top-level stats via the `load_player_stats()` function or the much more robust play-by-play numbers by using the `load_pbp()` function.

As you may imagine, there is a **very important distinction between the `load_player_stats()`** **and `load_pbp()`**. As mentioned, `load_player_stats()` will provide you with weekly, pre-calculated statistics for either offense or kicking. Conversely, `load_pbp()` will provide over 350 metrics for every single play of every single game dating back to 1999.

The `load_player_stats()` function includes the following offensive information:


```r
offensive.stats <- nflreadr::load_player_stats(2021)
dplyr::glimpse(offensive.stats)
```

```
## Rows: 5,698
## Columns: 48
## $ player_id                   <chr> "00-0019596", "00-0019596", "00-0019596", ~
## $ player_name                 <chr> "T.Brady", "T.Brady", "T.Brady", "T.Brady"~
## $ recent_team                 <chr> "TB", "TB", "TB", "TB", "TB", "TB", "TB", ~
## $ season                      <int> 2021, 2021, 2021, 2021, 2021, 2021, 2021, ~
## $ week                        <int> 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14~
## $ season_type                 <chr> "REG", "REG", "REG", "REG", "REG", "REG", ~
## $ completions                 <int> 32, 24, 41, 22, 30, 34, 20, 28, 23, 30, 25~
## $ attempts                    <int> 50, 36, 55, 43, 41, 42, 36, 40, 34, 46, 34~
## $ passing_yards               <dbl> 379, 276, 432, 269, 411, 297, 211, 375, 22~
## $ passing_tds                 <int> 4, 5, 1, 0, 5, 2, 4, 4, 2, 2, 1, 4, 2, 0, ~
## $ interceptions               <dbl> 2, 0, 0, 0, 0, 1, 0, 2, 2, 1, 1, 1, 0, 1, ~
## $ sacks                       <dbl> 0, 3, 3, 1, 2, 0, 0, 3, 0, 0, 2, 0, 2, 4, ~
## $ sack_yards                  <dbl> 0, 17, 21, 8, 15, 0, 0, 25, 0, 0, 9, 0, 12~
## $ sack_fumbles                <int> 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, ~
## $ sack_fumbles_lost           <int> 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, ~
## $ passing_air_yards           <dbl> 446, 347, 382, 459, 421, 283, 281, 340, 20~
## $ passing_yards_after_catch   <dbl> 176, 88, 238, 109, 225, 191, 75, 205, 113,~
## $ passing_first_downs         <dbl> 22, 14, 24, 12, 24, 17, 12, 13, 8, 16, 11,~
## $ passing_epa                 <dbl> 14.0069873, 1.8543642, 13.1633025, 2.24520~
## $ passing_2pt_conversions     <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
## $ dakota                      <dbl> 0.168449136, 0.118403623, 0.137870759, 0.0~
## $ carries                     <int> 0, 1, 3, 4, 1, 4, 0, 1, 1, 1, 2, 1, 7, 1, ~
## $ rushing_yards               <dbl> 0, 6, 14, 3, 13, 1, 0, 2, 2, 10, 2, -1, 16~
## $ rushing_tds                 <int> 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, ~
## $ rushing_fumbles             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, ~
## $ rushing_fumbles_lost        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, ~
## $ rushing_first_downs         <dbl> 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 4, 0, ~
## $ rushing_epa                 <dbl> NA, 1.2840404, 0.5629274, 1.6620587, 0.865~
## $ rushing_2pt_conversions     <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
## $ receptions                  <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
## $ targets                     <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
## $ receiving_yards             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
## $ receiving_tds               <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
## $ receiving_fumbles           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
## $ receiving_fumbles_lost      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
## $ receiving_air_yards         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
## $ receiving_yards_after_catch <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
## $ receiving_first_downs       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
## $ receiving_epa               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~
## $ receiving_2pt_conversions   <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
## $ special_teams_tds           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
## $ fantasy_points              <dbl> 27.16, 29.64, 28.68, 11.06, 37.74, 17.98, ~
## $ fantasy_points_ppr          <dbl> 27.16, 29.64, 28.68, 11.06, 37.74, 17.98, ~
## $ pacr                        <dbl> 0.8497758, 0.7953890, 1.1308901, 0.5860566~
## $ racr                        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~
## $ target_share                <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~
## $ air_yards_share             <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~
## $ wopr                        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~
```

As well, it contains the following kicking information:


```r
kicking.stats <- nflreadr::load_player_stats(2021, stat_type = "kicking")
dplyr::glimpse(kicking.stats)
```

```
## Rows: 561
## Columns: 40
## $ season              <int> 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021, 20~
## $ week                <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,~
## $ season_type         <chr> "REG", "REG", "REG", "REG", "REG", "REG", "REG", "~
## $ team                <chr> "ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "~
## $ player_name         <chr> "M.Prater", "Y.Koo", "J.Tucker", "T.Bass", "R.Sant~
## $ player_id           <chr> "00-0023853", "00-0033702", "00-0029597", "00-0036~
## $ fg_made             <int> 1, 2, 2, 3, 2, 0, 2, 0, 3, 2, 1, 1, 3, 1, 0, 2, 2,~
## $ fg_missed           <int> 1, 0, 0, 0, 0, 0, 0, 0, 2, 0, 1, 0, 0, 0, 1, 0, 0,~
## $ fg_blocked          <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ fg_long             <dbl> 34, 27, 47, 42, 29, 0, 53, 0, 48, 36, 49, 39, 40, ~
## $ fg_att              <dbl> 2, 2, 2, 3, 2, 0, 2, 0, 5, 2, 2, 1, 3, 1, 1, 2, 2,~
## $ fg_pct              <dbl> 0.5, 1.0, 1.0, 1.0, 1.0, NaN, 1.0, NaN, 0.6, 1.0, ~
## $ pat_made            <int> 5, 0, 3, 1, 1, 2, 3, 3, 2, 3, 2, 0, 4, 1, 3, 3, 4,~
## $ pat_missed          <int> 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ pat_blocked         <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ pat_att             <dbl> 5, 0, 3, 1, 2, 2, 3, 3, 3, 3, 2, 0, 4, 1, 3, 3, 4,~
## $ pat_pct             <dbl> 1.000, NaN, 1.000, 1.000, 0.500, 1.000, 1.000, 1.0~
## $ fg_made_distance    <dbl> 34, 48, 87, 104, 51, 0, 86, 0, 104, 59, 49, 39, 91~
## $ fg_missed_distance  <dbl> 43, 0, 0, 0, 0, 0, 0, 0, 91, 0, 51, 0, 0, 0, 55, 0~
## $ fg_blocked_distance <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ gwfg_att            <dbl> 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ gwfg_distance       <dbl> 0, 0, 0, 0, 0, 0, 33, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
## $ gwfg_made           <dbl> 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ gwfg_missed         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ gwfg_blocked        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ fg_made_0_19        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ fg_made_20_29       <dbl> 0, 2, 0, 1, 2, 0, 0, 0, 1, 1, 0, 0, 2, 1, 0, 1, 1,~
## $ fg_made_30_39       <dbl> 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0,~
## $ fg_made_40_49       <dbl> 0, 0, 2, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0,~
## $ fg_made_50_59       <dbl> 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,~
## $ fg_made_60_         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ fg_missed_0_19      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ fg_missed_20_29     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ fg_missed_30_39     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ fg_missed_40_49     <dbl> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ fg_missed_50_59     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0,~
## $ fg_missed_60_       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ fg_made_list        <chr> "34", "21;27", "40;47", "37;25;42", "22;29", "", "~
## $ fg_missed_list      <chr> "43", "", "", "", "", "", "", "", "31;60", "", "51~
## $ fg_blocked_list     <chr> "", "", "", "", "", "", "", "", "", "", "", "", ""~
```

While the data returned is not as robust as the play-by-play data we will covering next, the `load_player_stats()` function is extremely helpul when you need to quickly (and correctly!) recreate the official stats listed on either the NFL's website or on [Pro Football Reference](https://www.pro-football-reference.com/).

As an example, let's say you need to get Ben Roethlisberger's total passing yard and attempts from the 2021 season. You could do so via `load_pbp()` but, if you do not need further context, using `load_player_stats()` is much more efficient.

### Getting Weekly Player Stats via `load_player_stats()`


```r
weekly.data <- nflreadr::load_player_stats(2021)
rosters <- nflreadr::load_rosters(2021)

ben.weekly <- weekly.data %>%
  group_by(player_id) %>%
  filter(season_type == "REG") %>%
  summarize(total.yards = sum(passing_yards),
            n.attempts = sum(attempts)) %>%
  left_join(rosters, by = c("player_id" = "gsis_id")) %>%
  filter(full_name == "Ben Roethlisberger") %>%
  select(player_id, full_name, total.yards, n.attempts)

tibble(ben.weekly)
```

```
## # A tibble: 1 x 4
##   player_id  full_name          total.yards n.attempts
##   <chr>      <chr>                    <dbl>      <int>
## 1 00-0022924 Ben Roethlisberger        3740        605
```

As you can see in the `ben.weekly` output, we have matched his official 2021 regular stats perfectly with 3,740 passing yards on 605 attempts. The code we just created is doing several things. First, we are using `nflreadr::load_player_stats(2021)` to place the data into our R environment in a DF titled `weekly.data`. Directly after, we are using another function called `load_rosters()` which populates a DF with the information for every player in the NFL during the 2021 season (more on why this is needed in a moment).

Next, we group the data together by alike `player_id` (as every individual player has a unique ID number). After filtering for the regular season, we are able to summarize all of the weekly data into combined statistics by summing the weekly totals of passing yards and attempts.

Unfortunately, we are still not done. In order to get Roethlisberger's name attached to it, if you needed it, we needed to complete a `left_join` between our `ben.weekly` and `rosters` dataframes. To do so, we are matching up the `player_id` to the `gsis_id` within the roster information. After that information is combined, we are able to pull out just Roethlisberger's information and select the columns we want.

That lengthy explanation brings about a good question: **why could we not simply do `filter(player_name == "B.Roethlisberger" & season_type == "REG"` rather than loading rosters and merging the two DFs?**

Truth be told, that *would* be possible for Ben Roethliserger. As an example:


```r
ben.nomerge <- weekly.data %>%
  group_by(player_name) %>%
  filter(player_name == "B.Roethlisberger" & season_type == "REG") %>%
  summarize(total.yards = sum(passing_yards),
            n.attempts = sum(attempts))

tibble(ben.nomerge)
```

```
## # A tibble: 1 x 3
##   player_name      total.yards n.attempts
##   <chr>                  <dbl>      <int>
## 1 B.Roethlisberger        3740        605
```

Again, we are matching the official statistics. And this time we are doing so with much less code and without the need to pull in roster information to merge with.

But there is a reason you should not work with the data while grouping by `player_name`. An excellent example of this is Josh Allen. Let's attempt to do a "no roster, no merge" calcuation of this 2021 total passing yards and attempts and see what happens:


```r
josh.allen.nm <- weekly.data %>%
  group_by(player_name) %>%
  filter(player_name == "J.Allen" & season_type == "REG") %>%
  summarize(total.yards = sum(passing_yards),
            n.attempts = sum(attempts))

tibble(josh.allen.nm)
```

```
## # A tibble: 1 x 3
##   player_name total.yards n.attempts
##   <chr>             <dbl>      <int>
## 1 J.Allen            4049        603
```

The output tells us Allen threw for 4,049 yards on 603 attempts during the 2021 regular season. A check of his [Pro Football Reference page](https://www.pro-football-reference.com/players/A/AlleJo02.htm) tells us those numbers are incorrect. In fact, he had 4,407 passing yards on 646 attempts. How did we end up 358 passing yards and 43 attempts short?

The answer comes from Aaron Schatz, the creator of [Football Outsiders](https://www.footballoutsiders.com/), who explained in a [Tweet](https://twitter.com/fo_aschatz/status/1442191416826888192?s=21) that the official Buffalo Bills' scorer, during week 3 of the NFL season, decided to refer to Allen as "Jos.Allen" as a result of the Washington Commanders having a player named "Jonathan Allen."

To double check this, we can run the same code as above, but group by two variables (`player_id` and `player_name`) to see what happens:


```r
two.joshies <- weekly.data %>%
  group_by(player_id, player_name) %>%
  filter(season_type == "REG" & recent_team == "BUF") %>%
  summarize(total.yards = sum(passing_yards),
            n.attempts = sum(attempts))
```

```
## `summarise()` has grouped output by 'player_id'. You can override using the
## `.groups` argument.
```

```r
tibble(two.joshies)
```

```
## # A tibble: 17 x 4
##    player_id  player_name  total.yards n.attempts
##    <chr>      <chr>              <dbl>      <int>
##  1 00-0027685 E.Sanders              0          0
##  2 00-0029000 C.Beasley              0          1
##  3 00-0031588 S.Diggs                0          0
##  4 00-0031787 J.Kumerow              0          0
##  5 00-0033308 M.Breida               0          0
##  6 00-0033466 I.McKenzie             0          0
##  7 00-0033550 D.Webb                 0          0
##  8 00-0033869 M.Trubisky            43          8
##  9 00-0033904 D.Dawkins              0          0
## 10 00-0034857 J.Allen             4049        603
## 11 00-0034857 Jos.Allen            358         43
## 12 00-0035250 D.Singletary           0          0
## 13 00-0035308 T.Sweeney              0          0
## 14 00-0035689 D.Knox                 0          0
## 15 00-0036187 R.Gilliam              0          0
## 16 00-0036196 G.Davis                0          0
## 17 00-0036251 Z.Moss                 0          0
```

Grouping by `player_id` and `player_name` (as well as filtering down to Buffalo), we can see that, indeed, Josh Allen is in the data twice under the same `player_id`. Moreover, if you do the math, you can see that the numbers from his two entries add up to the official statistics on his Pro Football Reference page. To verify, we can include the necessary coding to merge Josh's `player_id` with the identical `gsis_id` from our roster information:


```r
josh.weekly <- weekly.data %>%
  group_by(player_id) %>%
  filter(season_type == "REG") %>%
  summarize(total.yards = sum(passing_yards),
            n.attempts = sum(attempts)) %>%
  left_join(rosters, by = c("player_id" = "gsis_id")) %>%
  filter(full_name == "Josh Allen") %>%
  select(player_id, full_name, total.yards, n.attempts)

tibble(josh.weekly)
```

```
## # A tibble: 1 x 4
##   player_id  full_name  total.yards n.attempts
##   <chr>      <chr>            <dbl>      <int>
## 1 00-0034857 Josh Allen        4407        646
```

With the addition of merged roster information, we are able to work around the issue of Josh Allen being in the data twice under slightly varied names.

### Using `load_player_stats()` To Find Leaders

While using `load_player_stats()` does not provide the ability to add context to your analysis as we will soon see with `load_pbp()`, it does provide an easy and efficient way to determine weekly or season-long leaders over many top-level, widely-used NFL statistics. In the below example, we will determine the 2021 leaders in air yards per attempt.

#### An Example: 2021 QB Air Yards per Attempt Leaders


```r
data <- nflreadr::load_player_stats(2021)

ay.per.attempt <- data %>%
  group_by(player_name) %>%
  filter(season_type == "REG") %>%
  summarize(n.attempts = sum(attempts),
            n.airyards = sum(passing_air_yards),
            ay.attempt = n.airyards / n.attempts) %>%
  filter(n.attempts >= 400) %>%
  arrange(desc(ay.attempt))
```

In the above example, we are using `group_by` to combine the desired statistics for each individual quarterback. After filtering to include just those statistics for the regular season, we use the `summarize` function to find two items: (1.) the total number of passing attempts by each QB which is outputted into a new row titled `n.attempts` and the regular season total of each QB's air yards, again outputted into a new row titled `n.airyards`.

It is important to note that the final row created with the `summarize` function is not a statistic included within `load_player_stats()`. In order to find a QB's average air yards per attempt, we must use the first two items we've created and do some simple division (the created `n.airyards` divided by `n.attempts`).

Finally, to "clear the noise" of those QBs with minimal attempts through the season, we included a filter to include those passers with at least 400 attempts. After, we arrange the new DF by sorting the QBs in descending order by average air yards per attempt.

The end results look like this:


```r
tibble(ay.per.attempt)
```

```
## # A tibble: 24 x 4
##    player_name   n.attempts n.airyards ay.attempt
##    <chr>              <int>      <dbl>      <dbl>
##  1 R.Wilson             400       3955       9.89
##  2 J.Hurts              432       3882       8.99
##  3 B.Mayfield           418       3651       8.73
##  4 M.Stafford           601       5094       8.48
##  5 K.Cousins            561       4575       8.16
##  6 J.Burrow             520       4225       8.12
##  7 D.Carr               626       5084       8.12
##  8 J.Allen              603       4889       8.11
##  9 T.Brady              719       5821       8.10
## 10 T.Bridgewater        426       3424       8.04
## # ... with 14 more rows
```

Russell Wilson, who *just* made the cutoff with 400 passing attempts, led the NFL in 2021 with 9.89 air yards per attempt.

## Using `load_pbp()` to Add Context to Statistics

As just mentioned above, using the `load_pbp()` function is preferable when you are looking to add context to a player's statistics, as the `load_player_stats()` function is, for all intents and purposes, aggregated statistics that limit your ability to find deeper meaning.

To highlight this, let's look at an example of how context can be added to a player's statistics using `load_pbp()`.

### An Example: QB Aggresiveness on 3rd Down

Prior to the writing of this book, I created a metric using `load_pbp()` data I coined **QB 3rd Down Aggressiveness**. The metric is designed to determine *which QBs in the NFL are most aggressive in 3rd down situations by gauging how often they throw the ball to, or pass, the first down line.*

First, let's highlight the code used to create the results for this metric and then break it down line-by-line.


```r
data <- nflreadr::load_pbp(2021)

aggressiveness <- data %>%
  group_by(passer, passer_id, posteam) %>%
  filter(down == 3, play_type == "pass", ydstogo >= 5, ydstogo <= 10) %>%
  summarize(total = n(),
            aggressive = sum(air_yards >= ydstogo, na.rm = TRUE),
            percentage = aggressive / total) %>%
  filter(total >= 50) %>%
  arrange(desc(percentage))
```

```
## `summarise()` has grouped output by 'passer', 'passer_id'. You can override
## using the `.groups` argument.
```

```r
tibble(aggressiveness)
```

```
## # A tibble: 30 x 6
##    passer       passer_id  posteam total aggressive percentage
##    <chr>        <chr>      <chr>   <int>      <int>      <dbl>
##  1 D.Prescott   00-0033077 DAL        84         53      0.631
##  2 K.Murray     00-0035228 ARI        60         37      0.617
##  3 J.Hurts      00-0036389 PHI        65         40      0.615
##  4 P.Mahomes    00-0033873 KC         93         56      0.602
##  5 B.Mayfield   00-0034855 CLE        59         35      0.593
##  6 T.Lawrence   00-0036971 JAX        78         46      0.590
##  7 J.Herbert    00-0036355 LAC        87         51      0.586
##  8 D.Jones      00-0035710 NYG        60         35      0.583
##  9 T.Tagovailoa 00-0036212 MIA        60         35      0.583
## 10 M.Stafford   00-0026498 LA         95         54      0.568
## # ... with 20 more rows
```

As you can see in the `tibble()` output of the results, Dak Prescott was the most aggressive quarterback in 3rd down passing situations in the 2021 season, passing to, our beyond, the line of gain just over 63% of the time.

After creating a new dataframe called `aggressiveness` from the 2021 play-by-play we originally collected using `data <- nflreadr::load_pbp(2021)`, we use `group_by` to ensure that the data is being collected *per individual quarterback* and then appending both their unique `passer_id` as well as the `posteam` which is simply their team's abbreviation (note: the `posteam` variable is not a vital part of the data collection/manipulation process, but does play an important role when taking this output into the data visualization process, which is covered in Chapter 4).

However, there are a couple items to point out and clarify with the above code. Moreover, there are certainly arguments to be made regarding how to "capture" scenarios in the data that require "aggressiveness."

After using the `group_by` function to lump data with each individual QB, we then use `filter()` function. Of course, we only want those `play_types` that are "pass" on 3rd downs. However, in the above code, we are filtering for *just* those 3rd down situations where the `yards to go` are between five and ten yards.

Doing so was a personal decision on my end when creating the metric. My logic? If there were less than five yards to go on 3rd down, the opposing defense would not be able to "sell out" to the pass as it would not be out of the question for an offense to attempt to gain the first down on the ground. Conversely, anything *over* ten yards likely results in the defense selling out to the pass, thus leaving an imprint on the aggressiveness output of the quarterbacks.

For the sake of curiosity, we can edit the above code to include all passing attempts on 3rd down with under 10 yards to go for the first down:


```r
aggressiveness.under.10 <- data %>%
  group_by(passer, passer_id, posteam) %>%
  filter(down == 3, play_type == "pass", ydstogo <= 10) %>%
  summarize(total = n(),
            aggressive = sum(air_yards >= ydstogo, na.rm = TRUE),
            percentage = aggressive / total) %>%
  filter(total >= 50) %>%
  arrange(desc(percentage))
```

```
## `summarise()` has grouped output by 'passer', 'passer_id'. You can override
## using the `.groups` argument.
```

```r
tibble(aggressiveness.under.10)
```

```
## # A tibble: 33 x 6
##    passer       passer_id  posteam total aggressive percentage
##    <chr>        <chr>      <chr>   <int>      <int>      <dbl>
##  1 K.Murray     00-0035228 ARI        98         67      0.684
##  2 J.Hurts      00-0036389 PHI       107         73      0.682
##  3 T.Lawrence   00-0036971 JAX       131         88      0.672
##  4 D.Prescott   00-0033077 DAL       136         89      0.654
##  5 J.Allen      00-0034857 BUF       138         88      0.638
##  6 J.Herbert    00-0036355 LAC       148         94      0.635
##  7 T.Tagovailoa 00-0036212 MIA        93         59      0.634
##  8 M.Stafford   00-0026498 LA        172        109      0.634
##  9 A.Rodgers    00-0023459 GB        128         80      0.625
## 10 D.Jones      00-0035710 NYG        85         53      0.624
## # ... with 23 more rows
```

The results are quite different from the first running of this metric, as Dak Prescott is now the 4th most aggressive QB, while Kyler Murray moves to the top by approaching a nearly 70% aggressiveness rate on 3rd down. This small change highlights an important element about analytics: much of the work is the result of the coder (ie., [you]{.ul}) being able to justify your decision-making process when developing the filters for each metric you create.

In this case, I stand by my argument that including just those pass attempts on 3rd down with between 5 and 10 yards to go is a more accurate assessment of aggressiveness as, for example, 3rd down with 8 yards to go is an obvious passing situation in [most]{.ul} cases.

That begs the question, though: in which cases is 3rd down with 8 yards to go [not]{.ul} an obvious passing situation?

#### QB Aggressiveness: Filtering for "Garbage Time?"

In our initial running of the QB Aggressiveness metric, Josh Allen is the 15th most aggressive QB in the NFL on 3rd down with between 5 and 10 yards to go. But how much does the success of the Buffalo Bills play into that 15th place ranking?

The Bills, at the conclusion of the 2021 season, had the largest positive point differential in the league at 194 (the Bills scored 483 points, while allowing just 289). Perhaps Allen's numbers are skewed because the Bills were so often playing with the lead late into the game?

To account for this, we can add information into the `filter()` function to attempt to remove what are referenced to in the analytics community as "garbage time stats."

Let's add the "garbage time" filter to the code we've already prepared:


```r
aggressiveness.garbage <- data %>%
  group_by(passer, passer_id, posteam) %>%
  filter(down == 3, play_type == "pass", ydstogo >= 5, ydstogo <= 10,
         wp > .05, wp < .95, half_seconds_remaining > 120) %>%
  summarize(total = n(),
            aggressive = sum(air_yards >= ydstogo, na.rm = TRUE),
            percentage = aggressive / total) %>%
  filter(total >= 50) %>%
  arrange(desc(percentage))
```

```
## `summarise()` has grouped output by 'passer', 'passer_id'. You can override
## using the `.groups` argument.
```

```r
tibble(aggressiveness.garbage)
```

```
## # A tibble: 26 x 6
##    passer     passer_id  posteam total aggressive percentage
##    <chr>      <chr>      <chr>   <int>      <int>      <dbl>
##  1 D.Prescott 00-0033077 DAL        61         40      0.656
##  2 T.Lawrence 00-0036971 JAX        51         33      0.647
##  3 K.Murray   00-0035228 ARI        51         31      0.608
##  4 M.Stafford 00-0026498 LA         79         48      0.608
##  5 P.Mahomes  00-0033873 KC         68         41      0.603
##  6 D.Jones    00-0035710 NYG        50         30      0.6  
##  7 J.Herbert  00-0036355 LAC        67         38      0.567
##  8 M.Jones    00-0036972 NE         57         31      0.544
##  9 J.Allen    00-0034857 BUF        59         32      0.542
## 10 R.Wilson   00-0029263 SEA        56         30      0.536
## # ... with 16 more rows
```

We are now using the same code, but have included three new items to the `filter()`. First, we are stipulating that, aside from the down and distance inclusion, we only want those plays that occured when the offense's `win probability` was between 5% and 95%, as well as ensuring that the plays did not happen after the two-minute warning of either half.

The decision on range of the `win probability` numbers is, again, a personal preference. When `nflfastR` was first released, analyst often used a 20-80% range for `win probability`. However, Sebastian Carl - one of the creators of the `nflverse` explained in the package's Discord:

> Sebastian Carl: "I am generally very conservative with filtering plays using wp. Especially the vegas wp model can reach \>85% probs early in the game because it incorporates market lines. I never understood the 20% \<= wp \<= 80% "garbage time" filter. This is removing a ton of plays. My general advice is a lower boundary of something around 5% (i.e., 5% \<= wp \<= 95%).

Ben Baldwin followed up on Carl's thoughts:

> Ben Baldwin: "agree with this. 20-80% should only be used as a filter for looking at how run-heavy a team is (because outside of this range is when teams change behavior a lot). and possibly how teams behave on 4th downs. but not for team or player performance."

Based on that advice, I typically stick to the 5-95% range when filtering for `win probability` using play-by-play data. And, in this case, it did have an impact.

As mentioned, prior to filtering for garbage time, Allen was the 15th most aggressive QB in the league at nearly 52%. However, once filtering for garbage time, Allen rose to 9th most aggressive QB, with a slight increase of percentage to 54%.

What is interesting about the above example, though, is Dak Prescott and the Cowboys. Dallas maintained the second largest point differntial in the league (530 points for and 358 points against, for a 172 point difference). Without the garbage time filter, Prescott was tops in the NFL with an aggressiveness rating of 63%.

Once adjusted for garbage time? Prescott remained atop the NFL with an aggressiveness rating of 65.5%.

Allen's increase in the standings, and Prescott remaining best in the league, in this specific metric, is a possible indicator that the inclusion of the "garbage time" filters provides a slightly more accurate result.

### The Inclusion of Contextual Statistics

As seen in the above example regarding QB aggressiveness on 3rd down, the using of the `load_pbp()` function provides the ability to create situation specific metrics that would otherwise be lost in aggregated weekly statistics.

## Retrieving & Working With Data for Multiple Seasons

In the case of both `load_pbp()` and `load_player_stats()`, it is possible to load data over multiple seasons.

In our above example calculating average air yard per attempt, it is important to note that Russell Wilson's league-eading average of 9.89 air yards per attempt is calculated using *all* passing attempts, meaning pass attempts that were both complete and incomplete.

In our first example of working with data across multiple seasons, let's examine average air yards for only completed passes. To begin, we will retrieve the play-by-play data for the last five seasons:


```r
ay.five.years <- nflreadr::load_pbp(2017:2021)
```

To retrieve multiple seasons of data, a colon `:` is placed between the years that you want. When you run the code, `nflreadr` will output the data to include the play-by-play data starting with the oldest season (in this case, the 2017 NFL season):


```r
tibble(ay.five.years)
```

```
## # A tibble: 243,131 x 372
##    play_id game_id     old_game_id home_team away_team season_type  week posteam
##      <dbl> <chr>       <chr>       <chr>     <chr>     <chr>       <int> <chr>  
##  1       1 2017_01_AR~ 2017091004  DET       ARI       REG             1 <NA>   
##  2      37 2017_01_AR~ 2017091004  DET       ARI       REG             1 ARI    
##  3      73 2017_01_AR~ 2017091004  DET       ARI       REG             1 ARI    
##  4      97 2017_01_AR~ 2017091004  DET       ARI       REG             1 ARI    
##  5     118 2017_01_AR~ 2017091004  DET       ARI       REG             1 ARI    
##  6     153 2017_01_AR~ 2017091004  DET       ARI       REG             1 ARI    
##  7     174 2017_01_AR~ 2017091004  DET       ARI       REG             1 ARI    
##  8     207 2017_01_AR~ 2017091004  DET       ARI       REG             1 ARI    
##  9     233 2017_01_AR~ 2017091004  DET       ARI       REG             1 DET    
## 10     254 2017_01_AR~ 2017091004  DET       ARI       REG             1 DET    
## # ... with 243,121 more rows, and 364 more variables: posteam_type <chr>,
## #   defteam <chr>, side_of_field <chr>, yardline_100 <dbl>, game_date <chr>,
## #   quarter_seconds_remaining <dbl>, half_seconds_remaining <dbl>,
## #   game_seconds_remaining <dbl>, game_half <chr>, quarter_end <dbl>,
## #   drive <dbl>, sp <dbl>, qtr <dbl>, down <dbl>, goal_to_go <dbl>, time <chr>,
## #   yrdln <chr>, ydstogo <dbl>, ydsnet <dbl>, desc <chr>, play_type <chr>,
## #   yards_gained <dbl>, shotgun <dbl>, no_huddle <dbl>, qb_dropback <dbl>, ...
```

## Other Sources of Data for NFL Analytics
