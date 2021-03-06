---
title: "An Introduction to NFL Analytics with R"
author: "Bradley J. Congelio"
date: "2022-07-28"
site: bookdown::bookdown_site
knit: bookdown::render_book
output: bookdown::bs4_book
documentclass: book
bibliography: [references.bib]
biblio-style: apalike
link-citations: yes
description: "This book provides in-depth instruction on how to use R and the nflverse family of packages to conduct advanced NFL analytics."
github-repo: "bcongelio/nfl-analytics-with-r-book"
url: "https://bradcongelio.com/nfl-analytics-with-r-book"
---



# Preface {.unnumbered}

<center>**Welcome to the online home of** <br>***An Introduction to NFL Analytics with R*****.**</center>

<img src="docs/images/big-ben-intro.jpg" width="100%" />

The online version of this book is published with the [Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International (CC BY-NC-ND 4.0) license.](https://creativecommons.org/licenses/by-nc-nd/4.0/)

As this book is published online, it allows me to continue real-time development of it. Because of this, make note of the following:

-   Please feel free to contribute to the book by filing an issue or making a pull request at the book's GitHub repository: [*An Introduction to NFL Analytics with R* Github Repository](https://github.com/bcongelio/nfl-analytics-with-r-book)

-   The majority of the chapters conclude with exercises. In some cases, prepared data will be provided with a link to download the file. In other cases, you are expected to use the `nflverse` to collect the data yourself. In either case, the book's github repository for the exercises (link to the specific directory coming soon) will include the R files that contain the answers to each exercise.

-   Soon there will be a YouTube series to go along with the written version of this book. In brief, the videos will include my going over each chapter, step-by-step, with additional instruction and/or approaches.

-   Are you an instructor hoping to create or grow your Sport Analytics course? Future plans for this book include the creation of Instructor Materials to include an example syllabus plus structured lesson plans, exercises, assignments, quizzes, and exams. As well, templates for lectures will be included in PowerPoint form so you may edit to fit your specific needs.

## Introduction {.unnumbered}

On April 27, 2020, Ben Baldwin hit send on a Tweet that announced the birth of `nflfastR`, an R package designed to scrape NFL play-by-play data, allowing the end-user to access it at speeds quicker than similar predecessors (hence the name).

<img src="docs/images/ben-baldwin-intro-tweet.jpg" width="100%" />

Thanks to the work of multiple people ([\@mrcaseB](https://twitter.com/mrcaseb), [\@benbbaldwin](https://twitter.com/benbbaldwin), [\@TanHo](https://twitter.com/_TanHo), [\@LeeSharpeNFL](https://twitter.com/LeeSharpeNFL), and [\@thomas_mock](https://twitter.com/thomas_mock) ... to just name a few), the process of getting started with advanced analytics using NFL data is now easier than ever.

That said, and without getting too far into the weeds of the history behind it all, the above-mentioned people are responsible in some shape or form for the current status of the `nflverse`, which is a superb collection of data and R-based packages that allows anybody the ability to access deeply robust NFL data as far back as the 1999 season.

The `nflverse` as we know it today was initially birthed from the `nflscrapR` project, which was started by the Carnegie Mellon University student and profess duo of [Maksim Horowitz](https://twitter.com/bklynmaks?lang=en) and [Sam Ventura](https://twitter.com/stat_sam). After Horowitz graduated - and got hired by the Atlanta Hawks - the `nflscrapR` package was taken over by fellow CMU student Ron Yorko (who would go on to receive his Ph.D. from the Statistics and Data Science program). The trio's work on `nflscrapR` ultimately led to a peer-reviewed paper titled "[nflWAR: A Reproducible Method for Offensive Player Evaluation in Football.](https://arxiv.org/pdf/1802.00998.pdf)" Ultimately, the `nflscrapR` project came to an end when the specific .json feed used to gather NFL data changed. At this point, Ben Baldwin and Sebastian Carl had already built upon the `nflscrapR` project's foundations to create `nflfastR`. Ron officially marked the end of the `nflscrapR` era and the beginning of the `nflfastR` era with a tweet on September 14, 2020:[^index-1]

[^index-1]: Thanks to Ben Baldwin for chatting with me on Discord and providing this brief understanding of the backstory.

<img src="docs/images/farewell-nflscrapr.jpg" width="100%" />

As a reply to his first tweet about the `nflfastR` project, Ben explained that he created the original function to scrape NFL data for the creation of his [NFL analytics website](https://rbsdm.com/stats/stats/). Thankfully, he and Seb did not keep the creation to themselves and released `nflfastR` to the public. Because of the "open source" nature of R and R packages, a laundry list of companion packages quickly developed alongside `nflfastR`. The original `nflfastR` package is now part of the larger `nflverse` of packages that drive the NFL analytics community on Twitter and beyond.

The creation of the `nflverse` allowed for anybody interested in NFL analytics to easily access data, manipulate it to their liking, and release their visualizations and/or metrics to the wider public. In fact, it is now a regular occurrence for somebody to advance their R programming ability because of the `nflverse` and then go on to win the Big Data Bowl. As of the 2022 version of the Big Data Bowl, over "30 participants have been hired to work in data and analytics roles in sports, including 22 that were hired in football" [@bigdatabowl-ws]. Most recently, the [Chargers hired 2020 participate Alex Stern](https://www.boltsfromtheblue.com/2021/7/9/22570490/chargers-news-nfl-big-data-bowl) and the [Chiefs hired Marc Richards](https://twitter.com/sethwalder/status/1532721476209627136), a member of the winning 2021 team, as a Football Research Analyst.

Kevin Clark, in a 2018 article for [*The Ringer*](https://www.theringer.com/nfl/2018/12/19/18148153/nfl-analytics-revolution), explained that despite not being as obvious as the sabermetrics movement in baseball, the analytics movement in the NFL is "happening in front of you all the time." The use of analytics in the NFL did, however, predate Clark's article. In 2014, Eagles head coach Doug Pederson explained that all decisions made by the organization - from game planning to draft strategy - were to be informed by hard data and analytics. Leading this early adoption of analytics, and reporting directly to team Vice President Howie Roseman, were Joe Douglas and Alec Halaby, "a 31-year-old Harvard grad with a job description" that had an emphasis on "integrating traditional and analytical methods in football decision-making." The result? A "blending of old-school scouting and newer approaches" that were often only seen in other leagues, such as the NBA and MLB [@rosenthal2018]. Pederson believed in and trusted the team's approach to analytics so much that a direct line of communication was created between the two during games, with the analytics department providing the head coach with math-based recommendations for any scenario Pederson requested [@awbrey2020].[^index-2]

[^index-2]: Thanks, again, to Ben Baldwin for providing his personal knowledge about the "early days" of the Eagles' analytics department.

In just under five years time since the publishing of that article, it has become hard to ignore the analytic movement within the NFL. Yet, there is still so much growth to happen in the marriage between the NFL and advanced metrics. For example, there is no denying that the sabermetrics movement drastically "altered baseball's DNA" @heifetz2019]. Moreover, as explained in Seth Partnow's outstanding [*The Midrange Theory: Basketball's Evolution in the Age of Analytics*](https://www.amazon.com/Midrange-Theory-Basketballs-Evolution-Analytics/dp/1637270968/ref=tmm_pap_swatch_0?_encoding=UTF8&qid=1656245879&sr=8-4), the analytics movement in the NBA essentially killed the midrange shot (briefly: it is more beneficial to try to work the ball in directly under the basket (for a high-percentage shot) or to take the 3-pointer, as the possible additional point is statistically worth more despite the lower success probability as opposed a 2-point midrange shot) as well as the traditional, "old-school" center position.

Compared to both the NBA and MLB, the NFL is playing catch up in analytics driving changes equivalent to the death of the midrange shot or the plethora of additional tactics and changes to baseball because of sabermetrics. Joe Banner, who served as the President of the Eagles from 2001-2012 and then the Chief Executive Officer of the Browns from 2012-2013, explained that some of the hesitation to incorporate analytics into NFL game planning was a result of the game being "very much driven by conventional wisdom to an extreme degree" [@fortier2020]. Perhaps nobody encapsulates this better than Pittsburgh Steelers Head Coach Mike Tomlin. When asked about his position on analytics during the 2015 season, Tomlin explained:

> I think that's why you play the game. You can take analytics to baseball and things like that but football is always going to be football. I got a lot of respect for analytics and numbers, but I'm not going to make judgements based on those numbers. The game is the game. It's an emotional one played by emotional and driven men. That's an element of the game you can't measure. Often times decisions such as that weigh heavily into the equation [@kozora2015].

Given that Tomlin's quote is from 2015, perhaps the Steelers pivoted since and are now more analytically inclined. That does not seem to be the case. In a poll of NFL analytics staffers conducted by ESPN, [the Steelers were voted as one of the least analytically advanced teams in the league.](https://www.espn.com/nfl/story/_/id/29939438/2020-nfl-analytics-survey-which-teams-most-least-analytically-inclined#least)

There is large gap between the least analytically inclined teams (Washington, Tennessee, Cincinnati, New York Giants, and Pittsburgh) and those voted as the most analytically inclined (Baltimore, Cleveland, Philadelphia, and Houston). In the ESPN poll, the Browns were voted as the analytics department producing the highest level of work. One of those polled spoke to the fact that much of this outstanding work is a result of General Manager Andrew Berry being a "true believer," explaining that he is one of the "rare guys you'll come across in life where you think to yourself, 'Man, this guy thinks at a different level. Just pure genius.' He's one of them."

[In his article for the *Washington Post*](https://www.washingtonpost.com/sports/2020/01/16/nfls-analytics-movement-has-finally-reached-sports-mainstream/), Sam Fortier argues that many teams became inspired to more intimately introduce analytics into game planning and on-field decisions after the 2017 season. On their run to becoming Super Bowl Champions, the Philadelphia Eagles were aggressive on 4th down, going for it 26 times during the season and converting on 17 of those for a conversion percentage of 65.4%. A quick examination and visualization of data highlights the absolutely staggering increase in 4th aggressiveness among NFL head coaches from 2017-2021:

<div class="figure">
<img src="docs/images/4th-down-attempts.png" alt="4th Down Attempts: 2000 - 2021" width="100%" />
<p class="caption">(\#fig:increase-in-4th-down-aggression)4th Down Attempts: 2000 - 2021</p>
</div>

There has been a 96.3% increase in the number of 4th down attempts from just 2017 to 2021. In fact, the numbers may actually be higher as I was quite conservative in building the above plot by only considering those 4th down attempts that took place when the offensive team had between a 5-to-95% winning probability and those prior to the two-minute warning of either half. Even with those conservative limitations, the increase is staggering. The numbers, however, support this aggression. During week one of both the 2020 and 2021 season, *not* going for it on 4th down "cost teams a cumulative 170 percentage points of win probability" [@bushnell2021].

Ben Baldwin, using the `nfl4th` package that is part of the `nflverse`, tracked the shift in NFL coaching mentality regarding 4th down decisions by comparing 2014's "go for it percentage" against the same for 2020. When compared to the 2014 season, NFL coaches are now much more in agreement with analytics on when to "go for it" on 4th down in relation to the expected gain in win probability.

<div class="figure">
<img src="docs/images/baldwin-graph-goforit.png" alt="Credit: Ben Baldwin" width="100%" />
<p class="caption">(\#fig:ben-baldwin-4th-down-graph)Credit: Ben Baldwin</p>
</div>

It should not be surprising then, considering Mike Tomlin's quote from above and other NFL analytics staffers voting the Steelers as one of the least analytically driven teams in the league, that Pittsburgh lost the most win probability by either kicking or punting in "go for it" situations during the 2020 NFL season. On the other end, the Ravens and Browns - two teams voted as the most analytically inclined - are the two best organizations at knowing when to "go for it" on 4th down based on win probability added. There seems to be a defined relationship between teams buying into analytics and those who do not:

<div class="figure">
<img src="docs/images/tomlin-go-for-it.png" alt="Credit: Ben Baldwin" width="100%" />
<p class="caption">(\#fig:tomlin-go-for-it)Credit: Ben Baldwin</p>
</div>

The NFL's turn towards more aggressive 4th-down decisions is just one of the many analytics-driven changes occurring in the league. Another significant example is Defense-Adjusted Value over Average (or DVOA), a formula created by Aaron Schatz, now the editor in chief of [Football Outsiders](https://www.footballoutsiders.com/info/methods#dvoa), that sought to challenge the notion that teams should, first, establish the running game in order to open up the passing game. Some of these changes are apparent on televisions screens on Sunday afternoons in the Fall, while others are occurring behind the scenes (analytics departments working on scouting and draft preparation, for example). Indeed, the use of analytics in the NFL is not as tightly ingrained as we see in other prominent leagues. And we must remember that there are certainly continued hold outs among some NFL coaches (like Mike Tomlin).

Despite some coaching hold outs on fully embracing analytics, the "thirst for knowledge in football is as excessive as any other sport and the desire to get the most wins per dollar is just as high." As the pipeline of data continues to grow, both internally in the league and data that becomes publicly accessible, "smart teams will continue to leave no rock unturned as they push the limits on how far data can take them." Joe Banner explained that while the NFL has long been a league of coaches saying "well, that is the way we've always done it," the league is ripe for a major shift [@bechtold2021].

Banner's belief that those teams searching for every competitive advantage will "leave no rock unturned" is the driving force behind this book. For all intents and purposes, the age of analytics in the NFL is still in its infancy. Turning back, again, to the 2017 season, the Eagles' management praised and credited the team's analytics department as part of the reason they were able to win Super Bowl LII. Doing so Danney Heifetz argues, "changed the language of football." The NFL, he explains, is a "copycat league" and, as witnessed with the increase in 4th down aggressiveness since 2017, teams immediately began to carbon copy Philadelphia's approach to folding traditional football strategy with a new age analytics approach. Because of the modernity of this relationship between long-held football dogmas and analytics, nobody can be quite sure what other impacts it will create on the gamesmanship of football.

However, as Heifetz opines, both the NBA and MLB can serve as a roadmap to where analytics will take the NFL. Importantly, the NFL's relationship with analytics is still in its "first frontier of what will likely be a sweeping change over the next two decades." Because of this, we cannot be sure what the next major impact analytics will make, nor when it may occur. But, with the ever-growing amount of publicly accessible data, it is only a matter of time until it is discovered. For example, in an interview with Heifetz, Brian Burke - one of the forefather's of NFL analytics and now a writer for ESPN - expressed his belief that the next major impact will be "quantifying how often quarterbacks make the correct decision on the field."

It seems that every new NFL season results in an amateur analyst bringing a groundbreaking model and/or approach to the table. Unlike, for example, MLB where there is little left to discover in terms of sabermetrics and new approaches to understanding the game and its associated strategy, the NFL is - for lack of a better phrase - an open playing field. With more and more data becoming available to the public, it is now easier than ever investigate your own ideas and suspicions and to create your own models to confirm your intuition.

For example, I am originally from the greater Pittsburgh area and am a big Steelers fan (which certainly explains some of the Steelers-centric examples I use in the writing of this book). I was adamant in my belief that Pittsburgh's TJ Watt should win the 2021 Defensive Player of the Year award, despite many others calling for Los Angeles' Aaron Donald to claim the title. In effort to prove my point, I sought out to design what I coined ***Adjusted Defensive Impact***. To begin, I wanted to explore the idea that not all defensive sacks are created equal, as a player's true impact is not always perfectly represented by top-level statistics.

To account for that, I opted to adjust and add statistical weight to sack statistics. This was done over multiple areas. For instance, not all players competed in all 17 regular-season games in 2021. To adjust for this, I took the total of game played in the data (2,936) and divided by 17 (a full season) to achieve a weighted adjustment of 0.0058. TJ Watt played in just 15 games in 2021. His adjusted equation, therefore, is (17-'games') \* 0.0058. The result? He gets a bit more credit for this adjustment than, say, Myles Garrett who played all 17 regular-season games.

Going further with the model, I created a weighted adjustment for solo sacks (0.90), a negative weighted adjustment (-0.14) for any sack charted as "unblocked," and a weighted adjustment to account for how many times a player actually rushed the QB compared to how many defensive snaps they played. Using data from the SIS Data Hub, the full code is below:


```r
options(digits = 2)

## selecting just information I want and then renaming
pass.data <- pass_rush_data %>%
  select(Player, Team, Games, `Pass Snaps`, `Pass Rushes`,
         `Solo Sacks`, `Ast. Sacks`, `Comb. Sacks`, 
         `Unblocked Sacks`, Hurries, Hits) %>%
  rename(total.snaps = `Pass Snaps`,
         total.rushes = `Pass Rushes`,
         solo.sacks = `Solo Sacks`,
         asst.sacks = `Ast. Sacks`,
         comb.sacks = `Comb. Sacks`,
         unblocked.sacks = `Unblocked Sacks`,
         player = Player,
         team = Team,
         games = Games,
         hurries = Hurries,
         hits = Hits)

## creating a new column to get percentages of snaps where player rushed
pass.data$rush.percent <- pass.data$total.rushes / pass.data$total.snaps

## getting weights to add to the equation
## for example, a solo sack is "more impressive" than an assisted sack. 
## to account for this, we will take the sum of combined sacks (995) and divide 
## by the sum of solo sacks (892) which is (0.9). Therefore, a weight of 0.9 will
## be placed on solo sacks.

## as well, a negative weight will be applied to unblocked sacks. Using the total
## of solo sacks (892) and dividing by the sum of unblocked sacks (128) gives us a
## NEGATIVE weight of 0.14 to be applied to unblocked sacks

## further, a weight must be applied to those players who played less than a complete season.
## the weight here works out to be 0.0058.

## summarizing information
calculated.impact <- pass.data %>%
  group_by(player) %>%
  summarize(
    adjusted.games = (17 - games) * 0.0058,
    adjusted.solo = solo.sacks * 0.9,
    adjusted.unblocked = unblocked.sacks / -0.14,
    adjusted.rush.percent = 0.81 - rush.percent,
    combined.impact = sum(adjusted.games + (solo.sacks * 0.9) + (unblocked.sacks * -0.14) + adjusted.rush.percent))
```

The end result? Taking into account the above adjusted defensive impact, TJ Watt was absolutely dominant during the 2021 season:

<div class="figure">
<img src="docs/images/adjusteddefense.png" alt="Adjusted Defensive Impact" width="100%" />
<p class="caption">(\#fig:adjusted-defensive-impact)Adjusted Defensive Impact</p>
</div>

All of these examples - from Ben Baldwin's 4th-down model, to Football Outsiders' DVOA, to my attempt to further quantify defensive player impact - are just the leading edge of the burgeoning analytics movement in the NFL. Moreover, the beauty of analytics is that you do not have to be a mathematician or statistics buff in order to enter the fray. All it takes is a genuine curiosity to explore what Bob Carroll, Pete Palmer, and John Thorn coined as the "[Hidden Game of Football](https://amzn.to/3y1GZTO)" and the desire to learn, if you have not already, the R programming language.

### Who This Book Is For

Writing a book that is wholly dependent on the R programming language to achieve the end goals is not an easy task, as there are likely two types of people reading this: those with familiarity in R and those without. If you are part of the former group, you can likely skip chapter 2 as it is a primer on installing R and learning the language of the `tidyverse`. On the other hand, if you are part of the latter group, you should skip ahead to chapter 2 before even looking at chapter 1, which serves as an introduction to the `nflverse` with examples. That said, this book can serve multiple audiences:

1.  Those interested in learning how to produce NFL analytics regardless of their current knowledge of the R programming language.

2.  Professors who instruct data science courses can provide lessons through the lens of sport or, perhaps better, create their own Sport Analytics courses designed around the book. Moreover, future plans for this book include instruction reference guides to include PowerPoint templates, assignments/project instructions and grading rubrics, and quiz/exam content for online management systems (D2L, Canvas, Moodle, etc.).

3.  Students are able to use this book in multiple ways. For example, in my Sport Analytics course, students are first introduced to R and the `tidyverse` using the built-in R data sets (`mtcars`, `iris`, and `nycflights13`). While those data sets certainly serve a purpose, I have found that students grasp the concepts and language of the `tidyverse` more quickly when the class turns to using data from the `nflverse`. Because of this, students can use this book to start learning the R programming language using football terms (passing yards, first downs, air yards, completion percentage over expected) as opposed to the variables they may find much less interesting housed in the built-in data. Moreover, students already fluid in R can use this book to construct machine learning models using football data, for example, as part of assignments in data science/analytics courses.

4.  Journalists, bloggers, and arm-chair quarterbacks alike can use the book to underpin their arguments and to provide hard data to backup their claims.

It is also important to note that it is not expected for you to be an expert in statistics, coding, or data modeling in order to find this book useful. In fact, I self-taught myself the R programming language after discovering its potential usefulness in my personal academic research. I only "became more serious" about advancing my understanding of the language's nuances, and more advanced methods, after discovering the `nflscrapR` package and using it as my learning tool for the deep dive into R. My decision to pursue an academic certificate in the language was driven by the creation of my Sport Analytics course, as the certificate provided the "academic training" proof that was necessary to move the course through the University's curriculum committee. Nonetheless, because of this self-taught journey - and despite being an academic myself - I find that I largely avoid the use of "complicated jargon" that is so evident in most formal writing.

Because of this, and regardless of which chapter you begin with, I believe that this book achieves its main goal: to provide a gentle introduction to doing NFL analytics with R.

## Overview of Chapters

-   **Chapter 1** provides an overview of the `nflverse` with specific attention paid to the difference between using `nflfastR` versus `nflreadr`. Serving as the first dive into analytics, the chapter showcases how to use `nflreadr` to retrieve both compiled weekly NFL stats and the deeply robust play-by-play statistics. In both cases, exercises are provided. Readers will do their first coding by, first, using the weekly stats to determine the 2021 leaders in air yards per attempt. Second, readers will use the play-by-play statistics from the 2021 season to create a brand new metric (QB aggressiveness on 3rd down pass attempts). Afterward, readers will learn how to retrieve multiple seasons of data at once.

-   **Chapter 2** covers the process of downloading both R and RStudio, as well as the necessary packages to do NFL analytics. As one of the most important chapters in the book (especially for those new to the R programming language), readers take a deep dive into wrangling NFL data with the `tidyverse` package. To begin, readers will learn about the `dplyr` pipe (`%>%`) and use, in exercises, the six most important verbs in the `dplyr` language: `filer()`, `select()`, `arrange()`, `summarize()`, `mutate()`, and `group_by()`. At the conclusion of the chapter, multiple exercises are provided to allow readers to practice using the `dplyr` verbs, relational operators within the `filter()` function and creating "new stats" by using the `summarize()` function. Moreover, readers will determine the relationship between the `dplyr` language and important variables within the `nflverse` data such as `player_name` and `player_id`, which is important for correct manipulation and cleaning of data.

-   **Chapter 3** examines the numerous and, often, bewildering amount of functions "underneath the hood" of the packages that makes up the `nflverse`. For example, `load_pbp()` and `load_player_stats()` are included in both `nflfastR` and `nflreadr`. However, `load_nextgen_stats()`, `load_pfr_stats()`, and `load_contracts()` are all part of just `nflreadr`. Because of this complexity, readers will learn how to efficiently operate within the `nflverse`. Moreover, chapter 3 provides dozens of examples and exercises related to all of the various functions included. For example, readers will learn to use `load_nextgen_stats()` to determine which running backs get to the line of scrimmage the quickest and will use `load_pfr_stats()` to explore advanced defensive metrics across multiple seasons.

-   **Chapter 4** moves readers from data cleaning and manipulation to an introduction to data visualization using `ggplot2`. As well, chapter 4 provides further instruction on `nflverse` functions such as `clean_player_names()`, `clean_team_abbrs()`, and `clean_homeaway()`. As well, to prep for data visualization, readers will be introduced to the `teams_colors_logos` and `load_rosters` functions as well as the `nflplotR` package, which provides "functions and geoms to help visualization of NFL related analysis" [@carl2022]. Readers will produce multiple types of visualizations, including `geom_bar`, `geom_point`, `geom_density`, and more. As well, readers will learn to use `facet_wrap` and `facet_grid` to display data over multiple seasons. For visualizations that include team logos or player headshots, instruction will cover both how to do the coding manually using `teams_colors_logos` or `load_rosters` and to use the `nflplotr` package to avoid the need to use `left_join` to merge `teams_colors_logos` to your dataframe.

-   **Chapter 5** introduces advanced methods in R using `nflverse` data, with a specific focus on modeling and machine learning. To streamline the process of learning, readers will be introduced to `tidymodels`, a "collection of packages for modeling and machine learning using `tidyverse` principles" [@silge]. As an example, readers will first be introduced to [Tej Seth's](https://twitter.com/tejfbanalytics) Rushing Yards Over Expected model ([GitHub](https://github.com/tejseth/RYOE), [ShinyApp](https://mfbanalytics.shinyapps.io/RYOE/)). The model will serve as a learning tool to help readers understand the relationship between `nflfastR` data and machine learning (in Tej's case, an `xgboost` model). Afterward, specific attention is given to binary classification, multiclass classification, and regression computer learning models. At the conclusion of the chapter, readers will be provided exercises to allow them to develop their own supervised and unsupervised machine learning models.

-   **Chapter 6** introduces data from sources outside of the `nflverse`, including premium statistics from Pro Football Focus and Sports Info Solutions. Readers will learned to use various functions, such as `clean_team_names`, in order to prepare the data to merge with data from the `nflverse`. As well, this chapter will introduce readers to working with player tracking data. To do so, data will be provided from the [NFL's Big Data Bowl](https://operations.nfl.com/gameday/analytics/big-data-bowl/). To highlight the work being completed using player tracking, this chapter will discuss the Big Data Bowl entries of [Matt Ploenzke](https://twitter.com/MPloenzke) ([*The Importance of Ball Carrier Downfield Acceleration and Unblocked Tackler Distance and Spacing*](https://operations.nfl.com/media/4204/bdb_ploenzke.pdf)) and the team of Kellin Rumsey & Brandon DeFlon ([*The Battle Between Blocker and Defender Is Often Decided by Leverage*](https://operations.nfl.com/media/4209/bdb_rumsey_deflon.pdf)).

## About The Author {.unnumbered}

I (Bradley Congelio) am currently an Assistant Professor in the College of Business at [Kutztown University of Pennsylvania](https://www.kutztown.edu/). Aside from my core area of instruction, I also instruct the very popular Sport Analytics (SPT 313) course.

I earned my Ph.D. from the University of Western Ontario and received a specialized certificate in R for Data Analytics from the University of California, San Diego in 2021. I am a proud undergraduate alumni of [West Liberty University](https://westliberty.edu/) and am a strong advocate of a broad-based liberal arts education.

My research focuses on using big data, the R programming language, and analytics to explore the impact of professional stadiums on neighboring communities. I use the proprietary Zillow ZTRAX database as well as U.S. Census and other forms of data to create robust, applied, and useful insight into how best to protect those living in areas where stadiums are proposed for construction.

As well, my work in sport analytics, specifically the NFL, has been featured on numerous media outlets, including the *USA Today* and *Sports Illustrated*.

Finally, my most recent academic, peer-reviewed publications include:

1.  Congelio, B. (2022). 'Examining the Impact of New Stadium Construction on Local Property Prices Using Data Analytics and the Zillow ZTRAX Database." *Journal of Business, Economics, and Technology* Spring 2022, 39-55.

2.  Congelio, B. (2021). "Monitoring the Policing of Olympic Host Cities: A Novel Approach Using Data Analytics and the LA2028 Olympic Summer Games." *Journal of Olympic Studies* 2(2), 129-145.

3.  Congelio, B. "Predicting the Impact of a New Stadium on Surrounding Neighborhoods Through the Use of a *k*-means Unsupervised Clustering Algorithm." [Currently under peer review.]{.underline}

4.  Congelio, B. "Examining Megaevent's Impact on Foot Traffic to Local Businesses Using Mobility and Demographic Aggregation Data." [Currently under peer review and funded by a \$15,000 grant.]{.underline}

### Why A Book Instead of Working in Analytics?

I am sometimes asked why I spend time in the classroom teaching this material rather than taking my domain knowledge to the "industry side" and working in the NFL or an otherwise NFL-connected outlet.

The honest and, likely boring, answer is this: I love teaching. My favorite experience in the classroom yet is always in my Sport Analytics course. The frustration and sense of helplessness is palpable in the first weeks of the semester as students attempt to wrap their head around, what a former student called, "this [censored] foreign language." I insist that they keep pushing through the exercises and assignments. Often, there is line out my door and extending down the hallway during office hours comprised of just students from the Sport Analytics class.

And then something amazing happens.

Typically about halfway through the semester, I start seeing the light bulbs go off. Instead of cursing in anger at the "foreign language," students begin randomly cursing in excitement as the flow of the `tidyverse` language "clicks." Once that happens, it is off to the races because, once they understand speaking in `tidyverse`, learning more difficult packages (like `tidymodels`) seems doable.

And that is why I teach. That moment where I realize my lecturing, assisting, explaining, and gentle nudging are all finally paying dividends - not for me, though. For the students.

This book serves as an extension of that classroom experience. As a reader of this book, you are now a "student" and I hope you do not hesitate to reach out to me if you ever have any questions or, more importantly, *when (not if)* you have that "light bulb moment" and everything begins to click for you.



## Technical Details {.unnumbered}

This book was written using RStudio's [Visual Editor for R Markdown](https://rstudio.github.io/visual-markdown-editing/). It was published using the `bookdown` R package [@xie2015]. As well, the following packages were used in this book:

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:packages-used)Packages Used In This Book</caption>
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> package </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> version </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> source </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> dplyr </td>
   <td style="text-align:left;"> 1.0.8 </td>
   <td style="text-align:left;"> CRAN (R 4.1.2) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ggplot2 </td>
   <td style="text-align:left;"> 3.3.5 </td>
   <td style="text-align:left;"> CRAN (R 4.1.0) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> gt </td>
   <td style="text-align:left;"> 0.6.0.9000 </td>
   <td style="text-align:left;"> Github (rstudio/gt\@035c64b0a03f0372750095a7d390edcea4192b91) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> nflfastR </td>
   <td style="text-align:left;"> 4.3.0 </td>
   <td style="text-align:left;"> CRAN (R 4.1.1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> nflreadr </td>
   <td style="text-align:left;"> 1.2.0.07 </td>
   <td style="text-align:left;"> https://nflverse.r-universe.dev (R 4.1.3) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> nflverse </td>
   <td style="text-align:left;"> 1.0.1 </td>
   <td style="text-align:left;"> CRAN (R 4.1.3) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> scales </td>
   <td style="text-align:left;"> 1.2.0 </td>
   <td style="text-align:left;"> CRAN (R 4.1.3) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tidymodels </td>
   <td style="text-align:left;"> 0.2.0 </td>
   <td style="text-align:left;"> CRAN (R 4.1.3) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tidyverse </td>
   <td style="text-align:left;"> 1.3.1 </td>
   <td style="text-align:left;"> CRAN (R 4.1.1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> webshot </td>
   <td style="text-align:left;"> 0.5.2 </td>
   <td style="text-align:left;"> CRAN (R 4.1.1) </td>
  </tr>
</tbody>
</table>

Finally, please note that this book uses the `dplyr` pipe operator (`%>%`) as opposed to the new, built-in pipe operator released with version 4.1 of R (`|>`). It is likely that you can work through the exercises and examples in this book by using either operator. I maintain my use of the `dplyr` pipe operator for no other reason than personal preference.
