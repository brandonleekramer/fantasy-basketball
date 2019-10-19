## Fantasy Basketball Tools

I have a vice... I love fantasy basketball. Over the past couple of years, I have been playing in a league with some friends, but after finishing grad school I have now expanded to four different leagues, including an "all-star" league where members of the league draft their team from the broader pool of NBA players based on existing contracts. Since seemingly everyone else in the league has participated in a draft like this before except for me, I decided to take an analytics-based approach to drafting my new team while also aggregating a broader database of all the contracts across my five leagues. After creating this database, I decided to cross reference the "value" of existing contracts against the performance of all NBA players in 2018 as well as the various projections that are released by fantasy basketball sites like last year's statistics from Basketball Refernce as well as this year's Hashtag Basketball projections and another variation of those projections I acquired from a veteran in one of my leagues. I also decided that I would run overall team projections to see whether I stand a chance of winning the league or whether I should take an alternative strategy to build assets for the future (e.g. "tanking" to maximize the value of my own draft picks next season). Once I did this work, I decided to expand some of the fantasy tools for my other leagues as well. In each page, you will see five sections. 

**Section 1.**

First, I load all of the projections statistics, contracts and team rosters for each league (Legacy Dynasty, Franchise NBA, NBA Best of the Best, and The Invitational. Each of these is a 30-team points leagues except for The Invitational, which is a 20-team nine categories league. For points leagues, this section converts raw projections totals into points totals depending on the league's rules. For the cats leagues, it breaks the totals into those cats. In this section, you can look at the basic descriptives of team contracts and projections by team. 

**Section 2 (Still in Development).** Second, I take the individual player projections and use them to ***calculate projections for each team*** in my leagues. Here, projections are calculated using (1) average fantasy points per game per player, (2) totals points scored per week based on those player averages, (3) total points projected based on NBA scheduling, and (4) total win-loss records for each team based on the current team projections. For my cats leagues, you see the projections broken down by categories, which includes an interactive spider chart to see those team comparisons in one compact location. There are two main reasons for doing this. In the leagues where my team is good, I want to know who my main competitors are and what I need to do to get better than them. Obviously, I have some idea who is good from last year or by looking at their teams, but this tells me more clearly how far I am ahead or behind other teams. The other main reason I created these projections is for when my team is rebuilding. Knowing who is good or bad is important when trading away assets. Obviously, you want to try to get all the best draft picks from the worst teams while largely ignoring all of the worst picks. 

**Section 3.** Third, there is a section where I look at ***value.*** For now, "value" is simply calculated as projected points totals (using the two different sets of projections) divided by the contract value for the current year. For points leagues, my main goal is to try to maximize the total points scored on the cheapest total contract amounts. This entails not only looking for the best "pure" contract values but also the guys that produce in higher points "tiers." In other words, we don't want guys that score 20 fantasy points per game (below average) on a $1 million dollars when a guy that scores above 35 fantasy points per game (well above average) at $4 million dollars. For now, I just do this through filtering lower tier movies. Lastly, this section also outlines some code that filters best contract values for the teams that are bad in my league where I am competing. I created this to help me look for specific players that are good (and thus might make bad teams better), so I can target them in trades. 

**Section 4.** Fourth, I developed a ***trade comparison tool.*** This basically uses the team projections in Section 2 but then draws data from a new column to compare how much better or worse a trade would make your team if you executed a trade. This tool is handy not only when competing, but also when a tanking team is trading for picks. You don't want to make a team too good so that there pick gets worse. Thus, you can use this tool to make sure that your trade gets you a pick while still making that team pretty bad or at least about the same. 

**Section 5 (Still in Development).** Fifth, I draw from these projections and the real-life NBA scheduling to develop an index for ***detecting "hot" and "cold" streaks.*** This effectively graphs players that are producting at above and/or below average totals. 

**NBA Draft Projection Tools (Still in Development).** During the 2018-19 season, I also developed some basic ***descriptive analyses of NBA prospects.*** While this work deserves considerably more attention, I just did some basic graphing of box stats based on position type (categorized as guards, wings and bigs). These exercises helped me understand which prospects deserve to have more attention for watching tape. While I did use these tools to last year, I failed to take my results seriously enough and ignored indicators that said players like Tyler Herro, Matisse Thybulle and Brandon Clarke. Apparentely, there is something to be said for the power of these basic descriptive analsyses. That said, my strong preference would be to leverage some machine learning strategies on past prospect's college data to find out what the best statistics are the best indicators on NBA success for each position. 
