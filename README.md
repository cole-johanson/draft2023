
# draft2023

<!-- badges: start -->
<!-- badges: end -->

This draft uses a technique I created to take some sort of cross-positional ranking (e.g. a rating, dollar 
amount, or Average Draft Pick (ADP)) and create a value from 0-100.

The algorithm works like this.

1. Take a cross-positional ranking and sort players in each position by their ranking. 
2. Define the "dropoff" for each player: The dropoff is the difference between that player's ranking and the 
   next best player's ranking. For instance, suppose our ranking is the ADP. If RB1's ADP is 1.3 and RB2's is 
   2.4, RB1's dropoff is defined as 2.4-1.3 = 1.1. Alternatively, if RB1 is rated 100 and RB2 is rated 96, the 
   dropoff is 100-96=4. 
3. Set the lowest player's dropoff as 0. 
4. Define each player's raw value as the sum of his and all lower players' dropoff. In the example above, if 
   RB2's value is 96, RB1's value would then necessarily be 97.1.
5. Across the data source, scale the ratings to be between 0 and 100. We do not normalize the ratings as we 
want to maintain the relative positioning (because points matter) but we scale so that we can compare each 
data source. 

I then take the simple approach of weighting each data source equally. However, I do include the standard
deviation calculation, and the raw rankings so I can have some real-time insight into how the single averaged
value is being created. 

This method consistently returns exponential samples with a median around 22. (As expected, we have very few
players with lots of value).
