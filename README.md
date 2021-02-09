# Charter Poker

Running the program:
```
> sbt compile
> sbt run
```

## Implementation details
The implementation here is pretty straight forward:

1. parse into a useable structure (with some validation)
2. evaluate each hand, and assign a "score"
3. compare each hand's score. 

## Pros / Cons
Pros: 
* The data structure / evaluation methods I've written make it easy enough to extend the evaluation to same types (i.e, straight vs straight). Would require some additional math/logic in the `handValue()` to assign hand type value + card / suit values


Cons:
* The main `handValue()` function is a little ugly. 
* Waterfall conditionals can lead to some computational overhead
* Parsing validation could be more uniform / optimized


## Potential alternate approaches:
This implementation could be modified to create more defined "Hand Types", and then precompute a list of known hand types. After which you could pattern match and assign the hand value. If this was part of a long running system, the up front memory cost might be worth the saved computation cycles. 

