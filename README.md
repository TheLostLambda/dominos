# Dominos
Danger, No Danger
6 & 5; -2 : (5130,4870)
6 & 5; -1 : (5106,4894)
6 & 5; 20 : (47958,52042)
6 & 5; -1 : (50383,49617)
6 & 5; -2 : (50588,49412)
6 & 5; -3 : (50588,49412)
6 & 5; -4 : (50387,49613)
6 & 5; -5 : (50371,49629)
6    ; -2 : (50234,49766)
6,5,4; -2 : (50422,49578)

```hs
*Main AI DomsMatch Game Control.Parallel.Strategies> foldr1 (\(a,b) (x,y) -> (a+x, b+y)) $ parMap rdeepseq (domsMatch playerHFE playerHFEBM 6250) [1..16]
(48173,51827)
*Main AI DomsMatch Game Control.Parallel.Strategies> foldr1 (\(a,b) (x,y) -> (a+x, b+y)) $ parMap rdeepseq (domsMatch playerHFEBM playerHFE 6250) [1..16]
(51683,48317)

*Main AI DomsMatch Game Control.Parallel.Strategies> foldr1 (\(a,b) (x,y) -> (a+x, b+y)) $ parMap rdeepseq (domsMatch playerHFEB playerHFE 6250) [1..16]
(50588,49412)
*Main AI DomsMatch Game Control.Parallel.Strategies> foldr1 (\(a,b) (x,y) -> (a+x, b+y)) $ parMap rdeepseq (domsMatch playerHFE playerHFEB 6250) [1..16]
(49340,50660)

*Main AI DomsMatch Game Control.Parallel.Strategies> foldr1 (\(a,b) (x,y) -> (a+x, b+y)) $ parMap rdeepseq (domsMatch playerHFEBM playerHFEB 6250) [1..16]
(51059,48941)
*Main AI DomsMatch Game Control.Parallel.Strategies> foldr1 (\(a,b) (x,y) -> (a+x, b+y)) $ parMap rdeepseq (domsMatch playerHFEB playerHFEBM 6250) [1..16]
(48839,51161)
```