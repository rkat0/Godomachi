# About Godomachi
Explanation of this game by inventor :  https://j344.exblog.jp/238344979/


## Requirements

- GHC

Compile `ghc godomachi.hs`.

## How to use

### All combination
```
$ ./godomachi <n>
```
Analyze all combination of n-omino except same ones.
( `n` > 8 : not tested )  

Example:
```
$ ./godomachi 4
```
```
4-ominoes : 5
total patterns : 10 (= 5 * 4 / 2)
        first  : 10
                 [(1,10)]
        second : 0
                 []
```
When `n` = 4, first player wins in all patterns with 1 step.

---

### Single board
```
$ ./godomachi
```
Then input two polyominoes separated by a blank line.  

- block : `#`
- blank : other character

Example:  
```
$ ./godomachi
##
###

 #
###
#
```
```
----------


##.
###

.#.
###
#..

----------

#
#

.#.
.##

.#.
.##

----------

First player wins in 1 step!
```

---

### Single board (all first moves)
```
$ ./godomachi --first
```
Same as above, but check all first move patterns and list patterns by which first player wins.

Example:  
```
$ ./godomachi --first
###
 #
 #

####
#
```
```
----------

Initial board

###
.#.
.#.

####
#...

----------

1 steps

#
#

###

.###

----------

1 steps

#

.##
.#.
.#.

###
#..

first wins in 2/6 patterns

```

---

### List polyominoes
```
$ ./godomachi --rank <n>
```
Print all n-ominoes.

Example:
```
$ ./godomachi --rank 4
```
```
All free 4-ominoes : 5
#
#
#
#

##
#.
#.

#.
##
#.

##
##

#.
##
.#

```
