# AoC2019

[Advent of Code 2019](https://adventofcode.com/2019). After [AoC2020](https://adventofcode.com/2020), gives me an excuse to work with Ada more. (You read that right.)

One theme of the 2019 competition was **Intcode**, a kind of assembly. I really enjoyed (most of) the Intcode puzzles. I guessed early on that it would be advantageous to prepare a dedicated `Intcode` package, and I was correct: after its introduction on Day 2, Intcode appears again on Day 5, and on every odd day after that. My package needed occasional enhancement, but only one serious rewrite... on Day 23, as it turns out, a tad annoying. Probably not coincidentally, that was the puzzle that finally motivated me to implement a `Print_Disassembly` procedure.

Another theme on many days was **mazes**. I didn't enjoy those so much, especially Days 18 and 20; see below. That's not the designer's fault, though. Well, mostly. ;-)

A final theme that appeared a lot with the mazes, but also elsewhere, was problems whose naive solutions have **Very Bad Asymptotic Behavior (TM)**. Those nearly broke me, in part because I know I should know better. A bad feedback loop can start in my head at times like that.

A related theme, probably unintended: starting from Day 12, I found the even-numbered days trying.

## Day 1: Tyranny of the Rocket Equation

Apply a simple formula to input sequence, then compound the formula on itself.

## Day 2: Program Alarm

Intcode's debut. Run an intcode program, then find program parameters that produce a particular result.

This day introduces the following opcodes, with the associated number of parameters:

   * `01` **`add`** a b c
   * `02` **`mul`** a b c
   * `99` **`halt`**

## Day 3: Crossed Wires

Find where two wires cross... closest to origin, then with shortest distance traveled.

## Day 4: Secure Container

Determine how many numbers in a certain range satisfy certain requirements, then again with different requirements.

## Day 5: Sunny with a Chance of Asteroids

Enhance Intcode interpreter with new opcodes, and parameter modes, then run it to test. Then rerun it with a different value.

The new opcodes are

   * `03` **`input`** a
   * `04` **`output`** a
   * `05` **`jump-if-true`** a b
   * `06` **`jump-if-false`** a b
   * `07` **`less-than`** a b c
   * `08` **`equals`** a b c

The parameter modes are

   * `0` **position** (I think of this as "absolute"; cf mode `2` on Day 9)
   * `1` **immediate**

## Day 6: Universal Orbit Map

Determine the number of (direct or indirect) orbits in a system. Then determine the number of orbital transfers needed to get from `YOU` to `SAN`. (Gee, what could those positions mean? ;-) )

## Day 7: Amplification Circuit

Determine the maximal value produced by a chained sequence of phased amplifiers. Then do it again with a feedback loop.

For this puzzle I resisted modifying the `Intcode` package to handle multiple devices; instead I instantiated the package 5 times. Eventually (Day 23) I had to break down and modify for multiple devices. I may go back and rework this to use the packages version. Or not

The major modification I did make at this point was to create a mechanism to supply the devices with input and output without having to use the command line: `Supply_Input` and `Report_Output`, etc.

## Day 8: Space Image Format

Determine the layer of an image that has the fewest 0's, report number of 1's times number of 2's. Then decode the image, where 0 is black, 1 is white, and 2 is transparent -- which means, move down to the next layer for data. Then report the message.

## Day 9: Sensor Boost

Complete the Intcode computer with relative mode addressing and a command to adjust the relative base; test with input `1`. Then run with input `2`.

The new opcode is

   * `9` **`adjust_base`** a

The new parameter mode is

   * `2` **relative** (to base pointer)

The intcode specification is now complete, but subsequemt tinkering with the implementation was still necessary.

## Day 10: Monitoring Station

Detect which location of an asteroid is best to monitor the others. Then destroy 200 asteroids from that base, report modified location of 200th.

## Day 11: Space Police

Write an Intcode robot that paints the hull, changing panels from black to white or back again, and report how many panels change color. Then run the program again with a different input and report the letters that get painted.

## Day 12: The n-Body Problem

Determine the total energy in a system of moons. Then determine how long it will take for the system to repeat state.

It turns out that in part 2 it suffices to determine how long it takes for the system to repeat initial state, which you can obtain by computing the lcm of how long it takes to repeat initial state in each dimension.

In fact, you can obtain this by computing the lcm of how long it takes the system to become stationary in each dimension, which indicates how long it takes the system to reach its "halfway point". Multiply that result by 2, and you're done.

This problem marked the beginning of my struggles with AoC2019. Part of this was the beginning of the semester, so I had less time and sometimes attacked these problems when I wasn't in the most alert state of mind. But part of it was also a combination of increased difficulty and stupidity.

Indeed, I didn't work this out on my own. After working with a slightly flawed approach for a while, and after just plain flailing a longer while, I looked up the general idea online. I don't seem to have downloaded other solutions, though; I simply read the discussions and went from there. Not all the days were that "good".

### MOMENT OF VENTING THAT I WILL REGRET LATER
I like part 2 in principle, but not in practice. No hints were given, nor were the examples useful in any way. Even the examples' size was unhelpful: the smallest has a period of 2772, with the dimensions' periods being 9, 14, and 22. Smaller periods would have made it easier to perform a useful analysis. I should have worked it out on my own, given that the solutions are basic number theory, but that didn't become clear to me at the time, perhaps on account of the practical constraints I was under.

## Day 13: Care Package

Determine the number of block tiles on screen when a breakout-style game exits. Then play the game, win, report the score. You probably want to automate the `Intcode`, because there are a lot of tiles. (I did!)

## Day 14: Space Stoichiometry

Determine how much `ORE` you need to create 1 `FUEL`. Then determine how much `FUEL` you can create with 1 trillion `ORE`.

Both parts were a bit tough, to a large extent because you have to take into account how to use leftover minerals. I came up with a solution to part 2 that I think will work and I think is not entirely unclever, but I don't know, because 10 1/2 [hours] passed before it completed, and by that time I had devised a better solution, thanks in large part to [pr0g's rust-based solution on GitHub](https://gist.github.com/pr0g/e346b4f34d0a6724a3ca7437db278947).

[*ed: I* think *it's hours. I seem to have left out the units when I first wrote it*]

My original version generates 1 unit of fuel at a time, waiting to see at what quantity of fuel all the minerals are exhausted, at which point it divides the 1 trillion by that amount of fuel generated, then computes how much it can compute with what's left. Whether this works in reasonable time apparently depends very much on your luck at drawing good numbers.

The new version, which, while not a copy of pr0g's version, is certainly inspired by it, iteratively calculates how much ore is necessary to produce a given amount of fuel, subtract that from the amount of ore available, then repeat. The amount of fuel generated is the quotient of the amount of ore left and the amount of ore necessary to generate 1 fuel.

Even at this point I didn't have it quite right; it was only when I realized that even if there isn't enough ore to generate 1 fuel, there can still be enough ore *and* minerals left to generate 1 fuel.

As an aside, a lot of solutions I saw online are wrong for the general problem: they provide incorrect answers for my input! I saw some discussion of this online. I suppose they simply got lucky with how the system worked on their formulas.

## Day 15: Oxygen System

Determine the shortest route from the droid to the oxygen system. Then determine how long it takes for oxygen to spread through the section.

If I remember correctly, this is both intcode and maze, but the maze is embedded in the intcode. I may misremember.

## Day 16: Flawed Frequency Transmission

Apply Flawed Frequency Transmission to transform an input sequence and after 100 phases report first 8 digits.

Then figure out the message, which is an ungodly number of digits into an ungodly long signal, which itself has to be processed 100 times... but even an efficient processor won't do the trick because it's so ungodly long that you need to analyze the procedure and the input to figure out how to solve it, which requires you to ignore what you did in part 1, not to improve, but to ignore it.

My solution to Part 2 appears as `Third_Approach_From_Reddit`. As indicated by the name, I found this clever approach in the reddit discussion. I actually thought of something like this, but I kept attempting to analyze it from the beginning instead of the end.

Essentially, the solution is problem-dependent. The message Start is chosen past halfway, so when you apply the pattern, the pattern's repetition means you always add the digits from there on. This means you can work backward: the last value of the output is always the last value of the input, and from then on you just add.

This needs a larger stack: `ulimit -S -s 131072` on bash was more than enough.

## Day 17: Set and Forget

Determine intersections of scaffolds and report sum of alignment parameters. Then program the vacuum droid to visit all the scaffolding, and report the amount of space dust collected. Again, a combination of intcode and maze.

Part 1 was pretty easy.

I had issues with part 2, due in large part to (a) misreading the input format, and (b) the seemingly random placement of the output in part 2.

There's also an issue with how my input & output interact; the intcode produces output and expects input, but the output seems not to be formatted correctly, so that prompts seem to appear *after* it reads the desired input.

The problem is probably that I don't have any way of specifiying input length, so that my code will read invalid input just so long as the input position has not advanced beyond the last buffer element.

The program works, but ideally I'd rework it so that this issue is resolved; alas, I probably won't.

*Added later: I wonder if I had forgotten that I did have a way to specify input length indirectly. I don't remember which day it was, probably day 7, but the input buffer has fixed length, say n, and if the input requires less length, say m, one supplies m-n along with the input buffer in `Supply_Input`.*

## Day 18: Many Worlds Interpretation

We've had some maze-like puzzles already, but now they go hard-core. Oof.

Find the shortest path through a maze which unlocks all the doors. Then repeat for four robots working simultaneously on a slightly modified maze. These are not intcode robots, but four simultaneously-executed breadth-first searching robots.

For some reason this puzzle bedeviled me. I was unaware of Dijkstra's algorithm, but once I learned of it from Reddit, implementing it helped a lot. Nevertheless, one of my solutions continues to produce wrong answers, and I am not sure why. As I recall, `Find_Route` should work with all four robots, but it only works with one. With all four I have to use `Find_Alternate`. I have checked and rechecked the former, and even compared it to the latter, but I have yet to determine the problem. This was probably my lowest point; I began to wonder whether I'd ever finish AoC2019, and let more than a month pass before I returned to it. In the end I found my solution by comparing my output to that [prscoelho's Rust solution](https://github.com/prscoelho/aoc2019/blob/master/src/aoc18/mod.rs), then rewriting the procedure from scratch to fix the problems as I detected them. The approach isn't the same as prscoehlo's (I *think*) but it would have taken me much, much longer without it.

## Day 19: Tractor Beam

Determine where tractor beam is active in a 50x50 grid. Then determine where Santa's 100x100 ship might fit into tractor beam.

Shockingly, I got part 1 on the first try, and part 2 on the second. In fact, I really had part 2 on the first try; I just reported the wrong data. A nice respite after Day 18.

## Day 20: Donut Maze

Find the shortest path through a maze where you have to traverse portals. In part 2, find the shortest path out when the maze is recursive.

I got part 1 easily on the first try, once I ironed out the wrinkles in the code. Part 2 was another story; the solution I came up with was incorrect in multiple ways. For instance, I didn't think about how the direction you pass through a portal affects which portals are available. The eventual solution is my own, but I had to look at some other people's solutions to determine what was going wrong: possibly [Matthias Buelens' Rust solution](https://github.com/MattiasBuelens/advent-of-code-2019/blob/531dc0d799a5bc59ce32a5aa33b68ae9cc133101/src/bin/day20/main.rs) but definitely [justinhsg's Python solution](https://github.com/justinhsg/AdventOfCode2019/blob/master/20/20.py). This was one of my two low points; as with Day 18, I let several weeks pass before picking it up again.

## Day 21: Springdroid Adventure

Program a springdroid to `WALK` and jump over holes in the hull so that it can count the damage, making use of sensors and your wits. In the second part, `RUN` instead of `WALK`, and use more sensors.

I had to modify the Intcode program to accept `ASCII` input, both from the command line and from the input buffer, because I didn't like the idea of converting text to `LONG_LONG_INTEGER` the way I did on Day 17.

With that out of the way, it was a relatively simple matter of determining the correct sequence of springdroid commands that would fit into 15 lines. Part 1 is no problem; part 2 requires a little Boolean algebra. (*very* little)

With the Intcode modified to accept `ASCII`, I entered the "springtext" on the command line. Solutions appear in the files `solution_to_part?.txt`. Notice that, on account of how Ada's `Ada.Text_IO.Get` processes `Character` input, my implementation requires one to type a `-` to indicate an end of line. I kept this through the rest of the puzzles, as well.

As with Day 19, I found this a welcome confidence-booster after Day 20.

## Day 22: Slam Shuffle

Finally something that's neither maze nor intcode!

First apply different techniques to shuffle some cards. Then unshuffle after shuffling an ungodly number of times.

*Note to self:* Read questions correctly! I was briefly stuck on part 1 because I thought it wanted "the card in position 2019" instead of "the position of card 2019".

The moment I saw the large numbers on part 2, I despaired, what on account of the issues I'd had on Days 18 and 20, and immediately went to the internet to see what people were saying about it. I probably shouldn't have, because the solution involves mathematics that I teach and like. The trouble is, I first looked at the problem and started thinking, "Euler's Theorem should make this go faster" but of course I thought about it backwards, so I couldn't for the life of me figure out how to use Euler's Theorem to do so. (See the mental feedback loop I mentioned earlier.) As it happened, I never actually used Euler's Theorem.

After reading some of the discussion, I concluded that I should compute a linear function to describe the input, raise that to a power, then somehow "unsolve" it. But, raising it to a power is still absurd! Then I realized (see? I didn't read *too* much on Reddit!) that the power is *composition* of *functions*, not *products* of *polynomials*.

It's still quite challenging after that! Essentially I set up a function *f* = *a x* + *b*, where *f* is the position of the item in position *x* after we apply the input shuffles. I then raise it to the given power, using fast "exponentiation" (graduate school was good for something! -- some people online call this squaring). This gives us a function *g* = *c x* + *d*, which tells us the position of the item in position *x* after we repeat *f* that aforementioned ungodly number of times.

Finally we compute *x* = *c*^(-1) ( *g* - *d* ), which tells us the initial position of the card that at the end is in position *g*.

This required me to go into `Big_Integer`. I read that some people solved it without going that far, but I'm pretty happy with what's here, especially as i worked it out without too much assistance, and once i actually got it working correctly i got it on the first try.

Besides, it's not another evil maze!

## Day 23: Category Six

As I recall, this is the one Intcode puzzle that gave me trouble. Ironically, it should have been easy.

The first task is to network 50 Intcode computers to communicate with each other. This motivated me to implement `aliased` types and use `access`, I think for the first time when doing Advent of Code. That required me to relearn how to use those keywords!

The second task is to add a 51st computer, called NAT, which monitors for when other computers are idle, at which point it sends the last message it received to the first one.

I had difficulty with part 1, because I misunderstood how the Intcode program would read the input. I supplied both inputs at once, then cleared the input. That caused the machine to lose its second input. After discovering the problem and thinking about it a little (in part by comparing to the output of [loocianos's Python solution](https://github.com/loociano/advent-of-code/tree/master/aoc2019/src/day23), in part by rereading the problem) I realized how to fix it. Then I remembered/realized that I had already set up the Intcode machines to handle cases where input sizes were not always the same, so I reverted to my original code and abstained from clearing input after supplying it. Works great!

I had difficulty with part 2, as well -- not as much as part 1, but more than I should have. One problem was that I misread the directions: I tried to return the first value returned twice, rather than the first value returned twice *in a row*. The last mistake I fixed was determining when the network was idle. I think I had that implemented correctly at first, but I had definitely misimplemented the NAT transmission conditions at one point, and the wording is a bit funny: "continuously trying to receive packets without sending packets": If they're trying to receive, why would they be sending? Well, OK, boss, if you say so... That extra condition led me astray, and I misimplemented the check for not sending, and I think I misdiagnosed the problem and broke the inputs.

Another thing that held me up on part 2 was the fact that a correct solution actually takes quite a while. I thought I was getting the wrong answer when 2-3 seconds pass and it's still running. That's kind of unusual for AoC puzzles in my experience, so I suspect there's a better way to implement this -- say, not using round-robin.

## Day 24: Planet of Discord

I really enjoyed this puzzle, a variant on the game of life. (Again, no maze or intcode.)

First we find the first pattern that repeats twice in a small game-of-life. Then we realize that, hey, the Plutonians did this, just as they were responsible for that evil, recursive donut maze on Day 20, so this is also recursive, but it's actually fun.

I didn't get this on the first try, but I did eventually get it, without too much confusion even, and found it rewarding.

## Day 25: Cryostasis

Send an intcode droid to explore Santa's ship.

The programming is trivial: read the intcode program and run it. It turns out that you end up playing an old-style Text Adventure Game (TAG). So, that effort you put into making that Intcode program read `ASCII` input pays off!

The main puzzle involves getting past a security sensor; the droid has to carry the correct weight. In various rooms (there are 18 in all) you find items to weigh the droid down. You want to pick up only a few of the items, as some are traps (see below) and others make you too heavy (probably varies by player). Some experimentation (at most 256) will get you past security, and you obtain the passcode. It's possible to eliminate a *lot* of the bad cases, since some "too heavy" combinations of a few items indicate that you shouldn't try combinations that add more items, and some "too light" combinations of many items indicate that you shouldn't try combinations that remove items. For my problem,

    * easter egg, shell, and space heater are already too heavy;
    * everything except the shell is too light.

So the shell must be part of the solution, while no solution can combine the shell with the easter egg and the space heater. Of course, figuring out those two already took me quite a few attempts, but I did get the correct answer in under 40 guesses. That's probably embarrassingly high, but it's still better than 256, or even 64!

I'm aware that one could automate the last part, but I didn't try that.

(Curiously, I managed to determine that the shell weighs more than the mug, the jam, the easter egg, the coin, and the fuel cell -- *combined*. What kind of shell is this?!?)

(Also curiously, the shell weighs less than the monolith, as determined in the following fashion: as noted above, everything except the shell is too light, but everything except the monolith is too heavy. I've seen *2001: A Space Odyssey*, and I'm not convinced that a monolith is that light!)

The TAG's commands are simple: directions to move, `take`, `drop`, `inv`. Several traps were fairly obvious (molten lava, infinite loop); several were less so (electromagnet, photons); and though I was at first certain that the escape pod was a trap, I misread some output at Security and thought I had to go back and take more things, so I unfortunately took that.

I didn't get the joke about a grue, but looking it up I learn that grues appeared in Zork, one of the original TAG's.

`solution.txt` contains the correct solution to my case.

As with AoC2020, Part 2 is even more trivial than Part 1: click a link, get your reward.
