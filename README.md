# :snowflake: :christmas_tree: :snowman: Advent of Code 2023 :snowman: :christmas_tree: :snowflake:

## About This Repo

Documenting my solutions and progress for [Advent of Code
2023](https://adventofcode.com/2023/). This is my second time attempting
the challenge.

Last year, I got through Day 8 in Python
<img src="images/python.svg" alt="Python symbol" width="25" height="25"/>.
This year, I’m starting off in R
<img src="images/R_logo.png" alt="R symbol" width="25" height="25"/> but
might jump around a bit. I have not had much opportunity to code over
the last year or so, so my skills (what little I have) are extra rusty.

Although I’m mainly focused on making it work and finding a solution,
I’ll probably come back once or twice to refactor the more
interesting/challenging puzzles, or to try a different approach with
performance benchmarks. No final answers are explicitly stated in the
code.

## Current Status

| Day    |    Completion     |
|--------|:-----------------:|
| Day 1  |   :star: :star:   |
| Day 2  |   :star: :star:   |
| Day 3  |   :star: :star:   |
| Day 4  |   :star: :star:   |
| Day 5  | :star: :hot_face: |
| Day 6  |   :star: :star:   |
| Day 7  |   :star: :star:   |
| Day 8  | :star: :hot_face: |
| Day 9  |   :star: :star:   |
| Day 10 | :star: :thinking: |
| Day 11 | :star: :thinking: |

## Caveats, Comments, and H/Ts:

- Day 1 - H/T to
  [ursulams](https://gist.github.com/ursulams/9e79aa2f478c83da14e78751139f03c2)
  for their approach to the mash-ups

- Day 3 - Had to do some serious troubleshooting and used someone’s code
  to get the right answer (whose? can’t remember!) for part 1, in order
  to work backward. Realized it was a super dumb fumble in my long
  chains of conditionals — one more reason to clean this up at some
  point!

- Day 5 - Part 1 was an eventual success but part 2 really threw me.
  Definitely plan to come back to finish part 2 but needed to move on…

- Day 8 - Similar to day 5. Part 1 was pretty straight-forward; part 2,
  just as soon as I started to wrap my head around it, evaded me. BUT I
  learned that R has an `lcm()` function, which I would assume it did
  but never had good reason to try!

- Day 9 - How on earth was this one so easy? Or did my skills magically
  grow overnight? (No, it was just easy.)

- Day 10 - Part 1. Dare I admit that I wasted a lot of time with the
  *right* solution but written recursively, and consistent running into
  a depth recursion error? Dare I admit how long it took me to just
  rewrite it as a simple iteration? Ugh. Lessons learned.

- Day 10 - Part 2. Struggled to translate my ideas for how to approach
  this into action. Pilfered code from a post by
  [u/KeroTheFrog](https://www.reddit.com/r/adventofcode/comments/18evyu9/comment/kcso138/)
  and translated to R; it taught me some geometry (the [shoelace
  formula](https://en.wikipedia.org/wiki/Shoelace_formula)) so I am
  counting it as a win for the learning rather than an honest
  achievement, lol. Also, learned about the `zeallot` package for
  destructuring assignment in R – score!

  - Also, shout out to [lauraschild’s
    solution](https://github.com/lauraschild/AOC2023/blob/main/day10.R)
    in
    <img src="images/R_logo.png" alt="R symbol" width="25" height="25"/>
    that graphed the final result for Part 2 — super cool!
