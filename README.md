
## Time Well Spent

Super duper double alpha non-release use-at-your-own-risk version of a
task tracker for emacs.

Categorize goals, track time spent on them, generate reports by
category.

If you want to try it out. Clone the repo, and load
`time-well-spent.el` in your init file.

NB *I had some weird troubles with load order, so I just load the file as
the very last step in my emacs init file. Obviously that approach is
not ideal - it should be fixed sometime.*

You should then be able to call the function `time-well-spent` to
start the process.

Type `?` to see a list of commands.

Depends on [emojify](https://melpa.org/#/emojify) at present.

### How Entries Are Sorted

I'm using a priority sorting that I've dubbed "barski sort" because it
is loosely based on [this post](http://www.lisperati.com/#!A_Productivity_System_For_Creators) 
from Conrad Barski.

Essentially, it places entries that are marked as "on the move"
before entries that are marked as "waiting", and those are before
entries marked as "in the future".  

Within those blocks, it puts entries in the least worked on categories
before entries in th more worked on categories. 

And within a single category, it places the most worked on entry
before less worked on entries.

