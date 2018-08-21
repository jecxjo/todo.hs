# todo.hs
A haskell implementation of todo.txt

[![Build Status](https://travis-ci.org/jecxjo/todo.hs.svg?branch=master)](https://travis-ci.org/jecxjo/todo.hs)

This application was created as a series of blog posts to cover some of the
concepts of Haskell. To go through the development process go [here][2]

## Supported Features (0.4.0)

_Note:_ Major rewrite in version 0.4, switched to an mtl style, making the code
much cleaner. No real functional changes.

**Add**

    $ todo add "Complete an example task +TodoExample"
    New Task: Complete an example task
    $ todo add "Do your homework by due:tomorrow"
    New Task: Do your homework by due:2017-04-05

Supports the format descripted in the [todo.txt format doc][1], including priority, start date,
contexts and projects.

*New:* The command `addx` will do both an add and a complete in a single shot.

    $ todo addx "An already completed task"
    COMPLETED: An already completed task

**List**

    $ todo list
    1: Pick up milk @errands
    2: Pick up eggs @errands
    3: Pay Bills +LifeProblems
    4: Pick up dog from vet
    $ todo ls "Pick up"
    1: Pick up milk @errands
    2: Pick up eggs @errands
    4: Pick up dog from vet
    $ todo ls "Pick up" @errands
    1: Pick up milk @errands
    2: Pick up eggs @errands

The list command supports optional filters for contexts and projects. List will find matches that
contain all filter options.

The `listpriorty` command is the same as list, filtering for only tasks with priorities.

**Complete**

    $ todo list
    1: Pick up milk @errands
    2: Pick up eggs @errands
    3: Pay Bills +LifeProblems
    $ todo complete 2
    Task Completed
    $ todo list
    1: Pick up milk @errands
    2: Pay Bills +LifeProblems
    $ cat todo.txt
    Pick up milk @errands
    Pay Bills +LifeProblems
    x 2016-07-30 Pick up eggs @errands

The complete/done command will change an incomplete task to completed.

*New:* Now supports the ability to complete multiple tasks at once.

    $ todo list
    1: Pick up milk @errands
    2: Pick up eggs @errands
    3: Pay Bills +LifeProblems
    $ todo complete 1 3
    Pay Bills +LifeProblems
    Pick up milk @errands
    Complete (N/y)? y
    Task Completed
    $ cat todo.txt
    2: Pick up eggs @errands
    x 2018-04-26 Pick up milk @errands
    x 2018-04-26 Pay Bills +LifeProblems

**Delete**

    $ todo
    1: Do not complete this task
    2: Complete this task
    $ todo delete 1
    Do not complete this task
    Delete (N/y)? y
    Task Deleted
    $ todo
    1: Complete this task
    $ cat todo.txt
    Complete this task

The delete command removes the incomplete task from your todo.txt file. Doesn't not mark complete,
just deletes the entry.

**Append/Prepend/Replace**

    $ todo
    1: Complete this
    $ todo append 1 "task tomorrow"
    Updated Task: Complete this task tomorrow
    $ todo prepend 1 "IMPORTANT"
    Updated Task: IMPORTANT Complete this task tomorrow
    $ todo replace 1 "Do stuff tomorrow"
    Updated Task: Do stuff tomorrow
    $ todo
    1: Do stuff tomorrow

**Priority**

    $ todo
    1: Example Task
    $ todo pri 1 a
    Updated Priority
    $ todo
    1: (A) Example Task
    $ todo pri 1
    Updated Priority
    $ todo
    1: Example Task

**Archive**

    $ todo
    1: Example Task
    $ todo complete 1
    Task Completed
    $ todo archive
    Completed Tasks Archived
    $ cat $HOME/todo.txt
    $ cat $HOME/done.txt
    x 2017-04-27 Example Task

**Report**

    $ todo
    1: Example Task
    2: Another Task
    $ todo complete 1
    Task Completed
    $ todo report
    Completed Tasks Archived
    Report Created: 1 1
    $ cat $HOME/report.txt
    2017-04-27T13:40:35 1 1

**NEW: Regular Expression Searches**

    $ todo
    1: Example Task
    2: Another Task
    3: Pick up milk
    4: Pick mike up from airport
    $ todo search Task
    1: Example Task
    2: Another Task
    $ todo search "mi(lk|ke)"
    3: Pick up milk
    4: Pick mike up from airport
    $ todo search Task

*Note:* Regular expressions are handled by PCRE so perl regex, not POSIX. 

**NEW: Regular Epxression Swaps**

    $ todo
    1: Pick mike up from airport
    $ todo swap "mike up" "up Mike"
    Updated Task: Pick up Mike from airport

You can even use indexed groups.

    $ todo
    1: Pick up Mike from airport
    $ todo swap "(Mike)" "@\1"
    Updated Task: Pick up @Mike from airport

Or change due dates.

    $ todo
    1: Pick up milk due:2017-06-14
    $ todo swap 1 "due:.*" "due:tomorrow"
    Updated Task: Pick up milk due:2017-06-15


## Future features

- Colored output

[1]: https://github.com/ginatrapani/todo.txt-cli/wiki/The-Todo.txt-Format
[2]: https://commentedcode.org/blog/2016/07/30/haskell-project-stack-and-data-types
