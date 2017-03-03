# todo.hs
A haskell implementation of todo.txt

This application was created as a series of blog posts to cover some of the
concepts of Haskell. To go through the development process go [here][2].

## Supported Features (0.0.1)

The todo.txt file is currently hardcoded to your `$HOME` directory. The default sort is based on
priority.

**Add**

    $ todo add "Complete an example task +TodoExample"
    New Task: Complete an example task

Supports the format descripted in the [todo.txt format doc][1], including priority, start date,
contexts and projects.

**List**

    $ todo list
    1: Pick up milk @errands
    2: Pick up eggs @errands
    3: Pay Bills +LifeProblems
    $ todo list @errands
    1: Pick up milk @errands
    2: Pick up eggs @errands
    $

The list command supports optional filters for contexts and projects. List will find matches that
contain all filter options.

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

**Delete**

    $ todo
    1: Do not complete this task
    2: Complete this task
    $ todo delete 1
    Task Deleted
    $ todo
    1: Complete this task
    $ cat todo.txt
    Complete this task

The delete command removes the incomplete task from your todo.txt file. Doesn't not mark complete,
just deletes the entry.


## Future features

- Sort by Due Dates (due:2017-07-29)
- Smart Due Dates (due:tomorrow, due:today+3, due:friday)

[1]: https://github.com/ginatrapani/todo.txt-cli/wiki/The-Todo.txt-Format
[2]: https://commentedcode.org/blog/2016/07/30/haskell-project-stack-and-data-types