module Tasks where

type Priority = Char -- (A)

type Project = String -- +ProjectName

type Context = String -- @Context

data Date = Date Int Int Int
  deriving Show

data Task = Incomplete (Maybe Priority) (Maybe Date) [Project] [Context] String
          | Completed Date Task
          deriving Show


