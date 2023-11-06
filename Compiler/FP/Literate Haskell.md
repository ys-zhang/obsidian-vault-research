#Haskell #literate-programming

There are 2 types of literate programs in Haskell: 
1. Bird-style or Bird-scripts
2. Latex-style


# Bird-scripts

1. comments are default;
2. code is introduced with a leading `>`
3. empty line btw comments and code is necessary

```
Hello world in Haskell

> module Main where

bla bla

> main = do 
>   putStrLn "Hello world"
```
# Latex style 

code is wrap in a  `code` environment 

```
\begin{code}
module Main where
\end{code}

bla bla bla

\begin{code}
main = putStrLn "Hello world"
\end{code}
```
