> module Terminal
> (
>   terminalAppearance
> )
> where

> import Char (isPrint)


Simplifies the input string by interpreting '\r' and '\b' characters
specially so that the result string has the same final (or "terminal",
pun intended) appearance as would the input string when written to a
terminal that overwrites character positions following carriage
returns and backspaces.

The helper function `ta` takes an accumlating `ShowS`-style function
that holds "committed" lines of text, a (reversed) list of characters
on the current line *before* the cursor, a (normal) list of characters
on the current line *after* the cursor, and the remaining input.

> terminalAppearance :: String -> String
> terminalAppearance str = ta id "" "" str
>  where
>   ta f bs as ('\n':cs) = ta (\t -> f (reverse bs ++ as ++ '\n' : t)) "" "" cs
>   ta f bs as ('\r':cs) = ta f "" (reverse bs ++ as) cs
>   ta f (b:bs) as ('\b':cs) = ta f bs (b:as) cs
>   ta f ""     as ('\b':cs) = error "'\\b' at beginning of line"
>   ta f bs as (c:cs) | not (isPrint c) = error "invalid nonprinting character"
>                     | null as   = ta f (c:bs) ""        cs
>                     | otherwise = ta f (c:bs) (tail as) cs
>   ta f bs as "" = f (reverse bs ++ as)
