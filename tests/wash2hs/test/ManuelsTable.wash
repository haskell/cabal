module Main where

import Prelude hiding (map, span, head, div)
import CGI

{-- 
    ms> -----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----
    ms> (define (fact n)
    ms>    (if (= n 0)
    ms>        1
    ms>        (* n (fact (- n 1)))))

    ms> (define (make-fact-row n)
    ms>    (tr (td :align 'center (bold n))
    ms>        (td :align 'right (it (fact n)))))

    ms> (define (make-fact-table n)
    ms>    (apply table :border 1
    ms> 	  (tr (th "n=") (th "fact"))
    ms> 	  (map make-fact-row (upto 3 n))))

    ms> (font :size -1 (make-fact-table 11))
    ms> -----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----
-}

fact n =
  if n == 0
     then 1
     else n * fact (n - 1)

makeFactRow n =
  <tr>
    <td align="center"><b> <%= show n %> </b></td>
    <td align="right"><i> <%= show (fact n) %> </i></td>
  </tr>

makeFactTable n =
  <table border="1">
    <tr><th>n=</th> <th>fact</th></tr>
    <% mapM makeFactRow [3..n] %>
  </table>

main = 
  run $ standardQuery "FactTable" $
  <font size="-1"><% makeFactTable 11 %></font>
