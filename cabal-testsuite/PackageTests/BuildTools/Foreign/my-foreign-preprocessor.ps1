get-content $args[1] | %{$_ -replace "0","1"} > $args[2]
