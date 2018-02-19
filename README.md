# Dirty

A two-dimensional, lazily evaluated recreational language, implemented in Clean.

**Building:**

    clm -dynamics -nw -nci -nr <memory> -IL Dynamics -IL Platform -IL Generics -IL StdLib main -o dirty
	
Where `memory` is the memory profile that you want the interpreter to be run with.  
For example, if you wanted a maximum of 300MB memory usage, you could pass `-gci 250K -gcf 10 -h 250M -s 50M`.