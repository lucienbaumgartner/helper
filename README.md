# helper-functions for R and Python

## R

### regexify names

Some names include optional parts, such as abbreviated (Henry *G.* Ford) and plain second names (Henry *George* Ford), in brackets of without, double lastnames (Henry *Ford-Washington*), and many more.

This function creates a regex string with conditional expressions for said conditional parts in order to match all different variants of a name. The regex shown below would is an output created by the function (with R-escape characters of course, i.e. double backslashes).

![regex](/res/img/regex.png)
