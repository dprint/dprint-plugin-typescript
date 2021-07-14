Tests are added into the `~/tests/specs` folder.

They have the format:

```
== description goes here ==
const    u    =     2;

[expect]
const u = 2;
```

- To add configuration, it applies per file, add something like `~~ lineWidth: 40 ~~` at the top of the file
- To only run a test, add the word "(only)" in parenthesis at the end of the test description.
- To run only a certain fail, add `_Only` (ex. `CatchClause_All_Only_.txt`) to the end of the file name and before the extension.
