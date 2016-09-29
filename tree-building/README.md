# Tree Building

Refactor a tree building algorithm.

Some web-forums have a tree layout, so posts are presented as a tree. However
the posts are typically stored in a database as an unsorted set of records. Thus
when presenting the posts to the user the tree structure has to be
reconstructed.

Your job will be to refactor a working but slow and ugly piece of code that
implements the tree building logic for highly abstracted records. The records
only contain an ID number and a parent ID number. The ID number is always
between 0 (inclusive) and the length of the record list (exclusive). Every record
has a parent ID lower than its own ID except the root record which
has ID 0 and parent ID -1.



## Submitting Incomplete Problems
It's possible to submit an incomplete solution so you can see how others have completed the exercise.

