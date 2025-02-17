(* {xinvalid markup} *)

(* valid markup {but no space} *)

(* valid markup {b this time with space} *)

(* A variant name: {Cold} *)

(* A variant name: {Red} *)

(* A variant name: {Lock} *)

(* In a bad spot: {vneeds spaces around and inside v} *)

(* Not closed: {v *)

(* Markup contains numbered list: {i O(log n)} *)

(* Here is an @ and here are two @@ and here's a bad tag @yo *)

(* {01 very important} *)

(* [     code
    with newline     ] *)

(* We want nested bulleted lists.
   - Here's one layer of the list
   - It can have several bullets
     + Here's another layer of the list
     + It, too, can have several bullets
       + And a third
     + Then return to the old list
   - And all the way back

   Now this is a paragraph

   - Now it's a list
     - One more time.
     Note this won't be part of this first bullet again --- that probably makes sense.
*)

(* - If the first line of a comment is a list
   - This should work, as long as we handle the comment sigil correctly
*)

(* 
   - This should even be true
   - If there is a blank line before the first line
*)

(* We want big things to be able to go inside lists nicely.
   - This is a list.
   - One of the bullets
     {v has a vertabim block v}
     inside.
   - Then there's another bullet.
*)

(* Also for explicit lists
   {ul
   {- This is a list.}
   {- One of the bullets

      has some paragraphs

      inside}
   {- Then there's another bullet.}
   }
*)

(* Note the trailing whitespace:

   {v
   no space
   some space    
   some more space    v}
*)

(* Numbered list examples:

   1. Top-level
   2. Second bullet
     - With a nested
     - numbered list
  - Or a not nested bulleted list
     (a) with a nested numbered list
   (b) this is still nested

   a) This is a new list

   Avoid breaking the line if list-like: 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1.

   A) We can also have a numbered list with line breaks between the bullets

   B) You can see this is true from the indentation of the seeeeeeeeeeeeeeeeeeeecond liiiiiiiiiiiiiiiiiiiine

   But this paragraph is already not part of the list

   C) And so this is not a third list item, but just a normal paragraph (see
      indeeeeeeeeeeeeeeeeeeeentation)

   (* CR-someday comments: Fix this edge case *)

   here's a funny edge case:
   a) + b)

   big num: 999999999999999999999999999999.

   big float: 999999999999999999999999999999.0
*)

(** {xinvalid markup} *)

(** valid markup {but no space} *)

(** valid markup {b this time with space} *)

(** A variant name: {Cold} *)

(** A variant name: {Red} *)

(** A variant name: {Lock} *)

(** In a bad spot: {vneeds spaces around and inside v} *)

(** Not closed: {v *)

(** Markup contains numbered list: {i O(log n)} *)

(** Here is an @ and here are two @@ and here's a bad tag @yo *)

(** {01 very important} *)

(** [     code
     with newline     ] *)

(** We want nested bulleted lists.
    - Here's one layer of the list
    - It can have several bullets
      + Here's another layer of the list
      + It, too, can have several bullets
        + And a third
      + Then return to the old list
    - And all the way back

    Now this is a paragraph

    - Now it's a list
      - One more time.
      Note this won't be part of this first bullet again --- that probably makes sense.
*)

(** - If the first line of a comment is a list
    - This should work, as long as we handle the comment sigil correctly
*)

(** 
   - This should even be true
   - If there is a blank line before the first line
*)

(** We want big things to be able to go inside lists nicely.
    - This is a list.
    - One of the bullets
      {v has a vertabim block v}
      inside.
    - Then there's another bullet.
*)

(** Also for explicit lists
    {ul
    {- This is a list.}
    {- One of the bullets

       has some paragraphs

       inside}
    {- Then there's another bullet.}
    }
*)

(** Note the trailing whitespace:

    {v
    no space
    some space    
    some more space    v}
*)

(** Numbered list examples:

    1. Top-level
    2. Second bullet
      - With a nested
      - numbered list
   - Or a not nested bulleted list
      (a) with a nested numbered list
    (b) this is still nested

    a) This is a new list

    Avoid breaking the line if list-like: 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1.

    A) We can also have a numbered list with line breaks between the bullets

    B) You can see this is true from the indentation of the seeeeeeeeeeeeeeeeeeeecond liiiiiiiiiiiiiiiiiiiine

    But this paragraph is already not part of the list

    C) And so this is not a third list item, but just a normal paragraph (see
       indeeeeeeeeeeeeeeeeeeeentation)

    here's a funny edge case:
    a) + b)

    big num: 999999999999999999999999999999.

    big float: 999999999999999999999999999999.0
*)

(*_ {xinvalid markup} *)

(*_ valid markup {but no space} *)

(*_ valid markup {b this time with space} *)

(*_ A variant name: {Cold} *)

(*_ A variant name: {Red} *)

(*_ A variant name: {Lock} *)

(*_ In a bad spot: {vneeds spaces around and inside v} *)

(*_ Not closed: {v *)

(*_ Markup contains numbered list: {i O(log n)} *)

(*_ Here is an @ and here are two @@ and here's a bad tag @yo *)

(*_ {01 very important} *)

(*_ [     code
     with newline     ] *)

(*_ We want nested bulleted lists.
    - Here's one layer of the list
    - It can have several bullets
      + Here's another layer of the list
      + It, too, can have several bullets
        + And a third
      + Then return to the old list
    - And all the way back

    Now this is a paragraph

    - Now it's a list
      - One more time.
      Note this won't be part of this first bullet again --- that probably makes sense.
*)

(*_ - If the first line of a comment is a list
    - This should work, as long as we handle the comment sigil correctly
*)

(*_ 
   - This should even be true
   - If there is a blank line before the first line
*)

(*_ We want big things to be able to go inside lists nicely.
    - This is a list.
    - One of the bullets
      {v has a vertabim block v}
      inside.
    - Then there's another bullet.
*)

(*_ Also for explicit lists
    {ul
    {- This is a list.}
    {- One of the bullets

       has some paragraphs

       inside}
    {- Then there's another bullet.}
    }
*)

(*_ Note the trailing whitespace:

    {v
    no space
    some space    
    some more space    v}
*)

(*_ Numbered list examples:

    1. Top-level
    2. Second bullet
      - With a nested
      - numbered list
   - Or a not nested bulleted list
      (a) with a nested numbered list
    (b) this is still nested

    a) This is a new list

    Avoid breaking the line if list-like: 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1.

    A) We can also have a numbered list with line breaks between the bullets

    B) You can see this is true from the indentation of the seeeeeeeeeeeeeeeeeeeecond liiiiiiiiiiiiiiiiiiiine

    But this paragraph is already not part of the list

    C) And so this is not a third list item, but just a normal paragraph (see
       indeeeeeeeeeeeeeeeeeeeentation)

    here's a funny edge case:
    a) + b)

    big num: 999999999999999999999999999999.

    big float: 999999999999999999999999999999.0
*)
