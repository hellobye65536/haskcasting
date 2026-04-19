# Serialization/Deserialization Type A

This method requires the following addon mods to hexcasting:
- moreiotas
- hexical

## Bootstrapping

Running this package will print these sections of strings:
- `bootstrap0`, which is a list of patterns you have to manually write to get the `bootstrap0` spell.
- `bootstrap1`, which is a string you have to import.
  The string is broken into chunks, each of which can fit in a chat message or a book page.
  Concatenate all the chunks, and run `bootstrap0` with the final string on the top of the stack to get the `bootstrap1` spell.
- `bootstrap1 semicolon placeholders`, which is a list of indices in the `bootstrap1` spell that must be replaced with a semicolon string.
- `bootstrap1 halt jump placeholders`, which is a list of indices in the `bootstrap1` spell that must be replaced with a halting jump iota.
  A halting jump iota is an iota constructed by casting
  ```
  Vacant Reflection
  Iris' Gambit
  ```
  with the staff.
- `deserializerA`, which is a list of strings, which when passed as input to the `bootstrap1` spell, puts the full `deserializerA` spell on the stack.

## Importing Spells

Importing is similar to the `deserializerA` part of bootstrapping.
Serializing a spell prints a list of strings, then run `deserializerA` with that list of strings on the stack.

## Importing Strings

The recommended method for importing strings is by pasting them into a Book and Quill, putting the book on a lectern, and using `Reader's Purification` on the lectern to get the list of strings.
It's important to make sure that there are no extra newlines in each page.
