# Serialization/Deserialization Type A

This method requires the following addon mods to hexcasting:
- moreiotas
- hexical

## Bootstrapping

Running this package will print three sections of strings:
- `bootstrap0`, which is a list of patterns you have to manually write to get the `bootstrap0` spell.
- `bootstrap1`, which is a string you have to import.
  Note that the string is too long to fit into one chat message, so it's necessary to break it into two parts and concatenate them after importing.
  Then, with the provided string on top of the stack, run `bootstrap0` to get the `bootstrap1` spell.
- `deserializerA`, which is a list of strings. Starting with an empty stack, for each string from first to last, add that string to the top of the stack, and run `bootstrap1`.
  At the end, the full `deserializerA` spell should be on top of the stack.

## Importing Spells

Importing is similar to the `deserializerA` part of Bootstrapping.
Serializing a spell prints a list of strings, and for each string from first to last, add that string to the top of the stack, and run `deserializerA`.
