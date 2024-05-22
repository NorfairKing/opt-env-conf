# Optparse Template

This is a template implementation of commands, flags, options, environment variable and configuration file parsing according to best practices.

## License

This template is **not** free to use.
See https://template.cs-syd.eu/template/NorfairKing/template-optparse for more information.

Copyright (c) 2020-2024 Tom Sydney Kerckhove.

All Rights Reserved.

## Instructions

You can build this template code using `stack build` according to the provided `stack.yaml` file.
You can also use cabal if you prefer, as long as you can get the appropriate dependencies in place.

To use this template, first choose whether your program will use one command or more commands.
For the multi-command version, use the `src/OptParse.hs` and `test/OptParseSpec.hs` files.
Otherwise, use the `src/OptParseOneCommand.hs` and `test/OptParseOneCommandSpec.hs` files.
Copy the modules into your project and follow instructions inside.

You can use [template-filler](https://github.com/NorfairKing/template-filler) to copy the template into your project, like this:

```
template-filler --source /path/to/the/OptParse.hs --destination /path/to/your/homeless/shelter/OptParse.hs --find FooBar --replace HomelessShelter
```

Run `stack haddock` to read the instructions in haddock form.
Otherwise, continue reading in [the source](src/OptParse.hs) and [the tests](test/OptParseSpec.hs).
