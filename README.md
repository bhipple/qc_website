# QC Website
This server displays ranges of git commmits for projects. Its current use is to display all commits for all services that are currently in our QC system and waiting to be deployed to production.

## Input format
Currently, it looks for all `*.txt` files in its configured directory.  Each file is assumed to be a commit range for a particular repository, withe name of the file corresponding to the name of the repository.

Each line in the file is a commit to display, in the format:
```
AuthorName|SHA|Date|Message
```
See `test/test_txt_generator.sh` or the cmdline git log flags to use to generate messages in this format.

## Building and Running
If not already done once, run `install.sh`.

Then simply start a REPL with `server.lisp`, load everything twice to make sure all functions are defined with the macros properly, and then call `(main)`.

## Logos
Credit for the logos goes to Conrad Barski at [lisperati.com](http://lisperati.com)
