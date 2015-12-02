# hunchentoot
This server displays ranges of git commmits for projects. Its current use is to display all commits for all services that are currently in our QC system and waiting to be deployed to production.

## Input format
Currently, it looks for all `*.txt` files in its configured directory.  Each file is assumed to be a commit range for a particular repository, withe name of the file corresponding to the name of the repository.

Each line in the file is a commit to display, in the format:
```
AuthorName|SHA|Date|Message
```

## Startup
Just compile and run server.lisp.  There's some funkiness at the moment where you have to call (ql:quickload "hunchentoot") manually in the REPL that I haven't figured out yet.

## Logos
Credit for the logos goes to Conrad Barski at [lisperati.com](http://lisperati.com)
