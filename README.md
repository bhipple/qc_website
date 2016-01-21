# QC Website
This server displays ranges of git commmits for projects. Its current use is to display all commits for all services that are currently in our QC system and waiting to be deployed to production.

![QC Website](https://raw.githubusercontent.com/brhCS/qc_website/master/img/screenshot.jpg)

## Input format
See `test/test_txt_generator.sh` for the cmdline git log flags to use to generate messages in the correct format.

Not included is a system to tie packages on production to a sha, packages on beta to a sha, and generate an output `.tar.gz` of all `.txt` files with the commit data for that sha range.  See the test suite for details on how this might be done.

## Configuration Parameters
Setup `config.lisp` with these parameters:
```
*github*                Link to github
*txt-dir*               Directory where .txt files will be extracted
*commit-location*       Where to look for new commits
*archive-location*      Where to move archives after processing
```

## Building and Running
If not already done once, run `install.sh`.

Then cd into `src`, start a REPL with `server.lisp` and call `(start)`

## TODO
* Package with `quickproject`
* Build with `buildapp`
* Makefile and test automation
* Isolate in container with Docker
* Chef recipe
* TravisCI

## Logos
Credit for the logos goes to Conrad Barski at [lisperati.com](http://lisperati.com)
