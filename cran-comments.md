## Test Environments

* Local MacOS 15.7.4 install, R 4.6
* macos-latest,   r: 'release'
* windows-latest, r: 'release'
* ubuntu-latest,   r: 'devel', http-user-agent: 'release'
* ubuntu-latest,   r: 'release'
* ubuntu-latest,   r: 'oldrel-1'

## R CMD check results

── R CMD check results ───────────────────────────── fable.bayesRecon 0.1.0 ────
Duration: 1m 32.9s

❯ checking CRAN incoming feasibility ... [3s/13s] NOTE
  Maintainer: ‘Dario Azzimonti <dario.azzimonti@gmail.com>’
  
  New submission

0 errors ✔ | 0 warnings ✔ | 1 note ✖


## Resubmission

This is a resubmission. In response to the previous CRAN review:

* Added \value tags to bayesRecon_BUIS.Rd, bayesRecon_MixCond.Rd, and 
  bayesRecon_t.Rd describing the class and structure of the returned 
  model specification objects.
* Added small executable \examples{} blocks to all three exported 
  reconciliation functions, using a reduced subset of tsibble::tourism 
  to keep runtime under a few seconds.
* Changed title in DESCRIPTION to conform with CRAN title case requirements;
  Updated Description field in the DESCRIPTION file to address Uwe's comments.
* In `bayesRecon_MixCond.Rd`, replaced the unnecessary `\dontrun{}` wrapping by 
  unwrapping the portion of the example because it is executable in < 5 sec. 

