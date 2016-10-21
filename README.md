# bank-ocr

Solutions to [Bank OCR Code Kata](http://codingdojo.org/cgi-bin/index.pl?KataBankOCR)
User Stories 1, 2, 3, and 4.

## Setup and Use

1. Install [stack](http://docs.haskellstack.org/en/stable/README.html).
2. Clone and `cd` into the repository.
3. `stack setup` to install GHC.
4. `stack build` to compile. 
5. `rm data/output.txt` to clear previous output.
6. Adjust `data/input.txt` if desired.
7. `stack exec bank-ocr` to run.
8. Check `data/output.txt` for decoded account numbers.
