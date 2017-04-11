"""Looking glass. Currently only implemented for emacs. Vim comes later.

Usage:
  lg.py [--num-days=<days>] [--major-mode=<mode>]

Options:
  -h --help            Show this screen.
  --version            Show version.
  --major-mode=<mode>  Limit results to the specified Emacs major-mode.
  --num-days=<days>    Number of days in the past to consider [default: 10].

"""
from docopt import docopt

if __name__ == "__main__":
    arguments = docopt(__doc__, version='Naval Fate 2.0')
    print(arguments)
