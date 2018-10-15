import sys

if len(sys.argv) < 2:
    print('No arguments given.')
else:
    print('The arguments are:')
    i = 0
    for a in sys.argv[1:]:
        print(str(i) + ': ' + a)
        i = i + 1