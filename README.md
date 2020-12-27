A simple command line utility for convert images, preferably of goku, into ascii art.

Installation/Usage:

```
git clone https://github.com/gnumonik/hascii.git
cd ./hascii
stack build
stack exec hascii -- -i /PATH/TO/SOURCE_IMAGE -s INTEGER_SCALE_FACTOR -o /PATH/TO/OUTPU_FILE.TXT
```

The -s and -o arguments are optional. Scaling defaults to 1, and no outputfile causes the image to be printed to the terminal. 

Works better at higher scale factors, however, pretty slow with massive images due to the way binarization works. 
