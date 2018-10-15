#include <stdio.h>
#include <stdlib.h>

#define LEN(arr) ((int) (sizeof (arr) / sizeof (arr)[0]))

int main(int argc, char** argv){

    double factorial(double n){
        if(n > 1.0){
            return n * factorial(n-1.0);
        }
        else
            return 1.0;
    }

    if (argc < 3)
        printf("Not enough arguments given to calculate bracelets\n");
    else {
        double BC;
        double CC;
        sscanf(argv[1], "%lf", &BC);
        sscanf(argv[2], "%lf", &CC);

        double COM = factorial(BC) / (factorial(BC-CC) * factorial(CC));
        if (COM <= 0){
            COM = factorial(CC) / (factorial(CC-BC) * factorial(BC));
        }
        printf("Number of Unique Bracelets: ");
        printf("%lf\n", COM);
    }
}