#include <stdio.h>
  
  int main(int argc, char** argv)
  {
      int i;
  
      if (argc < 2)
          printf("No arguments given.\n");
      else
      {
          printf("You gave the arguments:\n");
          for (i = 1; i < argc; i++)
          {
              printf("%d: %s\n", i - 1, argv[i]);
          }
      }
  }