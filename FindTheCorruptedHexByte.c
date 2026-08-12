#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

size_t find_corrupted_byte(size_t length, const char *const dump[length]){
  const char validChars[16] = {'A','B','C','E','F','0','1','2','3','4','5','6','7','8','9'};
  const size_t numOfValidChars = sizeof(validChars) / sizeof(validChars[0]);

  for (size_t i = 0; i < length; i++) {
    bool isValid = true;
    if(strlen(dump[i]) != 2){
      break;
    }
    for(size_t x = 0; x < 2; x++) {
        bool charValid = false;

        for(size_t y = 0; y < numOfValidChars; y++ ) {
          if(dump[i][x] == validChars[y]){
            printf("checking char %c found valid  %c \n", dump[i][x], validChars[y]);
            charValid = true;
            break;
        }
      }
      if (!charValid){
        isValid = false;
        break;
      }

    }
   if(!isValid){
     return i;
   }

  }
  return -1;
}

int main(void) {
  size_t len = 5;
  const char *dump[5] = {"4B", "65", "6", "6c", "6F"};
  find_corrupted_byte(len, dump);
  return 0;
}
