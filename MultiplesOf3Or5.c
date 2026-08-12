#include <stdio.h>
#include <stdlib.h>

int solution(int number) {
  if (number < 3){
    return 0;
  } else {
    int sum = 0;
    while(number >= 3){
        if (number % 3 == 0 || number % 5 == 0){
          sum += number;
          number --;
        } else {
          number--;
        }
    }
    return sum;
  }
}

int main(void){
  int num = 3;
  printf("the number was %d and the solution was %d", num, solution(num));
  return 0;
}
