#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

int compare(const void* a, const void* b) {
    return (*(int*)a - *(int*)b);
}



void sort_arry (size_t n, int arr[n]){
  int oddList[n];
  size_t oddCount = 0;

  for (size_t i = 0; i < n; i++){
    printf("the value in the arry is %d \n", arr[i]);
    if (arr[i] % 2 != 0) {
      oddList[oddCount] = arr[i];
      oddCount++;

    }
  }


  qsort(oddList, oddCount, sizeof(int), compare);

  size_t oddIndex = 0;
  for (size_t i = 0; i < n; i++){
    if (arr[i] % 2 != 0) {
      arr[i] = oddList[oddIndex];
      oddIndex++;
    }
  }
}

int main (void) {

  int arry[] = {5, 8, 6, 3, 4};
  int size = sizeof(arry)/sizeof(arry[0]);

  for(int i = 0; i < size; i++){
    printf("vaule of the arry at %d is %d \n", i, arry[i]);
  }
  sort_arry(size,arry);

  for(int i = 0; i < size; i++){
    printf("the vaule of the sorted arry at %d is %d \n", i, arry[i]);
  }
  return 0;
}
