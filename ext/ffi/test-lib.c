#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#ifdef _MSC_VER
# define EXPORT __declspec(dllexport)
#else
# define EXPORT
#endif

EXPORT int add(int x, int y)
{
  return x + y;
}

static void quicksort_(uintptr_t base,const size_t num,const size_t size
    ,void *temp,int (*compare)(const void *,const void *))
{
	size_t pivot = 0,first2last = 0,last2first = num-1;
	while(pivot+1 != num && !compare(base+size*pivot,base+size*(pivot+1))){
		pivot++;
	}
	if(pivot+1 == num){
		return;
	}
	if(0 > compare(base+size*pivot,base+size*(pivot+1))){
		pivot++;
	}
	while(first2last < last2first){
		while(0 < compare(base+size*pivot,base+size*first2last)
		    && first2last != num-1){
			first2last++;
		}
		while(0 >= compare(base+size*pivot,base+size*last2first)
		    && last2first){
			last2first--;
		}
		if(first2last < last2first){
			if(pivot == first2last || pivot == last2first){
				pivot = pivot^last2first^first2last;
			}
			memcpy(temp,base+size*first2last,size);
			memcpy(base+size*first2last,base+size*last2first,size);
			memcpy(base+size*last2first,temp,size);
		}
	}
	quicksort_(base,first2last,size,temp,compare);
	quicksort_(base+size*first2last,num-first2last,size,temp,compare);
}

EXPORT int quicksort(void *base, const size_t num, const size_t size,
	      int (*compare)(const void *, const void *))
{
  void *temp = malloc(size);
  if(!temp){
    return -1;
  }
  quicksort_((uintptr_t)base,num,size,temp,compare);
  free(temp);
  return 0;
}


static int compare_integer(const void *a,const void *b)
{
  return *(char *)a-*(char *)b;
}

struct data_to_store
{
  int value1;
  struct {
    int value2;
    char *str;
  } inner;
};

EXPORT void store_data(struct data_to_store *storage)
{
  storage->value1 = 100;
  storage->inner.value2 = 200;
  storage->inner.str = "message from C";
}


int main(void)
{
  char array[8] = {6,6,1,4,2,9,3,7};
  int errcode = quicksort(array, 8, sizeof(char),compare_integer);
  size_t counter = 0;

  if(errcode < 0){
    fputs("Error!\n",stderr);
    return -1;
  }
  while(counter != 8){
    printf("%d ",array[counter]);
    counter++;
  }

  return 0;
}
