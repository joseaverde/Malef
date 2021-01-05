#include "malef.h"
#include <stdio.h>

void* wrappedMain (void* name) {

   printf("Hello %s!\n", (char*)name);

   return NULL;
}

int main (void) {

   printf("Write your name: ");
   char name[512];
   scanf("%s", name);

   void* ret;
   malef_wrapper(wrappedMain, (void*)name, &ret);

}
