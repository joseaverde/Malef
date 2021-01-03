#include "malef.h"
#include <stdio.h>

void wrappedMain (void* name) {

   printf("Hello %s!\n", (char*)name);

   return;
}

int main (void) {

   printf("Write your name: ");
   char name[512];
   scanf("%s", name);

   malef_wrapper(wrappedMain, (void*)name);

}
