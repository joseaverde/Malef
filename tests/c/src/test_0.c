#include "malef.h"
#include <stdio.h>
#include "utils.h"
#include <stdlib.h>


int main (void) {

   malef_error_t err;

   malef_initialize ();
   err = malef_initialize ();

   char* name;
   char* message;

   if (err) {
      malef_finalize ();
      printf ("An error with ID:%d has been raised!\n", err);
      if (malef_isFlyingError () ) {
         printf ("The error has been detected by the library!\n");
         printf ("Retrieving information..."); delay(1000); printf("\n");
         name = malef_getErrorName ();
         message = malef_getErrorMessage ();
         printf ("Name    : %s\nMessage : %s\n", name, message);
         printf ("Catching error..."); delay(500); printf("\n");
         malef_catchError ();
         printf ("Trying to free both strings..."); delay(250); printf("\n");
         free (name);
         free (message);

         printf("Trying to get another error message...\n");
         name = malef_getErrorName ();
         message = malef_getErrorMessage ();
         printf ("Name    : %s\nMessage : %s\n", name, message);
         
      } else {
         printf ("The error hasn't been detected by the library, aborting!\n");
         return 2;
      }
   } else {
      printf ("There is a problem with the ERROR library!\n");
      return 1;
   }

   printf("DONE!\n");
   return 0;
}
