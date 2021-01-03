#ifndef UTILS_H
#define UTILS_H

#include <time.h>
#include <stdio.h>

void delay (unsigned int ms) {

   clock_t start_time = clock();
   while (clock() < start_time + ms*1000);

}

#endif//UTILS_H
