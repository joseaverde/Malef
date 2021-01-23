#!/usr/bin/awk -f
#=============================================================================#
 #                                                                           # 
 #                          A N A L Y Z E R . A W K                          # 
 #                                                                           # 
 #                                 M A L E F                                 # 
 #                                                                           # 
 #                            A W K   S C R I P T                            # 
 #                                                                           # 
 #---------------------------------------------------------------------------# 
 #     Copyright (c) 2021 José Antonio Verde Jiménez All Rights Reserved     # 
 #---------------------------------------------------------------------------# 
 # This file is part of Malef.                                               # 
 #                                                                           # 
 # This program is free software:  you  can redistribute it and/or modify it # 
 # under  the terms  of the  GNU  General License  as published by the  Free # 
 # Software  Foundation,  either  version 3  of  the  License,  or  (at your # 
 # opinion) any later version.                                               # 
 #                                                                           # 
 # This  program  is distributed  in the  hope that  it will be  useful, but # 
 # WITHOUT   ANY   WARRANTY;   without   even  the   implied   warranty   of # 
 # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General # 
 # Public License for more details.                                          # 
 #                                                                           # 
 # You should have received  a copy of the  GNU General Public License along # 
 # with this program. If not, see <https://www.gnu.org/licenses/>.           # 
 #                                                                           # 
#=============================================================================#

#=========#
## BEGIN ##
#=========#

BEGIN {

   # === AWK settings === #
   # The line is separated by tabs.
   #
   FS = "[\t]" ;

   # === Language settings === #
   # This is the meaning of the following variables:
   #
   #  $current_language : Tells which is the number of the current language.
   #  $n_languages      : Tells how many languages are there.
   #  $language         : Tells the string (the name) of the current language.
   #  $languages        : Array with Integer Indexes with the names of the
   #                      languages found so far.
   #
   current_language = 0 ;
   n_languages = 0 ;
   language = "none" ;

   # === Package settings === #
   # This is the meaning of the following variables.
   #
   #  $packages : An array with String indexes (the names of the languages) of
   #              other arrays.
   #              The arrays inside each item from the package array, start
   #              with an integer at "-1" telling the size of the array, and
   #              each of the elements of the array are also arrays.
   #              The arrays inside the arrays of the item package array,
   #              contain information about the test.
   #
   #     packages = ["ada"=[sub_arr], "c"=[sub_arr], "python3"=[sub_arr], ...]
   #     sub_arr  = [-1=<length>, 0=["name"=<str>, "count"=<int>, test, ...],
   #                 1, 2, 3, ..., length - 1]
   #     test     = ["status"=(passed/failed/skipped/fatal/known-to-fail/user),
   #                 "name"=<str>, info]
   #     info     = [if passed  => "time"=(-1/<int>)]
   #                [if fatal   => none]
   #                [if skipped => none]
   #                [else       => "expected"=<str>, "got"=<str>]
   #
   #  $report   : An array with String indexes (the names of the languages),
   #              that contains information about the number of passed and
   #              failed tests. Each of it's items is also an array with
   #              the following structure:
   #     ["passed"=<int>, "failed"=<int>, "skipped"=<int>, "fatal"=<int>,
   #      "known-to-fail"=<int>, "user"=<int>]
   #
   #  $passed        : The total number of passed tests.
   #  $failed        : The total number of failed tests.
   #  $skipped       : The total number of skipped tests.
   #  $fatal         : The total number of fatal tests.
   #  $known_to_fail : The total number of tests that are known to fail.
   #  $user          : The total number of tests that my be the user's fault.
   #
   #  $package         : The name of the current package.
   #  $current_package : The position of the current package.
   #
   passed = 0 ;
   failed = 0 ;
   skipped = 0 ;
   fatal = 0 ;
   known_to_fail = 0 ;
   user = 0 ;
   package = "none";
   current_package = 0;

}



#========#
## BODY ##
#========#

# ==== LANGUAGES ============================================================ #
/language/ {
   # If the first word of the line is "language" then it means that we have to
   # change the language settings.

   # First we check if the language is found in the list of already used
   # languages.
   language = $2
   if ( n_languages == 0 ) {
      current_language = 0 ;
      languages[n_languages++] = language ;
      packages[language][-1] = 0 ;
   } else {
      # We look in the stored languages and check if the language has already
      # been set.
      current_language = -1 ;
      for ( i = 0 ; i < n_languages ; i++ ) {
         if ( languages[i] == language ) {
            # If so, we do nothing.
            current_language = i ;
            break ;
         }
      }
      if ( current_language == -1 ) {
         # If not, we add a new language.
         current_language = n_languages++ ;
         languages[current_language] = language ;
         packages[language][-1] = 0;
      }
   }
}


/package/ {
   # If the first word is "package" then it's telling which package we are
   # testing right now.
   package = $2 ;
   current_package = packages[language][-1]++ ;
   packages[language][current_package]["name"] = package ;
   packages[language][current_package]["count"] = 0 ;

}


/failed/ {
   # If the test has failed we first have to check why it have failed.
   next_test = packages[language][current_package]["count"]++ ;
   expected = $3 ;
   got = $4 ;
   packages[language][current_package][next_test]["name"] = $2 ;
   packages[language][current_package][next_test]["expected"] = expected ;
   packages[language][current_package][next_test]["got"] = got ;

   if ( got ~ /\$/ ) {
      # This test has not actually failed, but it's an unimportant error.
      # A known error.
      status = "known-to-fail" ;
      known_to_fail++ ;
      
   } else if ( expected ~ /\@./ || got ~ /\@./ ) {
      # This test was done by the user so if it failed it can be a human-error.
      status = "user" ;
      user++ ;

   } else {
      # It's a normal failed test.
      status = "failed" ;
      failed++ ;
   }

   packages[language][current_package][next_test]["status"] = status ;
   report[language][status]++ ;
}


/passed/ {
   # If the test has passed we append it to the tests inside the packages.
   next_test = packages[language][current_package]["count"]++ ;
   packages[language][current_package][next_test]["status"] = "passed" ;
   packages[language][current_package][next_test]["name"] = $2 ;
   if ( $3 ~ /.null./ ) {
      packages[language][current_package][next_test]["time"] = -1 ;
   } else {
      packages[language][current_package][next_test]["time"] = $3 ;
   }

   # We update the statistics.
   report[language]["passed"]++ ;
   passed++ ;
}


/fatal/ {
   # A fatal test is a very important one, that isn't supposed to happen, which
   # can break lots of things.
   next_test = packages[language][current_package]["count"]++ ;
   packages[language][current_package][next_test]["status"] = "fatal" ;
   packages[language][current_package][next_test]["name"] = $2 ;
   report[language]["fatal"]++ ;
   fatal++ ;
}


/skipped/ {
   # A test that has been deliberately skipped.
   next_test = packages[language][current_package]["count"]++ ;
   packages[language][current_package][next_test]["status"] = "skipped" ;
   packages[language][current_package][next_test]["name"] = $2 ;
   report[language]["skipped"]++ ;
   skipped++ ;
}


#=======#
## END ##
#=======#

END {

   title["passed"]        = "\033[32mPASSED       \033[0m" ;
   title["failed"]        = "\033[31mFAILED       \033[0m" ;
   title["skipped"]       = "\033[33mSKIPPED      \033[0m" ;
   title["fatal"]         = "\033[31;2mFATAL      \033[0m  " ;
   title["user"]          = "\033[34mFAIL-BY-USER \033[0m" ;
   title["known-to-fail"] = "\033[35mKNOWN-TO_FAIL\033[0m" ;
   # We loop through all languages.
   for ( l = 0 ; l < n_languages ; l++ ) {
      language = languages[l] ;
      printf "\033[36mLANGUAGE\033[0m: \033[34m%s\033[0m\n", language ;
      # We loop through all packages inside the language.
      for ( p = 0 ; p < packages[language][-1] ; p++ ) {
         package = packages[language][p]["name"] ;
         printf "   \033[36mPACKAGE\033[0m: \033[33m%s\033[0m\n", package ;
         # We loop thtough all tests.
         for ( t = 0 ; t < packages[language][p]["count"] ; t++ ) {
            printf "      %s", title[packages[language][p][t]["status"]];
            switch (packages[language][p][t]["status"]) {
               case "passed":
                  if ( packages[language][p][t]["time"] != -1 ) {
                     printf "\t\033[36;2mTIME\033[0m=%f",
                            packages[language][p][t]["time"] ;
                  } else {
                     printf "\t\t" ;
                  }
                  break ;
               case "skipped":
               case "fatal":
                  break ;
               default:
                  printf "\t\033[36;2mEXPECTED\033[0m=%s" \
                         "\t\033[36;2mGOT\033[0m=%s\t",
                         packages[language][p][t]["expected"],
                         packages[language][p][t]["got"] ;
                  break ;
            }
            printf "\t\033[36mNAME\033[0m=%s\n",
                   packages[language][p][t]["name"] ;
         }
         printf "\n" ;
      }
      printf "\n\n" ;
   }

   printf "\033[36mREPORT\033[0m:\n" ;
   print "  ", title["skipped"]      , skipped ;
   print "  ", title["passed"]       , passed  ;
   print "  ", title["user"]         , user    ;
   print "  ", title["known-to-fail"], known_to_fail ;
   print "  ", title["failed"]       , failed  ;
   print "  ", title["fatal"]        , fatal   ;
   printf "\n";

   printf "\033[36mSTATUS\033[0m: "

   if ( fatal != 0 || failed != 0 ) {
      printf "\033[31;1mFAILED\033[0m!\n" ;
      exit fatal + failed ;
   } else {
      printf "\033[32;1mPASSED\033[0m!\n" ;
      exit 0 ;
   }
}


###=======================#########################=========================###
##=======================## E N D   O F   F I L E ##=========================##
###=======================#########################=========================###
