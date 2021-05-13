#!/bin/bash
#=============================================================================#
 #                                                                           # 
 #                          R U N - T E S T S . S H                          # 
 #                                                                           # 
 #                                 M A L E F                                 # 
 #                                                                           # 
 #                           B A S H   S C R I P T                           # 
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


###############
## FUNCTIONS ##
###############

function error_report {
   echo -e "\033[31;1mERROR\033[0m: \033[2m$1 \033[0m!"
}


function yn_question {
   local answer
   read -p "$1 [y/n] " answer
   case $answer in
      [Yy]*)
         return 1
      ;;
      [Nn]*)
         return 0
      ;;
      *)
         yn_question "$1"
         return $?
      ;;
   esac
}


###################
## PREPRARATIONS ##
###################

FALSE=0
TRUE=1

# Whether we have to do all tests or not.
DO_ALL_TESTS=false

# We declare an array to store the names of the languages whose tests we want
# to take.
declare -a LANGUAGES
LANGUAGES=( )

# We loop through all arguments given by the user and work with them.
for arg in "$@"
do
   if [ $arg = "all" ]; then
      DO_ALL_TESTS=true
      break
   else
      LANGUAGES+=( $arg )
   fi
done


# We then find all the avaiable languages.
if [ ${#LANGUAGES[@]} -eq 0 ] || [ $DO_ALL_TESTS = "true" ]; then
   LANGUAGES=( )
   for file in $(ls)
   do
      if [ -d $file ] && ! [[ $file =~ "bin" ]] && [ $file != "logs" ] && [ $file != "danger_zone" ]; then
         LANGUAGES+=( $file )
      fi
   done
fi


# We get the operating system.
case "$OSTYPE" in
   linux-gnu)
      SYSTEM=linux
   ;;
   darwin)
      SYSTEM=unix
   ;;
   msys)
      SYSTEM=windows
   ;;
   win32)
      SYSTEM=windows
   ;;
   freebsd)
      SYSTEM=unix
   ;;
   cygwin)
      SYSTEM=windows
   ;;
   *)
      SYSTEM=unix
   ;;
esac

# We create the bin directory.
BIN=bin-$SYSTEM
if ! [ -d $BIN ]; then
   mkdir $BIN
fi


##################
## COMPILATIONS ##
##################

declare -a COMPILING_ERRORS
COMPILING_ERRORS=( )
PARENT="$(pwd)"
# Now that we have all the languages we start compilations.
echo -e "\033[34;1mCOMPILATIONS\033[0m:"
for lang in "${LANGUAGES[@]}"
do
   echo -e "\033[35m   $lang\033[0m"
   cd $lang
      bash compile.sh $SYSTEM || COMPILING_ERRORS+=( $lang )
   cd "$PARENT"
done

# We check for failed compilations.
if ! [ ${#COMPILING_ERRORS[@]} -eq 0 ]; then
   error_report "There were some failed compilations"
   for lang in "${COMPILING_ERRORS[@]}"
   do
      echo -e "\033[33m * \033[2m$lang\033[0m"
   done

   yn_question "Do you still want to proceed?"
   if [ $? -eq $FALSE ]; then
      exit 1
   fi

   # We remove the languages that have failed.
   for lang in "${COMPILING_ERRORS[@]}"
   do
      for i in "${!LANGUAGES[@]}"
      do
         if [[ ${LANGUAGES[i]} = $lang ]]; then
            unset 'LANGUAGES[i]'
         fi
      done
   done
fi


###########
## TESTS ##
###########


if ! [ ${#LANGUAGES[@]} -eq 0 ]; then
   echo -e "\n\033[34;1mTESTS\033[0m:"
   printf "" > data.log
fi

# We run every test of every file found in bin and that is known to have been
# successfully compiled and we store it in a log file.
for lang in "${LANGUAGES[@]}"
do

   echo -e "\033[35m   $lang\033[0m"
   "./$BIN/tests-$lang" 2>>data.log

done

# Finally we pass the log file.
awk -f analyzer.awk data.log


##############
## CLEAN UP ##
##############

# We move the files to another location.
if ! [ -d logs ]; then
   mkdir logs
   printf "0" > logs/.count
fi

# We increase the number in the count.
COUNT=$(cat logs/.count)
printf "$(echo $COUNT + 1 | bc)" > logs/.count
COUNT=$(cat logs/.count)

# We finally move the data to the new location.
mv data.log "logs/data.$COUNT.log"


###=======================#########################=========================###
##=======================## E N D   O F   F I L E ##=========================##
###=======================#########################=========================###
