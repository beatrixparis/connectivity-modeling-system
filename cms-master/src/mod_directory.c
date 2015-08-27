//****************************************************************************
//* System: Connectivity Modeling System (CMS)                               *
//* File : directory.c                                                       *
//* Last Modified: 2011-07-22                                                *
//* Code contributors: Judith Helgers, Ashwanth Srinivasan, Claire B. Paris  * 
//*                                                                          *
//* Copyright (C) 2011, University of Miami                                  *
//*                                                                          *
//* This program is free software: you can redistribute it and/or modify     *
//* it under the terms of the GNU Lesser General Public License as published *
//* by the Free Software Foundation, either version 3 of the License, or     *
//*(at your option) any later version.                                       *
//*                                                                          *
//* This program is distributed in the hope that it will be useful,          *
//* but WITHOUT ANY WARRANTY; without even the implied warranty of           *
//* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
//* See the Lesser GNU General Public License for more details.              *
//*                                                                          *
//* You should have received a copy of the GNU Lesser General                *
//* Public License along with this program.                                  *
//* If not, see <http://www.gnu.org/licenses/>.                              *
//****************************************************************************

#include <sys/stat.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

#include "mod_directory.h"

//make a directory with the name filename
void make_dir_(char * filename, int * len_file)
{
//   printf ("%s %i", filename, *len_file);
   int status, errno;
   char file[*len_file];
   substr(file, filename, 0, *len_file);

   status = mkdir(file,  0777);
   if (( status == -1 ) && (errno != EEXIST) )
      {
      perror("mkdir Error");
      unlink(file);
      exit(1);
      }
} 

//copy files from directory dir1 to dir2
void rename_file_ (char * dir1, char * dir2, int * len_dir1, int * len_dir2)
{
   int status, errno;
   char dirOld[*len_dir1];
   char dirNew[*len_dir2];

   substr(dirOld, dir1, 0, *len_dir1);
   substr(dirNew, dir2, 0, *len_dir2); 

   status = remove(dirNew);
   status = rename(dirOld, dirNew);

   if (status == -1) 
   {
    perror("rename Error");
    exit(1);
   }
}


substr(char dest[], char src[], int offset, int len)
{
  int i;
  for(i = 0; i < len && src[offset + i] != '\0'; i++)
	dest[i] = src[i + offset];
  dest[i] = '\0';
}


